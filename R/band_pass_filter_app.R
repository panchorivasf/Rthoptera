#' Band-pass Filter Shiny App
#'
#' This function launches a Shiny app that allows users to visualize either the mean spectrum or the spectrogram before applying a frequency filter to a Wave object.
#'
#' @return A Shiny app interface for applying a frequency filter.
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the app
#' band_pass_filter_app()
#'
#' # Example usage after launching the app:
#' # 1. Select a wave object from the R environment.
#' # 2. Specify the the limits of the band-pass filter (high-pass, low-pass) in kHz, as needed.
#' # 3. Apply the filter and save the new wave object.
#' }
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom plotly plotlyOutput plot_ly layout config style add_ribbons
#' @importFrom seewave meanspec spectro ffilter duration
#' @importFrom bslib bs_theme
#' @importFrom shinycssloaders withSpinner


band_pass_filter_app <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui = function(request) {
    tagList(
      h1("Band-pass Filter", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        theme = bslib::bs_theme(bootswatch = "darkly"),
        tags$head(tags$style(
          HTML(
            "
                 body {
                margin: 5px; /* Adds margin around the entire page */
              }

                #audioPlot {
                height: calc(100vh - 120px); /* Adjusts height taking into account other elements */
                width: 100%;
              }
              .btn-group-vertical > .btn {
                margin-bottom: 5px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 3px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 2px; /* Reduces horizontal space between inputs */
              }

                   #plotMeanSpectrum {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
              }

                     #plotSpectro {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
              }
                 #saveEditedWave {
              border: 2px solid dodgerblue; /* Blue contour */
              border-radius: 5px; /* Optional: Rounded corners */
                 }

                 #close {
              border: 2px solid red; /* Red contour */
              padding: 5px 10px; /* Optional: Adjust padding */
              border-radius: 5px; /* Optional: Rounded corners */
              }
              "
          )
        )),

        fluidRow(
          column(3,
                 selectInput("selectedWave", "Select a wave object:",
                             choices = NULL, width = '100%')
          ),
          column(1, verticalLayout(
            actionButton("plotMeanSpectrum", "Mean Spectrum"),
            actionButton("plotSpectro", "Spectrogram")
          )
          ),
          column(1, verticalLayout(
            numericInput("highpass", "HPF (kHz)", value = NULL, min = 0),
            numericInput("lowpass", "LPF (kHz", value = NULL, min = 1)
          )
          ),
          column(1, actionButton("applyFilter", "Apply Filter")
          ),
          column(2, verticalLayout(
            textInput("newName", "Name for new wave:", value = ""),
            actionButton("saveEditedWave", "Save")
          )),
          column(1, actionButton("close", "Close App")),
        ),
        fluidRow(
          column(12,
                 div(style = "margin-top: 15px;",
                     withSpinner(plotlyOutput("audioPlot", height = "520px", width = "1480px"))))
        )
      )
    )
  }

  server = function(input, output, session) {

    waveObject <- reactiveVal(NULL)
    # plotly_obj <- reactiveVal()

    spectrogram_plotly <- function(wave,
                                   floor = -50,
                                   background = '#274C77',
                                   foreground = "white",
                                   hover_bgcolor = "white",
                                   hover_fontcolor = "black") {



      wl = round(wave@samp.rate * sqrt(seewave::duration(wave))*20e-4)
      # coerce wl to an even number
      if (wl %% 2 != 0) {
        wl <- wl + 1
      }

      # win_len <- 0.001 * wave@samp.rate
      spect <- wave |>
        seewave::spectro(
          wl = wl,
          ovlp = 95,
          zp = 200,
          plot = FALSE
        )

      colnames(spect$amp) <- spect$time
      rownames(spect$amp) <- spect$freq

      spect_df <- spect$amp |>
        as_tibble(rownames = "freq") |>
        pivot_longer(
          -freq,
          names_to = "time",
          values_to = "amp"
        ) |>
        mutate(
          freq = as.numeric(freq),
          time = as.numeric(time)
        )

      spect_df_floor <- spect_df |>
        mutate(
          amp_floor = ifelse(amp < floor, floor, amp)
        )

      spect_plot <- plot_ly(
        data = spect_df_floor,
        x = ~time,
        y = ~freq,
        z = ~amp_floor,
        type = "heatmap",
        colorscale = list(c(0, background), c(1, foreground)),
        zmin = floor,
        zmax = max(spect_df$amp),
        hoverinfo = "x+y",
        hovertemplate = paste(
          "Time: %{x:.3f} s<br>",
          "Freq: %{y:.1f} kHz<extra></extra>"
        ),
        showscale = FALSE
      ) %>%
        layout(
          xaxis = list(
            title = "Time (s)",
            titlefont = list(size = 10, color = foreground),
            tickfont = list(size = 10, color = foreground),
            tickcolor = foreground,
            linecolor = foreground,
            mirror = TRUE
          ),
          yaxis = list(
            title = "Frequency (kHz)",
            titlefont = list(size = 10, color = foreground),
            tickfont = list(size = 10, color = foreground),
            tickcolor = foreground,
            linecolor = foreground,
            mirror = TRUE
          ),
          paper_bgcolor = background,
          plot_bgcolor = background,
          margin = list(t = 25, r = 15, b = 55, l = 25),
          title = "",
          showlegend = FALSE
        ) %>%
        style(
          hoverlabel = list(
            bgcolor = hover_bgcolor,
            font = list(color = hover_fontcolor)
          )
        )

      return(spect_plot)
    }

    meanspectrum_plotly <- function(wave,
                                    background = '#274C77',
                                    foreground = "white",
                                    hover_bgcolor = "white",
                                    hover_fontcolor = "black") {
      mean_spectrum <- seewave::meanspec(wave,
                                         f = wave@samp.rate,
                                         wl = 2048,
                                         ovlp = 50,
                                         plot = FALSE)
      mean_spectrum_df <- data.frame(
        freq = mean_spectrum[, 1],
        mean_amp = mean_spectrum[, 2]
      )

      plot_ly(data = mean_spectrum_df, x = ~freq, y = ~mean_amp, type = 'scatter', mode = 'lines', line = list(color = 'white')) %>%
        add_ribbons(ymin = 0, ymax = ~mean_amp, fillcolor = foreground, line = list(color = foreground)) %>%
        layout(
          title = "",
          xaxis = list(
            title = "Frequency (kHz)",
            titlefont = list(size = 10, color = foreground),
            tickfont = list(size = 10, color = foreground),
            ticks = "outside",
            tickcolor = foreground,
            tickwidth = 1,
            linecolor = foreground,
            ticklen = 5,
            automargin = TRUE,
            zeroline = FALSE,
            showline = TRUE
          ),
          yaxis = list(
            title = "Mean Amplitude",
            titlefont = list(size = 10, color = foreground),
            tickfont = list(size = 10, color = foreground),
            ticks = "outside",
            tickvals = pretty(mean_spectrum_df$mean_amp, n = 3),
            tickcolor = foreground,
            tickwidth = 1,
            ticklen = 5,
            rangemode= 'tozero',
            linecolor = foreground,
            zeroline = FALSE,
            showline = TRUE
          ),
          paper_bgcolor = background,
          plot_bgcolor = background,
          shapes = list(
            list(
              type = "line",
              x0 = 0,
              x1 = max(mean_spectrum_df$freq),
              xref = "x",
              y0 = 0.1,
              y1 = 0.1,
              yref = "y",
              line = list(
                color = foreground,
                dash = "dot"
              )
            )
          ),
          margin = list(
            l = 50,
            r = 10,
            b = 60,
            t = 50
          ),
          showlegend = FALSE
        ) %>%
        config(displayModeBar = TRUE) %>%
        style(
          hovertemplate = paste0(
            "Frequency: %{x:.1f} kHz<br>",
            "<extra></extra>"
          )
        )
    }


    # Function to update wave object choices
    update_wave_choices <- function() {
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "selectedWave", choices = waveObjects)
    }

    # Observe to update wave object choices initially and on refresh
    observe({
      update_wave_choices()
    })

    # observeEvent(input$refresh, {
    #   update_wave_choices()
    # })

    # Update the reactive waveObject whenever the selection changes
    observeEvent(input$selectedWave, {
      req(input$selectedWave)
      tryCatch({
        newWave <- get(input$selectedWave, envir = .GlobalEnv)
        waveObject(newWave)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "Failed to load the selected wave object. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
    })

    output$audioPlot <- renderPlotly({
      req(waveObject())
      req(input$plotMeanSpectrum)
      meanspectrum_plotly(waveObject())
    })


    observeEvent(input$plotMeanSpectrum, {
      req(waveObject())
      output$audioPlot <- renderPlotly({
        meanspectrum_plotly(waveObject())
      })
    })

    output$audioPlot <- renderPlotly({
      req(waveObject())
      req(input$plotSpectro)
      spectrogram_plotly(waveObject())
    })


    observeEvent(input$plotSpectro, {
      req(waveObject())
      output$audioPlot <- renderPlotly({
        spectrogram_plotly(waveObject())
      })
    })

    observeEvent(input$applyFilter, {
      req(waveObject())
      tryCatch({
        filtered_wave <- ffilter(
          waveObject(),
          from = (input$highpass) * 1000,
          to = (input$lowpass) * 1000,
          output = "Wave"
        )
        waveObject(filtered_wave)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "Failed to apply the high-pass filter. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
    })

    observeEvent(input$saveEditedWave, {
      req(waveObject(), input$newName)
      tryCatch({
        assign(input$newName, waveObject(), envir = .GlobalEnv)
        showModal(modalDialog(
          title = "Saved!",
          paste0("Available as '", input$newName, "' in the R environment."),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "Failed to save the wave object. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
    })



    # Stop app when the tab is closed with the "X" button
    session$onSessionEnded(function() {
      stopApp()
    })

    # Stop app when the "Close app" button is used
    observeEvent(input$close, {
      shinyjs::runjs("window.close();")
      stopApp()
    })



  }


  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}

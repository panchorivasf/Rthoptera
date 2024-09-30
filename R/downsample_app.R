#' Downsample Audio Shiny App
#'
#' This function launches a Shiny app that allows users to downsample audio (wave objects) and visualize the mean spectrum. Users can select an existing wave object from the R environment, specify a new maximum frequency (kHz), downsample the audio, and save the downsampled wave object with a new name.
#'
#' @param input Internal Shiny parameter for handling input values.
#' @param output Internal Shiny parameter for rendering outputs in the app.
#' @param session Internal Shiny parameter for managing the app session.
#'
#' @return A Shiny app interface for downsampling and visualizing audio wave objects.
#' @export
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom plotly plotlyOutput renderPlotly plot_ly add_ribbons layout config style
#' @importFrom seewave meanspec resamp
#' @importFrom bslib bs_theme
#' @importFrom shinycssloaders withSpinner
#' @examples
#' \dontrun{
#' downsample_app()
#' }

downsample_app <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui = function(request) {
    tagList(
      h1("Downsample", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        theme = bslib::bs_theme(bootswatch = "darkly"),
        tags$head(tags$style(
          HTML(
            "
              body {
                margin: 20px; /* Adds margin around the entire page */
              }
              #audioPlot {
                height: calc(100vh - 120px); /* Adjusts height taking into account other elements */
                width: 100%;
              }
              .btn-group-vertical > .btn {
                margin-bottom: 10px; /* Adds space between vertical buttons */
              }
              .row {
                margin-bottom: 10px; /* Adds vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 10px; /* Increases horizontal space between inputs */
              }

               #plotMeanSpectrum {
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
        tags$script(
          HTML(
            "
              Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
                if (message) {
                  document.getElementById('spinner').style.display = 'block';
                } else {
                  document.getElementById('spinner').style.display = 'none';
                }
              });
              "
          )
        ),

        fluidRow(
          column(3,
                 div(class = 'inline', selectInput("selectedWave", "Select a wave object:", choices = NULL, width = '90%')),
                 div(style = "margin-top: 5px;", actionButton("refresh", "Refresh", width='70%'))
          ),
          column(1,
                 div(style = "margin-top: 20px;", actionButton("plotMeanSpectrum", "Plot"))
          ),
          column(2,
                 div(class = 'inline', selectInput('maxfreq', 'New Max. Freq. (kHz):', choices = c(48, 96, 125))),
                 div(style = "margin-top: 5px;", actionButton("downsample", "Downsample"))
          ),

          column(2,
                 div(class = 'inline', textInput("newName", "Name for new wave:", value = "")),
                 div(style = "margin-top: 5px;", actionButton("saveEditedWave", "Save"))
          ),

          column(1,actionButton("close", "Close App"))
        ),

        fluidRow(
          column(12,
                 div(style = "margin-top: 15px;",
                     withSpinner(plotly::plotlyOutput("audioPlot", height = "520px", width = "1480px"))))
        )
      )
    )
  }


  server = function(input, output, session) {
    waveObject <- reactiveVal(NULL)
    plotly_obj <- reactiveVal()


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

    observeEvent(input$refresh, {
      update_wave_choices()
    })

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


    output$audioPlot <- plotly::renderPlotly({
      req(waveObject())
      req(input$plotMeanSpectrum)
      p <- meanspectrum_plotly(waveObject())

      plotly_obj(p)
      p

    })




    observeEvent(input$downsample, {
      req(waveObject())
      tryCatch({
        resampled_wave <- resamp(waveObject(),
                                 g = (as.numeric(input$maxfreq) * 1000) * 2, # from kHz to Hz , from Nyquist to sampling rate
                                 output = "Wave"
        )
        waveObject(resampled_wave)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "Failed to downsample the wave object. Please try again.",
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

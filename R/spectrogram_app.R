#' Spectrogram Shiny App
#'
#' This Shiny app allows users to generate a standardized-resolution spectrogram from a wave object with options to display the mean spectrum and adjust the noise floor. The user can save the spectrogram as a PNG image.
#'
#' @return A Shiny app for visualizing and saving spectrograms of audio wave objects.
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom seewave spectro meanspec
#' @importFrom dplyr mutate case_when as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom patchwork plot_layout
#' @export
#'
#' @examples
#' if (interactive()) {
#'   spectrogram_app()
#' }
spectrogram_app <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui = function(request) {
    tagList(
      h1("Spectrogram", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        theme = bslib::bs_theme(bootswatch = "darkly"),
        tags$head(
          tags$style(
            HTML(
              "
              body {
                margin: 5px; /* Adds margin around the entire page */
              }
              .btn-group-vertical > .btn {
                margin-bottom: 5px; /* Space between vertical buttons */
              }
              .row {
                margin-bottom: 10px; /* Vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 2px; /* Horizontal space between inputs */
              }
              #plotSpectro {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
              }
              #saveImage {
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
          )
        ),
        fluidRow(
          column(2,
                 selectInput("waveObject", "Select a wave object:", choices = NULL, width = '100%')
          ),
          column(2,verticalLayout(
            checkboxInput("meanspec", "Add Mean Spectrum", value = TRUE),
            conditionalPanel(
              condition = "input.meanspec",
              selectInput("meanspecScale", "Mean Spectrum Amplitude:", selected = "Linear",
                          choices = c("Linear", "dB"))
            )
          )
          ),
          column(2,
                 numericInput("noise.floor", "Noise Floor (dB)",
                              value = -50, min = -80, max = -20, step = 5)),

          column(1,
                 numericInput("imgWidth", "Width (in):", value = 15, min = 1, step = 1)
          ),
          column(1,
                 numericInput("imgHeight", "Height (in):", value = 3, min = 1, step = 1)
          ),
          column(1,
                 actionButton("plotSpectro", "Plot")),
          column(2,
                 verticalLayout(
                   downloadButton("saveImage", "Save PNG"),
                   div(style = "font-size: 13px !important;",
                       checkboxInput("transparentBg", "Transparent Background", value = FALSE)
                   ),
                 )
          ),
          column(1, actionButton("close", "Close App"))
        ),

        mainPanel(
          column(12, uiOutput("specPlotOutput"))

        )
      )
    )
  }

  server = function(input, output, session) {
    plotVisible <- reactiveVal(FALSE)
    savedPlot <- reactiveVal(NULL)
    savedImage <- reactiveVal(NULL)

    spectro_df <- function(wave, floor = -35){
      dyn <- as.numeric(floor)

      wl = round(wave@samp.rate * sqrt(seewave::duration(wave))*20e-4)



      if (wl %% 2 != 0) {
        wl <- wl + 1
      }

      spect <- wave |>
        seewave::spectro(
          wl = wl,
          ovlp = 95,
          zp= 200,
          plot = FALSE,
          fftw = TRUE
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
          time = as.numeric(time),
          dyn = dyn
        )

      spect_df_floor <- spect_df |>
        mutate(
          amp_floor = case_when(
            amp < dyn ~ dyn,
            TRUE ~ amp
          )
        )

      return(spect_df_floor)
    }

    spectro_ggplot <- function(spectro_df,
                               margin.l = 0, margin.r = 0, margin.t = 0, margin.b = 0){
      spect_plot <- spectro_df |>
        ggplot(aes(time, freq)) +
        geom_raster(aes(fill = amp_floor)) +
        scale_fill_gradient(low = "white", high = "black",
                            limits = c(unique(spectro_df$dyn), max(spectro_df$amp)), na.value = "white") +
        scale_y_continuous(expand = c(0,0),
                           breaks = scales::breaks_pretty(),
                           labels = scales::label_number(accuracy = 1,
                                                         trim = TRUE,
                                                         zero.print = "")) +
        scale_x_continuous(expand = expansion(mult = c(0, 0)),
                           breaks = scales::breaks_pretty(),
                           labels = scales::label_number(accuracy = 0.1,
                                                         trim = TRUE,
                                                         zero.print = "")) +

        theme_minimal(base_size = 15) +
        theme_bw()+
        theme(
          plot.margin = margin(t=margin.t, r=margin.r, b=margin.b, l=margin.l, unit = 'pt'),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          axis.ticks = element_line(colour = "black"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
          legend.position = "none"
        ) +
        labs(
          x = "Time (s)",
          y = "Frequency (kHz)",
          title = ""
        )

      return(spect_plot)
    }


    meanspec_ggplot <- function(wave, scale = c("linear", "dB")){

      dB_value <- if (scale == "dB") "max0" else NULL
      mean_spectrum <- seewave::meanspec(wave,
                                         f = wave@samp.rate,
                                         wl = 510, ovlp = 50,
                                         dB = dB_value,
                                         plot = FALSE)
      meanspec_data <- data.frame(
        freq = mean_spectrum[, 1],
        mean_amp = mean_spectrum[, 2]
      )


      spectrum_plot <- meanspec_data |>
        ggplot(aes(x = freq, y = mean_amp)) +
        theme_minimal(base_size = 15)

      if(scale == "dB"){
        spectrum_plot <- spectrum_plot +
          geom_ribbon(aes(x = freq, ymin = -50, ymax = mean_amp), fill = "black") +
          scale_y_continuous(breaks = c(-40, -20, 0),
                             limits = c(-50, 0),
                             expand = expansion(mult = c(0, .1)),
                             position = 'right')
      } else {
        spectrum_plot <- spectrum_plot +
          geom_ribbon(aes(x = freq, ymin = 0, ymax = mean_amp), fill = "black") +
          scale_y_continuous(breaks = c(0, 0.5, 1),
                             expand = expansion(mult = c(0, .1)),
                             position = "right",
                             labels = function(x) ifelse(x == 0 | x == 1,
                                                         as.character(as.integer(x)),
                                                         as.character(x)))
      }


      spectrum_plot <- spectrum_plot +
        scale_x_continuous(expand = c(0,0),
                           position = "top",
                           breaks = scales::breaks_pretty(),
                           labels = scales::label_number(zero.print = "")) +
        theme_bw() +
        theme(
          plot.margin = margin(t=0, r=10, b=10, l=0, unit="pt"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          axis.ticks.y = element_line(colour = "black"),
          axis.ticks.x = element_line(colour = "black"),
          axis.title = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "none"
          # plot.title = element_text(face = title_style)
        ) +
        labs(
          x = NULL,
          y = "Mean Amplitude",
          title = ""
        ) +
        coord_flip()

      return(spectrum_plot)
    }

    spectrogram <- function(wave, meanspec = TRUE, floor = -50, scale = c("linear", "dB")) {
      spect_df <- spectro_df(wave, floor = floor)

      if(meanspec){
        spect_plot <- spectro_ggplot(spect_df, margin.l = 10, margin.r = 0, margin.t = 0, margin.b = 10)

        # meanspec_data <- meanspec_df(wave)
        spectrum_plot <- meanspec_ggplot(wave, scale = scale)

        combined_plot <- spect_plot + spectrum_plot + plot_layout(ncol = 2, widths = c(10, 1))

        return(combined_plot)

      } else {
        spect_plot <- spectro_ggplot(spect_df, margin.l = 10, margin.r = 10, margin.t = 0, margin.b = 10)
        return(spect_plot)
      }
    }

    observe({
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
        inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "waveObject", choices = waveObjects)
    })

    observeEvent(input$plotSpectro, {
      req(input$waveObject)
      wave <- get(input$waveObject, envir = .GlobalEnv)

      plotVisible(TRUE)

      output$specPlotOutput <- renderUI({
        withSpinner(plotOutput("specPlot", height = "auto", width = "auto"), type = 1)
      })

      output$specPlot <- renderPlot({
        tryCatch({
          combined_plot <- spectrogram(wave, meanspec = isolate(input$meanspec),
                                       floor = isolate(input$noise.floor),
                                       scale = isolate(input$meanspecScale))
          print(combined_plot)
          savedPlot(combined_plot)  # Save the plot reactively

          # Save the rendered image to a temporary file
          temp_file <- tempfile(fileext = ".png")
          bg <- ifelse(input$transparentBg, "transparent", "white")
          ggsave(temp_file,
                 plot = combined_plot,
                 width = isolate(input$imgWidth), height = isolate(input$imgHeight),
                 units = "in", dpi = 300, bg = bg)

          savedImage(temp_file)  # Save the temporary file path reactively

        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred:", e$message),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          NULL
        })
      }, width = function() { isolate(input$imgWidth) * 100 },  # convert to pixels using 100 dpi
      height = function() { isolate(input$imgHeight) * 100 }
      )
    })

    output$saveImage <- downloadHandler(
      filename = function() {
        paste(as.character(input$waveObject), "_spectrogram.png", sep = "")
      },
      content = function(file) {
        req(savedImage())
        file.copy(savedImage(), file)
      }
    )


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

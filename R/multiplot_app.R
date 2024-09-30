#' Spectrogram, Oscillogram, and Mean Power Spectrum Multiplot Shiny App
#'
#' This Shiny app allows users to visualize a combined plot of a spectrogram, mean spectrum, and oscillogram for a selected audio file (Wave object). Users can adjust settings such as noise floor, image dimensions, and background transparency, and can download the resulting plot as a PNG file.
#'
#' @return A Shiny app for visualizing, customizing, and downloading spectrogram and oscillogram plots.
#' @import shiny
#' @import ggplot2
#' @importFrom shinycssloaders withSpinner
#' @importFrom seewave spectro oscillo meanspec
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom patchwork plot_layout
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   multiplot_app()
#' }
#' }
multiplot_app <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui = function(request) {
    tagList(
      h1("Multiplot", style = "font-size: 28px; margin-left: 15px;"),
      # nav_links(),
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
                #specPlot {
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
                  margin-right: 2px; /* Reduces horizontal space between inputs */
                }

                "
            )
          ),

        ),
        fluidRow(
          column(4,
                 selectInput("waveObject", "Select a wave object:", choices = NULL, width = '100%'),
                 actionButton("refresh", "Refresh List"),
                 actionButton("spectroscillo", "Plot")
          ),
          column(2,
                 numericInput("noise.floor", "Noise Floor (dB)",
                              value = -35, min = -60, max = -20, step = 5)),
          column(2,
                 numericInput("imgWidth", "Image Width (in):", value = 15, min = 1)),
          column(2,
                 numericInput("imgHeight", "Image Height (in):", value = 5, min = 1),
                 checkboxInput("transparentBg", "Transparent Background", value = FALSE)),
          column(1,
                 downloadButton("saveImage", "Save Image")
                 ),
          column(1, actionButton("close", "Close App"))

        ),
        mainPanel(
          uiOutput("specPlotOutput", height = "auto", width = "auto"),
        )
      )
    )
  }

  server = function(input, output, session) {
    plotVisible <- reactiveVal(FALSE)
    spectrogramCache <- reactiveVal(NULL)

    spectroscillo <- function(wave, floor = -35) {


      # Adjust resolution trade-off according to sampling rate and duration
      wl = round(wave@samp.rate * sqrt(seewave::duration(wave))*20e-4)

      # coerce wl to an even number
      if (wl %% 2 != 0) {
        wl <- wl + 1
      }


      spect <- wave |> seewave::spectro(wl = wl,
                                        ovlp = 95,
                                        zp = 80,
                                        plot = FALSE)
      colnames(spect$amp) <- spect$time
      rownames(spect$amp) <- spect$freq

      spect_df <- spect$amp |> as_tibble(rownames = "freq") |>
        pivot_longer(-freq, names_to = "time", values_to = "amp") |>
        mutate(freq = as.numeric(freq), time = as.numeric(time))

      dyn <- as.numeric(floor)
      spect_df_floor <- spect_df |> mutate(amp_floor = ifelse(amp < dyn, dyn, amp))

      mean_spectrum <- seewave::meanspec(wave, f = wave@samp.rate, wl = 512, ovlp = 75, plot = FALSE)
      mean_spectrum_df <- data.frame(freq = mean_spectrum[, 2], mean_amp = mean_spectrum[, 1])

      spect_plot <- spect_df_floor |>
        ggplot(aes(time, freq)) +
        geom_raster(aes(fill = amp_floor)) +
        scale_fill_gradient(low = "white", high = "black",
                            limits = c(dyn, max(spect_df$amp)), na.value = "white") +
        scale_y_continuous(expand = c(0,0),
                           breaks = scales::breaks_pretty(),#n.breaks = 4,
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
          plot.margin = margin(t=0, r=0, b=10, l=10, unit = 'pt'),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          axis.ticks = element_line(colour = "black"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = "none"
        ) +
        labs(
          x = "Time (s)",
          y = "Frequency (kHz)",
          title = ""
        )



      spectrum_plot <- mean_spectrum_df |>
        ggplot(aes(x = mean_amp, y = freq)) +
        geom_line(color = "black") +
        geom_ribbon(aes(ymin = 0, ymax = freq), fill = "black") +
        theme_minimal(base_size = 15) +

        scale_y_continuous(breaks = c(0, 0.5, 1),
                           expand = expansion(mult = c(0, .1)),
                           position = 'right',
                           labels = function(x) ifelse(x == 0 | x == 1,
                                                       as.character(as.integer(x)),
                                                       as.character(x))) +
        scale_x_continuous(expand = c(0,0), position = "top",
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
        ) +
        labs(
          x = NULL,
          y = "Mean Amplitude",
          title = ""
        ) +
        coord_flip()

      oscillo_data <- seewave::oscillo(wave, plot = FALSE)
      time <- seq(0, (length(oscillo_data) - 1)) / wave@samp.rate
      amplitude <- oscillo_data / max(abs(oscillo_data))
      oscillo_df <- data.frame(time = time, amplitude = amplitude)

      oscillo_plot <- ggplot(oscillo_df, aes(x = time, y = amplitude)) +
        geom_line(color = "black") +
        theme_minimal(base_size = 15) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(n.breaks = 3, expand = c(0.03, 0.03)) +
        theme(
          plot.margin = margin(t = 0, r = 0, b = 0, l = 10, unit = "pt"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          axis.line.y = element_line(colour = "black"),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 10),
          legend.position = "none"
        ) +
        labs(y = "Relative Amplitude", x = "")

      combined_spect_mean <- spect_plot + spectrum_plot + plot_layout(ncol = 2, widths = c(5, 0.5))
      combined_spect_oscillo <- (spect_plot / oscillo_plot) + plot_layout(heights = c(5, 1))
      final_plot <- combined_spect_mean / (oscillo_plot + plot_spacer() + plot_layout(ncol = 2, widths = c(5, 0.5)))

      return(final_plot)
    }

    observe({
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
        inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "waveObject", choices = waveObjects)
    })

    observeEvent(input$refresh, {
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
        inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "waveObject", choices = waveObjects)
    })

    observeEvent(input$spectroscillo, {
      req(input$waveObject)
      req(input$noise.floor)
      wave <- get(input$waveObject, envir = .GlobalEnv)

      plotParams <- list(wave = wave, floor = input$noise.floor)

      spectrogramCache(plotParams)
      plotVisible(TRUE)


      output$specPlotOutput <- renderUI({
        withSpinner(plotOutput("specPlot", height = "auto", width = "auto"), type = 6)
      })


      output$specPlot <- renderPlot({
        tryCatch({
          plotParams <- spectrogramCache()
          combined_plot <- spectroscillo(plotParams$wave)
          print(combined_plot)
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("An error occurred:", e$message),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          NULL
        })
      }, height = function() {
        input$imgHeight * 100
      }, width = function() {
        input$imgWidth * 100
      })
    })


    output$saveImage <- downloadHandler(
      filename = function() {
        paste("spectroscillo", "_saved_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        req(spectrogramCache())
        plotParams <- spectrogramCache()
        bg <- ifelse(input$transparentBg, "transparent", "white")
        ggsave(file, plot = spectroscillo(plotParams$wave), width = input$imgWidth, height = input$imgHeight, units = "in", dpi = 300, bg = bg)
      }
    )


    # Add this inside the server
    observeEvent(input$close, {
      shinyjs::runjs("window.close();")
      stopApp()
    })

    # Stop app when the tab is closed with the "X" button
    session$onSessionEnded(function() {
      stopApp()
    })




  }

  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}

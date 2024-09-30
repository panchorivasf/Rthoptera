#' Spectrum Plot Shiny App
#'
#' This Shiny app allows users to visualize a spectrum plot for a selected audio wave object. It includes customizable options such as window length, overlap, window function, and amplitude scale (linear, dB, dBFS). Users can also adjust the axis positions, choose the number of frequency ticks, and add plot parameters or summary statistics to the plot.
#'
#' @return A Shiny app that generates and visualizes the spectrum plot of an audio wave object.
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs useShinyjs extendShinyjs runjs
#' @importFrom seewave meanspec
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @importFrom tibble tibble
#' @importFrom shinycssloaders withSpinner
#' @importFrom scales breaks_pretty label_number
#' @importFrom bslib bs_theme
#' @export
#'
#' @examples
#'  \dontrun{
#' if (interactive()) {
#'   spectrum_plot_app()
#' }
#' }
spectrum_plot_app <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui <- function(request){
    tagList(
      h1("Spectrum", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        theme = bslib::bs_theme(bootswatch = "darkly"),
        tags$head(
          tags$style(
            HTML(
              "
              body {
                margin: 5px;
              }
              #specPlot {
                height: calc(100vh - 120px);
                width: 100%;
              }
              .btn-group-vertical > .btn {
                margin-bottom: 10px;
              }
              .row {
                margin-bottom: 10px;
              }
              .shiny-input-container {
                margin-right: 2px;
              }

              #plot {
               border: 2px solid forestgreen; /* Blue contour */
               padding: 5px 5px; /* Button (inside) padding */
               border-radius: 5px; /* Optional: Rounded corners */
              }
              #downloadPlot {
                border: 2px solid dodgerblue; /* Blue contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Optional: Rounded corners */
              }
              #close {
                border: 2px solid red; /* Red contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Optional: Rounded corners */
              }
              "
            )
          )
        ),

        sidebarLayout(
          sidebarPanel(
            selectInput("waveObject", "Select a wave object:", choices = NULL),
            textInput("plotTitle", "Plot title:", value = ""),
            checkboxInput("italicTitle", "Italicize Title", value = TRUE),
            selectInput("wl", "Window Length:", selected = 4096,
                        choices = c(128, 256, 512, 1024, 2048, 4096, 8192)),
            numericInput("ovlp", "Overlap (%):", value = 75, min = 0, max = 100),
            selectInput("wn", "Window Function:",
                        choices = c("bartlett", "blackman", "flattop",
                                    "hamming", "hanning", "rectangle"),
                        selected = "hanning"),
            selectInput("scale", "Scale:", choices = c("linear", "dB", "dBFS")),
            conditionalPanel(
              condition = "input.scale == 'dB' || input.scale == 'dBFS'",
              numericInput("yMin", "Minimum amplitude (dB):", value = -100),
              textInput("yBreaks", "Amplitude Ticks:", value = "-40, -20, 0")
            ),
            width = 2
          ),
          mainPanel(
            fluidRow(
              column(2, verticalLayout(
                selectInput("yPosition", "Y-axis Position:", choices = c("left", "right")),
                selectInput("xPosition", "X-axis Position:", choices = c("bottom", "top"))
              )
              ),
              column(2, verticalLayout(
                checkboxInput("showXTitle", "Show X-axis Title", value = TRUE),
                checkboxInput("showYTitle", "Show Y-axis Title", value = TRUE),
                checkboxInput("addParams", "Add Parameters", value = TRUE),
                checkboxInput("flip", "Flip Axes", value = FALSE)
              )
              ),
              column(2,verticalLayout(
                textInput("fill", "Fill Color:", value = "black"),
                selectInput("fun", "Function:", choices = c("mean", "median", "var", "sd"))
              )
              ),
              column(2,
                     verticalLayout(
                       numericInput("plotWidth", "Width (in):", value = 15, min = 1),
                       numericInput("plotHeight", "Height (in):", value = 5, min = 1)
                     )
              ),
              column(1, actionButton("plot", "Plot Spectrum")),
              column(1, downloadButton("downloadPlot", "Save PNG")),
              column(1, actionButton("close", "Close App"))
            ),
            fluidRow(
              withSpinner(plotOutput("spectrum_plot"))
            ),
            fluidRow(
              column(2,numericInput("fmin", "Min. Freq:",
                                    value = 0, min =0, max=100, step = 1)),
              column(2,numericInput("fmax", "Max. Freq:",
                                    value = 48, min =10, max=193, step = 1)),
              column(2, numericInput("x.breaks", "Nr of Frequency Ticks:",
                                     value = 8, min = 4, max = 20))
            ),
            width = 10
          )
        )
      )
    )
  }

  server <- function(input, output, session) {

    spectrum_plot <- function(wave,
                              wl = 4096,
                              ovlp = 75,
                              scale = "linear",
                              y.min = -100, # use only when scale = "dB"
                              y.breaks = c(-40, -20, 0), # use only when scale = "dB"
                              x.breaks = 6,
                              y.position = "left",
                              x.position = "bottom",
                              flip = FALSE,
                              fill = 'black',
                              fun = "mean",
                              wn = "blackman",
                              show.x.title = TRUE,
                              show.y.title = TRUE,
                              add.params = TRUE,
                              plot.title = NULL,
                              italic_title = FALSE,
                              fmin = NULL,
                              fmax = NULL) {


      # Automatically set fmax to Nyquist frequency if not provided
      if (is.null(fmax) || fmax == 0) {
        fmax <- wave@samp.rate / 2 / 1000  # Convert to kHz
      }

      # Ensure fmin and fmax are numeric
      fmin <- as.numeric(fmin)
      fmax <- as.numeric(fmax)

      # Calculate meanspec_data
      if (scale == "dBFS") {
        meanspec_data <- seewave::meanspec(wave,
                                           f = wave@samp.rate,
                                           wl = wl,
                                           ovlp = ovlp,
                                           plot = FALSE,
                                           norm = FALSE,  # Don't normalize when using dBFS
                                           wn = wn,
                                           FUN = fun,
                                           flim = c(fmin, fmax))

        # Calculate amp_max based on bit depth
        amp_max <- if (wave@bit == 16) {
          32768
        } else if (wave@bit == 24) {
          8388607
        } else if (wave@bit == 32) {
          2147483647
        } else {
          stop("Unsupported bit depth")
        }

        # Convert amplitude to dBFS
        meanspec_data[, 2] <- 20 * log10(abs(meanspec_data[, 2]) / amp_max)

      } else {
        # Use regular meanspec for dB and linear scales
        meanspec_data <- seewave::meanspec(wave,
                                           f = wave@samp.rate,
                                           wl = wl,
                                           ovlp = ovlp,
                                           plot = FALSE,
                                           norm = TRUE,  # Normalize for other scales
                                           dB = if (scale == "dB") "max0" else NULL,
                                           wn = wn,
                                           FUN = fun,
                                           flim = c(fmin, fmax))
      }

      meanspec_data <- data.frame(
        freq = meanspec_data[, 1],
        mean_amp = meanspec_data[, 2]
      )

      spectrum_plot <- meanspec_data |>
        ggplot(aes(x = freq, y = mean_amp)) +
        theme_minimal(base_size = 15)

      if (scale == "dB" || scale == "dBFS") {
        spectrum_plot <- spectrum_plot +
          geom_ribbon(aes(x = freq, ymin = y.min, ymax = mean_amp), fill = fill) +
          scale_y_continuous(breaks = y.breaks,
                             limits = c(y.min, 0),
                             expand = expansion(mult = c(0, .1)),
                             position = y.position)
      } else {
        spectrum_plot <- spectrum_plot +
          geom_ribbon(aes(x = freq, ymin = 0, ymax = mean_amp), fill = fill) +
          scale_y_continuous(breaks = c(0, 0.5, 1),
                             expand = expansion(mult = c(0, .1)),
                             position = y.position,
                             labels = function(x) ifelse(x == 0 | x == 1,
                                                         as.character(as.integer(x)),
                                                         as.character(x)))
      }

      title_style <- if (italic_title) "italic" else "plain"

      spectrum_plot <- spectrum_plot +
        scale_x_continuous(limits = c(fmin, fmax),  # Explicitly set x-axis limits
                           expand = c(0, 0),
                           position = x.position,
                           breaks = scales::breaks_pretty(n = x.breaks),
                           labels = scales::label_number(zero.print = "")) +
        theme_bw() +
        theme(
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          axis.ticks.y = element_line(colour = "black"),
          axis.ticks.x = element_line(colour = "black"),
          axis.title = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          legend.position = "none",
          plot.title = element_text(face = title_style)
        ) +
        labs(
          x = if (show.x.title) "Frequency (kHz)" else NULL,
          y = if (show.y.title) "Amplitude" else NULL,
          title = plot.title
        )

      if (add.params) {

        range <- range(meanspec_data$mean_amp)
        minamp <- range[1]
        maxamp <- range[2]
        diff <- maxamp - minamp



        params_text <- paste0("win. function: ", wn,
                              "\nwin. size: ", wl,
                              "\noverlap:", ovlp, "%",
                              "\nfunction: ", fun,
                              "\nscale: ", scale,
                              "\nmin.amp:",round(minamp,2),
                              "\nmax.amp:",round(maxamp,2),
                              "\ndyn.range:",round(diff,2)
        )

        spectrum_plot <- spectrum_plot +
          annotate("text", x = Inf, y = Inf, label = params_text,
                   hjust = 1.1, vjust = 1.1, size = 4, color = "black")
      }

      if (flip) {
        spectrum_plot <- spectrum_plot +
          coord_flip()
      }


      return(list(spectrum_plot))
    }

    observe({
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
        inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "waveObject", choices = waveObjects)
    })

    observeEvent(input$waveObject, {
      req(input$waveObject)
      wave <- get(input$waveObject, envir = .GlobalEnv)

      # Calculate the Nyquist frequency
      nyquist_freq <- wave@samp.rate / 2 / 1000  # Convert to kHz

      # Update the fmax input to match the Nyquist frequency
      updateNumericInput(session, "fmax", value = nyquist_freq)
    })

    plotData <- eventReactive(input$plot, {
      req(input$waveObject)
      wave <- get(input$waveObject, envir = .GlobalEnv)

      # Set default fmax to Nyquist frequency if it wasn't updated
      fmax_value <- if (is.null(input$fmax) || input$fmax == "") {
        wave@samp.rate / 2 / 1000  # Convert to kHz
      } else {
        input$fmax
      }

      yBreaks <- as.numeric(unlist(strsplit(isolate(input$yBreaks), ",")))

      list(
        plot = spectrum_plot(wave,
                             wl = isolate(as.numeric(input$wl)),
                             plot.title = isolate(input$plotTitle),
                             italic_title = isolate(input$italicTitle),
                             ovlp = isolate(input$ovlp),
                             scale = isolate(input$scale),
                             y.min = if (isolate(input$scale) == "dB" || isolate(input$scale) == "dBFS") isolate(input$yMin) else NULL,
                             y.breaks = if (isolate(input$scale) == "dB" || isolate(input$scale) == "dBFS") yBreaks else NULL,
                             x.breaks = isolate(input$x.breaks),
                             y.position = isolate(input$yPosition),
                             x.position = isolate(input$xPosition),
                             flip = isolate(input$flip),
                             fill = isolate(input$fill),
                             fun = isolate(input$fun),
                             wn = isolate(input$wn),
                             show.x.title = isolate(input$showXTitle),
                             show.y.title = isolate(input$showYTitle),
                             add.params = isolate(input$addParams),
                             fmin = input$fmin,
                             fmax = fmax_value),
        width = isolate(input$plotWidth),
        height = isolate(input$plotHeight)
      )
    })

    output$spectrum_plot <- renderPlot({
      req(plotData())
      print(plotData()$plot)
    },
    width = 'auto', height = 'auto'
    )

    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste(input$waveObject, "_spectral_plot.png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plotData()$plot, width = plotData()$width,
               height = plotData()$height, dpi = 300)
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





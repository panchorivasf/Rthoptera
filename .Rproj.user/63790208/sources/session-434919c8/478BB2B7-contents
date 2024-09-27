#' Multi-Power Spectra Shiny App
#'
#' This Shiny app allows users to visualize and interact with the oscillogram and power spectra of selected wave objects. Users can add multiple selections on the oscillogram and visualize the corresponding mean power spectra. The app provides options to download individual or combined visualizations.
#'
#' @return A Shiny app for visualizing wave oscillograms and mean spectra.
#' @import shiny
#' @import patchwork
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom seewave meanspec
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select row_number pull
#' @importFrom shinycssloaders withSpinner
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   multi_meanspectra()
#' }

multi_meanspectra_static_app <- function(){

  ui <- function(request) {
    tagList(
      h1("Multi-Power Spectra", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        theme = bslib::bs_theme(bootswatch = "darkly"),
        tags$head(tags$style(
          HTML(
            "
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
          margin-right: 2px; /* Reduces horizontal space between inputs */
        }
        #spinner {
          position: fixed;
          top: 50%;
          left: 50%;
          width: 60px;
          height: 60px;
          margin: -30px 0 0 -30px;
          border: 6px solid #f3f3f3;
          border-radius: 50%;
          border-top: 6px solid #3498db;
          animation: spin 2s linear infinite;
          display: none; /* Hidden by default */
        }
        @-webkit-keyframes spin {
          0% { -webkit-transform: rotate(0deg); }
          100% { -webkit-transform: rotate(360deg); }
        }
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        "
          )
        ))),
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

      # titlePanel("Wave Oscillogram and Mean Spectrum"),

      fluidRow(
        column(12,
               div(style = "display: flex; justify-content: space-around; align-items: center;",
                   selectInput("wave_select", "Select a Wave Object:", choices = NULL, width = '30%'),
                   numericInput("opacity", "Fill opacity:", value = 0.9, min = 0.1, max = 1),
                   actionButton("add_selection", "Add Selection", class = "btn-small"),
                   downloadButton("download_oscillogram", "Download Oscillogram", class = "btn-small"),
                   downloadButton("download_power_spectra", "Download Power Spectra", class = "btn-small"),
                   downloadButton("download_together", "Download Together", class = "btn-small")
               )
        )

      ),

      fluidRow(
        column(12, plotOutput("oscillogram", height = "180px", width = "1500px",
                              brush = brushOpts(id = "wave_brush", direction = "x")))
      ),

      fluidRow(
        column(12, plotOutput("mean_spectrum", height = "380px", width = "1500px"))
      )
    )
  }


  server <- function(input, output, session) {

    # Functions
    wave_df <- function(wave){
      srate <- wave@samp.rate
      amplitude <- wave@left
      tbl <- tibble(amplitude = amplitude)
      tbl <- tbl %>%
        mutate(index = row_number(),
               time = (index - 1) / srate) %>%
        select(c(amplitude, time)) %>%
        mutate(amplitude = 2* (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1)
      return(tbl)
    }

    createOscillogram <- function(wave, brush_data = list(), colors = NULL) {
      tbl <- wave_df(wave)

      p <- ggplot(tbl, aes(x = time, y = amplitude)) +
        geom_line(color = "black", size = 0.5) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_line(color = "black")
        ) +
        expand_limits(y = c(-1.2, 1.2)) +
        labs(x = "Time (s)", y = "Amplitude")

      if (!is.null(colors) && length(brush_data) > 0) {
        for (i in seq_along(brush_data)) {
          range <- brush_data[[i]]
          selected_data <- tbl %>% dplyr::filter(time >= range[1] & time <= range[2])
          p <- p + geom_line(data = selected_data, aes(x = time, y = amplitude), color = colors[i], size = 0.5)
        }
      }

      return(p)
    }

    extract.meanspec <- function(wave, from = NULL, to = NULL, wl = 1024) {
      full_spec <- seewave::meanspec(wave, from = from, to = to, plot = FALSE, wl = wl, fftw = TRUE)
      full_spec_df <- tibble(frequency = full_spec[, 1], amplitude = full_spec[, 2])
      return(full_spec_df)
    }

    plot.meanspec <- function(wave, brush_data = list(), colors = NULL, opacity = 0.8) {
      full_spec <- extract.meanspec(wave)

      p <- ggplot(full_spec, aes(x = frequency, y = amplitude)) +
        geom_line(color = "black", size = 0.8) +
        labs(x = "Frequency (kHz)", y = "Amplitude") +
        theme_minimal()

      if (!is.null(colors) && length(brush_data) > 0) {
        for (i in seq_along(brush_data)) {
          range <- brush_data[[i]]
          spec <- extract.meanspec(wave, from = range[1], to = range[2])
          spec$amplitude <- spec$amplitude * max(wave_df(wave) %>% dplyr::filter(time >= range[1] & time <= range[2]) %>% pull(amplitude))
          p <- p + geom_line(data = spec, aes(x = frequency, y = amplitude), color = colors[i], size = 0.5) +
            geom_area(data = spec, aes(x = frequency, y = amplitude), fill = alpha(colors[i], opacity), color = NA)
        }
      }

      return(p)
    }



    # Reactive values to store selected wave and brushed ranges
    selected_wave <- reactiveVal()
    brushed_ranges <- reactiveVal(list())

    observeEvent(input$wave_select, {
      if (input$wave_select != "") {
        wave_obj <- get(input$wave_select, envir = .GlobalEnv)
        selected_wave(wave_obj)
        brushed_ranges(list())  # Reset brushed ranges when a new wave is selected
      }
    })

    observe({
      wave_names <- ls(envir = .GlobalEnv)
      wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "wave_select", choices = wave_names)
    })

    output$oscillogram <- renderPlot({
      req(selected_wave())
      wave <- selected_wave()
      brush_data <- brushed_ranges()
      colors <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
      p <- createOscillogram(wave, brush_data, colors)
      p
    })

    output$mean_spectrum <- renderPlot({
      req(selected_wave())
      wave <- selected_wave()
      brush_data <- brushed_ranges()
      colors <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
      p <- plot.meanspec(wave, brush_data, colors, opacity = input$opacity)
      p
    })

    observeEvent(input$add_selection, {
      req(input$wave_brush)
      brush <- input$wave_brush
      brush_data <- brushed_ranges()
      brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
      brushed_ranges(brush_data)
    })

    output$download_oscillogram <- downloadHandler(
      filename = function() {
        paste0("oscillogram_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(selected_wave())
        wave <- selected_wave()
        brush_data <- brushed_ranges()
        colors <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
        p <- createOscillogram(wave, brush_data, colors)

        # Save the plot using ggsave
        ggsave(filename = file, plot = p, device = "png", width = 20, height = 4,
               units = "in", dpi = 300)
      }
    )

    output$download_power_spectra <- downloadHandler(
      filename = function() {
        paste0("power_spectra_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(selected_wave())
        wave <- selected_wave()
        brush_data <- brushed_ranges()
        colors <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
        p <- plot.meanspec(wave, brush_data, colors)

        # Save the plot using ggsave
        ggsave(filename = file, plot = p, device = "png", width = 20, height = 8,
               units = "in", dpi = 300)
      }
    )

    output$download_together <- downloadHandler(
      filename = function() {
        paste0("oscillogram_power_spectra_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(selected_wave())
        wave <- selected_wave()
        brush_data <- brushed_ranges()
        colors <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
        p1 <- createOscillogram(wave, brush_data, colors)
        p2 <- plot.meanspec(wave, brush_data, colors)

        # Combine the plots using patchwork
        combined_plot <- p1 / p2

        # Save the combined plot using ggsave
        ggsave(filename = file, plot = combined_plot, device = "png", width = 20, height = 12,
               units = "in", dpi = 300)
      }
    )
  }

  # Stop app when the tab is closed with the "X" button
  session$onSessionEnded(function() {
    stopApp()
  })

  # Stop app when the "Close app" button is used
  observeEvent(input$close, {
    shinyjs::runjs("window.close();")
    stopApp()
  })

  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}

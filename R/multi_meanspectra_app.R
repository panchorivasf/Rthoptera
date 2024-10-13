#' Multi-Mean Spectra Shiny App
#'
#' This Shiny app allows users to visualize the mean spectra of multiple selections from an audio waveform. Users can interactively select regions of a waveform, generate their respective mean spectra, and overlay these spectra using a colorblind-safe palette. The app also provides functionality to download the oscillogram and mean spectra as image files.
#'
#'@param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app for visualizing and downloading multi-selection mean spectra and oscillograms.
#' @import shiny
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom dplyr mutate filter pull row_number
#' @importFrom tibble tibble
#' @importFrom seewave meanspec
#' @importFrom plotly plot_ly add_trace plotlyProxy plotlyProxyInvoke toRGB
#' @importFrom htmlwidgets saveWidget
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   multi_meanspectra_app()
#' }
#' }
multi_meanspectra_app <- function(launch.browser = FALSE) {

  # Javascript code to customize the "close" button
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui <- function(request) {
    tagList(
      h1("Multi-Mean Spectra", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        tags$head(tags$style(
          HTML(
            "
              /* General body styling */

              body {
                background-color: #252626;
                color: #ffffff;
                margin: 5px;
              }

              /* Styling for the inputs */
              .form-control {
                background-color: #495057;
                border: 1px solid #6c757d;
                color: #ffffff;
              }

               .btn-info {
                background-color: #252626 !important;
                border-color: #252626 !important;
                color: #ffffff;
              }

              /* Styling for buttons */
              .btn {
                background-color: #343a40;
                border-color: #6c757d;
                color: #ffffff;
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


              /* Styling for dialog boxes */
              .modal-dialog {
                border-radius: 10px !important; /* This applies rounding to the outer modal container */
              }

              .modal-content {
                background-color: #252626;
                color: #ffffff;
                border-radius: 15px !important; /* Rounded content container */
                overflow: hidden; /* Ensure content follows the rounded corners */
                box-shadow: 0 5px 15px rgba(0,0,0,.5); /* Optional: add a shadow */
              }
              .modal-header, .modal-footer {
                background-color: #343a40;
                color: #ffffff;
                border-top: none;
                border-bottom: none;
                border-radius: 15px 15px 0 0 !important;
              }

              .modal-footer {
                border-radius: 0 0 15px 15px !important; /* Round bottom corners */
              }

              .modal-body {
                 background-color: #252626;
                 color: #ffffff;
              }



              #plot_button {
            border: 2px solid forestgreen; /* Contour color */
            padding: 15px 25px; /* Button (inside) padding */
            border-radius: 5px; /* Optional: Rounded corners */
              }

             #add_selection {
            border: 2px solid forestgreen; /* Contour color */
            padding: 15px 25px; /* Button (inside) padding */
            border-radius: 5px; /* Optional: Rounded corners */
            }

              #download_oscillogram {
            border: 2px solid dodgerblue; /* Blue contour */
            padding: 5px 5px; /* Button (inside) padding */
            border-radius: 5px; /* Optional: Rounded corners */
              }

               #download_power_spectra {
            border: 2px solid dodgerblue; /* Blue contour */
            padding: 5px 5px; /* Button (inside) padding */
            border-radius: 5px; /* Optional: Rounded corners */
               }

              #close {
            border: 2px solid red; /* Red contour */
            padding: 5px 5px; /* Button (inside) padding */
            border-radius: 5px; /* Optional: Rounded corners */
              }


              #oscillogram {
            margin-left: 10px;
            margin-right: 10px;
            margin-bottom: 10px;
            }

              #mean_spectrum {
            margin-left: 10px;
            margin-right: 10px;
            margin-bottom: 10px;
            }

             "
          )
        ))),


      fluidRow(
        column(11,
               div(style = "display: flex; justify-content: space-around; align-items: center; flex-wrap: wrap;",
                   div(style = "margin-right: 5px;", selectInput("wave_select", "Select a Wave Object:", choices = NULL, width = '100%')),
                   div(style = "margin-right: 5px;", selectInput("wl", "Window Length: ", selected = 4096, choices = c(512,1024,2048,4096,8192), width='80%')),
                   div(style = "margin-right: 5px;", actionButton("plot_button", "Plot", class = "btn-small")),
                   div(style = "margin-right: 5px;", actionButton("add_selection", "Add Selection", class = "btn-small")),
                   div(style = "margin-right: 5px;", numericInput("alpha", "Fill Opacity", value = 0.9, min = 0.1, max = 1, step = 0.1), width='60%'),
                   div(style = "margin-right: 5px;", downloadButton("download_oscillogram", "Download Oscillogram", class = "btn-small")),
                   div(downloadButton("download_power_spectra", "Download Power Spectra", class = "btn-small"))
               )
        ),
        column(1, useShinyjs(),
               extendShinyjs(text = jscode, functions = c("closeWindow")),
               actionButton("close", "Close App"))

      ),

      fluidRow(
        mainPanel(

          plotOutput("oscillogram", height = "200px", width = "98%",
                     brush = brushOpts(id = "wave_brush", direction = "x")),
          width = 12),

        mainPanel(
          withSpinner(plotlyOutput("mean_spectrum", height = "450px", width = "98%")),
          width = 12)

      ),
    )
  }

  server <- function(input, output, session) {

    selected_wave <- reactiveVal()
    brushed_ranges <- reactiveVal(list())


    # Function to extract a data frame from a Wave object
    wave_df <- function(wave){
      srate <- wave@samp.rate
      amplitude <- wave@left
      tbl <- tibble(amplitude = amplitude)
      tbl <- tbl %>%
        mutate(index = row_number(),
               time = (index - 1) / srate) %>%
        select(c(amplitude, time)) %>%
        mutate(amplitude = 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1)
      return(tbl)
    }

    # Function to plot an oscillogram from a wave using wave_df and ggplot
    oscillo_ggplot <- function(wave, brush_data = list(), colors = NULL) {
      tbl <- wave_df(wave)
      time_range <- range(tbl$time)

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
        scale_x_continuous(limits = c(time_range[1], time_range[2]), expand = c(0,0)) +
        labs(x = "Time (s)", y = "Amplitude")

      if (!is.null(colors) && length(brush_data) > 0) {
        for (i in seq_along(brush_data)) {
          range <- brush_data[[i]]
          selected_data <- tbl %>% dplyr::filter(time >= range[1] & time <= range[2])
          p <- p + geom_line(data = selected_data, aes(x = time, y = amplitude),
                             color = colors[i], size = 0.5)
        }
      }

      return(p)
    }

    # Function to extract a tibble with meanspectrum information from seewave::meanspec
    spectrum_df <- function(wave, from = NULL, to = NULL, wl = as.numeric(input$wl)) {
      full_spec <- seewave::meanspec(wave, from = from, to = to, plot = FALSE,
                                     wl = wl, fftw = TRUE)
      full_spec_df <- tibble(frequency = full_spec[, 1], amplitude = full_spec[, 2])
      return(full_spec_df)
    }

    # Function to plot an interactive mean spectrum using spectrum_df and plotly
    meanspec_plotly <- function(wave, wl = as.numeric(input$wl)) {
      full_spec <- spectrum_df(wave, wl = as.numeric(input$wl))

      p <- plot_ly(full_spec, x = ~frequency, y = ~amplitude, type = 'scatter',
                   mode = 'lines',
                   line = list(color = 'black'),
                   name = 'Mean',
                   hovertemplate = 'Amplitude: %{y:.2f}'
      ) %>%
        config(displayModeBar = TRUE) %>%
        plotly::layout(hovermode = 'x')



      p <- p %>%
        layout(
          xaxis = list(
            title = list(text = "Frequency (Hz)", standoff = 10),
            ticklen = 5,
            automargin = TRUE,
            zeroline = FALSE,
            showline = TRUE
          ),
          yaxis = list(title = "Amplitude",
                       rangemode = "tozero",
                       ticklen = 5,
                       showline = TRUE),
          legend = list(
            orientation = "h",
            x = 0.5,
            y = 1.1,
            xanchor = "center"
          ),
          margin = list(
            l = 50,
            r = 10,
            b = 60,
            t = 50
          )
        )

      return(p)
    }



    # Automatic color stacking using a colorblind-safe palette
    brush_colors <- reactiveVal(c("#0072B2","#E69F00","#009E73", "#CC79A7",

                                  "#F0E442", "#56B4E9", "#999999","#D55E00" ))

    plotly_obj <- reactiveVal()

    observeEvent(input$wave_select, {
      if (input$wave_select != "") {
        wave_obj <- get(input$wave_select, envir = .GlobalEnv)
        selected_wave(wave_obj)
        brushed_ranges(list())
      }
    })

    observe({
      wave_names <- ls(envir = .GlobalEnv)
      wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "wave_select", choices = wave_names)
    })

    output$oscillogram <- renderPlot({
      req(input$plot_button)
      req(selected_wave())
      wave <- selected_wave()
      brush_data <- brushed_ranges()
      colors <- brush_colors()
      p <- oscillo_ggplot(wave, brush_data, colors)
      p
    })


    output$mean_spectrum <- renderPlotly({
      req(input$plot_button)
      req(selected_wave())
      wave <- selected_wave()
      p <- meanspec_plotly(wave, wl = as.numeric(input$wl))
      plotly_obj(p)
      p
    })

    observeEvent(input$add_selection, {
      req(input$wave_brush)

      # Round xmin and xmax to avoid floating-point precision issues
      brush <- input$wave_brush
      brush$xmin <- round(brush$xmin, 3)
      brush$xmax <- round(brush$xmax, 3)

      # Validate that xmin < xmax
      if (brush$xmin >= brush$xmax) {
        showModal(modalDialog(
          title = "Invalid Selection",
          "The selection is invalid. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }

      brush_data <- brushed_ranges()
      brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
      brushed_ranges(brush_data)

      wave <- selected_wave()
      range <- c(brush$xmin, brush$xmax)

      spec <- spectrum_df(wave, from = range[1], to = range[2], wl = as.numeric(input$wl))
      spec$amplitude <- spec$amplitude * max(wave_df(wave) %>%
                                               dplyr::filter(time >= range[1] & time <= range[2]) %>%
                                               pull(amplitude))

      colors <- brush_colors()
      selection_number <- length(brush_data)
      selection_name <- paste("Selection", selection_number)

      plotlyProxy("mean_spectrum", session) %>%
        plotlyProxyInvoke("addTraces", list(
          x = spec$frequency, y = spec$amplitude, type = 'scatter',
          mode = 'none',
          fill = 'tozeroy',
          fillcolor = toRGB(colors[selection_number], alpha = input$alpha),
          name = selection_name,
          hovertemplate = 'Amplitude: %{y:.2f}'
        ))

      # Update plotly object with new trace
      p <- plotly_obj()
      p <- p %>%
        add_trace(x = spec$frequency, y = spec$amplitude, type = 'scatter', mode = 'none',
                  fill = 'tozeroy', fillcolor = toRGB(colors[selection_number], alpha = input$alpha),
                  name = selection_name,
                  hovertemplate = 'Amplitude: %{y:.2f}', line = list(color = 'rgba(0,0,0,0)'))
      plotly_obj(p)
    })

    # observeEvent(input$add_selection, {
    #   req(input$wave_brush)
    #   brush <- input$wave_brush
    #   brush_data <- brushed_ranges()
    #   brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
    #   brushed_ranges(brush_data)
    #
    #   wave <- selected_wave()
    #   range <- c(brush$xmin, brush$xmax)
    #   spec <- spectrum_df(wave, from = range[1], to = range[2], wl = as.numeric(input$wl))
    #   spec$amplitude <- spec$amplitude * max(wave_df(wave) %>% dplyr::filter(time >= range[1] & time <= range[2]) %>% pull(amplitude))
    #   colors <- brush_colors()
    #
    #   selection_number <- length(brush_data)
    #   selection_name <- paste("Selection", selection_number)
    #
    #   plotlyProxy("mean_spectrum", session) %>%
    #     plotlyProxyInvoke("addTraces", list(
    #       x = spec$frequency, y = spec$amplitude, type = 'scatter',
    #       mode = 'none',
    #       fill = 'tozeroy',
    #       fillcolor = toRGB(colors[selection_number],
    #                         alpha = input$alpha), name = selection_name,
    #       hovertemplate = 'Amplitude: %{y:.2f}'
    #     ))
    #
    #   # Update plotly object with new trace
    #   p <- plotly_obj()
    #   p <- p %>%
    #     add_trace(x = spec$frequency, y = spec$amplitude, type = 'scatter', mode = 'none',
    #               fill = 'tozeroy', fillcolor = toRGB(colors[selection_number], alpha = input$alpha),
    #               name = selection_name,
    #               hovertemplate = 'Amplitude: %{y:.2f}', line = list(color = 'rgba(0,0,0,0)'))
    #   plotly_obj(p)
    # })

    output$download_oscillogram <- downloadHandler(
      filename = function() {
        paste0("oscillogram_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(selected_wave())
        wave <- selected_wave()
        brush_data <- brushed_ranges()
        colors <- brush_colors()
        p <- oscillo_ggplot(wave, brush_data, colors)

        ggsave(filename = file, plot = p, device = "png",
               width = 20, height = 4,
               units = "in", dpi = 300, bg="white")
      }
    )

    output$download_power_spectra <- downloadHandler(
      filename = function() {
        paste0("meanpowerspectra_", Sys.Date(), ".html")
      },
      content = function(file) {
        req(plotly_obj())
        saveWidget(plotly_obj(), file, selfcontained = TRUE)
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

  if(launch.browser){

    shinyApp(ui = ui, server = server, options = list(launch.browser = browser))

  } else {

    shinyApp(ui = ui, server = server)

  }

}

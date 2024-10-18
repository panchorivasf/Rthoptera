#' Color-Coded Oscillogram Shiny App
#'
#' This Shiny app allows users to visualize an oscillogram and paint selected sections with different colors using a color-blind-safe palette. The user can also customize the display options such as adding a scale bar, showing axes, and downloading the plot as a PNG file.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app for visualizing, customizing, and downloading color-coded oscillograms.
#' @import shiny
#' @import ggplot2
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom colourpicker colourInput
#' @importFrom dplyr mutate filter row_number select
#' @importFrom tibble tibble
#' @importFrom tuneR normalize
#' @export
#'
#' @examples
#' if (interactive()) {
#'   colorcode_oscillogram_app()
#' }
colorcode_oscillogram_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  colorblind_safe_palette <- c(
    "Orange" = "#E69F00",
    "Sky Blue" = "#56B4E9",
    "Bluish Green" = "#009E73",
    "Yellow" = "#F0E442",
    "Blue" = "#0072B2",
    "Vermillion" = "#D55E00",
    "Reddish Purple" = "#CC79A7",
    "Light Green" = "#8DD3C7",
    "Dark Purple" = "#BEBADA",
    "Light Orange" = "#FB8072"
  )

  ui <- fluidPage(
    titlePanel("Color-coded Oscillogram"),
    fluidPage(
      useShinyjs(),
      extendShinyjs(text = jscode, functions = c("closeWindow")),
      # theme = bslib::bs_theme(bootswatch = "darkly"),
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

/* Input with info button styling */
  .input-with-info {
    display: flex;
    align-items: center;
  }

.input-with-info label {
  margin-right: 5px;
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
          .js-irs-0 .irs-grid,
          .js-irs-0 .irs-min,
          .js-irs-0 .irs-max,
          .js-irs-0 .irs-single,
          .js-irs-0 .irs-grid-pol{
            display: none !important; /* Hides slider labels and ticks */
          }
          .slider-container .shiny-input-container {
            width: 100% !important; /* Ensure slider takes full width */
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
               border: 2px solid forestgreen; /* Blue contour */
               padding: 5px 5px; /* Button (inside) padding */
               border-radius: 5px; /* Optional: Rounded corners */
              }
              #download_oscillogram {
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
      ))),

    fluidRow(
      column(2, selectInput("wave_select", "Select a Wave Object:", choices = NULL, selected = NULL)),
      column(1, actionButton("plot_button", "Plot")),
      column(1, colourpicker::colourInput("color_picker", "Select Color", value = "#E69F00",
                                          showColour = "background",
                                          palette = "limited",
                                          allowedCols = colorblind_safe_palette)),
      column(1, actionButton("add_selection", "Paint Selection")),
      column(2, verticalLayout(
        radioButtons("toggle_option", "Display Options:",
                     choices = list("Show Axes" = "show_axes", "Add Scale Bar" = "add_scale_bar"),
                     selected = "show_axes"),
        numericInput("scale_bar_size", "Scale Bar Size (s)", value = 0.1, min = 0.1, step = 0.1)
      ) ),
      column(1, verticalLayout(
        numericInput("height", "Height (in):", value = 2, step = 1),
        numericInput("width", "Width (in):", value = 10, step = 1)
      )),
      column(1, numericInput("dpi", "DPI", value = 200, step = 10)),
      column(1, downloadButton("download_oscillogram", "Download PNG")),
      column(1, useShinyjs(),
             extendShinyjs(text = jscode, functions = c("closeWindow")),
             actionButton("close", "Close App"))

    ),
    fluidRow(
      column(12, withSpinner(plotOutput("oscillogram", height = "180px", width = "1450px",
                                        brush = brushOpts(id = "wave_brush", direction = "x")))
      )
    ),
    fluidRow(
      column(12, div(class = "slider-container",
                     sliderInput("scale_bar_position", "Scale bar position:",
                                 min = 0, max = 100, value = 0, step = 1, ticks = FALSE)))
    )
  )

  server <- function(input, output, session) {

    wave_df <- function(wave, norm = TRUE){

      if(norm){
        wave <- tuneR::normalize(object = wave, unit = "1", center = TRUE)
      }

      wave <- rmoffset(wave, output = "Wave")

      srate <- wave@samp.rate

      amplitude <- wave@left

      tbl <- tibble(amplitude = amplitude)
      tbl <- tbl %>%
        mutate(index = row_number(),
               time = (index-1 ) / srate)
      tbl <- tbl %>%
        select(c(amplitude, time))

      return(tbl)
    }

    createOscillogram <- function(wave, brush_data = list(), colors = NULL, show_axes = TRUE, add_scale_bar = FALSE, scale_bar_size = 1, scale_bar_position_pct = 0) {
      tbl <- wave_df(wave)
      time_range <- range(tbl$time)
      scale_bar_position <- time_range[1] + (scale_bar_position_pct / 100) * (time_range[2] - time_range[1])

      p <- ggplot(tbl, aes(x = time, y = amplitude)) +
        geom_line(color = "black", size = 0.5) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
          plot.margin = margin(t = 5, r = 5, b = 10, l = 5)
        ) +
        expand_limits(y = c(-1.2, 1.2)) +
        scale_x_continuous(limits = c(time_range[1], time_range[2]), expand = c(0,0)) +
        labs(x = "Time (s)", y = "Amplitude")

      if (!show_axes) {
        p <- p + theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        )
      } else {
        p <- p + theme(
          axis.line = element_line(color = "black")  # Show axis lines when enabled
        )
      }

      if (add_scale_bar) {
        scale_bar_label <- ifelse(scale_bar_size < 1, paste0(scale_bar_size * 1000, " ms"), paste0(scale_bar_size, " s"))
        p <- p + annotate("segment", x = scale_bar_position, xend = scale_bar_position + scale_bar_size, y = -1.1, yend = -1.1, colour = "black", size = 1) +
          annotate("text", x = scale_bar_position + scale_bar_size / 2, y = -1.25, label = scale_bar_label, size = 4, vjust = 1)
      }

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

    selected_wave <- reactiveVal()
    brushed_ranges <- reactiveVal(list())
    brush_colors <- reactiveVal(character())

    observeEvent(input$wave_select, {
      if (input$wave_select != "") {
        wave_obj <- get(input$wave_select, envir = .GlobalEnv)
        selected_wave(wave_obj)
        brushed_ranges(list())
        brush_colors(character())
      }
    })

    observe({
      wave_names <- ls(envir = .GlobalEnv)
      wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "wave_select", choices = wave_names)
    })

    output$oscillogram <- renderPlot({
      req(input$plot_button)
      req(input$height)
      req(input$width)
      req(input$dpi)
      req(selected_wave())
      wave <- selected_wave()
      brush_data <- brushed_ranges()
      colors <- brush_colors()
      max_position_pct <- 100 - (input$scale_bar_size / diff(range(wave_df(wave)$time))) * 100
      updateSliderInput(session, "scale_bar_position", max = max_position_pct)

      # Determine which option is selected
      show_axes <- input$toggle_option == "show_axes"
      add_scale_bar <- input$toggle_option == "add_scale_bar"

      p <- createOscillogram(wave, brush_data, colors, show_axes, add_scale_bar, input$scale_bar_size, input$scale_bar_position)
      p
    })

    observeEvent(input$add_selection, {
      req(input$wave_brush)
      brush <- input$wave_brush
      brush_data <- brushed_ranges()
      brush_data <- append(brush_data, list(c(brush$xmin, brush$xmax)))
      brushed_ranges(brush_data)

      colors <- brush_colors()
      colors <- append(colors, input$color_picker)
      brush_colors(colors)
    })

    output$download_oscillogram <- downloadHandler(
      filename = function() {
        paste0("_coded_oscillogram.png")
      },
      content = function(file) {
        req(selected_wave())
        wave <- selected_wave()
        brush_data <- brushed_ranges()
        colors <- brush_colors()
        max_position_pct <- 100 - (input$scale_bar_size / diff(range(wave_df(wave)$time))) * 100
        updateSliderInput(session, "scale_bar_position", max = max_position_pct)

        # Determine which option is selected
        show_axes <- input$toggle_option == "show_axes"
        add_scale_bar <- input$toggle_option == "add_scale_bar"

        p <- createOscillogram(wave, brush_data, colors, show_axes, add_scale_bar, input$scale_bar_size, input$scale_bar_position)

        ggsave(filename = file, plot = p, device = "png",
               width = input$width, height = input$height,
               units = "in", dpi = isolate(input$dpi), bg="white")
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


  if(launch.browser){

    shinyApp(ui = ui, server = server, options = list(launch.browser = browser))

  } else {

    shinyApp(ui = ui, server = server)

  }

}

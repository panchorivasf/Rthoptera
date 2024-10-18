#' Multi-Oscillogram Shiny App
#'
#' This function launches a Shiny app that allows users to select multiple wave objects, plot their oscillograms, and save the resulting plots as a PNG file. Users can control the maximum duration of the waves to be plotted, set the scale bar, and specify image dimensions.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app interface for plotting and saving multi-oscillograms of selected wave objects.
#' @export
#'
#' @import shiny
#' @import ggplot2
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom bslib bs_theme
#' @importFrom dplyr filter
#' @importFrom patchwork wrap_plots
#' @importFrom seewave duration
#' @importFrom shinycssloaders withSpinner
#' @examples
#' \dontrun{
#' multioscillo_app()
#'}
multioscillo_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui = function(request) {
    tagList(
      h1("Multi-Oscillogram", style = "font-size: 28px; margin-left: 15px;"),
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

              .btn-group-vertical > .btn {
                margin-bottom: 5px; /* Space between vertical buttons */
              }
              .row {
                margin-bottom: 5px; /* Vertical space between rows */
              }
              .shiny-input-container {
                margin-right: 2px; /* Horizontal space between inputs */
              }
              #plotMultiOscillogram {
               border: 2px solid forestgreen; /* Green contour */
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
        )),

        fluidRow(
          column(2, selectInput("waveObjects", "Select wave objects:", choices = NULL, selected = NULL, multiple = TRUE, width = '100%')),
          column(1, verticalLayout(
            numericInput("maxDur", "Max dur. (s):", value = 1, min = 0.5, step = 0.5, width = '80%')
          )),
          column(1, verticalLayout(
            numericInput("scalebarLength", "Scale bar length (s):", value = 0.1, min = 0.05, step = 0.01, width = '100%'),
            div(
              checkboxInput("allScalebar", "Add scale bar to each", value = FALSE),
              style = "font-size: 13px !important;"
            )
          )
          ),
          column(1, numericInput("imgWidth", "PNG Width (in):", value = 16, min = 1)),
          column(1, numericInput("imgHeight", "PNG Height (in/oscillogram):", value = 2, min = 1)),
          column(2, actionButton("plotMultiOscillogram", "Plot multi-oscillogram")),
          column(2, verticalLayout(
            downloadButton("saveImage", "Save PNG", width = '100%'),
            div(style = "font-size: 13px !important;",
                checkboxInput("transparentBg", "Save with \ntransparent background", value = FALSE)
            )
          )
          ),
          column(2, actionButton("close", "Close App"))
        ),

        fluidRow(
          column(12, uiOutput("plotOutput"))
        )

      )
    )

  }

  server = function(input, output, session) {
    # Function to create the multi-oscillogram plot
    multiOscillogram <- function(waves, maxDur, scalebarLength, allScalebar = FALSE) {
      max_samples <- maxDur * waves[[1]]@samp.rate
      wave_labels <- LETTERS[1:length(waves)]

      all_waves_df <- lapply(seq_along(waves), function(i) {
        wave <- waves[[i]]
        samp_rate <- wave@samp.rate
        wave_length <- length(wave@left)
        duration <- wave_length / samp_rate

        time <- seq(-maxDur / 2, maxDur / 2, length.out = max_samples)
        amplitude <- rep(0, max_samples)

        start_index <- round((max_samples - wave_length) / 2)
        end_index <- start_index + wave_length - 1

        amplitude[start_index:end_index] <- wave@left

        # Subtract mean and rescale to [-1, 1]
        amplitude <- amplitude - mean(amplitude)
        amplitude <- 2 * (amplitude - min(amplitude)) / (max(amplitude) - min(amplitude)) - 1

        data.frame(
          time = time,
          amplitude = amplitude,
          wave_name = wave_labels[i],
          id = i
        )
      }) |> bind_rows()

      plots <- lapply(unique(all_waves_df$id), function(id) {
        p <- ggplot(all_waves_df %>% dplyr::filter(id == !!id), aes(x = time, y = amplitude)) +
          geom_line(color = "black", linewidth = 0.5) +
          theme_void() +
          theme(legend.position = "none") +
          coord_cartesian(xlim = c(-maxDur / 2, maxDur / 2)) +
          expand_limits(y = c(-1.2, 1.2))

        # Add scalebar to each oscillogram
        if (allScalebar) {
          if (scalebarLength < 1) {
            scalebarText <- paste0(scalebarLength * 1000, " ms")
          } else {
            scalebarText <- paste0(scalebarLength, " s")
          }
          p <- p +
            annotate("segment", x = -maxDur / 2 + 0.05, xend = -maxDur / 2 + 0.05 + scalebarLength, y = -1.05, yend = -1.05, color = "black", size = 1) +
            annotate("text", x = -maxDur / 2 + 0.05 + scalebarLength / 2, y = -1.5, label = scalebarText, vjust = 0.5, hjust = 0.5)
        } else {
          if (id == length(waves)) {  # Add scale bar only to the last plot
            if (scalebarLength < 1) {
              scalebarText <- paste0(scalebarLength * 1000, " ms")
            } else {
              scalebarText <- paste0(scalebarLength, " s")
            }
            p <- p +
              annotate("segment", x = -maxDur / 2 + 0.05, xend = -maxDur / 2 + 0.05 + scalebarLength, y = -1.05, yend = -1.05, color = "black", size = 1) +
              annotate("text", x = -maxDur / 2 + 0.05 + scalebarLength / 2, y = -1.5, label = scalebarText, vjust = 0.5, hjust = 0.5)
          }
        }

        return(p)
      })

      combined_plot <- wrap_plots(plots, ncol = 1)

      return(combined_plot)
    }

    # Store reactive values
    values <- reactiveValues(
      colorSelections = list(),
      history = list(),
      plotFile = NULL # Store the temporary file path
    )

    # Observer to update available wave objects in the environment
    observe({
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
        inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "waveObjects", choices = waveObjects)
    })

    # This reactive expression will re-run only when the "Plot Multi-Oscillogram" button is clicked
    result <- eventReactive(input$plotMultiOscillogram, {
      req(input$waveObjects)
      waves <- lapply(input$waveObjects, function(x) get(x, envir = .GlobalEnv))
      combined_plot <- multiOscillogram(waves, input$maxDur, input$scalebarLength, input$allScalebar)

      # Calculate image dimensions
      img_width <- input$imgWidth
      img_height <- input$imgHeight * length(waves) # Multiply height by number of oscillograms

      # Save the plot to a temporary file
      temp_file <- tempfile(fileext = ".png")
      ggsave(temp_file, plot = combined_plot, width = img_width, height = img_height, units = "in", dpi = 300)
      values$plotFile <- temp_file

      combined_plot
    })

    output$plotOutput <- renderUI({
      req(result())
      req(input$plotMultiOscillogram)
      withSpinner(plotOutput("multiWavePlot",
                             height = paste0(100 * length(input$waveObjects), "px"),
                             width = "1450px"))
    })

    output$multiWavePlot <- renderPlot({
      req(result())
      result()
    })

    observeEvent(input$waveObjects, {
      valid_waveObjects <- input$waveObjects[sapply(input$waveObjects, function(wave_name) {
        wave <- get(wave_name, envir = .GlobalEnv)
        wave_dur <- seewave::duration(wave)
        wave_dur <= input$maxDur
      })]

      if (length(valid_waveObjects) < length(input$waveObjects)) {
        showModal(modalDialog(
          title = "Warning",
          "All waves should be equal or shorter than Max. Duration."
        ))
        updateSelectInput(session, "waveObjects", selected = valid_waveObjects)
      }
    })

    # Download handler for saving the image
    output$saveImage <- downloadHandler(
      filename = function() {
        paste("_multi_oscillo.png")
      },
      content = function(file) {
        req(values$plotFile)
        file.copy(values$plotFile, file, overwrite = TRUE)
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

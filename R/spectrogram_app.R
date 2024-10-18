#' Spectrogram Shiny App
#'
#' This Shiny app allows users to generate a standardized-resolution spectrogram from a wave object with options to display the mean spectrum and adjust the noise floor. The user can save the spectrogram as a PNG image.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app for visualizing and saving spectrograms of audio wave objects.
#' @import shiny
#' @import ggplot2
#' @importFrom htmltools HTML
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom seewave spectro meanspec
#' @importFrom dplyr mutate case_when as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom patchwork plot_layout
#' @export
#'
#' @examples
#'  \dontrun{
#' if (interactive()) {
#'   spectrogram_app()
#' }
#' }
spectrogram_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui = function(request) {
    tagList(
      h1("Spectrogram", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        tags$head(
          tags$style(
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


              "
            )
          )
        ),
        fluidRow(
          column(2,
                 selectInput("waveObject", "Select a wave object:", choices = NULL, width = '100%')
          ),
          column(1,verticalLayout(
            checkboxInput("meanspec", "Mean Spectrum", value = FALSE),
            conditionalPanel(
              condition = "input.meanspec == true",
              selectInput("meanspecScale", "Scale:", selected = "linear",
                          choices = c("linear", "dB"))
            )
          )
          ),
          column(1,
                 numericInput("noise.floor", "Cutoff (dB)",
                              value = -50, min = -80, max = -20, step = 5)
                 ),
          column(1,
                 numericInput("overlap", "Overlap (%):",
                              value = 95, min = 25, max = 95, step = 5)
                 ),
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

        fluidRow(
          column(12, uiOutput("specPlotOutput"))

        )
      )
    )
  }

  server = function(input, output, session) {
    plotVisible <- reactiveVal(FALSE)
    savedPlot <- reactiveVal(NULL)
    savedImage <- reactiveVal(NULL)

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

          if (isolate(input$meanspec)) {
            req(input$meanspecScale)  # Ensure meanspecScale is available
            scale_type <- isolate(as.character(input$meanspecScale))
          } else {
            scale_type <- NULL
          }
          combined_plot <- spectrogram_ggplot(wave,
                                              meanspec = isolate(input$meanspec),
                                              cutoff = isolate(input$noise.floor),
                                              scale.type = scale_type,
                                              overlap = isolate(input$overlap))
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

  if(launch.browser){

    shinyApp(ui = ui, server = server, options = list(launch.browser = browser))

  } else {

    shinyApp(ui = ui, server = server)

  }

}

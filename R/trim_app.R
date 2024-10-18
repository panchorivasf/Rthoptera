#' Select parts of a wave, and save them in the R environment
#'
#' This Shiny app allows users to select parts of a wave object, zoom in or out on the waveform, and save the selected region as a new wave object in the R environment.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return One or more Wave objects created from selected parts of the original wave object.
#' @export
#' @import shiny
#' @import ggplot2
#' @importFrom htmltools HTML
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom seewave oscillo
#' @importFrom tuneR extractWave
#' @importFrom bslib bs_theme
#' @importFrom shinycssloaders withSpinner
#' @examples
#'  \dontrun{
#' trim_app()
#'}
trim_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui =  function(request) {
    tagList(
      h1("Trim", style = "font-size: 28px; margin-left: 15px;"),
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

               #plot {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
              }

                 #saveSelection {
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
          column(2,
                 selectInput("selectedWave", "Select a wave object:",
                             choices = NULL, width = '100%')
                 # actionButton("refresh", "Refresh List"),
          ),
          column(1,
                 actionButton("plot", "Plot")
          ),
          column(3, verticalLayout(
            actionButton("zoomIn", "Zoom In"),
            actionButton("zoomOut", "Zoom Out")
          )
          ),
          # column(3,
          #   actionButton("resetView", "Reset View")
          # ),
          column(3,verticalLayout(
            textInput("selectionName", "Name for the selection:"),
            actionButton("saveSelection", "Save Selection")
          )
          ),
          column(1, actionButton("close", "Close App")),

        ),

        fluidRow(
          column(12,
                 div(style = "margin-top: 15px;",
                     withSpinner(plotOutput("wavePlot",
                                            brush = brushOpts(id = "waveBrush", direction = "x"),
                                            height = "520px", width = "1480px"))))
        )
      )
    )
  }

  server = function(input, output, session) {

    # selectedWave <- reactiveVal(NULL)

    waveObject <- reactiveVal(NULL)
    plotVisible <- reactiveVal(TRUE)
    selectedRegion <- reactiveVal(NULL)
    zoomedRegion <- reactiveVal(NULL)

    oscillo3 <- function(wave) {
      oscillo_data <- seewave::oscillo(wave, plot = FALSE)
      time <- seq(0, (length(oscillo_data) - 1)) / wave@samp.rate
      amplitude <- oscillo_data / max(abs(oscillo_data))
      oscillo_df <- data.frame(time = time, amplitude = amplitude)

      ggplot(oscillo_df, aes(x = time, y = amplitude)) +
        geom_line(color = "white") +
        theme_minimal(base_size = 15) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(n.breaks = 3, expand = c(0.1, 0.1)) +
        theme(
          plot.margin = margin(t = 15, r = 10, b = 15, l = 10, unit = 'pt'),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#274C77", color = "#274C77"),
          plot.background = element_rect(fill = "#274C77", color = "#274C77"),
          axis.line.y = element_line(colour = "white"),
          axis.line.x = element_line(colour = "white"),
          axis.ticks.x = element_line(colour = "white"),
          axis.ticks.y = element_line(colour = "white"),
          axis.title.x = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title = element_text(size = 10, colour = "white"),
          axis.text = element_text(size = 10, colour = "white"),
          legend.position = "none"
        ) +
        labs(y = "Relative Amplitude", x = "Time (s)")
    }

    # Function to update wave object choices
    update_wave_choices <- function() {
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "selectedWave", choices = waveObjects)
    }

    # Observe to update wave object choices initially
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



    output$wavePlot <- renderPlot({
      req(input$plot)
      req(plotVisible())
      wave <- get(input$selectedWave, envir = .GlobalEnv)
      if (!is.null(wave)) {
        if (!is.null(zoomedRegion())) {
          extract <- extractWave(wave, from = zoomedRegion()[1], to = zoomedRegion()[2], xunit = "time")
          oscillo3(extract)
        } else {
          oscillo3(wave)
        }
      }
    }, height = 400, width = 1500)

    observeEvent(input$waveBrush, {
      currentRegion <- c(input$waveBrush$xmin, input$waveBrush$xmax)
      if (!is.null(zoomedRegion())) {
        currentRegion <- c(zoomedRegion()[1] + currentRegion[1], zoomedRegion()[1] + currentRegion[2])
      }
      selectedRegion(currentRegion)
    })

    observeEvent(input$zoomIn, {
      req(selectedRegion())
      zoomedRegion(selectedRegion())
      selectedRegion(NULL)
    })

    # observeEvent(input$resetView, {
    #   zoomedRegion(NULL)
    # })
    #
    observeEvent(input$zoomOut, {
      if (!is.null(zoomedRegion())) {
        zoomRange <- zoomedRegion()[2] - zoomedRegion()[1]
        newStart <- max(0, zoomedRegion()[1] - zoomRange * 0.1)
        newEnd <- min(get(input$selectedWave, envir = .GlobalEnv)@samp.rate, zoomedRegion()[2] + zoomRange * 0.1)
        if (newEnd - newStart >= get(input$selectedWave, envir = .GlobalEnv)@samp.rate) {
          zoomedRegion(NULL)
        } else {
          zoomedRegion(c(newStart, newEnd))
        }
      }
    })

    observeEvent(input$saveSelection, {
      req(selectedRegion(), input$selectedWave, input$selectionName)
      wave <- get(input$selectedWave, envir = .GlobalEnv)
      selection <- extractWave(wave, from = selectedRegion()[1], to = selectedRegion()[2], xunit = "time")
      assign(input$selectionName, selection, envir = .GlobalEnv)
      showModal(modalDialog(
        title = "Saved",
        paste0("Available as '", input$selectionName, "' in the R environment."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })


    observeEvent(input$plot, {
      plotVisible(TRUE)
      selectedRegion(NULL)
      zoomedRegion(NULL)
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

  if(launch.browser){

    shinyApp(ui = ui, server = server, options = list(launch.browser = browser))

  } else {

    shinyApp(ui = ui, server = server)

  }

}

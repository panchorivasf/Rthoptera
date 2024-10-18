#' Multiplot (Spectrogram, Oscillogram, and Mean Power Spectrum) Shiny App
#'
#' This Shiny app allows users to visualize a combined plot of a spectrogram, mean spectrum, and oscillogram for a selected audio file (Wave object). Users can adjust settings such as noise cutoff, image dimensions, and background transparency, and can download the resulting plot as a PNG file.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app for visualizing, customizing, and downloading spectrogram and oscillogram plots.
#' @import shiny
#' @import ggplot2
#' @importFrom htmltools HTML
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
multiplot_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui = function(request) {
    tagList(
      h1("Multiplot", style = "font-size: 28px; margin-left: 10px;"),
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



    #multiplot {
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


    #specPlot {
      height: calc(100vh - 120px); /* Adjusts height taking into account other elements */
      width: 100%;
    }

    #specPlotOutput {
      padding-left: 0px; /* Removes padding on the left */
      margin-left: 0px;  /* Removes any margin on the left */
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
          column(2,
                 selectInput("waveObject", "Select a wave object:",
                             choices = NULL, width = '100%')
          ),
          column(1,
                 selectInput("meanspecScale", "Scale:", selected = "dB",
                             choices = c("linear", "dB"))
                 ),
          column(1,
                 numericInput("noise.cutoff", "Cutoff (dB)",
                              value = -35, min = -60, max = -20, step = 5)),
          column(1,
                 numericInput("osc.height", "Oscillogram Height (%)",
                              value = 20, min = 20, max = 80, step = 1)
          ),
          column(1,
                 numericInput("imgWidth", "Width (in):", value = 15, min = 1)),
          column(1,
                 numericInput("imgHeight", "Height (in):", value = 5, min = 1)
                 ),
          column(1,
                 actionButton("multiplot", "Plot")
                 ),
          column(1,
                 downloadButton("saveImage", "Save PNG"),
                 checkboxInput("transparentBg", "Transparent Background", value = FALSE)
                 ),
          column(1, actionButton("close", "Close App"))

        ),
        fluidRow(
          column(12,
                 uiOutput("specPlotOutput", height = "auto", width = "auto")
                )
          )
      )
    )
  }

  server = function(input, output, session) {
    plotVisible <- reactiveVal(FALSE)
    savedPlot <- reactiveVal(NULL)
    savedImage <- reactiveVal(NULL)
    spectrogramCache <- reactiveVal(NULL)

    observe({
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
        inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "waveObject", choices = waveObjects)
    })

    # Calculate the heights for the patchwork
    heights <- reactive({
      value2 <- (input$osc.height / 100) * 10
      value1 <- 10 - value2

      c(value1, value2)
    })

    observeEvent(input$multiplot, {
      req(input$waveObject)
      req(input$noise.cutoff)

      # wave <- get(input$waveObject, envir = .GlobalEnv)

      # Safely get the wave object
      wave <- tryCatch({
        get(input$waveObject, envir = .GlobalEnv)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Could not find the selected wave object:", e$message),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      })

      plotParams <- list(
        wave = wave,
        cutoff = isolate(input$noise.cutoff),
        scale.type = isolate(input$meanspecScale),
        heights = heights()
      )

      # print(plotParams)

      # spectrogramCache(plotParams)
      plotVisible(TRUE)

      output$specPlotOutput <- renderUI({
        withSpinner(plotOutput("specPlot", height = "auto", width = "auto"), type = 1)
      })

      output$specPlot <- renderPlot({
        tryCatch({
          # plotParams <- spectrogramCache()

          # combined_plot <- multiplot_ggplot(
          #   wave = plotParams$wave,
          #   cutoff = plotParams$cutoff,
          #   scale.type = plotParams$scale.type,
          #   heights = plotParams$heights
          # )
          combined_plot <- multiplot_ggplot(
            wave = wave,
            cutoff = isolate(input$noise.cutoff),
            scale.type = isolate(input$meanspecScale),
            heights = heights()
          )


          print(combined_plot)
          savedPlot(combined_plot)
          # Save the rendered image to a temporary file
          temp_file <- tempfile(fileext = ".png")
          bg <- ifelse(input$transparentBg, "transparent", "white")
          ggsave(temp_file,
                 plot = combined_plot,
                 width = isolate(input$imgWidth), height = isolate(input$imgHeight),
                 units = "in", dpi = 300, bg = bg)

          savedImage(temp_file)


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
        isolate(input$imgHeight) * 100
      }, width = function() {
        isolate(input$imgWidth) * 100
      })
    })


    output$saveImage <- downloadHandler(
      filename = function() {
        paste(as.character(input$waveObject), "_multiplot.png", sep = "")
      },
      content = function(file) {
        req(savedImage())
        file.copy(savedImage(), file)
      }
    )
    # output$saveImage <- downloadHandler(
    #   filename = function() {
    #     paste("multiplot", "_saved_", Sys.Date(), ".png", sep = "")
    #   },
    #   content = function(filename) {
    #     req(spectrogramCache())
    #     plotParams <- spectrogramCache()
    #     bg <- ifelse(input$transparentBg, "transparent", "white")
    #     ggsave(
    #       filename,
    #       plot = multiplot_ggplot(
    #         plotParams$wave,
    #         plotParams$cutoff,
    #         plotParams$scale.type,
    #         plotParams$heights
    #       ),
    #       width = input$imgWidth, height = input$imgHeight,
    #       units = "in", dpi = 300, bg = bg
    #     )
    #   }
    # )

    # Stop the app when "Close App" is clicked
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


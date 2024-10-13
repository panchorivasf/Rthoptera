#' Multiplot (Spectrogram, Oscillogram, and Mean Power Spectrum) Shiny App
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

    .modal-content {
      background-color: #252626;
      color: #ffffff;
    }

    .modal-header, .modal-footer {
      background-color: #343a40;
      color: #ffffff;
      border-bottom: 1px solid #6c757d;
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
                 selectInput("meanspecScale", "Scale:", selected = "Linear",
                             choices = c("Linear", "dB"))
                 ),
          column(1,
                 numericInput("noise.floor", "Floor (dB)",
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

      c(as.numeric(value1), as.numeric(value2))
    })

    observeEvent(input$multiplot, {
      req(input$waveObject)
      req(input$noise.floor)

      wave <- get(input$waveObject, envir = .GlobalEnv)

      plotParams <- list(
        wave = wave,
        floor = isolate(input$noise.floor),
        scale = isolate(input$meanspecScale),
        heights = heights()
      )

      spectrogramCache(plotParams)
      plotVisible(TRUE)

      output$specPlotOutput <- renderUI({
        withSpinner(plotOutput("specPlot", height = "auto", width = "auto"), type = 1)
      })

      output$specPlot <- renderPlot({
        tryCatch({
          plotParams <- spectrogramCache()

          combined_plot <- multiplot_ggplot(
            plotParams$wave,
            floor = plotParams$floor,
            scale = plotParams$scale,
            heights = plotParams$heights
          )
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
        isolate(input$imgHeight) * 100
      }, width = function() {
        isolate(input$imgWidth) * 100
      })
    })


    output$saveImage <- downloadHandler(
      filename = function() {
        paste("multiplot", "_saved_", Sys.Date(), ".png", sep = "")
      },
      content = function(filename) {
        req(spectrogramCache())
        plotParams <- spectrogramCache()
        bg <- ifelse(input$transparentBg, "transparent", "white")
        ggsave(
          filename,
          plot = multiplot_ggplot(
            plotParams$wave,
            plotParams$floor,
            plotParams$scale,
            plotParams$heights
          ),
          width = input$imgWidth, height = input$imgHeight,
          units = "in", dpi = 300, bg = bg
        )
      }
    )

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

  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}


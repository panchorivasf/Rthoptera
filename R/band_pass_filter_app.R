#' Band-pass Filter Shiny App
#'
#' This function launches a Shiny app that allows users to visualize either the mean spectrum or the spectrogram before applying a frequency filter to a Wave object.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#' @return A Shiny app interface for applying a frequency filter.
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the app
#' band_pass_filter_app()
#'
#' # Example usage after launching the app:
#' # 1. Select a wave object from the R environment.
#' # 2. Specify the the limits of the band-pass filter (high-pass, low-pass) in kHz, as needed.
#' # 3. Apply the filter and save the new wave object.
#' }
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom plotly plotlyOutput plot_ly layout config style add_ribbons
#' @importFrom seewave meanspec spectro ffilter duration
#' @importFrom bslib bs_theme
#' @importFrom shinycssloaders withSpinner


band_pass_filter_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui = function(request) {
    tagList(
      h1("Band-pass Filter", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
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

                   /* Input with info button styling */
              .input-with-info {
                display: flex;
                align-items: center;
              }

                 .input-with-info label {
                margin-right: 5px;
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

                   #plotMeanSpectrum {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
               margin-right: 5px !important;
              }

                     #plotSpectro {
               border: 2px solid forestgreen; /* Blue contour */
               border-radius: 5px; /* Optional: Rounded corners */
              }
                 #saveEditedWave {
              border: 2px solid dodgerblue; /* Blue contour */
              border-radius: 5px; /* Optional: Rounded corners */
              margin-right: 5px !important;
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
          ),
          column(2, verticalLayout(
            actionButton("plotMeanSpectrum", "Mean Spectrum"),
            actionButton("plotSpectro", "Spectrogram")
          )
          ),
          column(1, verticalLayout(
            numericInput("highpass", "HPF (kHz)", value = 0, min = 0),
            numericInput("lowpass", "LPF (kHz)", value = 48, min = 1)
          )
          ),
          column(1, actionButton("applyFilter", "Apply Filter")
          ),
          column(2, verticalLayout(
            textInput("newName", "Name for new wave:", value = ""),
            actionButton("saveEditedWave", "Save")
          )),
          column(1, actionButton("close", "Close App")),
        ),
        fluidRow(
          column(12,
                 div(style = "margin-top: 15px;",
                     withSpinner(plotlyOutput("audioPlot", height = "520px", width = "1480px"))))
        )
      )
    )
  }

  server = function(input, output, session) {

    waveObject <- reactiveVal(NULL)
    # plotly_obj <- reactiveVal()



    # Function to update wave object choices
    update_wave_choices <- function() {
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "selectedWave", choices = waveObjects)
    }

    # Observe to update wave object choices initially and on refresh
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

        # Update the lowpass value to Nyquist frequency (wave@samp.rate / 2)
        updateNumericInput(session, "lowpass", value = newWave@samp.rate / 2000)  # Divide by 2000 to convert Hz to kHz

      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "Failed to load the selected wave object. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
    })

    output$audioPlot <- renderPlotly({
      req(waveObject())
      req(input$plotMeanSpectrum)
      meanspectrum_plotly(waveObject())
    })


    observeEvent(input$plotMeanSpectrum, {
      req(waveObject())
      output$audioPlot <- renderPlotly({
        meanspectrum_plotly(waveObject())
      })
    })

    output$audioPlot <- renderPlotly({
      req(waveObject())
      req(input$plotSpectro)
      spectrogram_plotly(waveObject())
    })


    observeEvent(input$plotSpectro, {
      req(waveObject())
      output$audioPlot <- renderPlotly({
        spectrogram_plotly(waveObject())
      })
    })

    observeEvent(input$applyFilter, {
      req(waveObject())
      tryCatch({
        filtered_wave <- seewave::ffilter(
          waveObject(),
          from = isolate(input$highpass) * 1000,
          to = isolate(input$lowpass) * 1000,
          bandpass = TRUE,
          output = "Wave"
        )
        waveObject(filtered_wave)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "Failed to apply the band-pass filter. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
    })

    observeEvent(input$saveEditedWave, {
      req(waveObject(), input$newName)
      tryCatch({
        assign(input$newName, waveObject(), envir = .GlobalEnv)
        showModal(modalDialog(
          title = "Saved!",
          paste0("Available as '", input$newName, "' in the R environment."),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "Failed to save the wave object. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
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

#' Interactive Audio Import Shiny App
#'
#' This function launches a Shiny app that allows users to import and process audio files (WAV, MP3, or WAC format) into the R environment. The app supports stereo audio by allowing the user to select a channel (left or right) for processing, and it automatically removes any offset in the wave object. Users can assign a custom name to the processed wave object, which is then saved in the global environment.
#'
#' @return This function launches a Shiny app for importing audio files. The processed wave object is saved to the global environment with the name provided by the user.
#' @export
#' @import shiny
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom tuneR channel
#' @importFrom bioacoustics read_audio
#' @importFrom seewave rmoffset
#' @importFrom bslib bs_theme
#'
#' @examples
#' \dontrun{
#' # Launch the app
#' import_wave_app()
#' }
import_wave_app <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui = function(request) {
    tagList(
      h1("Import", style = "font-size: 28px; margin-left: 15px; margin-top: 0px; margin-bottom: 2px;"),
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
          .btn-group-vertical > .btn {
            margin-bottom: 5px; /* Adds space between vertical buttons */
          }
          .row {
            margin-bottom: 10px; /* Adds vertical space between rows */
          }
          #saveEditedWave {
            border: 2px solid dodgerblue; /* Blue contour */
            border-radius: 5px; /* Optional: Rounded corners */
          }
          #close {
            border: 2px solid red; /* Red contour */
            border-radius: 5px; /* Optional: Rounded corners */
          }
          .aligned-row {
            display: flex;
            align-items: left;
          }
          .aligned-row > div {
            margin-right: 15px; /* Adds consistent spacing between columns */
          }
          .aligned-row > div:last-child {
            margin-right: 0; /* Removes right margin for the last element */
          }
          "
          )
        )),
        fluidRow(
          column(3, div(class = "aligned-row",
                        fileInput(
                          "audioFile",
                          "Choose an audio file",
                          accept = c("audio/wav", ".wav", ".mp3", ".wac"))
          )),
          column(3, div(class = "aligned-row",
                        textInput("newName", "Name for new wave:", value = ""))
          ),
          column(2,div(class = "aligned-row",
                       actionButton("saveEditedWave", "Import Audio"))
          ),
          column(2, div(class = "aligned-row",
                        actionButton("close", "Close App"))
          )
        )
      )
    )

  }


  server = function(input, output, session) {
    # Increase maximum file size to 100 MB
    options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

    # Reactive values to store wave object and selected channel
    waveObject <- reactiveVal(NULL)
    selectedChannel <- reactiveVal(NULL)
    audioFilePath <- reactiveVal(NULL)

    # Function to read and process audio file
    read_and_process_audio <- function(filepath, selected_channel = NULL) {
      wave <- bioacoustics::read_audio(filepath)
      if (is.null(selected_channel) || !wave@stereo) {
        selected_channel <- 'left' # Default to the first channel for mono or unspecified channel
      }
      wave <- tuneR::channel(wave, which = selected_channel)
      wave <- seewave::rmoffset(wave, output = "Wave")
      wave
    }

    # Observe file input and read the audio file
    observeEvent(input$audioFile, {
      req(input$audioFile)
      audioFilePath(input$audioFile$datapath)
      tryCatch({
        wave <- read_audio(input$audioFile$datapath)
        if (wave@stereo) {
          showModal(modalDialog(
            title = "Stereo Audio Detected",
            radioButtons("channelSelect", "Select Channel",
                         choices = list("Left (1)" = 'left', "Right (2)" = 'right'),
                         selected = 'left'),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirmChannel", "Confirm")
            )
          ))
        } else {
          selectedChannel(1)  # Default to the first channel if mono
          waveObject(read_and_process_audio(input$audioFile$datapath))
        }
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          "Failed to read the audio file. Please try again.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      })
    })

    # Observe the channel selection and store the selected channel
    observeEvent(input$confirmChannel, {
      selectedChannel(input$channelSelect)
      waveObject(read_and_process_audio(audioFilePath(), selectedChannel()))
      removeModal()
    })

    # Observe save button, process the audio with the selected channel, and save the wave object
    observeEvent(input$saveEditedWave, {
      req(audioFilePath(), selectedChannel(), input$newName)
      tryCatch({
        wave <- read_and_process_audio(audioFilePath(), selectedChannel())
        waveObject(wave)
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

  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}

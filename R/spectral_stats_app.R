#' Spectral Statistics Shiny App
#'
#' This Shiny app allows users to analyze spectral statistics of audio data from wave objects. Users can select a wave object, input additional metadata (such as Specimen ID and sound type), and configure parameters for spectral analysis, including amplitude threshold and bandwidth. The app generates a Plotly plot of the spectrum, showing key spectral features such as peak frequency, bandwidth, and -20 dB threshold. Users can download the results in HTML or CSV format and save the data frame to the R environment.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app for analyzing and visualizing spectral statistics of audio wave objects.
#'
#' @import shiny
#' @import shinyBS
#' @import ggplot2
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom seewave meanspec sh
#' @importFrom dplyr mutate
#' @importFrom plotly plot_ly add_trace layout add_markers renderPlotly
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom htmlwidgets saveWidget
#' @importFrom utils write.csv

#' @export
#'
#' @examples
#'  \dontrun{
#' if (interactive()) {
#'   spec_stats_app()
#' }
#' }
spectral_stats_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui = function(request) {
    tagList(
      h1("Spectral Statistics",
         style = "font-size: 28px; margin-left: 15px; margin-top: 0px;
           margin-bottom: 2px; margin-right: 15px;"),

      fluidPage(
        title = "Spectral Statistics",
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



              #run{
               border: 2px solid forestgreen; /* Green contour */
               padding: 5px 5px; /* Button (inside) padding */
               border-radius: 5px; /* Rounded corners */
                       }

              #saveDataEnv {
                border: 2px solid dodgerblue; /* Blue contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Rounded corners */
                margin-bottom: 5px !important; /* Space below the button */

              }

                #downloadData {
                border: 2px solid dodgerblue; /* Blue contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Rounded corners */
                margin-bottom: 5px !important; /* Space below the button */

              }


              #savePlot {
                border: 2px solid dodgerblue; /* Blue contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Rounded corners */
                margin-bottom: 5px !important; /* Space below the button */

              }


              #close {
                border: 2px solid red; /* Red contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Rounded corners */
              }


              .btn:hover, .btn-info:hover {
                background-color: #5a6268;
                border-color: #5a6268;
              }

              /* Styling for popovers */
              .popover {
                background-color: #ffffff;
                border: 1px solid #252626;
                color: #252626;
              }


               /* DataTables Styling */
              .dataTables_wrapper .caption-top {
                caption-side: top !important;
                font-weight: bold;
                color: #ffffff;
              }

              .dataTables_wrapper .dataTables_length,
              .dataTables_wrapper .dataTables_filter,
              .dataTables_wrapper .dataTables_info,
              .dataTables_wrapper .dataTables_paginate,
              .dataTables_wrapper .dataTables_processing {
                color: #ffffff;
              }

              .dataTable thead th,
              .dataTable tfoot th {
                color: #ffffff;
                border-color: #ffffff;
              }

              .dataTable tbody td {
                color: #ffffff;
                border-color: #ffffff;
              }

              /* Ensure horizontal lines in tables are white */
              .dataTable tbody tr {
                border-top: 1px solid #ffffff;
                border-bottom: 1px solid #ffffff;
              }

              /* Input with info button styling */
              .input-with-info {
                display: flex;
                align-items: center;
              }

              .input-with-info label {
                margin-right: 5px;
              }
            "
          )
        )),

        # This goes on the "Side" panel, which is replaced with a 'manual' configuration for flexibility
        column(2,
               fluidRow(
                 column(12,
                        selectInput("selectedWave", "Select a Wave Object:",
                                    choices = NULL)
                 )
               ),

               fluidRow(
                 column(12,
                        tagList(
                          tags$label("Specimen ID"),
                          bsButton("specimen_info", label = "", lib="font-awesome",
                                   icon = icon("circle-info"), style = "default",
                                   size = "extra-small", class = "btn-info")
                        ),
                        textInput("specimen.id", label = NULL, value = "")
                 )
               ),
               bsPopover(
                 id = "specimen_info",
                 title = "Specimen ID",
                 content = HTML(paste0("A unique identifier for the specimen.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        tagList(
                          tags$label("Sound Type"),
                          bsButton("soundtype_info", label = "", lib="font-awesome",
                                   icon = icon("circle-info"), style = "default",
                                   size = "extra-small", class = "btn-info")
                        ),
                        textInput("sound.type", label = NULL, value = "Calling song")
                 )
               ),
               bsPopover(
                 id = "soundtype_info",
                 title = "Sound Type",
                 content = HTML(paste0("The type of sound under analysis.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),



               fluidRow(
                 column(12,
                        tagList(
                          tags$label("Temperature (C)"),
                          bsButton("temp_info", label = "", lib="font-awesome",
                                   icon = icon("circle-info"), style = "default",
                                   size = "extra-small", class = "btn-info")
                        ),
                        numericInput("temp",
                                     label = NULL,
                                     value = NA,
                                     min = 0,
                                     max = 60,
                                     step = 0.1),
                 )
               ),
               bsPopover(
                 id = "temp_info",
                 title = "Temperature",
                 content = HTML(paste0("The ambient temperature at the moment of the recording, in degrees Celcius.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        tagList(
                          tags$label("HPF (kHz)"),
                          bsButton("hpf_info", label = "", lib="font-awesome",
                                   icon = icon("circle-info"), style = "default",
                                   size = "extra-small", class = "btn-info")
                        ),
                        numericInput("hpf",
                                     label = NULL,
                                     value = 0,
                                     min = 0,
                                     max = 15,
                                     step = 1),

                 )
               ),
               bsPopover(
                 id = "hpf_info",
                 title = "High Pass Fiter",
                 content = HTML(paste0("If a High-Pass Filter was applied to the Wave before the analysis, inform which frequency was used, in kHz.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        tagList(
                          tags$label("Scale"),
                          bsButton("scale_info", label = "", lib="font-awesome",
                                   icon = icon("circle-info"), style = "default",
                                   size = "extra-small", class = "btn-info")
                        ),
                        selectInput("ampMax",
                                    label = NULL,
                                    choices = list("dB (max0)" = 0, "Linear" = 1),
                                    selected = 1)

                 )
               ),
               bsPopover(
                 id = "scale_info",
                 title = "Scale",
                 content = HTML(paste0("Select either decibel or linear scale. Both scales are normalized by the maximum amplitude value in the Wave. The ranges are [0:1] for the linear scale and [-(min):0] for the decibel scale.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),

               fluidRow(
                 column(12,
                        tagList(
                          tags$label("Threshold"),
                          bsButton("threshold_info", label = "", lib="font-awesome",
                                   icon = icon("circle-info"), style = "default",
                                   size = "extra-small", class = "btn-info")
                        ),
                        numericInput("dbth",
                                     label = NULL,
                                     value = -20,
                                     min = -3,
                                     max = -100,
                                     step = 1)
                 )
               ),
               bsPopover(
                 id = "threshold_info",
                 title = "Threshold",
                 content = HTML(paste0("Select an amplitude threshold to be used for the calculation of the frequency bandwidth. A common threshold is -20 dB below the peak, which in the linear scale is equivalent to 0.1.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               )

        ),


        # This goes on the "Main" panel
        column(10,
               fluidRow(
                 column(12,
                        column(2,
                               fluidRow(
                                 column(8, checkboxInput("total", "Total Bandwidth", value = FALSE)),
                                 column(4, bsButton("total_bandwidth_info", label = "", lib="font-awesome",
                                                    icon = icon("circle-info"), style = "margin-left: 1px;",
                                                    size = "extra-small", class = "btn-info"))
                               ),
                               fluidRow(
                                 column(8, checkboxInput("robust", "Robust", value = FALSE)),
                                 column(4, bsButton("robust_info", label = "", lib="font-awesome",
                                                    icon = icon("circle-info"), style = "default",
                                                    size = "extra-small", class = "btn-info"))
                               )
                        ),
                        column(2, actionButton("run", "Run Analysis")),
                        column(2, textInput("dataName", "Table name", value = "")),
                        column(2, verticalLayout(
                          actionButton("saveDataEnv", "Table to R"),
                          downloadButton("downloadData", "Export CSV"),
                          downloadButton("savePlot", "Save Plot")
                        )),
                        column(1, actionButton("close", "Close App", style = 'white-space: nowrap;'))
                 )
               ),

               # Popovers
               bsPopover(
                 id = "total_bandwidth_info",
                 title = "Total Bandwidth",
                 content = HTML("This option uses the first and last samples (from left to right) above the amplitude threshold to assess the frequency bandwidth of the signal, regardless of the gaps (i.e., where the sampled frequencies go below the threshold) that might occur between the extremes and the peak frequency."),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),

               bsPopover(
                 id = "robust_info",
                 title = "Robust",
                 content = HTML("This option performs a robust analysis, where the frequency resolution is fixed at ~244.1 Hz, intended to reflect broad spectral structure, thus ignoring subtle differences between individuals."),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),

               fluidRow(
                 column(12,
                        div(style = "margin-top: 10px; margin-left: 10px; margin-right: 10px;",
                            withSpinner(uiOutput("plotOutput")))
                 )
               ),

               fluidRow(
                 column(12,
                        withSpinner(DTOutput("dataOutput"))
                 )
               )
        )
      )
    )
  }

  server = function(input, output, session) {
    # Function definition
    specStats <- function(wave, specimen.id = "Specimen ID",
                          total.range = FALSE,
                          robust = FALSE, ampMax = 1,  dbth = -20,
                          lines = TRUE,
                          sound.type = "Call 1",
                          temp = NULL,
                          hpf = NULL) {

      ampMax <- as.numeric(ampMax)

      freq_per_bin <- if (robust) {
        244.1406
      } else {
        30.51758
      }

      sampling_rate <- wave@samp.rate
      wlen <- sampling_rate / freq_per_bin


      if(ampMax==0){
        spec1 <- meanspec(wave, wl = wlen, dB = NULL, plot = FALSE)
        spEnt <- sh(spec1)
        spFlat <- sfm(spec1)
        rm(spec1)
        spec <- meanspec(wave, wl = wlen, dB = "max0", plot = FALSE)
      }else{
        spec <- meanspec(wave, wl = wlen, dB = if (ampMax == 0) "max0" else NULL, plot = FALSE)
        spEnt <- sh(spec)
        spFlat <- sfm(spec)
      }

      spec_df <- as.data.frame(spec)
      names(spec_df) <- c("Frequency", "Amplitude")

      max_amp_index <- which.max(spec_df$Amplitude)
      peak_frequency <- spec_df$Frequency[max_amp_index]
      A_peak <- spec_df$Amplitude[max_amp_index]

      A_ref <- if (ampMax == 1) {
        A_peak * dbth
        # A_peak * 10^(dbth / 20)
      } else {
        A_peak - abs(dbth)
      }

      minfreq_index <- if (total.range) {
        which(spec_df$Amplitude[1:max_amp_index] >= A_ref)[1]
      } else {
        max(which(spec_df$Amplitude[1:max_amp_index] <= A_ref))
      }

      maxfreq_index <- if (total.range) {
        max_amp_index + which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] >= A_ref)[length(which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] >= A_ref))]
      } else {
        max_amp_index + min(which(spec_df$Amplitude[max_amp_index:nrow(spec_df)] <= A_ref)) - 1
      }

      minfreq <- spec_df$Frequency[minfreq_index]
      maxfreq <- spec_df$Frequency[maxfreq_index]

      p <- plot_ly(
        spec_df,
        x = ~ Frequency,
        y = ~ Amplitude,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'black', width = 0.5),
        hovertemplate = paste("Frequency: %{x:.2f} kHz<br>Amplitude: %{y:.2f}<extra></extra>"),
        showlegend = FALSE
      )

      text_label <- paste(
        "<b> Summary Statistics</b>",
        "<br> Low Freq:", round(minfreq, 1), "kHz",
        "<br> Peak Freq:", round(peak_frequency, 1), "kHz",
        "<br> High Freq:", round(maxfreq, 1), "kHz",
        "<br> Bandwidth:", round(maxfreq - minfreq, 1), "kHz",
        "<br> Spectral Entropy:", round(spEnt,1),
        "<br> Spectral Flatness:", round(spFlat,1),
        "<br> Temp:", temp, "C",
        "<br> HPF:", hpf, "kHz"
      )

      p <- p %>%
        layout(
          yaxis = list(range = ifelse(ampMax == 0, c(-50, 0), c(0, 1))),
          margin = list(l = 50, r = 50, t = 100, b = 50),
          title = list(text = sprintf("<i>%s</i>", specimen.id), x = 0, y = 1.1, xref = "paper", yref = "paper", xanchor = 'left', yanchor = 'top'),
          xaxis = list(title = "Frequency (kHz)"),
          yaxis = list(title = ifelse(ampMax == 1, "Relative Amplitude", "Amplitude (dB)")),
          annotations = list(
            x = 1, y = .95, text = text_label, showarrow = FALSE, xref = 'paper', yref = 'paper', xanchor = 'right', yanchor = 'top',
            font = list(size = 12), bgcolor = 'rgba(255,255,255,1)', bordercolor = '#404040', align = 'left'
          )
        )

      if (lines) {
        p <- p %>%
          add_trace(
            x = c(minfreq, minfreq), y = c(min(spec_df$Amplitude), A_ref),
            type = 'scatter', mode = 'lines', line = list(color = "#1E90FF", width = 1, dash = 'solid'),
            name = "Min Frequency", hovertemplate = paste("MinFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
          ) %>%
          add_trace(
            x = c(peak_frequency, peak_frequency), y = c(min(spec_df$Amplitude), max(spec_df$Amplitude)),
            type = 'scatter', mode = 'lines', line = list(color = "#EE0000", width = 1, dash = 'solid'),
            name = "Peak Frequency", hovertemplate = paste("PeakFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
          ) %>%
          add_trace(
            x = c(maxfreq, maxfreq), y = c(min(spec_df$Amplitude), A_ref),
            type = 'scatter', mode = 'lines', line = list(color = "#FF7F00", width = 1, dash = 'solid'),
            name = "Max Frequency", hovertemplate = paste("MaxFreq: %{x} kHz <extra></extra>"), showlegend = TRUE
          ) %>%
          add_trace(
            x = spec_df$Frequency, y = rep(A_ref, nrow(spec_df)),
            type = 'scatter', mode = 'lines', line = list(color = 'forestgreen', width = 1, dash = 'dash'),
            name = "dB Threshold", hovertemplate = paste("-20 dB Threshold <extra></extra>"), showlegend = TRUE
          )
      }

      p <- p %>% add_markers(
        x = peak_frequency, y = max(spec_df$Amplitude), type = 'scatter', mode = 'markers',
        marker = list(symbol = 'triangle-down', color = "#EE0000", size = 10), name = "Peak", showlegend = TRUE, hoverinfo = 'none', inherit = FALSE
      )

      scaling <- ifelse(ampMax == 1, "max1", "max0")
      q_factor <- peak_frequency / (maxfreq - minfreq)

      df <- tibble(
        specimen.id = specimen.id,
        sound.type = sound.type,
        low.f = round(minfreq, 1),
        high.f = round(maxfreq, 1),
        bandw = round(maxfreq - minfreq, 1),
        peak.f = round(peak_frequency, 1),
        q = round(q_factor, 1),
        spec.ent = round(spEnt, 1),
        spec.flat = round(spFlat,1),
        temp = temp,
        par.hpf = hpf,
        par.dbth = dbth,
        par.samprate = sampling_rate / 1000,
        par.wlen = round(wlen),
        par.freq.res = round(freq_per_bin, 1),
        par.robust = robust,
        par.scale = scaling
      )

      list(data = df, plot = p)
    }

    # Store reactive values
    values <- reactiveValues(speciesName = "", callType = "")

    # Update the title whenever the input changes
    observe({
      values$speciesName <- input$specimen.id
      values$callType <- input$sound.type
    })

    # Observer to update available wave objects in the environment
    observe({
      wave_names <- ls(envir = .GlobalEnv)
      wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "selectedWave", choices = wave_names)
    })


    observeEvent(input$ampMax, {
      if (input$ampMax == 0) { # If "dB (max0)" is selected
        updateNumericInput(session, "dbth",
                           value = -20, min = -100, max = -3, step = 1)
      } else { # If "Linear" is selected
        updateNumericInput(session, "dbth",
                           value = 0.1, min = 0.01, max = 0.99, step = 0.01)
      }
    })


    # This reactive expression will re-run only when the "Plot" button is clicked
    result <- eventReactive(input$run, {
      req(input$selectedWave)
      wave <- get(input$selectedWave, envir = .GlobalEnv)
      specStats(
        wave = wave,
        specimen.id = input$specimen.id,
        sound.type = input$sound.type,
        temp = input$temp,
        hpf = input$hpf,
        dbth = as.numeric(input$dbth),
        total.range = input$total,
        lines = TRUE,
        ampMax = as.numeric(input$ampMax),
        robust = input$robust
      )
    })

    output$plotOutput <- renderUI({
      req(result())
      plotlyOutput("plotlyPlot", width = '1150px')
    })

    output$plotlyPlot <- renderPlotly({
      req(result())
      result()$plot %>%
        layout(margin = list(
          l = 80, r = 0, t = 80, b = 80
        ),
        annotations = list(
          list(
            text = input$sound.type,
            font = list(size = 13, color = 'black'),
            showarrow = FALSE, align = 'right',
            x = 0, y = 1.1, xref = 'x', yref = 'paper'
          )
        ))
    })

    output$dataOutput <- renderDT({
      req(result())
      datatable(result()$data,
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: center;",
                  class = "caption-top",
                  "Spectral Data"
                ),
                options = list(
                  pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                  columnDefs = list(list(orderable = FALSE, targets = "_all"))
                ))
    })


    # Download handler for downloading data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(tolower(values$speciesName), "_", tolower(values$callType), "_spectral_stats.csv")
      },
      content = function(file) {
        write.csv(result()$data, file, row.names = FALSE)
      }
    )

    # Save data frame in the R environment
    observeEvent(input$saveDataEnv, {
      req(result(), input$dataName)
      assign(input$dataName, result()$data, envir = .GlobalEnv)
      showModal(modalDialog(
        title = "Saved",
        paste0("Available as '", input$dataName, "' in the R environment."),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })

    # Save plot
    output$savePlot <- downloadHandler(
      filename = function() {
        paste0(tolower(values$speciesName), "_", tolower(values$callType), "_spectral_analysis.html")
      },
      content = function(file) {
        htmlwidgets::saveWidget(result()$plot, file, selfcontained = TRUE)
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



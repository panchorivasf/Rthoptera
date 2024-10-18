#' Temporal Statistics HQ Shiny App
#'
#' This Shiny app performs temporal statistics analysis on wave objects with "HQ" sounds (i.e., near pure tones such as those made by most crickets). It allows the user to configure multiple parameters such as smoothing window, overlap, and detection thresholds to analyze the temporal patterns of acoustic signals. The app outputs interactive visualizations of the results and provides data tables summarizing trains, motifs, and summary statistics.
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app that analyzes temporal statistics of acoustic waveforms.
#' @import shiny
#' @import shinyBS
#' @import dplyr
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom plotly plot_ly add_lines layout renderPlotly
#' @importFrom DT datatable renderDT
#' @importFrom writexl write_xlsx
#' @importFrom seewave env resamp
#' @importFrom purrr map
#' @export
#'
#' @examples
#'  \dontrun{
#' if (interactive()) {
#'   temp_stats_hiq_app()
#' }
#' }
temporal_stats_hq_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui <- function(request) {
    tagList(
      h1("Temporal Statistics HQ", style = "font-size: 28px; margin-left: 15px;"),
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


              #run {
               border: 2px solid forestgreen; /* Blue contour */
               padding: 5px 5px; /* Button (inside) padding */
               border-radius: 5px; /* Optional: Rounded corners */
                       }

              #saveData {
                border: 2px solid dodgerblue; /* Blue contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Optional: Rounded corners */
              }

              #savePlot {
                border: 2px solid dodgerblue; /* Blue contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Optional: Rounded corners */
              }

              #close {
                border: 2px solid red; /* Red contour */
                padding: 5px 5px; /* Button (inside) padding */
                border-radius: 5px; /* Optional: Rounded corners */
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

        column(2,
               fluidRow(
                 column(12,
                        selectInput("selectedWave", "Select a Wave Object:", choices = NULL)
                 )
               ),
               fluidRow(
                 column(12,
                        tagList(
                          tags$label("Presets"),
                          bsButton("preset_info", label = "", lib="font-awesome",
                                   icon = icon("circle-info"), style = "default",
                                   size = "extra-small", class = "btn-info")
                        ),
                        selectInput("preset", label = NULL, choices = c("Tettigoniidae", "Gryllidae"), selected = "Gryllidae")
                 )
               ),
               bsPopover(
                 id = "preset_info",
                 title = "Preset",
                 content = HTML(paste0("Optimized parameters for call patterns in particular taxa.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Specimen ID"),
                              bsButton("specimen_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        textInput("specimen_id", label = NULL, value = "")
                 )
               ),

               bsPopover(
                 id = "specimen_info",
                 title = "Specimen ID",
                 content = HTML(paste0("The Specimen ID is used to identify the individual specimen in your analysis.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),

               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Smoothing Window (points)"),
                              bsButton("msmooth_window_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")

                            )
                        ),
                        numericInput("msmooth_window", label = NULL, value = 100,
                                     min = 10, max = 1000, step = 10)
                 )
               ),
               bsPopover(
                 id = "msmooth_window_info",
                 title = "Smoothing Window Length",
                 content = HTML(paste0("Window size (samples) used to smooth the envelope. A larger window will result in a smoother envelope.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),

               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Overlap (%)"),
                              bsButton("msmooth_overlap_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("msmooth_overlap", label = NULL, value = 50, min = 0, max = 100, step = 5)
                 )
               ),

               bsPopover(
                 id = "msmooth_overlap_info",
                 title = "Window Overlap",
                 content = HTML(paste0("Overlap percentage between successive windows during smoothing. Higher overlap results in more smoothing.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),

               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Upper Detection Threshold"),
                              bsButton("upDet_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("upper_detection_threshold",
                                     label = NULL,
                                     value = 0.2,
                                     min = 0.01,
                                     max = 0.99,
                                     step = 0.01)
                 )
               ),
               bsPopover(
                 id = "upDet_info",
                 title = "Upper Detection Threshold",
                 content = HTML(paste0("Minimum amplitude (proportion) required for a train to be included in the analysis. Trains with maximum amplitude below this value will be excluded.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Lower Detection Threshold"),
                              bsButton("loDet_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("lower_detection_threshold", label = NULL,
                                     value = 0.1,
                                     min = 0.01,
                                     max = 0.99,
                                     step = 0.01)
                 )
               ),
               bsPopover(
                 id = "loDet_info",
                 title = "Lower Detection Threshold",
                 content = HTML(paste0("Amplitude threshold as a proportion of the maximum amplitude. Only trains with an amplitude above this threshold will be detected.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),



               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Max Train Gap (s)"),
                              bsButton("max_train_gap_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("max_train_gap",
                                     label = NULL,
                                     value = 0.08,
                                     min = 0.01, max = 1,
                                     step = 0.01)
                 )
               ),
               bsPopover(
                 id = "max_train_gap_info",
                 title = "Max Train Gap",
                 content = HTML(paste0("Maximum gap allowed between trains to be considered in the same motif. If the gap exceeds this value, a new motif is started.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


        ),

        column(10,
               fluidRow(
                 column(2, actionButton("run", "Run Analysis")),
                 column(2, downloadButton("saveData", "Export Excel Workbook")),
                 column(2, verticalLayout(
                   downloadButton("savePlot", "Export HTML Plot"),
                   # checkboxInput("show_annotations", "Show Annotations", value = TRUE)  # Add this checkbox
                 )
                 ),
                 column(1, actionButton("close", "Close App"))
               ),

               fluidRow(
                 column(12,
                        h3("NOTICE: This function resamples Wave objects to 192 kHz for consistent
             minimum resolution of time measurements. Sampling rates equal or higher are not resampled. See documentation for details.",
                           style = "font-size: 12px; color: lightgrey;"),
                        withSpinner(plotlyOutput("audioPlot")),
                        DTOutput("summary_data"),
                        DTOutput("motif_data"),
                        DTOutput("train_data"),
                        # DTOutput("gap_data"),
                        DTOutput("params"),
                        style = "padding: 10px;"
                 )
               )
        )
      )
    )

  }

  server <- function(input, output, session) {

    observe({
      wave_names <- ls(envir = .GlobalEnv)
      wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "selectedWave", choices = wave_names)
    })

    result <- eventReactive(input$run, {
      req(input$selectedWave)
      wave <- get(input$selectedWave, envir = .GlobalEnv)
      temporal_stats_hq(wave,
                        specimen.id = isolate(input$specimen_id),
                        msmooth_window = as.numeric(isolate(input$msmooth_window)),
                        msmooth_overlap = as.numeric(isolate(input$msmooth_overlap)),
                        upper_detection_threshold = as.numeric(isolate(input$upper_detection_threshold)),
                        lower_detection_threshold = as.numeric(isolate(input$lower_detection_threshold)),
                        min_train_dur = as.numeric(0.002),
                        max_train_gap = as.numeric(isolate(input$max_train_gap)),
                        norm.env = TRUE)

    })


    observeEvent(input$preset, {

      if (input$preset == "Gryllidae") {

        updateTextInput(session, "specimen_id", value = "")
        updateNumericInput(session, "msmooth_window", value = 100)
        updateNumericInput(session, "msmooth_overlap", value = 50)
        updateNumericInput(session, "lower_detection_threshold", value = 0.08)
        updateNumericInput(session, "upper_detection_threshold", value = 0.2)
        updateNumericInput(session, "max_train_gap", value = 0.08)

      } else if (input$preset == "Tettigoniidae") {

        updateTextInput(session, "specimen_id", value = "")
        updateNumericInput(session, "msmooth_window", value = 900)
        updateNumericInput(session, "msmooth_overlap", value = 50)
        updateNumericInput(session, "upper_detection_threshold", value = 0.05)
        updateNumericInput(session, "lower_detection_threshold", value = 0.02)
        updateNumericInput(session, "max_train_gap", value = 0.1)

      }
    })

    output$audioPlot <- renderPlotly({
      req(result())
      temp_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(result()$plot, temp_file, selfcontained = TRUE)
      temp_file <<- temp_file

      p <- result()$plot

    })

    output$summary_data <- renderDT({
      req(result())
      datatable(result()$summary_data,
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: left;",
                  class = "caption-top",
                  "Summary"
                ),
                options = list(
                  pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                  columnDefs = list(list(orderable = FALSE, targets = "_all"))
                ))
    })

    output$motif_data <- renderDT({
      req(result())
      datatable(result()$motif_data,
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: left;",
                  class = "caption-top",
                  "Motif Data"
                ),
                options = list(
                  pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                  columnDefs = list(list(orderable = FALSE, targets = "_all"))
                ))
    })

    output$train_data <- renderDT({
      req(result())
      datatable(result()$train_data,
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: left;",
                  class = "caption-top",
                  "Train Data"
                ),
                options = list(
                  pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                  columnDefs = list(list(orderable = FALSE, targets = "_all"))
                ))
    })


    output$params <- renderDT({
      req(result())
      datatable(result()$params,
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: left;",
                  class = "caption-top",
                  "Parameters"
                ),
                options = list(
                  pageLength = 1, lengthChange = FALSE, searching = FALSE,
                  paging = FALSE, info = FALSE,
                  columnDefs = list(list(orderable = FALSE, targets = "_all"))
                ))
    })

    output$savePlot <- downloadHandler(
      filename = function() {
        paste0(input$specimen_id, "_tempstats_plot.html")
      },
      content = function(file) {
        req(temp_file)
        file.copy(temp_file, file)
      }
    )

    output$saveData <- downloadHandler(
      filename = function() {
        paste(input$specimen_id, "tempstats_data.xlsx", sep = "_")
      },
      content = function(file) {
        req(result())
        data_list <- list(
          "Summary" = result()$summary_data,
          "Motif Data" = result()$motif_data,
          "Train Data" = result()$train_data,
          "Parameters" = result()$params
        )
        writexl::write_xlsx(data_list, path = file)
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

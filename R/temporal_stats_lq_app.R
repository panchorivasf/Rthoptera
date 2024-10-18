#' Temporal Statistics LQ Shiny App
#'
#' This Shiny app performs temporal statistics analysis on Wave objects
#' containing 'LQ' signals (broadband). It allows the user to configure
#' parameters such as smoothing window, peak finder thresholds,
#' and gap limits for trains and peaks. The app generates interactive
#' plots and data tables summarizing motifs, trains, and peaks, along with
#' parameter settings.
#'
#' New metrics provided by this function include the Temporal Complexity Index (within motif),
#' Temporal Excursion (within train), and Dynamic Excursion (within train).
#'
#' @param launch.browser Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.
#'
#' @return A Shiny app for analyzing temporal statistics of acoustic waveforms.
#'
#' @import shiny
#' @import shinyBS
#' @importFrom htmltools HTML
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom plotly plot_ly add_lines layout renderPlotly add_markers
#' @importFrom DT datatable renderDT
#' @importFrom seewave duration
#' @importFrom warbleR envelope
#' @importFrom writexl write_xlsx
#' @importFrom dplyr group_by summarize ungroup mutate lead tibble relocate
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' if (interactive()) {
#'   temporal_stats_lq_app()
#' }
#' }
temporal_stats_lq_app <- function(launch.browser = FALSE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui <- function(request) {
    tagList(
      h1("Temporal Statistics LQ", style = "font-size: 28px; margin-left: 15px;"),
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
                        selectInput("preset", label = NULL,
                                    choices = c("Tettigoniidae", "Gryllidae"),
                                    selected = "Tettigoniidae")
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
                 content = HTML(paste0("A unique identifier for the specimen. For example, &#39GRYCAM_001&#39, is the &#39Alpha code&#39 for Gryllus campestris, specimen 001.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),

               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Detection Threshold"),
                              bsButton("detection_threshold_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("detection_threshold",
                                     label = NULL,
                                     value = 0.01,
                                     min = 0.001,
                                     max = 1,
                                     step = 0.001)
                 )
               ),
               bsPopover(
                 id = "detection_threshold_info",
                 title = "Detection Threshold",
                 content = HTML(paste0("Set the threshold for detecting peaks. Any peak below this value will be discarded.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),



               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Smoothing"),
                              bsButton("ssmooth_window_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("ssmooth", label = NULL, value = 100,
                                     min = 10, max = 1000, step = 10)
                 )
               ),
               bsPopover(
                 id = "ssmooth_window_info",
                 title = "Smoothing",
                 content = HTML(paste0("Window size (samples) used to smooth the envelope. A larger window will result in a smoother envelope.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Peakfinder Window"),
                              bsButton("peakfinder_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("peakfinder_ws",
                                     label = NULL,
                                     value = 40,
                                     min = 10,
                                     max = 200,
                                     step = 5)
                 )
               ),
               bsPopover(
                 id = "peakfinder_info",
                 title = "Peakfinder Window",
                 content = HTML(paste0("Window size (samples) used to find peaks along the envelope.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Peakfinder Threshold"),
                              bsButton("peakfinder_thr_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("peakfinder_threshold",
                                     label = NULL,
                                     value = 0.005,
                                     min = 0.001,
                                     max = 0.5,
                                     step = 0.001)
                 )
               ),
               bsPopover(
                 id = "peakfinder_thr_info",
                 title = "Peakfinder Threshold",
                 content = HTML(paste0("The minimum distance between a valley and a peak. This distance is measured as a proportion relative to the maximum amplitude [0:1].")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Max Peak Gap"),
                              bsButton("max_peak", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("max_peak_gap", label = NULL, value = 0.01,
                                     min = 0.001, max = 0.1, step = 0.001)

                 )
               ),
               bsPopover(
                 id = "max_peak",
                 title = "Max Peak Gap",
                 content = HTML(paste0("The maximum gap (in seconds) allowed between peaks to be considered as belonging to the same train.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               ),


               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Max Train Gap"),
                              bsButton("max_train", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("max_train_gap", label = NULL, value = 0.08,
                                     min = 0.01, max = 1, step = 0.01)
                 )
               ),
               bsPopover(
                 id = "max_train",
                 title = "Max Train Gap",
                 content = HTML(paste0("The maximum gap (in seconds) allowed between trains to be grouped in the same motif.")),
                 placement = "right",
                 trigger = "click",
                 options = list(container = "body")
               )

        ),

        column(10,
               fluidRow(
                 column(2, actionButton("run", "Run Analysis")),
                 column(2, downloadButton("saveData", "Export Excel Workbook")),
                 column(2, downloadButton("savePlot", "Export HTML Plot")),
                 # column(2, actionButton("help", "Help")),
                 column(1, actionButton("close", "Close App")),
                 style = "margin-bottom: 20px;"

               ),
               fluidRow(
                 column(12,
                        withSpinner(plotlyOutput("audioPlot")),
                        DTOutput("summary_data"),
                        DTOutput("motif_data"),
                        DTOutput("train_data"),
                        DTOutput("peak_data"),
                        DTOutput("params")
                 )
               )
        )
      )
    )
  }

  server <- function(input, output, session) {


    # Observer to update available wave objects in the environment
    observe({
      wave_names <- ls(envir = .GlobalEnv)
      wave_names <- wave_names[sapply(wave_names, function(x) inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "selectedWave", choices = wave_names)
    })

    result <- eventReactive(input$run, {
      req(input$selectedWave)
      wave <- get(input$selectedWave, envir = .GlobalEnv)
      temporal_stats_lq(wave,
                        specimen_id = input$specimen_id,
                        ssmooth = input$ssmooth,
                        peakfinder_ws = input$peakfinder_ws,
                        peakfinder_threshold = input$peakfinder_threshold,
                        max_train_gap = input$max_train_gap,
                        max_peak_gap = input$max_peak_gap,
                        detection_threshold = input$detection_threshold,
                        norm_env = TRUE)
    })

    observeEvent(input$preset, {

      if (input$preset == "Gryllidae") {

        # updateTextInput(session, "specimen_id", value = "")
        updateNumericInput(session, "ssmooth", value = 0)
        updateNumericInput(session, "peakfinder_ws", value = 5)
        updateNumericInput(session, "peakfinder_threshold", value = 0.05)
        updateNumericInput(session, "max_peak_gap", value = 0.01)
        updateNumericInput(session, "max_train_gap", value = 0.05)
        updateNumericInput(session, "detection_threshold", value = 0.1)


      } else if (input$preset == "Tettigoniidae") {

        # updateTextInput(session, "specimen_id", value = "")
        updateNumericInput(session, "ssmooth", value = 100)
        updateNumericInput(session, "peakfinder_ws", value = 50)
        updateNumericInput(session, "peakfinder_threshold", value = 0.01)
        updateNumericInput(session, "max_peak_gap", value = 0.01)
        updateNumericInput(session, "max_train_gap", value = 0.05)
        updateNumericInput(session, "detection_threshold", value = 0.001)


      }
    })

    output$audioPlot <- renderPlotly({
      req(result())
      result()$plot %>%
        layout(title = input$specimen_id,
               margin = list(l = 80, r = 0, t = 80, b = 80))
    })


    output$summary_data <- renderDT({
      req(result())
      datatable(result()$summary_data,
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: left;",
                  class = "caption-top",
                  "Summary Data"
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

    output$peak_data <- renderDT({
      req(result())
      datatable(result()$peak_data,
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: left;",
                  class = "caption-top",
                  "Peak Data"
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
                  pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                  columnDefs = list(list(orderable = FALSE, targets = "_all"))
                ))
    })


    output$saveData <- downloadHandler(
      filename = function() {
        paste(input$specimen_id, "tempstats_lq_data.xlsx", sep = "_")
      },
      content = function(file) {
        req(result())
        # Collect all data tables
        data_list <- list(
          "Summary Data" = result()$summary_data,
          "Motif Data" = result()$motif_data,
          "Train Data" = result()$train_data,
          "Peak Data" = result()$peak_data,
          "Parameters" = result()$params
        )

        # Write the list of data frames to an Excel file
        writexl::write_xlsx(data_list, path = file)
      }
    )

    output$savePlot <- downloadHandler(
      filename = function() {
        paste(input$specimen_id, "tempstats_hq_plot.html", sep = "_")
      },
      content = function(file) {
        req(temp_file)
        file.copy(temp_file, file)
      }
    )


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







#' Temporal Statistics HQ Shiny App
#'
#' This Shiny app performs temporal statistics analysis on wave objects with "HQ" sounds (i.e., near pure tones such as those made by most crickets). It allows the user to configure multiple parameters such as smoothing window, overlap, and detection thresholds to analyze the temporal patterns of acoustic signals. The app outputs interactive visualizations of the results and provides data tables summarizing trains, motifs, and summary statistics.
#'
#' @return A Shiny app that analyzes temporal statistics of acoustic waveforms.
#' @import shiny
#' @import shinyBS
#' @import dplyr
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
temporal_stats_hq_app <- function() {

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
                        numericInput("max_train_gap", label = NULL, value = 0.08,
                                     min = 0.01, max = 1, step = 0.01)
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
    temporal_stats_hq <- function(wave,
                                  specimen.id = "",
                                  msmooth_window,
                                  msmooth_overlap,
                                  upper_detection_threshold,
                                  lower_detection_threshold,
                                  min_train_dur,
                                  max_train_gap,
                                  norm.env = TRUE) {

      # library(plotly)
      # library(htmlwidgets)
      # library(tibble)
      # library(dplyr)
      # library(purrr)
      # library(seewave)
      # library(tuneR)

      # Store input parameters in a tibble
      params <- tibble(
        specimen.id = specimen.id,
        msmooth_window = msmooth_window,
        msmooth_overlap = msmooth_overlap,
        max_train_gap = max_train_gap,
        upper_detection_threshold = upper_detection_threshold,
        lower_detection_threshold = lower_detection_threshold,
        norm.env = norm.env
      )


      if (wave@samp.rate < 192000) {
        wave <- resamp(wave, g = 192000, output = "Wave")
      }

      msmooth_vec <- c(msmooth_window, msmooth_overlap)

      # Get envelope of the wave
      envelope_vector <- seewave::env(wave, msmooth = msmooth_vec, plot = FALSE)
      # envelope_vector <- warbleR::envelope(as.numeric(wave@left), ssmooth = msmooth_window)

      if (norm.env) {
        envelope_vector <- (envelope_vector - min(envelope_vector)) / (max(envelope_vector) - min(envelope_vector))
      }

      # Determine threshold based on maximum amplitude
      max_amplitude <- max(envelope_vector)
      amp_threshold <- max_amplitude * lower_detection_threshold
      min_amp_threshold <- max_amplitude * upper_detection_threshold

      # Create the time vector
      sample_rate <- wave@samp.rate
      time_vector <- seq(0, (length(wave@left) - 1) / sample_rate, length.out = length(envelope_vector))  # In seconds

      # Detect trains based on the amplitude lower_detection_threshold
      train_starts <- which(diff(c(0, envelope_vector >= amp_threshold, 0)) == 1)
      train_ends <- which(diff(c(0, envelope_vector >= amp_threshold, 0)) == -1) - 1

      # Initialize data storage
      trains <- list()

      for (i in seq_along(train_starts)) {
        train_start <- time_vector[train_starts[i]]
        train_end <- time_vector[train_ends[i]]
        train_max_amp <- max(envelope_vector[train_starts[i]:train_ends[i]])

        # Exclude trains below upper_detection_threshold
        if (train_max_amp < upper_detection_threshold) {
          next
        }

        # Append train to the list as a numeric vector
        trains <- append(trains, list(c(train_start, train_end)))
      }

      train_data <- tibble(
        specimen.id = rep(specimen.id, length(trains)),
        train.start = round(sapply(trains, `[[`, 1), 3),
        train.end = round(sapply(trains, `[[`, 2), 3),
        train.dur = round(sapply(trains, function(x) if (length(x) > 1) x[2] - x[1] else NA), 3)
      ) %>%
        dplyr::filter(!is.na(train.dur) & train.dur > min_train_dur)

      # Calculate train.period and train.gap directly from train_data
      train_data <- train_data %>%
        mutate(
          train.period = round(lead(train.start) - train.start, 3),  # Period: next train.start - current train.start
          train.gap = round(lead(train.start) - train.end, 3)        # Gap: next train.start - current train.end
        )

      # Calculate train.id and motif.id based on train.gap and max_train_gap
      train_data <- train_data %>%
        mutate(
          motif.id = cumsum(c(TRUE, train.gap[-n()] > max_train_gap)),
          train.id = sequence(rle(cumsum(c(TRUE, train.gap[-n()] > max_train_gap)))$lengths)
        )

      # Summarize motif data
      motif_data <- train_data %>%
        group_by(motif.id) %>%
        summarize(
          motif.start = min(train.start),
          motif.end = max(train.end),
          motif.dur = round(motif.end - motif.start, 3),
          n.trains = n(),
          train.rate = round(n() / motif.dur, 3),
          duty.cycle = round(sum(train.dur) / motif.dur * 100, 1)
        ) %>%
        ungroup()

      # Start the interactive plot
      p <- plot_ly() %>%
        add_lines(x = ~time_vector, y = ~envelope_vector, name = "Summary Statistics",
                  hoverinfo = "none",  line = list(color = 'rgba(20, 20, 20, 0)',
                                                   width = 2), legendgroup = "Summary Stats") %>%
        add_lines(x = ~time_vector, y = ~envelope_vector,
                  name = "Envelope",
                  hoverinfo = "none",
                  line = list(color = 'rgb(20, 20, 20)',
                              width = 2,
                              shape = 'spline')) %>%
        add_lines(x = c(min(time_vector), max(time_vector)), y = c(lower_detection_threshold, lower_detection_threshold),
                  name = "Lower Threshold", line = list(color = "#D55E00", dash = "dash"), showlegend = TRUE)

      # Add train lines to the plot
      for (i in seq_len(nrow(train_data))) {
        train_start_time <- train_data$train.start[i]
        train_end_time <- train_data$train.end[i]
        show_legend <- if (i == 1) TRUE else FALSE
        p <- p %>%
          add_lines(x = c(train_start_time, train_end_time), y = c(0.98, 0.98),
                    name = "Trains", line = list(color = "#009E73", width = 6),
                    showlegend = show_legend, legendgroup = "trains",
                    hoverinfo = "x", text = paste("Time:", round(c(train_start_time, train_end_time), 2)))
      }

      # Add motif lines to the plot
      for (i in seq_len(nrow(motif_data))) {
        motif_start_time <- motif_data$motif.start[i]
        motif_end_time <- motif_data$motif.end[i]
        show_legend <- if (i == 1) TRUE else FALSE
        p <- p %>%
          add_lines(x = c(motif_start_time, motif_end_time), y = c(1, 1),
                    name = "Motifs", line = list(color = "#0072B2", width = 6),
                    showlegend = show_legend, legendgroup = "Motifs",
                    hoverinfo = "x", text = paste("Motif:", i))
      }

      motif_data <- motif_data %>%
        select(motif.id, n.trains, everything())

      # Add proportions and complexity metrics
      motif_data <- motif_data %>%
        mutate(
          proportions = map(motif.id, function(eid) {
            train_durations <- train_data %>% filter(motif.id == eid) %>% pull(train.dur)
            gap_durations <- train_data %>% filter(motif.id == eid) %>% pull(train.gap)
            motif_start <- motif_data %>% filter(motif.id == eid) %>% pull(motif.start)
            motif_end <- motif_data %>% filter(motif.id == eid) %>% pull(motif.end)
            motif_duration <- motif_data$motif.dur[eid]
            proportions <- numeric(0)

            for (i in seq_along(train_durations)) {
              # Add train duration as a proportion of the motif duration
              proportions <- c(proportions, train_durations[i] / motif_duration)

              # Check if the gap is not NA and falls within the motif start and end
              if (!is.na(gap_durations[i])) {
                gap_start <- train_data %>% filter(motif.id == eid) %>% pull(train.end) %>% dplyr::nth(i)
                gap_end <- train_data %>% filter(motif.id == eid) %>% pull(train.start) %>% dplyr::nth(i + 1)

                # Check if gap_start, gap_end, motif_start, and motif_end are not NA
                if (!is.na(gap_start) && !is.na(gap_end) && !is.na(motif_start) && !is.na(motif_end)) {
                  if (gap_start >= motif_start && gap_end <= motif_end) {
                    proportions <- c(proportions, gap_durations[i] / motif_duration)
                  }
                }
              }
            }

            round(proportions, 2)
          })
        ) %>%
        rowwise() %>%
        mutate(specimen.id = base::unique(train_data$specimen.id),
               props.sd = round(sd(unlist(proportions)), 3),
               props.ent = round(-sum(unlist(proportions)[unlist(proportions) > 0] * log(unlist(proportions)[unlist(proportions) > 0])), 3),
               props.mean = round(mean(unlist(proportions)), 3),
               props.cv = round((props.sd / props.mean), 3),
               props.diff.sd = round(sd(diff(unlist(proportions))), 3),
               pci = round((props.ent * props.cv + sqrt(n.trains)) /  (sqrt(motif.dur) + 1), 3)
        ) %>%
        ungroup() %>%
        select(specimen.id, everything(), -proportions, proportions)

      # Prepare summary data
      summary_data <- tibble(
        specimen.id = base::unique(train_data$specimen.id),
        n.motifs = nrow(motif_data),
        n.trains = nrow(train_data),
        mean.pci = round(mean(motif_data$pci, na.rm = TRUE),2),
        sd.pci = round(sd(motif_data$pci, na.rm = TRUE),2),
        mean.duty.cycle = round(mean(motif_data$duty.cycle, na.rm = TRUE),1),
        sd.duty.cycle = round(sd(motif_data$duty.cycle, na.rm = TRUE),1),
        mean.ent = round(mean(motif_data$props.ent, na.rm = TRUE),1),
        mean.trains.motif = round(mean(motif_data$n.trains, na.rm = TRUE),1),
        sd.trains.motif = round(sd(motif_data$n.trains, na.rm = TRUE),1),
        mean.motif.dur = round(mean(motif_data$motif.dur, na.rm = TRUE),3),
        sd.motif.dur = round(sd(motif_data$motif.dur, na.rm = TRUE),3),
        mean.train.rate = round(mean(motif_data$train.rate, na.rm = TRUE),1),
        sd.train.rate = round(sd(motif_data$train.rate, na.rm = TRUE),1),
        mean.train.dur = round(mean(train_data$train.dur, na.rm = TRUE), 3),
        sd.train.dur = round(sd(train_data$train.dur, na.rm = TRUE), 3),
        mean.train.per = round(mean(train_data$train.period[train_data$train.gap <= max_train_gap], na.rm = TRUE), 3),
        sd.train.per = round(sd(train_data$train.period[train_data$train.gap <= max_train_gap], na.rm = TRUE), 3),
        mean.gap.dur = round(mean(train_data$train.gap[train_data$train.gap <= max_train_gap], na.rm = TRUE), 3),
        sd.gap.dur = round(sd(train_data$train.gap[train_data$train.gap <= max_train_gap], na.rm = TRUE), 3)
      )

      annotations <- list(
        list(
          x = 0.01,
          y = 0.01,
          xref = 'paper',
          yref = 'paper',
          text = paste("<b> Summary Statistics</b>",
                       "<br> N. motifs: ", summary_data$n.motifs,
                       "<br> Mean trains/motif: ", summary_data$mean.trains.motif,
                       "<br> Mean motif duration: ", summary_data$mean.motif.dur, "s",
                       "<br> Mean train duration: ", summary_data$mean.train.dur, "s",
                       "<br> Mean train gap: ", summary_data$mean.gap.dur, "s",
                       "<br> Mean train rate: ", summary_data$mean.train.rate, "pps",
                       "<br> Mean duty cycle: ", summary_data$mean.duty.cycle,"%",
                       "<br> Mean entropy: ", summary_data$mean.ent,
                       "<br> Mean PCI: ", summary_data$mean.pci
          ),
          showarrow = FALSE,
          font = list(size = 12),
          align = "left",
          bgcolor = 'rgba(255, 255, 255, 0.8)',
          bordercolor = 'rgba(0, 0, 0, 0.5)',
          borderwidth = 1,
          opacity = 1,
          visible = TRUE
        )
      )

      p <- p %>%
        layout(
          annotations = annotations,
          title = summary_data$specimen.id,
          xaxis = list(
            title = list(text = "Time (s)", standoff = 10),
            ticklen = 5,
            automargin = TRUE,
            zeroline = FALSE,
            showline = TRUE
          ),
          yaxis = list(
            title = "Amplitude",
            rangemode = "tozero",
            ticklen = 5,
            showline = TRUE
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            y = 1.05,
            xanchor = "center",
            bgcolor = "rgba(0, 0, 0, 0)"
          ),
          margin = list(
            l = 70,
            r = 10,
            b = 50,
            t = 50
          )
        )


      # Add functionality to toggle the visibility of the Summary Statistics text box
      p <- htmlwidgets::onRender(p, "
  function(el, x) {
    el.on('plotly_restyle', function(d) {
      // We assume 'Summary Statistics' is the second trace (index 1)
      var traceVisible = x.data[0].visible;
      var annotations = x.layout.annotations;

      // Toggle annotation visibility based on the 'Summary Statistics' trace visibility
      if (traceVisible === true || traceVisible === undefined) {
        annotations[0].visible = true;
      } else {
        annotations[0].visible = false;
      }

      // Apply the updated annotation visibility
      Plotly.relayout(el, {annotations: annotations});
    });
  }
")

      return(list(plot = p, summary_data = summary_data,
                  train_data = train_data,
                  motif_data = motif_data,
                  params = params))
    }

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

  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}

#' Temporal Statistics Low-Quality Shiny App
#'
#' This Shiny app performs low-quality temporal statistics analysis on wave objects. It allows the user to configure parameters such as smoothing window, peak finder thresholds, and gap limits for trains and peaks. The app generates interactive plots and data tables summarizing motifs, trains, and peaks, along with parameter settings.
#'
#' @return A Shiny app for analyzing temporal statistics of acoustic waveforms.
#' @import shiny
#' @import shinyBS
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
#'   temp_stats_lq_app()
#' }
#' }
temporal_stats_lq_app <- function() {

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
                 trigger = "hover",
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
                 trigger = "hover",
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
                 trigger = "hover",
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
                 trigger = "hover",
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
                 trigger = "hover",
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
                 trigger = "hover",
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
                 trigger = "hover",
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
                 trigger = "hover",
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

    temporal_stats_lq <- function(wave,
                                  specimen.id,
                                  ssmooth = 100,
                                  peakfinder_ws = 50,
                                  peakfinder_threshold = 0.005,
                                  max_train_gap = 0.5,
                                  max_peak_gap = 0.01,
                                  detection_threshold = 0.01,
                                  norm.env = TRUE) {

      # Store input parameters in a tibble
      params <- tibble(
        specimen.id = specimen.id,
        ssmooth = ssmooth,
        peakfinder_ws = peakfinder_ws,
        peakfinder_threshold = peakfinder_threshold,
        max_train_gap = max_train_gap,
        max_peak_gap = max_peak_gap,
        norm.env = norm.env
      )

      window_size <- peakfinder_ws

      waveDuration <- seewave::duration(wave)

      # envelope_vector <- seewave::env(wave, ssmooth = ssmooth, plot = FALSE)

      # warbleR's envelope is faster
      envelope_vector <- warbleR::envelope(as.numeric(wave@left), ssmooth = ssmooth)

      if (norm.env) {
        envelope_vector <- (envelope_vector - min(envelope_vector)) / (max(envelope_vector) - min(envelope_vector))
      }

      max_amplitude <- max(envelope_vector)
      amp_threshold <- max_amplitude * peakfinder_threshold
      peaks <- c()

      for (i in (window_size + 1):(length(envelope_vector) - window_size)) {
        window <- envelope_vector[(i - window_size):(i + window_size)]
        center_value <- envelope_vector[i]
        if (center_value == max(window) && (center_value - min(window)) > amp_threshold) {
          peaks <- c(peaks, i)
        }
      }

      # Create the time vector
      sample_rate <- wave@samp.rate
      time_vector <- seq(0, (length(wave@left) - 1) / sample_rate, length.out = length(envelope_vector))  # In seconds
      time_vector_ms <- time_vector * 1000  # Convert to milliseconds

      peaks <- peaks[peaks <= length(time_vector)]


      # Filter out peaks below the detection threshold
      peak_amplitudes <- envelope_vector[peaks]  # Get amplitudes of the detected peaks
      valid_peaks <- which(peak_amplitudes >= detection_threshold)  # Only keep peaks above threshold
      peaks <- peaks[valid_peaks]  # Update the peaks list

      peak_times_ms <- time_vector_ms[peaks]

      peak_periods <- diff(peak_times_ms)
      peak_periods[peak_periods > max_peak_gap * 1000] <- NA

      peak_data <- tibble(
        specimen.id = rep(specimen.id, length(peaks)),
        motif.id = integer(length(peaks)),
        train.id = integer(length(peaks)),
        peak.id = integer(length(peaks)),
        peak.time = peak_times_ms,
        peak.period = c(peak_periods, NA)
      )

      trains <- list()
      motifs <- list()
      motif_id <- 1
      train_id <- 1
      train_start <- peaks[1]
      current_motif <- list(c(train_start, NULL))

      peak_data <- peak_data %>%
        mutate(motif.id = ifelse(row_number() == 1, motif_id, motif.id),
               train.id = ifelse(row_number() == 1, train_id, train.id),
               peak.id = ifelse(row_number() == 1, 1, peak.id))

      peak_data <- peak_data %>%
        mutate(peak.period = round(peak.period,3))

      peak_counter <- 1

      for (i in 2:length(peaks)) {
        if (peak_times_ms[i] - peak_times_ms[i - 1] > max_peak_gap * 1000) {
          train_end <- peaks[i - 1]
          trains <- append(trains, list(c(train_start, train_end)))
          train_start <- peaks[i]
          current_motif[[length(current_motif)]][2] <- train_end
          current_motif <- append(current_motif, list(c(train_start, NULL)))
          peak_counter <- 0
          if (peak_times_ms[i] - peak_times_ms[i - 1] > max_train_gap * 1000) {
            motifs <- append(motifs, list(current_motif))
            current_motif <- list(c(train_start, NULL))
            motif_id <- motif_id + 1
            train_id <- 1
          } else {
            train_id <- train_id + 1
          }
        }
        peak_counter <- peak_counter + 1
        peak_data <- peak_data %>%
          mutate(motif.id = ifelse(peak.time == peak_times_ms[i], motif_id, motif.id),
                 train.id = ifelse(peak.time == peak_times_ms[i], train_id, train.id),
                 peak.id = ifelse(peak.time == peak_times_ms[i], peak_counter, peak.id))
      }

      # Round peak.time to 4 decimals
      peak_data <- peak_data %>%
        mutate(peak.time = round(peak.time, 4))

      train_end <- peaks[length(peaks)]
      trains <- append(trains, list(c(train_start, train_end)))
      current_motif[[length(current_motif)]][2] = train_end
      motifs <- append(motifs, list(current_motif))

      # Create tibble for peak train measurements
      train_data <- peak_data %>%
        group_by(specimen.id, motif.id, train.id) %>%
        summarize(
          train.start = round(min(peak.time),4),
          train.end = round(max(peak.time),4),
          train.dur = round((train.end - train.start),3),
          n.peaks = n()
        ) %>%
        mutate(peak.rate = round((n.peaks / (train.dur/1000)),1)) %>%
        ungroup() %>%
        # Set last train.period of each motif to NA and round to 3 decimals
        mutate(train.period = ifelse(is.na(lead(motif.id)) | lead(motif.id) != motif.id, NA, lead(train.start) - train.start),
               train.gap = round(lead(train.start) - train.end, 3)        # Gap: next train.start - current train.end
        ) %>%
        relocate(train.period, .after = train.dur) %>%
        relocate(train.gap, .after = train.period) %>%
        mutate(train.period = round(train.period,3))



      # Create tibble for motif measurements
      motif_data <- train_data %>%
        group_by(specimen.id, motif.id) %>%
        summarize(
          motif.start = round(min(train.start),3),
          motif.end = round(max(train.end),3),
          motif.dur = round(motif.end - motif.start,3),
          motif.period = round((lead(motif.start) - motif.start),3),
          n.trains = n(),
          # train.rate = round(n()/motif.dur),
          train.rate = round(n() / ((max(train.end)/1000) - (min(train.start)/1000)),1),
          duty.cycle = round((sum(train.dur) / motif.dur)*100,2)
        ) %>%
        ungroup()

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
                gap_start <- train_data %>% filter(motif.id == eid) %>% pull(train.end) %>% nth(i)
                gap_end <- train_data %>% filter(motif.id == eid) %>% pull(train.start) %>% nth(i + 1)

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

      # Prepare annotations for the plot
      annotations <- list(
        list(
          x = 0.01,
          y = 0.99,
          xref = 'paper',
          yref = 'paper',
          text = paste("<b> Summary Statistics</b>",
                       "<br> N. motifs:", length(motifs),
                       "<br> motif duration: ", round(mean(motif_data$motif.dur/1000),3), "s",
                       "<br> Duty Cycle: ", round(mean(motif_data$duty.cycle),1), "%",
                       "<br> Trains / motif:", mean(motif_data$n.trains),
                       "<br> Train Rate: " , round(mean(motif_data$train.rate)), "tps",
                       "<br> Train Duration: ", round(mean(train_data$train.dur, na.rm = TRUE)), "ms",
                       "<br> Peaks / Train: ", round(mean(train_data$n.peaks, na.rm = TRUE)),
                       "<br> Peak Rate: ", round(mean(train_data$peak.rate, na.rm = TRUE)), "pps",
                       "<br> PCI: ", mean(motif_data$pci, 3)),

          showarrow = FALSE,
          font = list(size = 12),
          align = "left",
          bgcolor = 'rgba(255, 255, 255, 0.5)',
          bordercolor = 'rgba(0, 0, 0, 0.5)',
          borderwidth = 1,
          opacity = 1,
          visible = TRUE
        )
      )

      # Start the interactive plot
      p <- plot_ly() %>%
        add_lines(x = ~time_vector, y = ~envelope_vector, name = "Summary Statistics",
                  hoverinfo = "none",  line = list(color = 'rgba(20, 20, 20, 0)',
                                                   width = 2), legendgroup = "Summary Stats") %>%
        add_lines(x = ~time_vector, y = ~envelope_vector, name = "Envelope",
                  hoverinfo = "none",  line = list(color = 'rgb(20, 20, 20)',
                                                   width = 2,
                                                   shape = "spline"))

      # Add train lines to the plot
      for (i in seq_along(trains)) {
        train_start_time <- time_vector[trains[[i]][1]]
        train_end_time <- time_vector[trains[[i]][2]]
        show_legend <- if (i == 1) TRUE else FALSE
        p <- p %>%
          add_lines(x = c(train_start_time, train_end_time), y = c(0.98, 0.98),
                    name = "Trains", line = list(color = "#009E73", width = 6),
                    showlegend = show_legend, legendgroup = "Trains",
                    hoverinfo = "x", text = paste("Time:", round(c(train_start_time, train_end_time), 2)))
      }

      # Add motif lines to the plot using motif_data
      for (i in seq_len(nrow(motif_data))) {
        motif_start <- motif_data$motif.start[i]/1000
        motif_end <- motif_data$motif.end[i]/1000
        show_legend <- if (i == 1) TRUE else FALSE
        p <- p %>%
          add_lines(x = c(motif_start, motif_end), y = c(1, 1),
                    name = "Motifs", line = list(color = "#0072B2", width = 6),
                    showlegend = show_legend, legendgroup = "motifs",
                    hoverinfo = "x", text = paste("Time:", round(c(motif_start, motif_end), 2)))
      }

      # Add peak markers
      p <- p %>%
        add_markers(x = ~time_vector[peaks], y = ~envelope_vector[peaks],
                    name = "Peaks", marker = list(color = "#D55E00" , size = 8),
                    hoverinfo = "none")

      p <- p %>%
        layout(
          annotations = annotations,
          # title = summary_data$specimen.id,
          xaxis = list(
            title = list(text = "Time (s)", standoff = 10),
            ticklen = 5,
            automargin = TRUE,
            zeroline = FALSE,
            showline = TRUE
          ),
          yaxis = list(title = "Amplitude",
                       rangemode = "tozero",
                       ticklen = 5,
                       showline = TRUE),
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


      return(list(plot = p,
                  peak_data = peak_data,
                  train_data = train_data,
                  motif_data = motif_data,
                  params = params))
    }

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
                        specimen.id = input$specimen_id,
                        ssmooth = input$ssmooth,
                        peakfinder_ws = input$peakfinder_ws,
                        peakfinder_threshold = input$peakfinder_threshold,
                        max_train_gap = input$max_train_gap,
                        max_peak_gap = input$max_peak_gap,
                        detection_threshold = input$detection_threshold,
                        norm.env = TRUE)
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
                  "Parameter Data"
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


  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
}







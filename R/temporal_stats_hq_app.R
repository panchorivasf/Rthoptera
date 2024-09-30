#' Temporal Statistics HQ Shiny App
#'
#' This Shiny app performs temporal statistics analysis on wave objects with "HQ" sounds (i.e., near pure tones such as those made by most crickets). It allows the user to configure multiple parameters such as smoothing window, overlap, and detection thresholds to analyze the temporal patterns of acoustic signals. The app outputs interactive visualizations of the results and provides data tables summarizing elements, motifs, and summary statistics.
#'
#' @return A Shiny app that analyzes temporal statistics of acoustic waveforms.
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom shinyBS bsButton bsPopover
#' @importFrom plotly plot_ly add_lines layout renderPlotly
#' @importFrom DT datatable renderDT
#' @importFrom writexl write_xlsx
#' @importFrom seewave env resamp
#' @importFrom purrr map
#' @importFrom dplyr n mutate filter lead group_by summarize ungroup select tibble rowwise
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
                        selectInput("preset", label = NULL, choices = c("Default", "Cesare_Gryllus"), selected = "Default")
                 )
               ),
               bsPopover(
                 id = "preset_info",
                 title = "Preset",
                 content = HTML(paste0("Optimized parameters for call patterns in particular taxa.")),
                 placement = "right",
                 trigger = "focus",
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
                 trigger = "focus",
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
                 trigger = "focus",
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
                 trigger = "focus",
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
                        numericInput("upper_detection_threshold", label = NULL, value = 0.5,
                                     min = 0.01, max = 0.99, step = 0.01)
                 )
               ),
               bsPopover(
                 id = "upDet_info",
                 title = "Upper Detection Threshold",
                 content = HTML(paste0("Minimum amplitude (proportion) required for a element to be included in the analysis. Elements with maximum amplitude below this value will be excluded.")),
                 placement = "right",
                 trigger = "focus",
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
                        numericInput("lower_detection_threshold", label = NULL, value = 0.3,
                                     min = 0.01, max = 0.99, step = 0.01)
                 )
               ),
               bsPopover(
                 id = "loDet_info",
                 title = "Lower Detection Threshold",
                 content = HTML(paste0("Amplitude threshold as a proportion of the maximum amplitude. Only elements with an amplitude above this threshold will be detected.")),
                 placement = "right",
                 trigger = "focus",
                 options = list(container = "body")
               ),



               fluidRow(
                 column(12,
                        div(style = "display: flex; align-items: center;",
                            tagList(
                              tags$label("Max Element Gap (s)"),
                              bsButton("max_element_gap_info", label = "", lib="font-awesome",
                                       icon = icon("circle-info"), style = "default",
                                       size = "extra-small", class = "btn-info")
                            )
                        ),
                        numericInput("max_element_gap", label = NULL, value = 0.08,
                                     min = 0.01, max = 1, step = 0.01)
                 )
               ),
               bsPopover(
                 id = "max_element_gap_info",
                 title = "Max element Gap",
                 content = HTML(paste0("Maximum gap allowed between elements to be considered in the same motif. If the gap exceeds this value, a new motif is started.")),
                 placement = "right",
                 trigger = "focus",
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
                        DTOutput("element_data"),
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
                                  msmooth_window = 100,
                                  msmooth_overlap = 0,
                                  upper_detection_threshold = 0.5,
                                  lower_detection_threshold = 0.3,
                                  min_element_dur = 0.002,
                                  max_element_gap = 0.05,
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
        max_element_gap = max_element_gap,
        upper_detection_threshold = upper_detection_threshold,
        lower_detection_threshold = lower_detection_threshold,
        norm.env = norm.env
      )

      msmooth_vec <- c(msmooth_window, msmooth_overlap)

      if (wave@samp.rate < 192000) {
        wave <- resamp(wave, g = 192000, output = "Wave")
      }

      # Get envelope of the wave
      envelope_vector <- seewave::env(wave, msmooth = msmooth_vec, plot = FALSE)

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

      # Detect elements based on the amplitude lower_detection_threshold
      element_starts <- which(diff(c(0, envelope_vector >= amp_threshold, 0)) == 1)
      element_ends <- which(diff(c(0, envelope_vector >= amp_threshold, 0)) == -1) - 1

      # Initialize data storage
      elements <- list()

      for (i in seq_along(element_starts)) {
        element_start <- time_vector[element_starts[i]]
        element_end <- time_vector[element_ends[i]]
        element_max_amp <- max(envelope_vector[element_starts[i]:element_ends[i]])

        # Exclude elements below upper_detection_threshold
        if (element_max_amp < upper_detection_threshold) {
          next
        }

        # Append element to the list as a numeric vector
        elements <- append(elements, list(c(element_start, element_end)))
      }

      element_data <- tibble(
        specimen.id = rep(specimen.id, length(elements)),
        element.start = round(sapply(elements, `[[`, 1), 3),
        element.end = round(sapply(elements, `[[`, 2), 3),
        element.dur = round(sapply(elements, function(x) if (length(x) > 1) x[2] - x[1] else NA), 3)
      ) %>%
        dplyr::filter(!is.na(element.dur) & element.dur > min_element_dur)

      # Calculate element.period and element.gap directly from element_data
      element_data <- element_data %>%
        mutate(
          element.period = round(lead(element.start) - element.start, 3),  # Period: next element.start - current element.start
          element.gap = round(lead(element.start) - element.end, 3)        # Gap: next element.start - current element.end
        )

      # Calculate element.id and motif.id based on element.gap and max_element_gap
      element_data <- element_data %>%
        mutate(
          motif.id = cumsum(c(TRUE, element.gap[-n()] > max_element_gap)),
          element.id = sequence(rle(cumsum(c(TRUE, element.gap[-n()] > max_element_gap)))$lengths)
        )

      # Summarize motif data
      motif_data <- element_data %>%
        group_by(motif.id) %>%
        summarize(
          motif.start = min(element.start),
          motif.end = max(element.end),
          motif.dur = round(motif.end - motif.start, 3),
          n.elements = n(),
          element.rate = round(n() / motif.dur, 3),
          duty.cycle = round(sum(element.dur) / motif.dur * 100, 1)
        ) %>%
        ungroup()

      # Start the interactive plot
      p <- plot_ly() %>%
        add_lines(x = ~time_vector, y = ~envelope_vector, name = "Summary Statistics",
                  hoverinfo = "none",  line = list(color = 'rgba(20, 20, 20, 0)',
                                                   width = 2), legendgroup = "Summary Stats") %>%
        add_lines(x = ~time_vector, y = ~envelope_vector, name = "Envelope",
                  hoverinfo = "none",  line = list(color = 'rgb(20, 20, 20)', width = 2)) %>%
        add_lines(x = c(min(time_vector), max(time_vector)), y = c(lower_detection_threshold, lower_detection_threshold),
                  name = "Lower Threshold", line = list(color = "#D55E00", dash = "dash"), showlegend = TRUE)

      # Add element lines to the plot
      for (i in seq_len(nrow(element_data))) {
        element_start_time <- element_data$element.start[i]
        element_end_time <- element_data$element.end[i]
        show_legend <- if (i == 1) TRUE else FALSE
        p <- p %>%
          add_lines(x = c(element_start_time, element_end_time), y = c(0.98, 0.98),
                    name = "Elements", line = list(color = "#009E73", width = 6),
                    showlegend = show_legend, legendgroup = "Elements",
                    hoverinfo = "x", text = paste("Time:", round(c(element_start_time, element_end_time), 2)))
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
        select(motif.id, n.elements, everything())

      # Add proportions and complexity metrics
      motif_data <- motif_data %>%
        mutate(
          proportions = map(motif.id, function(eid) {
            element_durations <- element_data %>% filter(motif.id == eid) %>% pull(element.dur)
            gap_durations <- element_data %>% filter(motif.id == eid) %>% pull(element.gap)
            motif_duration <- motif_data$motif.dur[eid]
            proportions <- numeric(0)

            for (i in seq_along(element_durations)) {
              proportions <- c(proportions, element_durations[i] / motif_duration)
              if (i < length(element_durations) && !is.na(gap_durations[i]) && gap_durations[i] <= max_element_gap) {
                proportions <- c(proportions, gap_durations[i] / motif_duration)
              }
            }

            round(proportions, 2)
          })
        ) %>%
        rowwise() %>%
        mutate(specimen.id = base::unique(element_data$specimen.id),
               props.sd = round(sd(unlist(proportions)), 3),
               props.ent = round(-sum(unlist(proportions)[unlist(proportions) > 0] * log(unlist(proportions)[unlist(proportions) > 0])), 3),
               props.mean = round(mean(unlist(proportions)), 3),
               props.cv = round((props.sd / props.mean), 3),
               props.diff.sd = round(sd(diff(unlist(proportions))), 3),
               pci = round((props.ent * props.cv + sqrt(n.elements)) /  (sqrt(motif.dur) + 1), 3)
        ) %>%
        ungroup() %>%
        select(specimen.id, everything(), -proportions, proportions)

      # Prepare summary data
      summary_data <- tibble(
        specimen.id = base::unique(element_data$specimen.id),
        n.motifs = nrow(motif_data),
        n.elements = nrow(element_data),
        mean.pci = round(mean(motif_data$pci, na.rm = TRUE),1),
        sd.pci = round(sd(motif_data$pci, na.rm = TRUE),1),
        mean.duty.cycle = round(mean(motif_data$duty.cycle, na.rm = TRUE),1),
        sd.duty.cycle = round(sd(motif_data$duty.cycle, na.rm = TRUE),1),
        mean.ent = round(mean(motif_data$props.ent, na.rm = TRUE),1),
        mean.elements.motif = round(mean(motif_data$n.elements, na.rm = TRUE),1),
        sd.elements.motif = round(sd(motif_data$n.elements, na.rm = TRUE),1),
        mean.motif.dur = round(mean(motif_data$motif.dur, na.rm = TRUE),3),
        sd.motif.dur = round(sd(motif_data$motif.dur, na.rm = TRUE),3),
        mean.element.rate = round(mean(motif_data$element.rate, na.rm = TRUE),1),
        sd.element.rate = round(sd(motif_data$element.rate, na.rm = TRUE),1),
        mean.element.dur = round(mean(element_data$element.dur, na.rm = TRUE), 3),
        sd.element.dur = round(sd(element_data$element.dur, na.rm = TRUE), 3),
        mean.element.per = round(mean(element_data$element.period[element_data$element.gap <= max_element_gap], na.rm = TRUE), 3),
        sd.element.per = round(sd(element_data$element.period[element_data$element.gap <= max_element_gap], na.rm = TRUE), 3),
        mean.gap.dur = round(mean(element_data$element.gap[element_data$element.gap <= max_element_gap], na.rm = TRUE), 3),
        sd.gap.dur = round(sd(element_data$element.gap[element_data$element.gap <= max_element_gap], na.rm = TRUE), 3)
      )

      annotations <- list(
        list(
          x = 0.01,
          y = 0.01,
          xref = 'paper',
          yref = 'paper',
          text = paste("<b> Summary Statistics</b>",
                       "<br> N. motifs: ", summary_data$n.motifs,
                       "<br> Mean elements/motif: ", summary_data$mean.elements.motif,
                       "<br> Mean motif duration: ", summary_data$mean.motif.dur, "s",
                       "<br> Mean element duration: ", summary_data$mean.element.dur, "s",
                       "<br> Mean element gap: ", summary_data$mean.gap.dur, "s",
                       "<br> Mean element rate: ", summary_data$mean.element.rate, "pps",
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
      # p <- p %>%
      #   layout(
      #
      #     annotations = list(
      #       x = 0,
      #       y = 1.1,
      #       text = "NOTICE: Wave objects with a sampling rate
      #       below 192 kHz are resampled to that value for consistent
      #       time measurements. See documentation for details. ",
      #       showarrow = FALSE,
      #       xref = "paper",  # Position relative to the paper (the whole plot area)
      #       yref = "paper",  # Position relative to the paper (the whole plot area)
      #       xanchor = "center",
      #       yanchor = "bottom",
      #       font = list(size = 10, color = "black")  # Customize font if needed
      #     )
      #   )

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
                  element_data = element_data,
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
                        min_element_dur = as.numeric(0.002),
                        max_element_gap = as.numeric(isolate(input$max_element_gap)),
                        norm.env = TRUE)#,
      # plot.stats = isolate(input$show_annotations))

    })


    observeEvent(input$preset, {

      if (input$preset == "Cesare_Gryllus") {

        updateTextInput(session, "specimen_id", value = "")
        updateNumericInput(session, "msmooth_window", value = 100)
        updateNumericInput(session, "msmooth_overlap", value = 50)
        updateNumericInput(session, "lower_detection_threshold", value = 0.08)
        updateNumericInput(session, "upper_detection_threshold", value = 0.5)
        updateNumericInput(session, "max_element_gap", value = 0.08)

      } else if (input$preset == "Default") {

        updateTextInput(session, "specimen_id", value = "")
        updateNumericInput(session, "msmooth_window", value = 100)
        updateNumericInput(session, "msmooth_overlap", value = 50)
        updateNumericInput(session, "upper_detection_threshold", value = 0.5)
        updateNumericInput(session, "lower_detection_threshold", value = 0.3)
        updateNumericInput(session, "max_element_gap", value = 0.1)

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
                  "motif Data"
                ),
                options = list(
                  pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                  columnDefs = list(list(orderable = FALSE, targets = "_all"))
                ))
    })

    output$element_data <- renderDT({
      req(result())
      datatable(result()$element_data,
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
          "motif Data" = result()$motif_data,
          "element Data" = result()$element_data,
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


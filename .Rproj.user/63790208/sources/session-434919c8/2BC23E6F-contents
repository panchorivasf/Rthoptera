#' Temporal Statistics Low-Quality Shiny App
#'
#' This Shiny app performs low-quality temporal statistics analysis on wave objects. It allows the user to configure parameters such as smoothing window, peak finder thresholds, and gap limits for trains and peaks. The app generates interactive plots and data tables summarizing echemes, trains, and peaks, along with parameter settings.
#'
#' @return A Shiny app for analyzing temporal statistics of acoustic waveforms.
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly add_lines layout renderPlotly add_markers
#' @importFrom DT datatable renderDT
#' @importFrom seewave env duration
#' @importFrom writexl write_xlsx
#' @importFrom dplyr group_by summarize ungroup mutate lead tibble relocate
#' @importFrom bslib bs_theme
#' @export
#'
#' @examples
#' if (interactive()) {
#'   temp_stats_lq_app()
#' }

temporal_stats_lq_app <- function() {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"


  ui <- function(request) {
    tagList(
      h1("Temporal Statistics", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        theme = bslib::bs_theme(bootswatch = "darkly"),
        tags$head(tags$style(
          HTML(
            "
              body {
                margin: 5px; /* Adds margin around the entire page */
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
              .dataTables_wrapper .caption-top {
                caption-side: top !important;
                font-weight: bold;
                color: white;
              }
            "
          )
        )),
        tags$script(
          HTML(
            "
              Shiny.addCustomMessageHandler('toggleSpinner', function(message) {
                if (message) {
                  document.getElementById('spinner').style.display = 'block';
                } else {
                  document.getElementById('spinner').style.display = 'none';
                }
              });
            "
          )
        ),
        fluidRow(
          column(
            width = 2,
            div(
              sidebarPanel(
                selectInput("selectedWave", "Select a Wave Object:", choices = NULL),
                textInput("specimen_id", "Specimen ID", value = ""),
                numericInput("ssmooth", "Smoothing Window", value = 100,
                             min = 10, max = 1000, step = 10),
                numericInput("peakfinder_ws", "Peakfinder Window", value = 40,
                             min = 10, max = 200, step = 5),
                numericInput("peakfinder_threshold", "Peakfinder Threshold",
                             value = 0.005, min = 0.001, max = 0.5, step = 0.001),
                numericInput("max_train_gap", "Max Train Gap", value = 0.08,
                             min = 0.01, max = 1, step = 0.01),
                numericInput("max_peak_gap", "Max Peak Gap", value = 0.01,
                             min = 0.001, max = 0.1, step = 0.001),
                actionButton("run", "Run Analysis"),
                downloadButton("saveData", "Save Data"),
                actionButton("help", "Help"),
                column(1, actionButton("close", "Close App")),
                width = 12,
                style = "height: 100%; overflow-y: auto; padding: 10px;"
              ),
              style = "width: 100%;"
            )
          ),
          column(
            width = 10,
            mainPanel(
              withSpinner(plotlyOutput("audioPlot")),
              DTOutput("echeme_data"),
              DTOutput("train_data"),
              DTOutput("peak_data"),
              DTOutput("params"),
              width = 12,
              style = "padding: 10px;"
            )
          )
        )
      )
    )
  }

  server <- function(input, output, session) {

    temporal_stats <- function(wave,
                               specimen.id,
                               ssmooth = 100,
                               peakfinder_ws = 50,
                               peakfinder_threshold = 0.005,
                               max_train_gap = 0.5,
                               max_peak_gap = 0.01,
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

      envelope_vector <- seewave::env(wave, ssmooth = ssmooth, plot = FALSE)

      # warbleR's envelope is faster but the package is not available in CRAN at the moment
      # envelope_vector <- warbleR::envelope(as.numeric(wave@left), ssmooth = ssmooth)

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

      peak_times_ms <- time_vector_ms[peaks]

      peak_periods <- diff(peak_times_ms)
      peak_periods[peak_periods > max_peak_gap * 1000] <- NA

      peak_data <- tibble(
        specimen.id = rep(specimen.id, length(peaks)),
        echeme.id = integer(length(peaks)),
        train.id = integer(length(peaks)),
        peak.id = integer(length(peaks)),
        peak.time = peak_times_ms,
        peak.period = c(peak_periods, NA)
      )

      trains <- list()
      echemes <- list()
      echeme_id <- 1
      train_id <- 1
      train_start <- peaks[1]
      current_echeme <- list(c(train_start, NULL))

      peak_data <- peak_data %>%
        mutate(echeme.id = ifelse(row_number() == 1, echeme_id, echeme.id),
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
          current_echeme[[length(current_echeme)]][2] <- train_end
          current_echeme <- append(current_echeme, list(c(train_start, NULL)))
          peak_counter <- 0
          if (peak_times_ms[i] - peak_times_ms[i - 1] > max_train_gap * 1000) {
            echemes <- append(echemes, list(current_echeme))
            current_echeme <- list(c(train_start, NULL))
            echeme_id <- echeme_id + 1
            train_id <- 1
          } else {
            train_id <- train_id + 1
          }
        }
        peak_counter <- peak_counter + 1
        peak_data <- peak_data %>%
          mutate(echeme.id = ifelse(peak.time == peak_times_ms[i], echeme_id, echeme.id),
                 train.id = ifelse(peak.time == peak_times_ms[i], train_id, train.id),
                 peak.id = ifelse(peak.time == peak_times_ms[i], peak_counter, peak.id))
      }

      # Round peak.time to 4 decimals
      peak_data <- peak_data %>%
        mutate(peak.time = round(peak.time, 4))

      train_end <- peaks[length(peaks)]
      trains <- append(trains, list(c(train_start, train_end)))
      current_echeme[[length(current_echeme)]][2] = train_end
      echemes <- append(echemes, list(current_echeme))

      # Create tibble for peak train measurements
      train_data <- peak_data %>%
        group_by(specimen.id, echeme.id, train.id) %>%
        summarize(
          train.start = round(min(peak.time),4),
          train.end = round(max(peak.time),4),
          train.duration = round((train.end - train.start),3),
          n.peaks = n(),
          peak.rate = round((n() / ((max(peak.time) - min(peak.time))/1000)),1)
        ) %>%
        ungroup() %>%
        # Set last train.period of each echeme to NA and round to 3 decimals
        mutate(train.period = ifelse(is.na(lead(echeme.id)) | lead(echeme.id) != echeme.id, NA, lead(train.start) - train.start)) %>%
        relocate(train.period, .after = train.duration) %>%
        mutate(train.period = round(train.period,3))


      # Create tibble for echeme measurements
      echeme_data <- train_data %>%
        group_by(specimen.id, echeme.id) %>%
        summarize(
          echeme.start = round(min(train.start),4),
          echeme.end = round(max(train.end),4),
          echeme.duration = round(echeme.end - echeme.start,3),
          echeme.period = round((lead(echeme.start) - echeme.start),3),
          n.trains = n(),
          train.rate = round(n() / ((max(train.end)/1000) - (min(train.start)/1000)),1),
          duty.cycle = round((sum(train.duration) / echeme.duration)*100,2)
        ) %>%
        ungroup()

      # Prepare annotations for the plot
      annotations <- list(
        list(
          x = 0.01,
          y = 0.99,
          xref = 'paper',
          yref = 'paper',
          text = paste("<b> Summary Statistics</b>",
                       "<br> N. echemes:", length(echemes),
                       "<br> Echeme duration: ", round(mean(echeme_data$echeme.duration/1000),3), "s",
                       "<br> Duty Cycle: ", round(mean(echeme_data$duty.cycle),1), "%",
                       "<br> Trains / Echeme:", mean(echeme_data$n.trains),
                       "<br> Train Rate: " , round(mean(echeme_data$train.rate)), "tps",
                       "<br> Train Duration: ", round(mean(train_data$train.duration, na.rm = TRUE)), "ms",
                       "<br> Peaks / Train: ", round(mean(train_data$n.peaks, na.rm = TRUE)),
                       "<br> Peak Rate: ", round(mean(train_data$peak.rate, na.rm = TRUE)), "pps"),



          showarrow = FALSE,
          font = list(size = 12),
          align = "left",
          bgcolor = 'rgba(255, 255, 255, 0.5)',
          bordercolor = 'rgba(0, 0, 0, 0.5)',
          borderwidth = 1
        )
      )

      # Start the interactive plot
      p <- plot_ly() %>%
        add_lines(x = ~time_vector, y = ~envelope_vector, name = "Envelope",
                  hoverinfo = "none",  line = list(color = 'rgb(20, 20, 20)',
                                                   width = 2))

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

      # Add echeme lines to the plot using echeme_data
      for (i in seq_len(nrow(echeme_data))) {
        echeme_start <- echeme_data$echeme.start[i]/1000
        echeme_end <- echeme_data$echeme.end[i]/1000
        show_legend <- if (i == 1) TRUE else FALSE
        p <- p %>%
          add_lines(x = c(echeme_start, echeme_end), y = c(1, 1),
                    name = "Echemes", line = list(color = "#0072B2", width = 6),
                    showlegend = show_legend, legendgroup = "Echemes",
                    hoverinfo = "x", text = paste("Time:", round(c(echeme_start, echeme_end), 2)))
      }

      # Add peak markers
      p <- p %>%
        add_markers(x = ~time_vector[peaks], y = ~envelope_vector[peaks],
                    name = "Peaks", marker = list(color = "#D55E00" , size = 8),
                    hoverinfo = "none")

      p <- p %>%
        layout(
          annotations = annotations,
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
            y = 1.1,
            xanchor = "center"
          ),
          margin = list(
            l = 70,
            r = 10,
            b = 50,
            t = 50
          )
        )

      list(plot = p, peak_data = peak_data, train_data = train_data,
           echeme_data = echeme_data, params = params)
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
      temporal_stats(wave,
                     specimen.id = input$specimen_id,
                     ssmooth = input$ssmooth,
                     peakfinder_ws = input$peakfinder_ws,
                     peakfinder_threshold = input$peakfinder_threshold,
                     max_train_gap = input$max_train_gap,
                     max_peak_gap = input$max_peak_gap,
                     norm.env = TRUE)
    })

    output$audioPlot <- renderPlotly({
      req(result())
      result()$plot %>%
        layout(title = input$specimen_id,
               margin = list(l = 80, r = 0, t = 80, b = 80))
    })

    output$echeme_data <- renderDT({
      req(result())
      datatable(result()$echeme_data,
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: center;",
                  class = "caption-top",
                  "Echeme Data"
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
                  style = "caption-side: top; text-align: center;",
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
                  style = "caption-side: top; text-align: center;",
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
                  style = "caption-side: top; text-align: center;",
                  class = "caption-top",
                  "Parameter Data"
                ),
                options = list(
                  pageLength = 1, lengthChange = FALSE, searching = FALSE, paging = FALSE, info = FALSE,
                  columnDefs = list(list(orderable = FALSE, targets = "_all"))
                ))
    })


    # Define the help modal dialog
    observeEvent(input$help, {
      showModal(modalDialog(
        title = "Help - Parameter Information",
        HTML("
          <b>Smoothing Window:</b> Window size (samples) used to smooth the envelope.<br><br>
          <b>Peakfinder Window:</b> Size of the moving window used for peak detection.<br><br>
          <b>Peakfinder Threshold:</b> Amplitude threshold for identifying peaks based on their distance to the 'valleys'.<br><br>
          <b>Max Train Gap:</b> Maximum gap allowed between trains to be considered in the same echeme.<br><br>
          <b>Max Peak Gap (max_peak_gap):</b> Maximum gap allowed between consecutive peaks to be considered in the same train.<br>
        "),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    output$saveData <- downloadHandler(
      filename = function() {
        paste(input$specimen_id, "tempstats_data.xlsx", sep = "_")
      },
      content = function(file) {
        req(result())
        # Collect all data tables
        data_list <- list(
          "Echeme Data" = result()$echeme_data,
          "Train Data" = result()$train_data,
          "Peak Data" = result()$peak_data,
          "Parameter Data" = result()$params
        )

        # Write the list of data frames to an Excel file
        writexl::write_xlsx(data_list, path = file)
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


  shinyApp(ui = ui, server = server)
}

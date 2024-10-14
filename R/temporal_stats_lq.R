#' Temporal Statistics Function for Acoustic Analysis of LQ signals
#'
#' This function calculates temporal statistics from a wave object,
#' including peak detection, train detection, and motif grouping.
#' It is designed for analyzing LQ acoustic signals,
#' detecting peaks in amplitude envelopes,
#' and measuring the duration and timing of motifs and trains in the waveform.
#'
#' New metrics provided by this function include the Temporal Complexity Index (within motif),
#' Temporal Excursion (within train), and Dynamic Excursion (within train).
#'
#' @param wave A `Wave` object from the `tuneR` package representing the acoustic signal.
#' @param specimen_id A character string representing the identifier for the specimen (or recording) being analyzed.
#' @param ssmooth Numeric value representing the smoothing window size (in samples) for the amplitude envelope.
#' @param peakfinder_ws Numeric value specifying the window size (in samples) for the peak detection algorithm.
#' @param peakfinder_threshold Numeric value specifying the amplitude threshold for peak detection, as a proportion of the maximum amplitude.
#' @param max_train_gap Numeric value representing the maximum allowed gap (in seconds) between peaks to consider them part of the same train.
#' @param max_peak_gap Numeric value representing the maximum allowed gap (in seconds) between peaks to consider them as part of the same group.
#' @param detection_threshold Numeric value specifying the minimum amplitude for a peak to be considered valid.
#' @param norm_env Logical value indicating whether to normalize the amplitude envelope between 0 and 1. Defaults to `TRUE`.
#'
#' @return A list containing the following:
#' \item{plot}{An interactive plotly plot showing the waveform envelope, detected peaks, trains, and motifs.}
#' \item{peak_data}{A tibble containing detailed information about detected peaks, including their times and periods.}
#' \item{train_data}{A tibble containing details about detected trains, including start and end times, durations, peak rates, and gaps.}
#' \item{motif_data}{A tibble summarizing the detected motifs, including motif duration, train counts, and duty cycle.}
#' \item{params}{A tibble of input parameters for reference.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(coryphoda)
#' # result <- temporal_stats_lq(coryphoda)
#' # plot(result$plot)
#' }
#'
#' @importFrom dplyr mutate filter group_by summarize ungroup lead
#' @importFrom tibble tibble
#' @importFrom seewave env resamp
#' @importFrom plotly plot_ly add_lines layout
#' @importFrom purrr map
#' @importFrom htmlwidgets onRender
temporal_stats_lq <- function(wave,
                              specimen_id = "",
                              ssmooth = 100,
                              peakfinder_ws = 50,
                              peakfinder_threshold = 0.005,
                              max_train_gap = 0.5,
                              max_peak_gap = 0.01,
                              detection_threshold = 0.01,
                              norm_env = TRUE) {

  # Store input parameters in a tibble
  params <- tibble(
    specimen_id = specimen_id,
    ssmooth = ssmooth,
    peakfinder_ws = peakfinder_ws,
    peakfinder_threshold = peakfinder_threshold,
    max_train_gap = max_train_gap,
    max_peak_gap = max_peak_gap,
    norm_env = norm_env
  )

  window_size <- peakfinder_ws

  waveDuration <- seewave::duration(wave)

  # envelope_vector <- seewave::env(wave, ssmooth = ssmooth, plot = FALSE)

  # warbleR's envelope is faster
  envelope_vector <- warbleR::envelope(as.numeric(wave@left), ssmooth = ssmooth)

  if (norm_env) {
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
  # time_vector_ms <- time_vector * 1000  # Convert to milliseconds

  peaks <- peaks[peaks <= length(time_vector)]


  # Filter out peaks below the detection threshold
  peak_amplitudes <- envelope_vector[peaks]  # Get amplitudes of the detected peaks
  valid_peaks <- which(peak_amplitudes >= detection_threshold)  # Only keep peaks above threshold
  peaks <- peaks[valid_peaks]  # Update the peaks list
  peak_amplitudes <- peak_amplitudes[valid_peaks]  # Update amplitudes list

  peak_times <- time_vector[peaks]

  peak_periods <- diff(peak_times)
  peak_periods[peak_periods > max_peak_gap] <- NA

  peak_data <- tibble(
    specimen.id = rep(specimen_id, length(peaks)),
    motif.id = integer(length(peaks)),
    train.id = integer(length(peaks)),
    peak.id = integer(length(peaks)),
    peak.time = peak_times,
    peak.period = c(peak_periods, NA),
    peak.amp = round(peak_amplitudes,4)
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
    mutate(peak.period = round(peak.period,4))

  peak_counter <- 1

  for (i in 2:length(peaks)) {
    if (peak_times[i] - peak_times[i - 1] > max_peak_gap) {
      train_end <- peaks[i - 1]
      trains <- append(trains, list(c(train_start, train_end)))
      train_start <- peaks[i]
      current_motif[[length(current_motif)]][2] <- train_end
      current_motif <- append(current_motif, list(c(train_start, NULL)))
      peak_counter <- 0
      if (peak_times[i] - peak_times[i - 1] > max_train_gap) {
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
      mutate(motif.id = ifelse(peak.time == peak_times[i], motif_id, motif.id),
             train.id = ifelse(peak.time == peak_times[i], train_id, train.id),
             peak.id = ifelse(peak.time == peak_times[i], peak_counter, peak.id))
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
    mutate(peak.rate = round(((n.peaks-1) / train.dur),1)) %>%
    ungroup() %>%
    # Set last train.period of each motif to NA and round to 3 decimals
    mutate(train.period = ifelse(is.na(lead(motif.id)) | lead(motif.id) != motif.id, NA, lead(train.start) - train.start),
           train.gap = round(lead(train.start) - train.end, 3)        # Gap: next train.start - current train.end
    ) %>%
    relocate(train.period, .after = train.dur) %>%
    relocate(train.gap, .after = train.period) %>%
    mutate(train.period = round(train.period,3))



  # Calculate temporal excursion (variability in timing of peaks, per train)

  # Add peak period column in milliseconds
  peak_data$peak.period.ms <- round((peak_data$peak.period * 1000), 4)

  tem_exc_data <- peak_data %>%
    group_by(train.id) %>%
    summarize(tem.exc = round(sum(abs(diff(peak.period.ms)), na.rm = TRUE),3))

  train_data <- train_data %>%
    left_join(tem_exc_data, by = "train.id")


  # Calculate tynamic excutsion (variability in energy among peaks, per train)
  # Create the peak_dyn_data data frame
  peak_dyn_data <- peak_data %>%
    group_by(train.id) %>%
    mutate(peak.diff = abs(peak.amp - lag(peak.amp))) %>%  # Calculate differences between consecutive peak.amp values
    filter(!is.na(peak.diff))  # Remove rows where peak.diff is NA (first peak in each train)

  # Sum the absolute differences for each train.id
  dyn_exc_data <- peak_dyn_data %>%
    group_by(train.id) %>%
    summarize(dyn.exc = sum(peak.diff, na.rm = TRUE))%>%
    mutate(dyn.exc = round(dyn.exc,3))# Sum the absolute peak differences for each train

  # Add dyn.exc to train_data by matching train.id
  train_data <- train_data %>%
    left_join(dyn_exc_data, by = "train.id")

  train_data <- train_data %>%
    relocate(c(tem.exc, dyn.exc), .after = train.id)


  # Create tibble for motif measurements
  motif_data <- train_data %>%
    group_by(specimen.id, motif.id) %>%
    summarize(
      motif.start = min(train.start),
      motif.end = max(train.end),
      motif.dur = round(motif.end - motif.start,4),
      motif.period = round((lead(motif.start) - motif.start),4),
      n.trains = n(),
      duty.cycle = round((sum(train.dur) / motif.dur)*100,2),
      tem.exc.mean = round(mean(tem.exc),3),
      tem.exc.var = round(var(tem.exc),3),
      tem.exc.sd = round(sd(tem.exc),3),
      dyn.exc.mean = round(mean(dyn.exc),3),
      dyn.exc.var = round(var(dyn.exc),3),
      dyn.exc.sd = round(sd(dyn.exc),3)
    ) %>%
    mutate(
      train.rate = round((n.trains-1)/motif.dur)
      # tem.exc.mean = round(mean(tem.exc),3),
      # tem.exc.var = round(var(tem.exc),3),
      # tem.exc.sd = round(sd(tem.exc),3),
      # dyn.exc.mean = round(mean(dyn.exc),3),
      # dyn.exc.var = round(var(dyn.exc),3),
      # dyn.exc.sd = round(sd(dyn.exc),3)
    ) %>%
    ungroup() %>%
    relocate(train.rate, .after = n.trains) %>%
    relocate(c(tem.exc.mean:dyn.exc.sd), .after = motif.id)


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
    select(specimen.id, pci, everything(), -proportions, proportions)

  # Prepare annotations for the plot
  annotations <- list(
    list(
      x = 0.01,
      y = 0.99,
      xref = 'paper',
      yref = 'paper',
      text = paste("<b> Summary Statistics</b>",
                   "<br> N. motifs:", length(motifs),
                   "<br> Motif Dur.: ", round(mean(motif_data$motif.dur),3), "s",
                   "<br> Duty Cycle: ", round(mean(motif_data$duty.cycle),1), "%",
                   "<br> Trains/Motif:", round(mean(motif_data$n.trains),1),
                   "<br> Train Rate: " , round(mean(motif_data$train.rate)), "tps",
                   "<br> Train Dur.: ", round(mean(train_data$train.dur, na.rm = TRUE)), "ms",
                   "<br> Peaks/Train: ", round(mean(train_data$n.peaks, na.rm = TRUE)),
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
    train_start_time <- train_data$train.start[i] #time_vector[trains[[i]][1]]
    train_end_time <- train_data$train.end[i] # time_vector[trains[[i]][2]]
    show_legend <- if (i == 1) TRUE else FALSE
    p <- p %>%
      add_lines(x = c(train_start_time, train_end_time), y = c(0.98, 0.98),
                name = "Trains", line = list(color = "#009E73", width = 6),
                showlegend = show_legend, legendgroup = "Trains",
                hoverinfo = "x", text = paste("Time:", round(c(train_start_time, train_end_time), 2)))
  }

  # Add motif lines to the plot using motif_data
  for (i in seq_len(nrow(motif_data))) {
    motif_start <- motif_data$motif.start[i]
    motif_end <- motif_data$motif.end[i]
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

  p

  return(list(plot = p,
              motif_data = motif_data,
              train_data = train_data,
              peak_data = peak_data,
              params = params))
}

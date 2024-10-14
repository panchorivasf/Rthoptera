#' Calculate Temporal Statistics for HQ calls
#'
#' This function analyzes the acoustic characteristics of a wave object by detecting
#' amplitude "trains" in the signal envelope, grouping them into motifs, and computing
#' various summary statistics and metrics related to train and motif structure.
#' An interactive plot visualizes the results, showing the envelope of the signal,
#' detected trains, motifs, and key statistics. The plot can be downloaded as an HTML
#' (interactive) or PNG (static) file.
#'
#' @param wave A Wave object containing the sound data.
#' @param specimen.id A character string representing the specimen ID (default: "").
#' @param msmooth_window An integer specifying the window size (in milliseconds) for
#' smoothing the envelope (default: 100).
#' @param msmooth_overlap An integer specifying the overlap (in milliseconds) for
#' smoothing the envelope (default: 50).
#' @param upper_detection_threshold A numeric value representing the upper amplitude
#' detection threshold (default: 0.2).
#' @param lower_detection_threshold A numeric value representing the lower amplitude
#' detection threshold (default: 0.1).
#' @param min_train_dur A numeric value specifying the minimum duration (in seconds)
#' of a valid amplitude train (default: 0.002).
#' @param max_train_gap A numeric value representing the maximum gap (in seconds)
#' allowed between two trains before considering them part of different motifs (default: 0.08).
#' @param norm.env A logical indicating whether to normalize the envelope (default: TRUE).
#'
#' @return A list with the following components:
#' \item{plot}{An interactive `plotly` plot showing the signal envelope, detected trains, and motifs.}
#' \item{summary_data}{A tibble with summary statistics of the acoustic motifs.}
#' \item{train_data}{A tibble with details about the detected trains.}
#' \item{motif_data}{A tibble with details about the detected motifs.}
#' \item{params}{A tibble containing the input parameters.}
#' @export
#' @examples
#' \dontrun{
#' data(gryllus)
#' # result <- temporal_stats_hq(gryllus)
#' # plot(result$plot)
#' }
#'
#' @importFrom dplyr mutate filter group_by summarize ungroup lead
#' @importFrom tibble tibble
#' @importFrom seewave env resamp
#' @importFrom plotly plot_ly add_lines layout
#' @importFrom purrr map
#' @importFrom htmlwidgets onRender
temporal_stats_hq <- function(wave,
                              specimen.id = "",
                              msmooth_window = 100,
                              msmooth_overlap = 50,
                              upper_detection_threshold = 0.2,
                              lower_detection_threshold = 0.1,
                              min_train_dur = 0.002,
                              max_train_gap = 0.08,
                              norm.env = TRUE) {

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
      train.rate = round((n.trains-1) / motif.dur, 3),
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

<!-- badges: start -->
[![R-CMD-check](https://github.com/naturewaves/Rthoptera/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/naturewaves/Rthoptera/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

*Rthoptera* is an R package that provides interactive Shiny applications 
for standard analysis of insect sounds. The package is intended to be used 
on high signal-to-noise recordings, helping researchers make standardized 
measurements and plots to support the scientific description of the "acoustic types".
We define "acoustic type" as the first description of the calling song of a species,
which should be accompanied by a high-quality recording and ideally the voucher specimen
identifier in the collection it was deposited.
It offers functions for pre-processing, spectral and temporal analysis, as well as 
several interactive and static visualizations.

## Pre-processing

The package offers several pre-processing tools that allow you to handle audio 
files before performing detailed analysis:

- **Import**: Browse your local data to import audio files (WAV) as Wave objects 
into R. During this process, the waveform is centered to zero with the `rmoffset()`
function to ensure accurate measurements. 
  
- **Downsample**: When required, you can reduce the sampling rate of your audio 
files. This helps improve the computing speed for certain analyses and plots 
without losing important sound features. 
  
- **Band-pass Filter**: When necessary, apply a user-defined high-pass and/or 
low-pass filter to eliminate noise or non-target sounds from your audio recordings. 
  
- **Trim**: Using an interactive oscillogram, you can visually select and trim 
specific sections of a Wave object. The trimmed sections can be saved as new Wave 
objects in your R environment for further analysis.

We encourage new users to see the "Preprocessing" vignette for guidance.

## Analysis
Once pre-processing is complete, *Rthoptera* offers analysis tools to extract 
spectral and temporal statistics from your audio files:

- **Spectral Statistics**: Automatically calculate spectral metrics based on the 
mean power spectrum of a Wave object. These statistics provide insights into the 
frequency domain of the audio signal.

- **Temporal Statistics**: Automatically extract temporal metrics from your audio recordings. 
This includes identifying and analyzing elements ("tooth impacts"), trains (syllables), 
and echemes (groups of syllables or trills) in the insect sounds. Two apps are available: 
`temporal_stats_hq_app` is optimized to work with "tonal" (i.e., "high-Q") signals, 
such as those produced by most crickets. It creates an envelope of the waveform and 
measures the duration of sounds and gaps based on a user-defined detection threshold. 
The  `temporal_stats_lq_app` works better for broadband calls with wide amplitude variability, 
where the threshold approach would leave fainter sounds undetected or poorly measured. 
This app detects each peak in the envelope, often corresponding to a single tooth impact, 
and groups them into trains and echemes with user-defined thresholds. 

## Plotting

- **Multi-Power Spectrum**: An interactive tool that overlays multiple power 
spectrum plots selected from the oscillogram, allowing for easy comparison and 
visualization of spectral features across different time intervals. Each selection
is assigned to its own color-blind-safe color both in the oscillogram and the 
mean spectrum plots. 

- **Spectrogram**: Generate standard spectrograms, optionally alongside a lateral 
mean power spectrum. This combination allows you to visualize both time-frequency 
representations and the overall spectral distribution. The spectrogram window size 
is automatically adjusted based on the sampling rate and duration of the recording 
to obtain a standard frequency/time resolution trade-off. 

- **Multi Plot**: A combined visualization of the spectrogram, mean spectrum, and 
oscillogram, providing a comprehensive view of both the time-domain and frequency-domain 
characteristics of the insect sounds.

- **Oscillogram**: Create standard oscillograms as well as interactive oscillograms 
that allow you to zoom in and explore specific sections of the waveform.

- **Multi-oscillogram**: Create a stacked oscillogram plot for comparing the sounds 
of multiple species. This is particularly useful for analyzing the differences in 
acoustic patterns between species.

## Installation
To install the Rthoptera package, follow these steps:

1. Install the `remotes` package if you haven't already:

    ```r
    install.packages("remotes")
    ```

2. Load the `remotes` package:

    ```r
    library(remotes)
    ```

3. Install **Rthoptera** from GitHub:

    ```r
    remotes::install_github("naturewaves/Rthoptera")
    ```

4. Load the **Rthoptera** package:

    ```r
    library(Rthoptera)
    ```


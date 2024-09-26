Interactive functions (Shiny apps) for standard analysis of insect sounds, including apps for:

Pre-processing:

-Import: Browse your data to import audio files as Wave objects into R. All files are automatically corrected for DC offset.

-Downsample: When appropriate, reduce the sampling rate to increase computing speed for analyses and plots.

-High-pass Filter: When needed and possible, apply a user-defined high-pass filter to eliminate low-frequency noise.

-Trim: Use an interactive oscillogram to select and trim a Wave object, saving selections to the R environment.

Analysis

-Spectral Statistics: automated spectral metrics based on the mean power spectrum of a wave object.

-Temporal Statistics: automated temporal metrics of peaks (pulses), train (syllables), and echemes (groups of syllables, trills).

-Multi-Power Spectra: Interactive color-coded oscillogram + multiple, overlayed power spectra.

-Spectrogram: Standard spectrogram + lateral mean power spectrum.

-Multi Plot: Spectrogram + oscillogram.

-Oscillogram: Standard oscillogram + interactive oscillogram

-Multi-oscillogram: Standard multi-species oscillogram stack plot.


How to install:
1) Download the ZIP file from the top right corner and unzip it in your directory of preference. 
2) Install devtools if not already installed: install.packages('devtools').
3) Install Rthoptera: devtools::install("path/to/Rthoptera"). Add the path to the directory where you stored the package. 
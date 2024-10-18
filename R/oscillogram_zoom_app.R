#' Oscillogram + Zoomed Stack Shiny App
#'
#' This Shiny app provides an interface to visualize an oscillogram along with zoomed-in portions of the waveform stacked below.
#' The user can select portions of the oscillogram to zoom in on, display either axes or a scale bar, and save the resulting image.
#'
#' @param dark.mode Logical, if TRUE the app will use a dark theme.
#' @return A Shiny app that displays an oscillogram and zoomed sections below, with options to save the plot as a PNG.
#' @import shiny
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom shinycssloaders withSpinner
#' @importFrom dplyr mutate row_number filter select pull
#' @importFrom tibble tibble
#' @importFrom tuneR normalize
#' @importFrom seewave rmoffset
#' @importFrom cowplot plot_grid
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom bslib bs_theme
#' @export
#'
#' @examples
#' \dontrun{
#' multioscillo_app()
#'}
#' if (interactive()) {
#'   oscillogram_zoom_app(dark.mode = TRUE)
#' }

oscillogram_zoom_app <- function(dark.mode = TRUE) {

  jscode <- "shinyjs.closeWindow = function() { window.close(); }"

  ui <- function(request) {
    tagList(
      h1("Oscillogram", style = "font-size: 28px; margin-left: 15px;"),
      fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        theme = if (dark.mode) bslib::bs_theme(bootswatch = "darkly") else NULL,
        tags$head(tags$style(
          HTML(
            "
            body {
              margin: 5px; /* Adds margin around the entire page */
            }
            #wavePlot, #zoomedPlot, #finalZoomedPlot {
              height: calc(100vh - 200px); /* Adjusts height taking into account other elements */
              width: 100%;
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

             #oscillogram {
               border: 2px solid forestgreen; /* Green contour */
               border-radius: 5px; /* Optional: Rounded corners */
             }

              #zoomedPortion {
               border: 2px solid forestgreen; /* Green contour */
               border-radius: 5px; /* Optional: Rounded corners */
             }

              #saveImage {
              border: 2px solid dodgerblue; /* Blue contour */
              border-radius: 5px; /* Optional: Rounded corners */
              }

              #close {
              border: 2px solid red; /* Red contour */
              padding: 5px 10px; /* Optional: Adjust padding */
              border-radius: 5px; /* Optional: Rounded corners */
              }

            "
          )
        )),

        fluidRow(
          column(2,
                 selectInput("waveObject", "Select a wave object:",
                             choices = NULL, selected = NULL, width = '100%'),
          ),
          column(1, radioButtons("displayOption", "Display Option:",
                                 choices = c("Axes" = "axes", "Scale Bar" = "scalebar"),
                                 selected = "axes")), # Radio buttons to toggle between axes and scalebar)
          column(2, verticalLayout(
            tags$style(HTML("
           .shiny-input-container {
             margin-bottom: 2px; /* Adjusts the space between inputs */
           }
         ")),
            numericInput("scaleBar1", "Scale Bar 1 (ms):", value = 500, min = 1, step = 100), # Input for scale bar 1
            numericInput("scaleBar2", "Scale Bar 2 (ms):", value = 50, min = 1, step = 10), # Input for scale bar 2
            numericInput("scaleBar3", "Scale Bar 3 (ms):", value = 10, min = 1, step = 1)  # Input for scale bar 3
          )),
          column(1, numericInput("imgWidth", "Image Width (in):", value = 16, min = 1)),
          column(1, numericInput("imgHeight", "Image Height (in):", value = 2, min = 1)),
          column(2,
                 verticalLayout(
                   actionButton("oscillogram", "Plot Oscillogram"),
                   actionButton("zoomedPortion", "Add Selection to Stack")
                 )
          ),
          column(2,
                 downloadButton("saveImage", "Save PNG"),
                 checkboxInput("transparentBg", "Save with transparent background", value = FALSE)
          ),
          column(1, actionButton("close", "Close App")
          )
        ),
        mainPanel(
          uiOutput("wavePlotUI"),
          uiOutput("zoomedPlotUI"),
          uiOutput("finalZoomedPlotUI")
        )
      )
    )
  }

  server <- function(input, output, session) {

    selectedRegion <- reactiveVal(NULL)
    finalZoomedRegion <- reactiveVal(NULL)
    thirdZoomedRegion <- reactiveVal(NULL)

    fullPlot <- reactiveVal(NULL)
    zoomedPlot <- reactiveVal(NULL)
    finalZoomedPlot <- reactiveVal(NULL)

    tempImagePath <- reactiveVal(NULL)  # Path to the temporary image file

    plotTriggered <- reactiveVal(FALSE)

    wave_df <- function(wave, norm = TRUE){

      if(norm){
        wave <- tuneR::normalize(object = wave, unit = "1", center = TRUE)
      }

      wave <- rmoffset(wave, output = "Wave")

      srate <- wave@samp.rate

      amplitude <- wave@left

      tbl <- tibble(amplitude = amplitude)
      tbl <- tbl %>%
        mutate(index = row_number(),
               time = (index-1 ) / srate)
      tbl <- tbl %>%
        select(c(amplitude, time))

      return(tbl)
    }

    createOscillogram <- function(wave, brush_data = list(), colors = NULL, display_option = "axes", scalebar_length = NULL, vertical_lines = NULL) {

      tbl <- wave_df(wave)

      p <- ggplot(tbl, aes(x = time, y = amplitude)) +
        geom_line(color = "black", linewidth = 0.4) +
        expand_limits(y = c(-1.2, 1.2)) +
        labs(x = "Time (s)", y = "Amplitude") +
        scale_x_continuous(expand = c(0, 0))

      # Display axes or scale bar based on user selection
      if (display_option == "axes") {
        p <- p + theme_minimal() +
          theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            axis.line = element_line(color = "black"),
            axis.title = element_text(size = 14)
          )
      } else if (display_option == "scalebar" && !is.null(scalebar_length)) {
        scalebar_length_sec <- scalebar_length / 1000  # Convert ms to seconds
        scalebarText <- ifelse(scalebar_length < 1000, paste0(scalebar_length, " ms"), paste0(scalebar_length_sec, " s"))

        # Calculate position for the scalebar
        scalebar_x_start <- 0.5 / input$imgWidth * max(tbl$time)
        scalebar_x_end <- scalebar_x_start + scalebar_length_sec

        # Check if the scale bar fits within the plot duration
        if (scalebar_length_sec > max(tbl$time)) {
          showModal(modalDialog(
            title = "Scale Bar Too Long",
            "The scale bar length exceeds the duration of the oscillogram. Please select a shorter scale bar length.",
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
        } else {
          p <- p + theme_void() +
            annotate("segment", x = scalebar_x_start, xend = scalebar_x_end, y = -1.1, yend = -1.1, color = "black", size = 1) +
            annotate("text", x = (scalebar_x_start + scalebar_x_end) / 2, y = -1.3, label = scalebarText, vjust = 0.5, hjust = 0.5)
        }
      }

      # if (!is.null(colors) && length(brush_data) > 0) {
      #   for (i in seq_along(brush_data)) {
      #     range <- brush_data[[i]]
      #     selected_data <- tbl %>% filter(time >= range[1] & time <= range[2])
      #     p <- p + geom_line(data = selected_data, aes(x = time, y = amplitude),
      #                        color = colors[i], linewidth = 0.5)
      #   }
      # }

      # Add vertical lines if specified
      if (!is.null(vertical_lines)) {
        p <- p + geom_vline(xintercept = vertical_lines,
                            col = "grey20",
                            linewidth = 1,
                            linetype = "dashed")
      }

      return(p)
    }

    selected_wave <- reactiveVal()
    brushed_ranges <- reactiveVal(list())
    tempWave <- reactiveVal(NULL)

    observeEvent(input$waveObject, {
      if (input$waveObject != "") {
        wave_obj <- get(input$waveObject, envir = .GlobalEnv)
        selected_wave(wave_obj)
        brushed_ranges(list())
      }
    })

    observe({
      waveObjects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(x)
        inherits(get(x, envir = .GlobalEnv), "Wave"))]
      updateSelectInput(session, "waveObject", choices = waveObjects)
    })

    output$wavePlotUI <- renderUI({
      if (plotTriggered()) {
        withSpinner(
          plotOutput("wavePlot", height = "250px", width = "1500px",
                     brush = brushOpts(id = "waveBrush", direction = "x")),
          type = 1
        )
      }
    })

    observeEvent(input$oscillogram, {
      req(input$waveObject)

      plotTriggered(TRUE)
      selectedRegion(NULL)
      finalZoomedRegion(NULL)
      thirdZoomedRegion(NULL)
      tempWave(NULL)
      fullPlot(NULL)
      zoomedPlot(NULL)
      finalZoomedPlot(NULL)
      tempImagePath(NULL)  # Clear the temp image path

      removeUI(selector = "#zoomedPlot")
      removeUI(selector = "#finalZoomedPlot")

      wave <- get(input$waveObject, envir = .GlobalEnv)

      isolate({
        output$wavePlot <- renderPlot({
          tryCatch({
            if (!is.null(wave)) {
              p <- createOscillogram(wave, brushed_ranges(),
                                     display_option = input$displayOption,
                                     scalebar_length = input$scaleBar1)
              if (!is.null(finalZoomedRegion()) && length(finalZoomedRegion()) == 2) {
                p <- p + geom_vline(xintercept = finalZoomedRegion(),
                                    col = "grey20",
                                    linewidth = 1,
                                    linetype = "dashed")
              }
              fullPlot(p)
              p
            }
          }, error = function(e) {
            showModal(modalDialog(
              title = "Error",
              paste("An error occurred:", e$message),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
            NULL
          })
        }, height = 250, width = 1500)
      })
    })

    observeEvent(input$waveBrush, {
      currentRegion <- c(input$waveBrush$xmin, input$waveBrush$xmax)
      selectedRegion(currentRegion)
    })

    observeEvent(input$zoomedPortion, {
      req(selectedRegion())

      wave <- get(input$waveObject, envir = .GlobalEnv)

      isolate({
        selected_duration <- diff(selectedRegion())

        if (is.null(finalZoomedRegion())) {
          if (input$displayOption == "scalebar" && input$scaleBar2 / 1000 > selected_duration) {
            showModal(modalDialog(
              title = "Scale Bar Too Long",
              "The scale bar length exceeds the duration of the selected portion. Please select a shorter scale bar length.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
            return()
          }

          finalZoomedRegion(selectedRegion())

          temp_wave <- extractWave(
            wave,
            from = finalZoomedRegion()[1],
            to = finalZoomedRegion()[2],
            xunit = "time"
          )

          tempWave(temp_wave)

          output$wavePlotUI <- renderUI({
            plotOutput("wavePlot", height = "250px", width = "1500px")
          })

          output$zoomedPlotUI <- renderUI({
            plotOutput("zoomedPlot", height = "250px", width = "1500px",
                       brush = brushOpts(id = "zoomedBrush", direction = "x"))
          })

          output$zoomedPlot <- renderPlot({
            req(finalZoomedRegion())

            tryCatch({
              if (!is.null(tempWave())) {
                p <- createOscillogram(tempWave(), display_option = input$displayOption, scalebar_length = input$scaleBar2)

                zoomedPlot(p)
                p
              }
            }, error = function(e) {
              showModal(modalDialog(
                title = "Error",
                paste("An error occurred:", e$message),
                easyClose = TRUE,
                footer = modalButton("OK")
              ))
              NULL
            })
          }, height = 250, width = 1500)

        } else {
          req(tempWave())

          if (input$displayOption == "scalebar" && input$scaleBar3 / 1000 > selected_duration) {
            showModal(modalDialog(
              title = "Scale Bar Too Long",
              "The scale bar length exceeds the duration of the selected portion. Please select a shorter scale bar length.",
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
            return()
          }

          third_zoomed_wave <- extractWave(
            tempWave(),
            from = input$zoomedBrush$xmin,
            to = input$zoomedBrush$xmax,
            xunit = "time"
          )

          thirdZoomedRegion(c(input$zoomedBrush$xmin, input$zoomedBrush$xmax))

          output$finalZoomedPlotUI <- renderUI({
            plotOutput("finalZoomedPlot", height = "250px", width = "1500px")
          })

          output$finalZoomedPlot <- renderPlot({
            req(thirdZoomedRegion())

            tryCatch({
              if (!is.null(third_zoomed_wave)) {
                p <- createOscillogram(third_zoomed_wave, display_option = input$displayOption, scalebar_length = input$scaleBar3)
                finalZoomedPlot(p)

                # Save the combined plot to a temporary file after the third oscillogram is added
                temp_file <- tempfile(fileext = ".png")
                total_height <- input$imgHeight * 3  # 3 plots (full, zoomed, and final zoomed)
                combinedPlot <- plot_grid(fullPlot(), zoomedPlot(), finalZoomedPlot(), ncol = 1)
                bg <- ifelse(input$transparentBg, "transparent", "white")

                ggsave(temp_file, combinedPlot, width = input$imgWidth, height = total_height, units = "in", dpi = 300, bg = bg)
                tempImagePath(temp_file)  # Store the path to the temp image
                p  # Ensure p is returned for rendering
              }
            }, error = function(e) {
              showModal(modalDialog(
                title = "Error",
                paste("An error occurred:", e$message),
                easyClose = TRUE,
                footer = modalButton("OK")
              ))
              NULL
            })
          }, height = 250, width = 1500)

          output$zoomedPlot <- renderPlot({
            req(finalZoomedRegion())

            tryCatch({
              if (!is.null(tempWave())) {
                p <- createOscillogram(tempWave(), display_option = input$displayOption, scalebar_length = input$scaleBar2, vertical_lines = thirdZoomedRegion())

                zoomedPlot(p)
                p
              }
            }, error = function(e) {
              showModal(modalDialog(
                title = "Error",
                paste("An error occurred:", e$message),
                easyClose = TRUE,
                footer = modalButton("OK")
              ))
              NULL
            })
          }, height = 250, width = 1500)
        }
      })
    })

    output$saveImage <- downloadHandler(
      filename = function() {
        paste(as.character(input$waveObject),"oscillogram.png")
      },
      content = function(file) {

        # showPageSpinner(type = 1)

        # If a temporary image path exists, use it instead of regenerating the plots
        if (!is.null(tempImagePath())) {
          file.copy(tempImagePath(), file)
        } else {
          req(fullPlot(), zoomedPlot(), finalZoomedPlot())

          total_height <- input$imgHeight * 3  # 3 plots (full, zoomed, and final zoomed)

          combinedPlot <- plot_grid(fullPlot(), zoomedPlot(), finalZoomedPlot(), ncol = 1)

          bg <- ifelse(input$transparentBg, "transparent", "white")
          ggsave(file, combinedPlot, width = input$imgWidth, height = total_height, units = "in", dpi = 300, bg = bg)
        }

        # hidePageSpinner()
      }
    )



    session$onSessionEnded(function() {
      stopApp()
    })

    observeEvent(input$close, {
      shinyjs::runjs("window.close();")
      stopApp()
    })

  }

  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}

library(shiny)

# User interface
ui <- fluidPage(
  # Title
  titlePanel("zltrajectory: Analysis of Zebrafish Larvae Trajectories"),

  # Main content
  verticalLayout(
    # Step 1: create a trajectory
    tags$h3("Step 1: Create a Trajectory"),
    sidebarLayout(
      sidebarPanel(
        # File upload
        tags$p("Start by uploading a CSV file containing trajectory data on a
               zebrafish larva."),
        fileInput(
          inputId = "data",
          label = "CSV File Upload",
          accept = ".csv"
        ),

        # Create trajectory
        tags$p("Specify the names of the columns within the file that contain
               the x- and y-positions of the zebrafish larva. Additionally,
               specify the time in one of two ways. You can either specify the
               name of a column within the file that contains the times of each
               observation, if such a column is present, or specify a constant
               rate denoting the time elapsed between consecutive observations
               in the file."),
        fluidRow(
          column(
            width = 6,
            textInput(
              inputId = "x",
              label = "x-position Column Name"
            ),
            textInput(
              inputId = "t",
              label = "Time Column Name"
            )
          ),
          column(
            width = 6,
            textInput(
              inputId = "y",
              label = "y-position Column Name"
            ),
            numericInput(
              inputId = "rate",
              label = "Rate",
              value = NULL
            )
          )
        ),

        # Specify units
        tags$p("Specify the units of length and time used in the data. For
               example, if positions were measured in pixels and times were
               recorded in seconds, you could try entering either \"pixels\" and
               \"seconds\" or \"px\" and \"s\" depending on whether you prefer
               full unit names or unit symbols. These units will be included in
               plot titles and labels."),
        fluidRow(
          column(
            width = 6,
            textInput(
              inputId = "unit_length",
              label = "Length Unit"
            )
          ),
          column(
            width = 6,
            textInput(
              inputId = "unit_time",
              label = "Time Unit"
            )
          )
        ),

        # Use example dataset
        tags$p("If you would like to try an example dataset, click the button
               below. Results may take some time to appear. Note that this will
               override all settings."),
        actionButton(
          inputId = "cadmium_zebrafish",
          label = "Use Example Dataset"
        )
      ),
      mainPanel(
        # Trajectory data frame
        dataTableOutput(
          outputId = "trajectory"
        )
      )
    ),

    # Step 2: find bouts
    tags$h3("Step 2: Find Bouts"),
    sidebarLayout(
      sidebarPanel(
        # Plot trajectory
        tags$p("The first plot in this step is a plot of the raw trajectory.
               This plot displays the x-position, y-position, and time of each
               observation. Time is indicated by the color of a point. Use the
               checkbox below to toggle the presence of a colorbar showing the
               mapping between times and colors of points."),
        checkboxInput(
          inputId = "time_colorbar",
          label = "Time Colorbar",
          value = TRUE
        ),

        # Plot diagnostics
        tags$p("The second plot shows distances traveled between each
               consecutive pair of positions in the trajectory data. This plot
               is intended to assist the user in selecting parameter values for
               finding bouts. To change the limits of the y-axis on this plot,
               specify both the lower and upper limits below. Use the checkbox
               to toggle the presence of a legend for bouts and peaks that will
               appear once they are calculated."),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = "ymin",
              label = "Lower Limit of y-axis",
              value = NULL
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "ymax",
              label = "Upper Limit of y-axis",
              value = NULL
            )
          )
        ),
        checkboxInput(
          inputId = "legend_default",
          label = "Legend",
          value = TRUE
        ),

        # Time limits on plots
        tags$p("To adjust the range of times plotted in both plots, specify both
               the minimum and maximum times to plot below."),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = "time_min",
              label = "Minimum Time to Plot",
              value = NULL
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "time_max",
              label = "Maximum Time to Plot",
              value = NULL
            )
          )
        ),

        # Find bouts
        tags$p("The last two inputs in this step will be used for finding the
               beginnings of swim bouts. The bout detection algorithm first
               finds peaks (local maxima) in the time series of distances
               traveled between consecutive recorded positions, which is
               displayed in the second plot. The distance for each peak must be
               greater than a minimum distance threshold, which is specified in
               the first input below. This threshold will appear as a dotted
               blue horizontal line in the second plot. Additionally, the
               minimum amount of elapsed time between consecutive peaks is
               specified in the second input below. Bouts are then found by
               detecting the first point to the left of each peak with a
               distance lower than the distance threshold, assuming that such a
               point exists. Both bouts and peaks will be displayed as X's in
               the second plot."),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = "min_distance",
              label = paste0("Minimum Distance Traveled Between ",
                             "Consecutive Positions for Peaks"),
              value = NULL
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "min_time",
              label = "Minimum Time Between Peaks",
              value = NULL
            )
          )
        )
      ),
      mainPanel(
        # Trajectory plot
        plotOutput(
          outputId = "plot_trajectory"
        ),

        # Diagnostic plot
        plotOutput(
          outputId = "plot_diagnostics"
        )
      )
    ),

    # Step 3: perform further analyses
    tags$h3("Step 3: Perform Further Analyses"),
    sidebarLayout(
      sidebarPanel(
        # Analyze trajectory and plot distribution
        tags$p("Use these settings to generate a plot of the distribution of
               turn angles formed by consecutive groups of three bouts, bouts
               per specified unit of time, or distances between consecutive
               bouts. Supported output types are histograms, kernel density
               estimate (KDE) plots, and empirical cumulative distribution
               (ECDF) plots. To change the x-axis limits on the plot, specify
               both the lower and upper limits below. For histograms, you may
               also specify a suggested number of bins."),
        tags$p("When calculating the number of bouts per specified unit of time,
               you must specify the unit of time relative to the original time
               unit used in the data and entered previously. For example, if
               times are recorded in seconds and 60 is entered in the
               user-specified time unit input, the number of bouts per minute
               (60 seconds) will be calculated."),
        fluidRow(
          column(
            width = 6,
            radioButtons(
              inputId = "analyze_trajectory",
              label = "Calculation Type",
              choices = c("Angles", "Bouts per Time Unit", "Distances")
            )
          ),
          column(
            width = 6,
            radioButtons(
              inputId = "type",
              label = "Plot Type",
              choices = c("Histogram" = "histogram",
                          "KDE" = "kde",
                          "ECDF" = "ecdf")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = "unit",
              label = "User-Specified Time Unit",
              value = NULL
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "breaks",
              label = "Suggested Number of Bins",
              value = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = "xmin",
              label = "Lower Limit of x-axis",
              value = NULL
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = "xmax",
              label = "Upper Limit of x-axis",
              value = NULL
            )
          )
        )
      ),
      mainPanel(
        # Distribution plot
        plotOutput(
          outputId = "plot_distribution"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Current dataset
  data <- reactiveVal()

  # Current trajectory
  trajectory <- reactive({
    req(data())
    req(input$x, input$y, xor(isTruthy(input$t), isTruthy(input$rate)))
    validate(
      need(input$x %in% names(data()),
           "x-position column name must be in the data."),
      need(input$y %in% names(data()),
           "y-position column name must be in the data."),
      need(!isTruthy(input$t) || input$t %in% names(data()),
           "Time column name must be in the data."),
      need(!isTruthy(input$rate) || input$rate > 0,
           "Rate must be a positive number.")
    )
    if (isTruthy(input$t)) {
      t <- input$t
      rate <- NULL
    } else if (isTruthy(input$rate)) {
      t <- NULL
      rate <- input$rate
    }
    create_trajectory(data(), x = input$x, y = input$y, t = t, rate = rate)
  })

  # Use uploaded CSV
  observeEvent(
    input$data, {
      # Set current dataset
      data(read.csv(input$data$datapath))
    }
  )

  # Use example dataset (cadmium_zebrafish)
  observeEvent(
    input$cadmium_zebrafish, {
      # Set current dataset
      data(cadmium_zebrafish)

      # Update inputs in Step 1
      updateTextInput(
        inputId = "x",
        value = "X.center"
      )
      updateTextInput(
        inputId = "y",
        value = "Y.center"
      )
      updateTextInput(
        inputId = "t",
        value = "Trial.time"
      )
      updateNumericInput(
        inputId = "rate",
        value = NULL
      )
      updateTextInput(
        inputId = "unit_length",
        value = "mm"
      )
      updateTextInput(
        inputId = "unit_time",
        value = "s"
      )

      # Update inputs in step 2
      updateCheckboxInput(
        inputId = "time_colorbar",
        value = TRUE
      )
      updateNumericInput(
        inputId = "ymin",
        value = NULL
      )
      updateNumericInput(
        inputId = "ymax",
        value = NULL
      )
      updateCheckboxInput(
        inputId = "legend_default",
        value = TRUE
      )
      updateNumericInput(
        inputId = "time_min",
        value = 1240
      )
      updateNumericInput(
        inputId = "time_max",
        value = 1260
      )
      updateNumericInput(
        inputId = "min_distance",
        value = 2
      )
      updateNumericInput(
        inputId = "min_time",
        value = 0.6
      )

      # Update inputs in step 3
      updateRadioButtons(
        inputId = "analyze_trajectory",
        selected = "Angles"
      )
      updateRadioButtons(
        inputId = "type",
        selected = "histogram"
      )
      updateNumericInput(
        inputId = "unit",
        value = 60
      )
      updateNumericInput(
        inputId = "breaks",
        value = 36
      )
      updateNumericInput(
        inputId = "xmin",
        value = NULL
      )
      updateNumericInput(
        inputId = "xmax",
        value = NULL
      )
    }
  )

  # Trajectory data frame output
  output$trajectory <- renderDataTable({
    trajectory()
  }, options = list(pageLength = 10))

  # Trajectory plot output
  output$plot_trajectory <- renderPlot({
    req(trajectory())
    xlab <- ifelse(isTruthy(input$unit_length),
                   paste0("x-position (", input$unit_length, ")"),
                   "x-position")
    ylab <- ifelse(isTruthy(input$unit_length),
                   paste0("y-position (", input$unit_length, ")"),
                   "y-position")
    time_min <- ifelse(isTruthy(input$time_min),
                       input$time_min,
                       min(trajectory()$t))
    time_max <- ifelse(isTruthy(input$time_max),
                       input$time_max,
                       max(trajectory()$t))
    if (input$time_colorbar) {
      time_colorbar_lab <- ifelse(isTruthy(input$unit_time),
                                  paste0("Time\n(", input$unit_time, ")"),
                                  "Time")
    } else {
      time_colorbar_lab <- NULL
    }
    plot_trajectory(trajectory(),
                    xlab = xlab, ylab = ylab,
                    time_min = time_min, time_max = time_max,
                    time_colorbar = input$time_colorbar,
                    time_colorbar_lab = time_colorbar_lab)
  })

  # Bouts and peaks found
  bouts_and_peaks_list <- reactiveVal()

  # Update bouts and peaks whenever an argument changes
  update_bouts_and_peaks_list <- function() {
    bouts_and_peaks_list(NULL)
    req(trajectory())
    req(input$min_distance, input$min_time)
    validate(
      need(input$min_distance > 0,
           "Minimum distance traveled must be a positive number."),
      need(input$min_time > 0,
           "Minimum time between peaks must be a positive number.")
    )
    bouts_and_peaks_list(find_bouts(trajectory(),
                                    min_distance = input$min_distance,
                                    min_time = input$min_time,
                                    return_peaks = TRUE))
  }
  observeEvent(trajectory(), update_bouts_and_peaks_list())
  observeEvent(input$min_distance, update_bouts_and_peaks_list())
  observeEvent(input$min_time, update_bouts_and_peaks_list())

  # Diagnostic plot output
  output$plot_diagnostics <- renderPlot({
    req(trajectory())
    xlab <- ifelse(isTruthy(input$unit_time),
                   paste0("Time (", input$unit_time, ")"),
                   "Time")
    ylab <- ifelse(isTruthy(input$unit_length),
                   paste0("Distance (", input$unit_length, ")"),
                   "Distance")
    if (isTruthy(bouts_and_peaks_list)) {
      time_indices <- bouts_and_peaks_list()
      hlines <- input$min_distance
    } else {
      time_indices <- NULL
      hlines <- NULL
    }
    time_min <- ifelse(isTruthy(input$time_min),
                       input$time_min,
                       min(trajectory()$t))
    time_max <- ifelse(isTruthy(input$time_max),
                       input$time_max,
                       max(trajectory()$t))
    if (isTruthy(input$ymin) && isTruthy(input$ymax)) {
      ylim <- c(input$ymin, input$ymax)
    } else {
      ylim <- NULL
    }
    plot_diagnostics(trajectory(),
                     xlab = xlab, ylab = ylab,
                     time_indices = time_indices, hlines = hlines,
                     time_min = time_min, time_max = time_max,
                     ylim = ylim, legend_default = input$legend_default)
  })

  # Distribution plot output
  output$plot_distribution <- renderPlot({
    req(trajectory(), bouts_and_peaks_list())
    req(input$analyze_trajectory, input$type)
    validate(
      need(!isTruthy(input$breaks) || input$breaks > 0,
           "Suggested number of bins must be a positive number.")
    )
    bouts <- bouts_and_peaks_list()$bouts
    if (input$analyze_trajectory == "Angles") {
      observations <- calculate_angles(trajectory(), bouts = bouts)
      main <- "Distribution of Angles"
      xlab <- "Angle (degrees)"
    } else if (input$analyze_trajectory == "Bouts per Time Unit") {
      req(input$unit)
      validate(
        need(input$unit > 0,
             "User-specified time unit must be a positive number.")
      )
      observations <- calculate_bouts_per_time_unit(trajectory(), bouts,
                                                    unit = input$unit)
      main <- paste("Distribution of Bouts per", input$unit, input$unit_time)
      xlab <- paste("Bouts per", input$unit, input$unit_time)
    } else if (input$analyze_trajectory == "Distances") {
      observations <- calculate_distances(trajectory(), bouts = bouts)
      main <- "Distribution of Distances"
      xlab <- paste0("Distance (", input$unit_length, ")")
    }
    ylab <- ifelse(input$type == "ecdf", "Cumulative Density", "Density")
    breaks <- ifelse(isTruthy(input$breaks), input$breaks, "Sturges")
    if (isTruthy(input$xmin) && isTruthy(input$xmax)) {
      xlim <- c(input$xmin, input$xmax)
    } else {
      xlim <- range(observations)
    }
    plot_distribution(observations, input$type,
                      main = main, xlab = xlab, ylab = ylab,
                      breaks = breaks, xlim = xlim)
  })
}

shinyApp(ui, server)

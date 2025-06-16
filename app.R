# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    https://shiny.posit.co/
# #
# 
# library(shiny)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)



library(shiny)
library(ggplot2)
library(minpack.lm) # For nlsLM
library(shinyFiles)

# # van Genuchten model function
# van_genuchten <- function(theta_r, theta_s, alpha, n, psi) {
#   theta_r + (theta_s - theta_r) / ((1 + (abs(alpha * psi))^n)^(1 - 1/n))
# }
# 
# ui <- fluidPage(
#   titlePanel("Water Content vs Water Potential"),
#   
#   sidebarLayout(
#     sidebarPanel(
#      directoryInput('directory', label = 'Choose Directory with CSV Files'),
#       selectInput("model_type", "Choose Model to Fit:",
#                   choices = c("Van Genuchten" = "vg", "4th Order Polynomial" = "poly")),
#       actionButton("fit_model", "Fit Model")
#     ),
#     
#     mainPanel(
#       plotOutput("scatterPlot"),
#       verbatimTextOutput("modelSummary")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
#   
#   # Custom directory chooser
#   shinyDirChoose(input, 'directory', roots = volumes)
#   
#   dataset <- reactive({
#     req(input$directory)
#     dir_path <- parseDirPath(volumes, input$directory)
#     files <- list.files(dir_path, pattern = "*.csv", full.names = TRUE)
#     data_list <- lapply(files, read.csv)
#     do.call(rbind, data_list)
#   })
#   
#   model_result <- eventReactive(input$fit_model, {
#     data <- dataset()
#     req("water_content" %in% names(data), "water_potential" %in% names(data))
#     
#     if (input$model_type == "poly") {
#       model <- lm(water_content ~ poly(water_potential, 4), data = data)
#     } else {
#       start_vals <- list(theta_r = min(data$water_content),
#                          theta_s = max(data$water_content),
#                          alpha = 0.01,
#                          n = 1.2)
#       model <- tryCatch({
#         nlsLM(water_content ~ van_genuchten(theta_r, theta_s, alpha, n, water_potential),
#               data = data,
#               start = start_vals)
#       }, error = function(e) e)
#     }
#     model
#   })
#   
#   output$scatterPlot <- renderPlot({
#     data <- dataset()
#     req("water_content" %in% names(data), "water_potential" %in% names(data))
#     
#     p <- ggplot(data, aes(x = water_potential, y = water_content)) +
#       geom_point() +
#       theme_minimal()
#     
#     model <- model_result()
#     if (inherits(model, "lm") || inherits(model, "nls")) {
#       pred_data <- data.frame(water_potential = seq(min(data$water_potential),
#                                                     max(data$water_potential), length.out = 100))
#       pred_data$water_content <- predict(model, newdata = pred_data)
#       p <- p + geom_line(data = pred_data, aes(x = water_potential, y = water_content), color = "blue")
#     }
#     p
#   })
#   
#   output$modelSummary <- renderPrint({
#     model <- model_result()
#     if (inherits(model, "lm") || inherits(model, "nls")) {
#       summary(model)
#     } else {
#       cat("Model fitting failed:", model$message)
#     }
#   })
# }
# 
# 
# global <- reactiveValues(datapath = getwd())
# 
# dir <- reactive(input$dir)
# 
# output$dir <- renderText({
#   global$datapath
# })
# 
# shinyApp(ui = ui, server = server)


# 
# library(shiny)
# library(shinyFiles)
# 
# ui <- fluidPage( # Application title
#   mainPanel(
#     shinyDirButton("dir", "Input directory", "Upload"),
#     verbatimTextOutput("dir", placeholder = TRUE)  
#   ))
# 
# server <- function(input, output) {
#   shinyDirChoose(
#     input,
#     'dir',
#     roots = c(home = '~'),
#     filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
#   )
#   
# 
#   
#   observeEvent(ignoreNULL = TRUE,
#                eventExpr = {
#                  input$dir
#                },
#                handlerExpr = {
#                  if (!"path" %in% names(dir())) return()
#                  home <- normalizePath("~")
#                  global$datapath <-
#                    file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
#                })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)
# 



# library(shiny)
# library(ggplot2)
# library(minpack.lm) # For nlsLM
# library(shinyFiles)


library(shiny)
library(ggplot2)
library(minpack.lm) # For stable nonlinear least squares using nlsLM
library(shinyFiles)  # For directory selection UI
library(randomcoloR)

# site_colors <- c(
#   "INATWE" = "#66B8FF",
#   "Martell" = "#66B8FF",
#   "IN-Martell" = "#66B8FF",
#   "US-UMBd" = "#002D6B",
#   "US-UMd" = "#002D6B",
#   "US-UMB" = "#002D6B",
#   "US-Moz" = "#2270B5",
#   "US-MOZ" = "#2270B5",
#   "US-MMS" = "#72B9CC",
#   "US-VCP" = "#BCD2ED",
#   "US-Me2" = "#7176FF",
#   "US-Me6" = "#4542C6",
#   "US-HB2" = "#0055F5",
#   "US-HB3" = "#5A82CF", ##Find colors for
#   "US-Ha1" = "#339FB0", ##Find colors for
#   "US-HA1" = "#339FB0", ##Find colors for
#   "US-NR1" = "#7A57F6", ##Find colors for
#   
#   "US-CPK" = "#339FB0", ##Find colors for
#   "US-GLE" = "#339FB0", ##Find colors for
#   "US-" = "#7A57F6", ##Find colors for
#   
#   "US-Ha1" = "#339FB0", ##Find colors for
#   "US-HA1" = "#339FB0", ##Find colors for
#   "US-NR1" = "#7A57F6", ##Find colors for
#   
#   
#   "US-SRM" = "#fd8d3c",
#   "US-CDM" = "#fecc5c",
#   "US-WJS" = "#FF4434",
#   "US-SEG" = "#45140A",
#   "US-Bi2" = "#238443",
#   "US-VAR" = "darkolivegreen2",
#   "US-TON" = "#719C1D"
#   
# )


site_colors <- c()

# Define the van Genuchten model function
van_genuchten <- function(theta_r, theta_s, alpha, n, psi) {
  theta_r + (theta_s - theta_r) / ((1 + (abs(alpha * psi))^n)^(1 - 1/n))
}

# Define the UI layout
ui <- fluidPage(
  titlePanel("ReESH: The Regional Ecosystem Soil Hydraulics Network"),
  
  sidebarLayout(
    sidebarPanel(
      # Directory selection button using shinyFiles
      shinyDirButton('directory', label = 'Choose Directory with CSV Files', title = 'Select Folder'),
      
      # Text input to specify a pattern for file filtering
      textInput("file_pattern", "Enter pattern to match file names:", value = "SoilWaterRetentionCurves"),
      
      # Output directory selector
      shinyDirButton('output_dir', label = 'Choose Output Directory', title = 'Select Output Folder'),
      
      # Dynamic selectors for site and depth
      uiOutput("site_selector"),
      uiOutput("depth_selector"),
      
      # Dropdown to select model type
      selectInput("model_type", "Choose Model to Fit:",
                  choices = c("Van Genuchten" = "vg", "Logarithmic" = "log")),
      
      # Button to trigger model fitting
      actionButton("fit_model", "Fit Model"),
      actionButton("export_model", "Export Model Summary")
    ),
    
    mainPanel(
      # Output plot of data and fitted model
      plotOutput("scatterPlot"),
      
      # Display model summary
      verbatimTextOutput("modelSummary")
    )
  )
)


# Define the server logic
server <- function(input, output, session) {
  # Define volume roots for directory input
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  # Initialize directory chooser
  shinyDirChoose(input, 'directory', roots = volumes)
  shinyDirChoose(input, 'output_dir', roots = volumes)
  
  
  # Reactive expression to read and combine CSV files from selected directory
  raw_data <- reactive({
    req(input$directory)
    dir_path <- parseDirPath(volumes, input$directory)
    
    pattern <- input$file_pattern
    if (nzchar(pattern)) {
      files <- list.files(dir_path, pattern = pattern, full.names = TRUE, recursive = TRUE)
    } else {
      files <- list.files(dir_path, pattern = "SoilWaterRetentionCurves", full.names = TRUE, recursive = TRUE)
    }
    
    data_list <- lapply(files, read.csv)
    do.call(rbind, data_list)
  })
  
  # Update site selector based on available sites in the dataset
  output$site_selector <- renderUI({
    data <- raw_data()
    if ("Site" %in% names(data)) {
      selectInput("site_filter", "Select Site(s):",
                  choices = unique(data$Site),
                  selected = unique(data$Site),
                  multiple = TRUE)
      
    } else {
      helpText("No 'site' column found in data.")
    }
  })
  
  # Update depth selector based on available depths in the dataset
  output$depth_selector <- renderUI({
    data <- raw_data()
    if ("Depth_cm" %in% names(data)) {
      selectInput("depth_filter", "Select Depth(s):",
                  choices = unique(data$Depth_cm),
                  selected = unique(data$Depth_cm),
                  multiple = TRUE)
    } else {
      helpText("No 'depth' column found in data.")
    }
  })
  
  # Filtered dataset based on selected site(s) and depth(s)
  dataset <- reactive({
    data <- raw_data()
    if (!is.null(input$site_filter) && "Site" %in% names(data)) {
      data <- subset(data, Site %in% input$site_filter)
    }
    if (!is.null(input$depth_filter) && "Depth_cm" %in% names(data)) {
      data <- subset(data, Depth_cm %in% input$depth_filter)
    }
    data
  })
  
  
  
  # Reactive expression to read and combine CSV files from selected directory
  # dataset <- reactive({
  #   req(input$directory)  # Ensure directory is selected
  #   dir_path <- parseDirPath(volumes, input$directory)  # Get full path from input
  #   files <- list.files(dir_path, pattern = "SoilWaterRetentionCurves", full.names = TRUE)  # List all CSV files
  #   data_list <- lapply(files, read.csv)  # Read all CSV files into a list
  #   do.call(rbind, data_list)  # Combine all data frames into one
  # })
  # 
  # Reactive expression to fit the selected model
  model_result <- eventReactive(input$fit_model, {
    data <- dataset()  # Get combined dataset
    req("Site" %in% names(data), 
        "Sample_ID" %in% names(data), 
        "Plot" %in% names(data),
        "Depth_cm" %in% names(data), 
        "Vol_Water" %in% names(data), 
        "MPa_Abs" %in% names(data), 
        "Hypropfilter" %in% names(data),
        "Sampling_Date" %in% names(data), 
        "Bulk_Den_g_m3" %in% names(data))  # Ensure required columns exist
  
    
      
      split_data <- split(data, list(data$Site, data$Depth_cm))
      
      models <- lapply(split_data, function(df) {
        if (nrow(df) < 3) return(NULL)
        if (input$model_type == "log") {
          df <- subset(df, MPa_Abs > 0)
          tryCatch(lm(Vol_Water ~ log(MPa_Abs), data = df), 
                   error = function(e) NULL)
        } else {
          start_vals <- list(theta_r = min(df$Vol_Water), 
                             theta_s = max(df$Vol_Water), 
                             alpha = 0.01, n = 1.2)
          tryCatch(nlsLM(Vol_Water ~ van_genuchten(theta_r, theta_s, alpha, n, MPa_Abs), 
                         data = df, start = start_vals), 
                   error = function(e) NULL)
        }
      })
      models
    })
    
    
      
  #   
  #   if (input$model_type == "log") {
  #     # Fit 4th-order polynomial using linear regression
  #     model <- lm(Vol_Water ~ log(MPa_Abs), data = data)
  #   } else {
  #     # Initial parameter estimates for van Genuchten model
  #     start_vals <- list(
  #       theta_r = min(data$Vol_Water),
  #       theta_s = max(data$Vol_Water),
  #       alpha = 0.01,
  #       n = 1.2
  #     )
  #     
  #     # Try fitting the van Genuchten model using nlsLM
  #     model <- tryCatch({
  #       nlsLM(Vol_Water ~ van_genuchten(theta_r, theta_s, alpha, n, MPa_Abs),
  #             data = data,
  #             start = start_vals)
  #     }, error = function(e) e)  # Capture and return error if fitting fails
  #   }
  #   model  # Return fitted model or error
  # })
  
  # Render the plot of raw data and fitted model line
  output$scatterPlot <- renderPlot({
    data <- dataset()
    req("Site" %in% names(data), 
        "Sample_ID" %in% names(data), 
        "Plot" %in% names(data),
        "Depth_cm" %in% names(data), 
        "Vol_Water" %in% names(data), 
        "MPa_Abs" %in% names(data), 
        "Hypropfilter" %in% names(data),
        "Sampling_Date" %in% names(data), 
        "Bulk_Den_g_m3" %in% names(data))
    
    
    site_colors <- randomColor(length(unique(data$Site)), luminosity="light")
    
    # Base scatter plot of water content vs water potential
    p <- ggplot() +
      geom_point(data = data, aes(y = log10(MPa_Abs), x = Vol_Water, 
                                  shape = Site, 
                                  size = factor(Depth_cm)), 
                 size = 3) +
      # scale_color_manual(values = site_colors) +
      ylab("Soil Water Potential [-MPa]") + 
      xlab("Soil Water Content [%]") + 
      
      theme_minimal()
    
    # Add fitted model line if fitting was successful
    models <- model_result()
    if (!is.null(models) && inherits(models, "lm") || inherits(models, "nls")) {
      # Generate prediction data frame
      pred_data <- data.frame(MPa_Abs = seq(min(data$MPa_Abs),
                                                    max(data$MPa_Abs), length.out = 100))
      pred_data$Vol_Water <- predict(models, newdata = pred_data)  # Predict values
      
      # Add fitted model line to plot
      p <- p + geom_line(data = pred_data, aes(x = Vol_Water, y = log10(MPa_Abs), 
                                               color = ), color = "blue")
    }
    p  # Return plot
  })
  
  # Render model summary or error message
  # output$modelSummary <- renderPrint({
  #   model <- model_result()
  #   if (inherits(model, "lm") || inherits(model, "nls")) {
  #     summary(model)  # Show model summary
  #   } else {
  #     cat("Model fitting failed:", model$message)  # Show error message
  #   }
  # })
  # 
  
  output$modelSummary <- renderPrint({
    models <- model_result()
    summaries <- lapply(models, function(model) {
      if (!is.null(model) && (inherits(model, "lm") || inherits(model, "nls"))) {
        summary(model)
      } else {
        "Model fitting failed."
      }
    })
    names(summaries) <- names(models)
    summaries
  })
  
  # Save model summary to a file when export button is clicked
  # observeEvent(input$export_model, {
  #   req(input$output_dir)
  #   model <- model_result()
  #   output_path <- parseDirPath(volumes, input$output_dir)
  #   
  #   if (inherits(model, "lm") || inherits(model, "nls")) {
  #     summary_text <- capture.output(summary(model))
  #     file_name <- paste0("model_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  #     writeLines(summary_text, file.path(output_path, file_name))
  #     showNotification("Model summary exported successfully.", type = "message")
  #   } else {
  #     showNotification("Model export failed: Model fitting not successful.", type = "error")
  #   }
  # })
  # 
  
  observeEvent(input$export_model, {
    req(input$output_dir)
    models <- model_result()
    output_path <- parseDirPath(volumes, input$output_dir)
    
    for (key in names(models)) {
      model <- models[[key]]
      if (!is.null(model) && (inherits(model, "lm") || inherits(model, "nls"))) {
        summary_text <- capture.output(summary(model))
        file_name <- paste0("model_", gsub("\\.", "_", key), "_summary.txt")
        writeLines(summary_text, file.path(output_path, file_name))
      }
    }
    showNotification("Model summaries exported.", type = "message")
  })
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

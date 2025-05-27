library(shiny)
library(tidyverse)
library(httpuv)
library(shinylive)

# limit for upload
options(shiny.maxRequestSize = 30 * 1024^2) # 30 MB

keyword_dict <- list(
  "AI Terms" = c("AI", "artificial intelligence", "machine learning", "learning analytics", 
                 "intelligent tutoring", "deep learning", "ML", "Large language model", 
                 "LLM", "Chatbot", "chatgpt"),
  "Ethics Terms" = c("ethics", "bias", "equity", "privacy", "surveillance", 
                     "transparency", "responsibility", "agency", "data ownership")
)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .form-control[multiple] {
      height: 200px !important;
      overflow-y: auto !important;
    }
  "))),
  titlePanel("ISLS Literature Review Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload ISLS CSV File", accept = ".csv"),
      selectInput(
        inputId = "keywords",
        label = "Select Keywords",
        choices = sort(unique(c(
          "AI", "artificial intelligence", "machine learning", "learning analytics", 
          "intelligent tutoring", "deep learning", "ML", "Large language model", 
          "LLM", "Chatbot", "chatgpt",
          "ethics", "bias", "equity", "privacy", "surveillance", 
          "transparency", "responsibility", "agency", "data ownership"
        ))),
        selected = NULL,
        multiple = TRUE,
        selectize = TRUE
      ),
      sliderInput("yearRange", "Filter by year", min = 2000, max = 2030, value = c(2020, 2025)),
      uiOutput("typeUI"),
      actionButton("run", "Search"),
      downloadButton("downloadData", "Download Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Filtered Results", dataTableOutput("filteredTable")),
        tabPanel("Raw Data", dataTableOutput("rawData"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  
  raw_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  observeEvent(raw_data(), {
    df <- raw_data()
    updateSliderInput(session, "yearRange", 
                      min = min(df$year, na.rm = TRUE),
                      max = max(df$year, na.rm = TRUE),
                      value = c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE)))
  })
  
output$typeUI <- renderUI({
    df <- raw_data()
    selectInput(
      inputId = "paperType",
      label = "Type of Paper",
      choices = unique(df$type),
      selected = unique(df$type),
      multiple = TRUE,
      selectize = FALSE  # Important: turn off selectize to get native select box with scroll
    )
  })
  
  
  filtered_data <- eventReactive(input$run, {
    df <- raw_data()
    
    # Filter by year and paper type
    df <- df %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2],
             type %in% input$paperType)
    
    keywords <- input$keywords
    
    if (length(keywords) == 0) return(df[0, ])
    
    # Add tags
    df$Tags <- ""
    for (kw in keywords) {
      matches <- str_detect(df$Title, regex(kw, ignore_case = TRUE)) |
        str_detect(df$Abstract, regex(kw, ignore_case = TRUE))
      df$Tags[matches] <- paste(df$Tags[matches], kw, sep = ", ")
    }
    
    df$Tags <- str_trim(str_replace_all(df$Tags, "^,\\s*", ""))
    df <- df %>% filter(Tags != "")
    return(df)
  })
  
  output$filteredTable <- renderDataTable({
    filtered_data()
  })
  
  output$rawData <- renderDataTable({
    raw_data()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("filtered_isls_results.csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)

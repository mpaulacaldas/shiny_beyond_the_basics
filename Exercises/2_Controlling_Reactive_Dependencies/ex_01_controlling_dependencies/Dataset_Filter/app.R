library(dplyr)
library(readr)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Dataset Filter"),
  dashboardSidebar(
    fileInput("file", "Upload a CSV file"),
    textInput("filter", "Enter a valid dplyr filter", placeholder = "e.g. mpg > 30"),
    actionButton("applyFilter", "Apply filter"),
    actionButton("saveData", "Save filtered data")

  ),
  dashboardBody(
    h3("Filtered dataset"),
    tableOutput("filteredTable")
  )
)

server <- function(input, output) {
  
  # Reactive expression to save processing
  dataset <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  getFilteredData <- reactive({
    # Here I use the input + isolate() approach. The solution proposed by
    # instructor (and the one that I recommend) uses eventReactive().
    input$applyFilter
    isolatedFilter <- isolate(input$filter)
    # Don't filter anything if no filter is specified.
    if (!isTruthy(isolatedFilter)) filt <- "TRUE" else filt <- isolatedFilter
    # Apply the filter
    filter_(dataset(), filt)
  })
  
  observeEvent(input$saveData, {
    write_csv(getFilteredData(), "filteredData.csv")
  })
  
  output$filteredTable <- renderTable({
    getFilteredData()
  })

}
shinyApp(ui, server)

library(shiny)
library(dplyr)
ui <- fluidPage(
  h3("Dataset Summary"),
  fileInput("file", label = "Upload a CSV"),
  plotOutput("pairs"),
  verbatimTextOutput("summary")
)
server <- function(input, output) {
  
  getData <- reactive({
    
    # Better to put the restrictions in the reactive because of DRY
    validate(
      need(input$file$datapath, "Please upload a CSV file.")
    )
    
    if (isTruthy(input$file$datapath)) {
      validate(
        need(grepl(".*\\.csv$", input$file$datapath), "File must end in '.csv'")
      )
    }
    
    read.csv(input$file$datapath)
    
  })
  
  output$pairs <- renderPlot({
    pairs(select_if(getData(), is.numeric))
  })
  
  output$summary <- renderPrint({
    req(input$file$datapath)
    summary(getData())
  })
}
shinyApp(ui, server)

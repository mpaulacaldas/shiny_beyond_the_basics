library(shiny)

# Controlling Reactive Dependencies 

# Multiple inputs, one output
ui_01 <- fluidPage(
  textInput("title", "Title:", value = "Random normals"),
  selectInput("colour", "Select a colour", 
              choices = c("orange", "blue", "green")),
  numericInput("sampleSize", "Select size of data:", 
               min = 10, max = 500, value = 100),
  plotOutput("histogram")
)
server_01a <- function(input, output){
  output$histogram <- renderPlot(
    hist(x = rnorm(input$sampleSize),
         main = input$title,
         col = input$colour)
    # Values are recalculated when we change colors
  )
}
server_01b <- function(input, output){
  output$histogram <- renderPlot(
    hist(x = rnorm(input$sampleSize),
         main = isolate(input$title),
         col = isolate(input$colour))
    # We break the dependency to the title and colour. The plot will be re-run
    # only when the sample size is changed. The color and title will be updated
    # only then.
  )
}

# Protip: Google "Shiny reactive log". There is a way of getting the reactive
# graph of an app (very messy for now, being improved by RStudio)

shinyApp(ui_01, server_01a) # before isolate()
shinyApp(ui_01, server_01b) # after isolate()

# --- Preventing Dependencies with isolate -------------------------------------
ui_02 <- fluidPage(
  textInput("title", "Title:", value = "Random normals"),
  selectInput("colour", "Select a colour", 
              choices = c("orange", "blue", "green")),
  numericInput("sampleSize", "Select size of data:", 
               min = 10, max = 500, value = 100),
  plotOutput("histogram")
)
server_02 <- function(input, output){
  output$histogram <- renderPlot(
    hist(x = rnorm(input$sampleSize),
         main = isolate(input$title),
         col = isolate(input$colour))
  )
}
shinyApp(ui_02, server_02)

# --- Defining Our Own Dependencies --------------------------------------------

# --- Defining Dependencies with eventReactive
ui_03 <- fluidPage(
  sliderInput("n", "Number of samples", 
              min = 25, max = 500, step = 25, value = 100),
  radioButtons("dist", "Distribution type:",
               c("Normal" = "norm",
                 "Uniform" = "unif",
                 "Exponential" = "exp")),
  actionButton("go", "Go"),
  plotOutput("distPlot")
)
server_03 <- function(input, output) {
  
  getDistData <- reactive({
    # To get the same behaviour as eventReactive(input$go, {...}), you can call
    # the input and use isolate()
    input$go
    isolate({
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     exp = rexp,
                     rnorm)
      dist(input$n)
    })

  })
  
  output$distPlot <- renderPlot({
    hist(getDistData(), col = "orange")
  })
}
shinyApp(ui_03, server_03)

# ------------------------------------------------------------------------------
# Supplementary note: using eventReactive is the same as reactive + isolate
# E.g. this app has the same dependency graph as that above.

ui_04 <- fluidPage(
  sliderInput("n", "Number of samples", 
              min = 25, max = 500, step = 25, value = 100),
  radioButtons("dist", "Distribution type:",
               c("Normal" = "norm",
                 "Uniform" = "unif",
                 "Exponential" = "exp")),
  actionButton("go", "Go"),
  plotOutput("distPlot")
)
server_04 <- function(input, output) {
  
  getDistData <- eventReactive(input$go, {
      dist <- switch(input$dist,
                     norm = rnorm,
                     unif = runif,
                     exp = rexp,
                     rnorm)
      dist(input$n)
  })
  # Conclusion: You don't need to use the eventReactive() in your apps. You
  # could get the same behaviour using reactive() + isolate(). This last
  # approach could however get a bit verbose and difficult to understand.
  
  output$distPlot <- renderPlot({
    hist(getDistData(), col = "orange")
  })
}
shinyApp(ui_04, server_04)


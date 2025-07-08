#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)

# UI
ui <- fluidPage(
  
  titlePanel("Interactive mtcars Plot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X-axis Variable:",
                  choices = names(mtcars), selected = "wt"),
      
      selectInput("yvar", "Select Y-axis Variable:",
                  choices = names(mtcars), selected = "mpg"),
      
      selectInput("color", "Color by (factor):",
                  choices = c("None", "cyl", "gear", "carb"), selected = "None"),
      
      checkboxInput("logx", "Log-transform X-axis", value = FALSE),
      checkboxInput("logy", "Log-transform Y-axis", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    
    data <- mtcars
    
    # Axis transformations
    x <- data[[input$xvar]]
    y <- data[[input$yvar]]
    if (input$logx) x <- log(x)
    if (input$logy) y <- log(y)
    
    # Plot
    if (input$color == "None") {
      plot(x, y,
           xlab = input$xvar,
           ylab = input$yvar,
           main = "Scatter Plot of mtcars",
           pch = 19, col = "steelblue")
    } else {
      factor_col <- as.factor(data[[input$color]])
      palette <- rainbow(length(unique(factor_col)))
      plot(x, y,
           col = palette[factor_col],
           pch = 19,
           xlab = input$xvar,
           ylab = input$yvar,
           main = paste("Scatter Plot Colored by", input$color))
      legend("topright", legend = levels(factor_col), 
             col = palette, pch = 19, title = input$color)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

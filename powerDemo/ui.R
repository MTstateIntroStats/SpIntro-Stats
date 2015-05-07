##  ui.R for Power demo

library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Power Demo"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    sliderInput("n", "SampleSize:", 
                min=4, max=50, value=10),
    
    # std deviation
    sliderInput("sd", "Standard Deviation:", 
                min = 0.4, max = 3.2, value = 1.0, step= 0.2),
    
    # 
    sliderInput("altMean", "Alternative Mean:",
                min = 0, max = 8, value = 2, step=.1),
    
    
    # alpha level
    sliderInput("alpha", "Significance Level:", 
                min = 0.01, max = .15, value = .04, step= 0.01)
    
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    plotOutput("powerPlot"),
    tableOutput("values")
  )
))


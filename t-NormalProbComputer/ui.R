library(shiny)
#library(dichromat)
#library(scales)
#library(ggplot2)


shinyUI(pageWithSidebar(
  
  headerPanel("Normal and t Probability Look Up"),
  
  sidebarPanel(
    #helpText("Either enter  "),
    textInput('z.txt', label='Standardized Value: ', value=" "),
    #helpText("Or "),
    textInput('prob.txt', 'or  Probability: ', " "),
    selectInput("area", "To go into which area? ", 
                c("Lower","Upper","Extremes","Center"), NA),
    selectInput("dist", "Distribution: ",c("Normal","t"), "Normal"),
    conditionalPanel( 
      condition = "input.dist =='t'", 
      numericInput("df", "t degrees of freedom?", 10 )
    )

  ),
  
  mainPanel(
    plotOutput('plot')
  )
))
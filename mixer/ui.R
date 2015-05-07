#thanks to https://www.trestletechnology.net/2012/12/reconstruct-gene-networks/

require(shiny)

reactiveMix <- function (outputId) 
{
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}

functionList <-  c( "Count in Group 1","Count in Group 2", "Max Run Length","Matches")
 
  ## what can we save?

shinyUI(pageWithSidebar(    
  headerPanel(HTML("Mix It Up")),
  
  sidebarPanel(
    numericInput("nCat", "Number of categories:", 3, min = 2, max = 10),
    textInput("categories","Category names (separate with commas):", "A, B, C"),
    textInput("counts", "Numbers of each (separate with commas):", "5, 3, 2"),
    selectInput("replace","Replace each draw?",c("yes","no"),1) , 

    HTML("<hr>") ,
    selectInput("stopRule","Stop after", c("Fixed number of draws", "One draw in 1st category", "One of each type")),
    conditionalPanel(
       condition = "input.stopRule =='Fixed number of draws'", 
        numericInput("nDraws", "Stop after how many draws?", 3 )
      ),
    
    selectInput("reps", "Number of Trials:", c("1","10","100","1000"),1),
    ## helpText("Start with 1 for an animation"),
     HTML("<hr>") ,
    conditionalPanel(
       condition = "input.reps != 1 && input.stopRule =='Fixed number of draws'" ,
        selectInput("fn","Store what result?", functionList)
      ),
    conditionalPanel(
       condition = "input.stopRule !='Fixed number of draws' && input.reps != 1", 
        helpText("Display shows number of draws needed" )
      ),
    actionButton("runButton", "Run")  
    ),  
  
  mainPanel(
    includeHTML("mix.js"),
    reactiveMix(outputId = "mixPlot"),
     conditionalPanel(
        condition = "input.reps == 1",
     verbatimTextOutput("summary") ),
     HTML("<hr>") ,
     conditionalPanel(
        condition = "input.reps != 1",
           plotOutput("histogrm")
       ),
     conditionalPanel(
        condition = "input.reps != 1",
           verbatimTextOutput("summry2")
       ),
    conditionalPanel(
      condition = "input.reps != 1",
      tableOutput("summry3")
    )

  )

))



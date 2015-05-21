
##thanks to https://www.trestletechnology.net/2012/12/reconstruct-gene-networks/

reactiveSpin <- function (outputId) {
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}

  functionList <- c( "Count in Group1","Count in Group 2", "Max Run Length")
 

  shinyUI(pageWithSidebar(    
    headerPanel(HTML("Spinner")),
  
    sidebarPanel(
      numericInput("nCat", "Number of categories:", 3, min = 2, max = 10),
      textInput("categories","Category names (separate with commas):", "A, B, C"),
      textInput("probs", "Percentages (separate with commas):", "50, 30, 20"),
      helpText("Relative size is the key."),
        hr() ,
      selectInput("stopRule","Stop after", c("Fixed number of spins",
                   "One spin in 1st category", "One of each type")),
      conditionalPanel( 
        condition = "input.stopRule =='Fixed number of spins'", 
        numericInput("nDraws", "Stop after how many spins?", 5 )
      ),
    
      selectInput("reps", "Number of Trials:", c("1","10","100","1000"),1),
      ## helpText("Start with 1 for an animation"),
       hr() ,
      conditionalPanel(
        condition = "input.reps != 1 && input.stopRule =='Fixed number of spins'" ,
        selectInput("fn","Store what result?", functionList)
      ),
      conditionalPanel(
        condition = "input.stopRule !='Fixed number of spins' && input.reps != 1", 
        helpText("Display shows number of spins needed" )
      ),
      actionButton("runButton", "Run")  
    ),  
       ## end of inputs ---------------------------------------------
                   
  mainPanel(
     includeHTML("spin.js"),
     reactiveSpin(outputId = "spinPlot"),
     conditionalPanel(
        condition = "input.reps == 1",
     verbatimTextOutput("summary") ),
     hr() ,
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
    )   ## end main panel
  )
 )


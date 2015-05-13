library(markdown)

shinyUI(
  navbarPage("Variable Type:",
    ####   One Categorical  -----------------------------------
    navbarMenu("One Categorical",
       tabPanel("Descriptives",
          verbatimTextOutput("Describe 1 Cat")
       ),
       tabPanel("Test",
         verbatimTextOutput("Test 1 Proportion")
       ),
       tabPanel("Estimate",
         verbatimTextOutput("Estimate 1Proportions")
       ),
       tabPanel("Normal Distribution",
         verbatimTextOutput("Normal Approx")
    ),
    ####   One Quantitative  -----------------------------------
    navbarMenu("One Quantitative",
      tabPanel("Descriptives",
        verbatimTextOutput("Describe 1 Quantitative")
      ),
      tabPanel("Test",
        verbatimTextOutput("Test 1 mean")
      ),
      tabPanel("Estimate",
        verbatimTextOutput("Estimate 1 mean")
      ),
      tabPanel("Bootstrap Demo",
        verbatimTextOutput("BootStrap Demo")
      ),
      tabPanel("Normal Distribution",
        verbatimTextOutput("t distribution")
      )                         
    ),
    
    ####   Two Categorical  -----------------------------------
    navbarMenu("Two Cat.",
      tabPanel("Descriptives",
        verbatimTextOutput("Describe 2 Cat")
      ),
      tabPanel("Test",
        verbatimTextOutput("Test Equality of 2 Proportions")
      ),
      tabPanel("Estimate",
        verbatimTextOutput("Estimate Difference in Proportions")
      ),
      tabPanel("Normal Distribution",
       verbatimTextOutput("Normal Approx")
      )
  ),
  ####   Two Quantitative -----------------------------------
  navbarMenu("Two Quant.",
    tabPanel("Descriptives",
      verbatimTextOutput("Describe 2 Quant")
    ),
    tabPanel("Test",
      verbatimTextOutput("Test Slope/Correlation")
    ),
    tabPanel("Estimate Slope/Correlation",
      verbatimTextOutput("Estimate Difference in Proportions")
    ),
    tabPanel("t- Distribution",
      verbatimTextOutput("Normal Approx")
    )
  ),
  ####   One of Each  -----------------------------------
  navbarMenu("One of Each",
    tabPanel("Descriptives",
       verbatimTextOutput("Describe")
    ),
    tabPanel("Test",
       verbatimTextOutput("Test Equality of 2 Means")
    ),
    tabPanel("Estimate",
       verbatimTextOutput("Estimate Difference in Means")
    ),
    tabPanel("t Distribution",
       verbatimTextOutput("t Approx")
    )
  ),
  ####   Theory  -----------------------------------
  navbarMenu("Theoretical Tools",
    tabPanel("Probabilities"),
    tabPanel("Power")
  )       
 )
)
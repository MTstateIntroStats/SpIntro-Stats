library(markdown)
library(shiny)
## library(rhandsontable)

  ##  User should choose a variable type, then 
    ## load Data  and
    ## describe, test, or estimate
  ## https://github.com/jrowen/rhandsontable/blob/master/inst/examples/shiny.R

shinyUI(navbarPage("Variable Type:", id="top-nav", collapsible=TRUE,
                   theme = shinytheme("spacelab"),

    ####   One Categorical  ---------------------------------------------------
    navbarMenu("One Categ.",
       tabPanel("Descriptives",
   ##             rHandsontableOutput("dataTable"),
                h6("Table the categories")
       ),
       tabPanel("Test",
         h6("Test 1 Proportion")
       ),
       tabPanel("Estimate",
         h6("Estimate 1 Proportions")
        ),
        tabPanel("Normal Distribution" ,
          h6("Normal Approx")
       )
    ),
    
    ####   One Quantitative  -------------------------------------------------
    navbarMenu("One Quant.",
      tabPanel("Descriptives",
        h6("Describe 1 Quantitative")
      ),
      tabPanel("Test",
        h6("Test 1 mean")
      ),
      tabPanel("Estimate",
        h6("Estimate 1 mean")
      ),
      tabPanel("Bootstrap Demo",
        h6("BootStrap Demo")
      ),
      tabPanel("t Distribution",
        h6("t distribution")
      )                         
    ),
    
    ####   Two Categorical  -------------------------------------------------
    navbarMenu("Two Cat.",
      tabPanel("Descriptives",
        h6("Describe 2 Cat")
      ),
      tabPanel("Test",
        h6("Test Equality of 2 Proportions")
      ),
      tabPanel("Estimate",
        h6("Estimate Difference in Proportions")
      ),
      tabPanel("Normal Distribution",
       h6("Normal Approx")
      )
    ),
    ####   Two Quantitative -------------------------------------------------
    navbarMenu("Two Quant.",
      tabPanel("Descriptives",
        h6("Describe 2 Quant")
      ),
      tabPanel("Test",
        h6("Test Slope/Correlation")
      ),
      tabPanel("Estimate Slope/Correlation",
        h6("Estimate Slope and Correlation")
#       ),
#       tabPanel("t- Distribution",
#         h6("Normal Approx")
      )
    ),
    ####   1 categorical & 1 quantitative  -----------------------------------
    navbarMenu("One of Each",
      tabPanel("Descriptives",
        h6("Describe")
      ),
      tabPanel("Test",
        h6("Test Equality of 2 Means")
      ),
      tabPanel("Estimate",
        h6("Estimate Difference in Means")
      ),
      tabPanel("t Distribution",
        h6("t Approx")
      )
    ),
    ####   ----  Other Tools  ------------------------------------------------
    navbarMenu("Other Tools",
      tabPanel("Probabilities",
             ## ui.r from probability finder
           titlePanel("Normal and t Probability Look Up"),
               column(4, inputPanel(
               #helpText("Either enter  "),
               textInput('prb_z_txt', label='Standardized Value: ', value=" "),
               #helpText("Or "),
               textInput('prb_prob_txt', 'or  Probability: ', " "),
               selectInput("prb_area", "To go into which area? ", 
                           c("Lower","Upper","Extremes","Center"), NA),
               selectInput("prb_dist", "Distribution: ",c("Normal","t"), "Normal"),
               conditionalPanel( 
                 condition = "prb_input.dist =='t'", 
                 numericInput("prb_df", "t degrees of freedom?", 10 )
               )
               )),   
           ##  sidebarPanel appears, but no plot.
           ##  dist'n shows 'normal' by default, but conditional box for t df appears.
             column(8,
               plotOutput('probPlot')
             )
           ),
    tabPanel("Power",
      ## ui.r from power app
        #  Application title
        titlePanel("Power Demo"),
        # Sidebar with sliders that demonstrate various available options
        fluidRow(
        column(4, inputPanel(
          sliderInput("pwr_n", "SampleSize:", 
                      min=4, max=50, value=10),
          # std deviation
          sliderInput("pwr_sd", "Standard Deviation:", 
                      min = 0.4, max = 3.2, value = 1.0, step= 0.2),
          # 
          sliderInput("pwr_altMean", "Alternative Mean:",
                      min = 0, max = 8, value = 2, step=.1),
          # alpha level
          sliderInput("pwr_alpha", "Significance Level:", 
                      min = 0.01, max = .15, value = .04, step= 0.01)    
        )),
        column(8, plotOutput("powerPlot"))),
      # Show a table summarizing the values entered
        fluidRow(column(8, offset = 4, tableOutput("values")))
    )
  )
)
)
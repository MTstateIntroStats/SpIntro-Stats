library(markdown)
library(shiny) 
library(shinythemes)
library(knitr)
## library(rhandsontable)

  ##  User should choose a variable type, then 
    ## load Data  and
    ## describe, test, or estimate

  ## Load data using:
  ## https://github.com/jrowen/rhandsontable/blob/master/inst/examples/shiny.R

shinyUI(navbarPage("Variable Type:", id="top-nav", collapsible=TRUE,
                   theme = shinytheme("spacelab"),

    ## empty tabPanel to avoid printing "tab-pane active"               
    tabPanel(""), 
    
    ####   One Categorical  ---------------------------------------------------
    navbarMenu("One Categ.",        
       tabPanel("Descriptives", value="1catDescrp",
                column(4, inputPanel(
                  textInput('cat1_n', label='Total Number: ', value="0"),
                  textInput('cat1_y', label='Number of Successes: ', value="0")
                )),
                column(4, 
                       plotOutput('cat1Plot'))
       ),
       tabPanel("Test", value="1catTest",
         h6("Test 1 Proportion")
       ),
       tabPanel("Estimate", value="1catEstimate",
         h6("Estimate 1 Proportions")
        ),
        tabPanel("Normal Distribution" , value="1catNormal",
                 titlePanel("Normal Probabilities"),
                 column(4, inputPanel(
                   #helpText("Either enter  "),
                   textInput('prb_z_txt', label='Standardized Value: ', value=" "),
                   #helpText("Or "),
                   textInput('prb_prob_txt', 'or  Probability: ', " "),
                   selectInput("prb_area", "To go into which area? ", 
                               c("Lower","Upper","Extremes","Center"), NA),
                   selectInput("prb_dist", "Distribution: ",c("Normal"), "Normal")
#                    conditionalPanel( 
#                      condition = "input.prb_dist =='t'", 
#                      numericInput("prb_df", "t degrees of freedom?", 10 ))
                 )),   
                 column(8,
                        plotOutput('probPlot1')
                 )  #,
                 #h6("Normal Approx")
       )
    ),
    
    ####   One Quantitative  -------------------------------------------------
    navbarMenu("One Quant.",
      tabPanel("Descriptives", value="1quantDescrp",
        h6("Describe 1 Quantitative")
      ),
      tabPanel("Test", value="1quantTest",
        h6("Test 1 mean")
      ),
      tabPanel("Estimate", value="1quantEstimate",
        h6("Estimate 1 mean")
      ),
      tabPanel("Bootstrap Demo", value="1quantBoot",
        a(href="BootDemo.html", "Demo of Bootstrap Process")
      ),
      tabPanel("t Distribution",  value="1quantT",
               titlePanel("t Probabilities"),
               column(4, inputPanel(
                 #helpText("Either enter  "),
                 textInput('prb_z_txt', label='Standardized Value: ', value=" "),
                 #helpText("Or "),
                 textInput('prb_prob_txt', 'or  Probability: ', " "),
                 selectInput("prb_area", "To go into which area? ", 
                             c("Lower","Upper","Extremes","Center"), NA),
                 selectInput("prb_dist", "Distribution: ",c("t"), "t"),
                 numericInput("prb_df", "t degrees of freedom?", 10 )
               )),   
               ##  dist'n shows 'normal' by default, but conditional box for t df appears.
               column(8,
                      plotOutput('probPlot3')
               )  #,
               #  h6("t distribution")
      )                         
    ),
    
    ####   Two Categorical  -------------------------------------------------
    navbarMenu("Two Categ.",
      tabPanel("Descriptives", value="2catDescrp",
               column(4, inputPanel(
                 textInput('cat2_n1', label='Group 1 Total: ', value="0"),
                 textInput('cat2_y1', label='Group 1 Successes: ', value="0"),
                 textInput('cat2_n2', label='Group 2 Total: ', value="0"),
                 textInput('cat2_y2', label='Group 2 Successes: ', value="0")
               )),
               column(4, 
                      plotOutput('cat2Plot')),
               column(4,
                      h6("Proportions:"),
                      tableOutput('cat2Summary'))
      ),
      tabPanel("Test", value="2catTest",
        h6("Test Equality of 2 Proportions")
      ),
      tabPanel("Estimate", value="2catEstimate",
        h6("Estimate Difference in Proportions")
      ),
      tabPanel("Normal Distribution", value="2catNormal",
               titlePanel("Normal Probabilities"),
               column(4, inputPanel(
                 #helpText("Either enter  "),
                 textInput('prb_z_txt', label='Standardized Value: ', value=" "),
                 #helpText("Or "),
                 textInput('prb_prob_txt', 'or  Probability: ', " "),
                 selectInput("prb_area", "To go into which area? ", 
                             c("Lower","Upper","Extremes","Center"), NA),
                 selectInput("prb_dist", "Distribution: ",c("Normal"), "Normal"),
                 conditionalPanel( 
                   condition = "input.prb_dist =='t'", 
                   numericInput("prb_df", "t degrees of freedom?", 10 ))
               )),   
               column(8,
                      plotOutput('probPlot4')
               )  #,
            ##  h6("Normal Approx")
      )
    ),
    ####   Two Quantitative -------------------------------------------------
    navbarMenu("Two Quant.",
      tabPanel("Descriptives", value="2quantDescrp",
        h6("Describe 2 Quant")
      ),
      tabPanel("Test", value="2quantTest",
        h6("Test Slope/Correlation")
      ),
      tabPanel("Estimate Slope/Correlation", value="2quantEstimate",
        h6("Estimate Slope and Correlation")
#       ),
#       tabPanel("t- Distribution", value="2quantT",
#         h6("Normal Approx")
      )
    ),
    ####   1 categorical & 1 quantitative  -----------------------------------
    navbarMenu("One of Each",
      tabPanel("Descriptives", value="1cat1quantDescrp",
        h6("Describe 2 means")
      ),
      tabPanel("Test", value="1cat1quantTest",
        h6("Test Equality of 2 Means")
      ),
      tabPanel("Estimate", value="1cat1quantEstimate",
        h6("Estimate Difference in Means")
      ),
      tabPanel("t Distribution", value="1cat1quantT",
               titlePanel("t Probabilities"),
               column(4, inputPanel(
                 #helpText("Either enter  "),
                 textInput('prb_z_txt', label='Standardized Value: ', value=" "),
                 #helpText("Or "),
                 textInput('prb_prob_txt', 'or  Probability: ', " "),
                 selectInput("prb_area", "To go into which area? ", 
                             c("Lower","Upper","Extremes","Center"), NA),
                 selectInput("prb_dist", "Distribution: ",c("t"), "t"),
                 numericInput("prb_df", "t degrees of freedom?", 10 )
               )),   
               ##  dist'n shows 'normal' by default, but conditional box for t df appears.
               column(8,
                      plotOutput('probPlot2')
                )  #,
#               h6("t test")
      )
    ),
    ####   ----  Other Tools  ------------------------------------------------
    navbarMenu("Other Tools",
      tabPanel("Probabilities", value = "probabilities",
             ## ui.r from probability finder
           titlePanel("Normal and t Probability Look Up"),
           column(4, inputPanel(
             #helpText("Either enter  "),
             textInput('prb_z_txt', label='Standardized Value: ', value=" "),
             #helpText("Or "),
             textInput('prb_prob_txt', 'or  Probability: ', " "),
             selectInput("prb_area", "To go into which area? ", 
                         c("Lower","Upper","Extremes","Center"), NA),
             selectInput("prb_dist", "Distribution: ",c("Normal","t"), "t"),
            # conditionalPanel( 
            #   condition = "input.prb_dist =='t'", 
               numericInput("prb_df", "t degrees of freedom?", 10 )
             #)
           )),   
           ##  dist'n shows 'normal' by default, but conditional box for t df appears.
           column(8,
                  plotOutput('probPlot')
           )
      ),
    tabPanel("Power", value = "Power",
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
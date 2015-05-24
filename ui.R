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

shinyUI(navbarPage("Intro Stat Apps", id="top-nav", collapsible=TRUE,
                   theme = shinytheme("spacelab"),
    ## EXTRA COMMENT
    ## empty tabPanel to avoid printing "tab-pane active"               
    tabPanel(""), 
    
    ####   One Categorical  -----------------------------------------------------   1 cat
    navbarMenu("One Categ.",  
       tabPanel("Enter /Describe Data", label="1catDataEntry",       
                h5(textOutput('cat1DataIn')),
                ##  Input counts and labels         
                br(),    
                fluidRow(
                  column(5,  div( label = "cat1Input", height = "300px",
                                  tags$label('Category 1: ', 
                                             tags$input(name='cat1_name1', type='text', value='Success', size='10')),
                                  tags$label('Count: ',
                                             tags$input(name='cat1_n1', type='text', value='0', size='5')),
                                  br(),
                                  tags$label('Category 2: ', 
                                             tags$input(name='cat1_name2', type='text', value='Failure', size='10')),
                                  tags$label('Count: ', 
                                             tags$input(name='cat1_n2', type='text', value='0', size='5')),
                                  HTML("&nbsp; &nbsp;"),
                                  actionButton("cat1_submitButton", "Submit")
                                  )),
                  column(3, plotOutput('cat1Plot',width="80%")),
                  column(3, tableOutput("cat1Summary"))       
                  )
                ),
       tabPanel("Test", value="cat1_Test",
                h6("Test 1 Proportion - under construction")
                ),
       tabPanel("Estimate", value="cat1_Estimate",
                h6("Estimate 1 Proportion - under construction")
                ),
        tabPanel("Normal Distribution" , value="cat1_Normal",
                 titlePanel("Normal Probabilities"),
                 column(3, inputPanel(
                   #helpText("Either enter  "),
                   textInput('cat1_z_txt', label='Standardized Value: ', value=" "),
                   #helpText("Or "),
                   textInput('cat1_prob_txt', 'or  Probability: ', " "),
                   selectInput("cat1_area", "Which area? ", 
                               c("Lower","Upper","Extremes","Center"), NA),
                   selectInput("cat1_dist", "Distribution: ",c("Normal"), "Normal")
                 )),   
                 column(9,
                        plotOutput('normalProbPlot1')
                 )  #,
                 #h6("Normal Approx")
       )
    ),
    
    ####   One Quantitative  ----------------------------------------------------  1 Quant
    navbarMenu("One Quant.",
      tabPanel("Enter /Describe Data", value="1quantDataEntry",
               h5(textOutput('q1DataIn')),
               ## copied from: http://shiny.rstudio.com/gallery/file-upload.html
               h6("testing version"),     
               ## Choices: 
                 ##  preloaded data  - built into our own package?
                 ##  read local csv file
                 ##  open empty table to copy or type in data.
                 ##
               sidebarPanel(
                 fileInput('q1_file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 tags$hr(),
                 checkboxInput('q1_header', 'Check if first row contains column names', TRUE),
                 radioButtons('q1_sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('q1_quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
               ),
               mainPanel(
                 tableOutput('q1_dataDF')
               )
#         sidebarPanel(
#           fileInput('file1', 'Choose CSV File',
#                   accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
#           tags$hr(),
#           checkboxInput('q1_header', 'Header', TRUE),
#           radioButtons('q1_sep', 'Separator',
#                      c(Comma=',',
#                        Semicolon=';',
#                        Tab='\t'),
#                      'Comma'),
#           radioButtons('q1_quote', 'Quote',
#                      c(None='',
#                        'Double Quote'='"',
#                        'Single Quote'="'"),
#                      'Double Quote')
#         ),
#         mainPanel(
#           tableOutput('quant1_dataDF')
#         )
      ),
      tabPanel("Test", value="1quantTest",
        h6("Test 1 mean - under construction")
      ),
      tabPanel("Estimate", value="1quantEstimate",
        h6("Estimate 1 mean - under construction")
      ),
      tabPanel("Bootstrap Demo", value="1quantBoot",
        a(href="BootDemo.html", "Click here for a demo of Bootstrap Process")
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
    
    ##  br(),  didn't work inside the navBar

    ####   Two Categorical  -------------------------------------------------  --  2 cat
    navbarMenu("Two Categ.",
      tabPanel("Enter /Describe Data", value="2catDataEntry",
               h5(textOutput('cat2DataIn')),
               br(),
               fluidRow( 
                 column(5,   ##  Inputs
                        fluidRow( 
                          column(4,
                               div(
                                   actionButton("cat2_submitButton", "Submit", height = 15),
                                   br(),
                                   tags$input(name='cat2_name1', type='text', value='Success', size='10'),
                                   br(),
                                   tags$input(name='cat2_name2', type='text', value='Failure', size='10')
                               )
                           ),
                          column(4,
                                 div( br(),
                                   tags$input(name='cat2_grp1', type='text', value='Group1', size='10', height = 20),
                                   br(), 
                                   tags$input(name='cat2_n11', type='text', value='0', size='10'),
                                   br(),
                                   tags$input(name='cat2_n21', type='text', value='0', size='10')
                                 )
                          ),
                          column(4,
                                 div(  br(),
                                   tags$input(name='cat2_grp2', type='text', value='Group2', size='10', height = 20),
                                   br(),
                                   tags$input(name='cat2_n12', type='text', value='0', size='10'),
                                   br(),
                                   tags$input(name='cat2_n22', type='text', value='0', size='10')
                                 )
                          )
                        )
                 ),
                 column(4,  plotOutput('cat2Plot', width="90%")),         
                 column(3, tableOutput("cat2Summary"))       
                 )
#                
#                column(4, inputPanel(
#                  textInput('cat2_n1', label='Group 1 Total: ', value="0"),
#                  textInput('cat2_y1', label='Group 1 Successes: ', value="0"),
#                  textInput('cat2_n2', label='Group 2 Total: ', value="0"),
#                  textInput('cat2_y2', label='Group 2 Successes: ', value="0")
#                )),
#                column(4, 
#                       plotOutput('cat2Plot')),
#                column(4,
#                       h6("Proportions:"),
#                       tableOutput('cat2Summary'))
      ),
      
      tabPanel("Test", value="2catTest",
        h6("Test Equality of 2 Proportions - under construction")
      ),
      
      tabPanel("Estimate", value="2catEstimate",
        h6("Estimate Difference in Proportions - under construction")
      ),

      tabPanel("Normal Distribution", value="2catNormal",
               titlePanel("Normal Probabilities"),
               column(4, inputPanel(
                 #helpText("Either enter  "),
                 textInput('cat2_z_txt', label='Standardized Value: ', value=" "),
                 #helpText("Or "),
                 textInput('cat2_prob_txt', 'or  Probability: ', " "),
                 selectInput("cat2_area", "To go into which area? ", 
                             c("Lower","Upper","Extremes","Center"), NA),
                 selectInput("cat2_dist", "Distribution: ",c("Normal"), "Normal"),
                 conditionalPanel( 
                   condition = "input.prb_dist =='t'", 
                   numericInput("cat2_df", "t degrees of freedom?", 10 ))
               )),   
               column(8,
                      plotOutput('normalProbPlot2')
               )  #,
            ##  h6("Normal Approx")
      )
    ),

    ####   Two Quantitative -------------------------------------------------
    navbarMenu("Two Quant.",
      tabPanel("Enter /Describe Data", value="2quantDataEntry",
        h6("Describe 2 Quant - under construction")
      ),
      
      tabPanel("Test", value="2quantTest",
        h6("Test Slope/Correlation - under construction")
      ),

      tabPanel("Estimate Slope/Correlation", value="2quantEstimate",
        h6("Estimate Slope and Correlation - under construction")
#       ),
#       tabPanel("t- Distribution", value="2quantT",
#         h6("Normal Approx")
      )
    ),

    ####   1 categorical & 1 quantitative  -----------------------------------
    navbarMenu("One of Each",
               
      tabPanel("Enter /Describe Data", value="1cat1quantDataEntry",
        h6("Describe 2 means - under construction")
      ),
      
      tabPanel("Test", value="1cat1quantTest",
        h6("Test Equality of 2 Means - under construction")
      ),
      
      tabPanel("Estimate", value="1cat1quantEstimate",
        h6("Estimate Difference in Means - under construction")
      ),

      tabPanel("t Distribution", value="1cat1quantT",
               titlePanel("t Probabilities"),
               column(4, inputPanel(
                 #helpText("Either enter  "),
                 textInput('quant2_z_txt', label='Standardized Value: ', value=" "),
                 #helpText("Or "),
                 textInput('quant2_prob_txt', 'or  Probability: ', " "),
                 selectInput("quant2_area", "To go into which area? ", 
                             c("Lower","Upper","Extremes","Center"), NA),
                 selectInput("quant2_dist", "Distribution: ",c("t"), "t"),
                 numericInput("quant2_df", "t degrees of freedom?", 10 )
               )),   
               ##  dist'n shows 'normal' by default, but conditional box for t df appears.
               column(8,
                      plotOutput('tProbPlot2')
                )  
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
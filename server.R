includeScript("www/helper.js")
source("helpers.R")
##source("packrat/init.R")

includeScript("www/d3.v3.min.js")
includeScript( "www/costs.js")
    
quant1_contents <- load("data/quant1.RData")
quant2_contents <- load("data/quant2.RData")
c1q1_contents <- load("data/cat1quant1.RData")

load("data/quant1.RData")
load("data/quant2.RData")
load("data/cat1quant1.RData")

 ##  These were created to hold sample data with:
 ##  save(birthWeights, REDvsCntrl, REDvsREDA, REDAvsCntrl, MusicvsSilence1,MusicvsSilence2,file = "data/cat1quant1.RData")
 ##  save(bodytemp, birthweights, arsenic, geyser2011, file="data/quant1.RData")
 ##  save(shuttle, womenRateMen, menRateWomen, Dental, file = "data/quant2.RData")

## setup transparent colors
grn <- rgb(0, 1, 0, alpha=.4)
rd  <- rgb(1, 0, 0, alpha=.5)
blu <- rgb(0, 0, 1, alpha=.4)

options(scipen = 3, digits = 5)

 shinyServer( function(input, output, session) {

  ## 1 Categorical  -----------------------------------------------------------
 
    ##  Enter and Describe ---------------------------------- cat 1

{
    ##  Using Submit Button to keep plots from changing too soon
  cat1_data <- reactiveValues(counts = NULL, names = NULL, total = NULL)
  
  cat1Test <- reactiveValues(phat = NULL, colors = NULL, cutoff = NULL, moreExtremeCount = NULL, 
                             sampleCount = NULL, pvalue = NULL)
  
  cat1Estimate <- reactiveValues(phat = NULL, observed = NULL, colors = blu, confLevel = NULL, 
                                 tailCount = NULL, CI = NULL)
  
  observeEvent(input$cat1_submitButton, {
      cat1Test$phat <- cat1Test$colors <-   cat1Test$sampleCount <- NULL
      cat1Estimate$phat <- cat1Estimate$observed <- cat1Estimate$colors <- NULL
      cat1_data$counts <- as.numeric(c(input$cat1_n1, input$cat1_n2))
      cat1_data$names <- c(input$cat1_name1, input$cat1_name2)
      cat1_data$total <- sum(as.numeric(c(input$cat1_n1, input$cat1_n2)))
      shinyjs::enable("cat1_EstimateToggle") ## enable Estimate
      shinyjs::enable("cat1_TestToggle")     ## enable Test 
      shinyjs::disable("cat1_InputToggle")   ## disable Input button
  })

  observeEvent(input$cat1_InputToggle, {
    shinyjs::show("cat1Data")            ##  show data input
    shinyjs::disable("cat1_EstimateToggle") ##  disable  Estimate btn
    shinyjs::hide("cat1Estimate")         ## hide Estimate page
    shinyjs::hide("cat1Test")             ## hide Test page
    shinyjs::disable("cat1_TestToggle")   ## disable Test btn
  })
  
  observeEvent(input$cat1_TestToggle, {
    shinyjs::hide("cat1Data")             ##  hide data input
    shinyjs::enable("cat1_InputToggle")   ## disable Input button
    shinyjs::hide("cat1Estimate")         ## hide Estimate page
    shinyjs::show("cat1Test")             ## show Test page
  })
  
  observeEvent(input$cat1_EstimateToggle, {
    shinyjs::hide("cat1Data")             ##  hide data input
    shinyjs::enable("cat1_InputToggle")   ## disable Input button
    shinyjs::hide("cat1Test")             ## hide Test page
    shinyjs::show("cat1Estimate")             ## show Estimate page
  })
  
  output$cat1_triplePlay <- renderUI({
    fluidPage(
      fluidRow(
        column(3, offset = 1, 
           actionButton("cat1_InputToggle", "Input Data", class="btn btn-primary")),
        column(3, offset = 1, 
           actionButton("cat1_EstimateToggle", "Estimate", class="btn btn-primary", disabled=TRUE)),
        column(3, offset = 1, 
           actionButton("cat1_TestToggle", "Test", class="btn btn-primary", disabled = TRUE))
      ),

    div( id ="cat1Data", width = "500px",
      ##  Input counts and labels         
      br(),     
      fluidRow(
        column(6, offset = 2,
           div( label = "cat1Input", height = "300px",
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
                  actionButton("cat1_submitButton", "Use These Data", class="btn btn-primary")
          )),
          ## column(3, plotOutput('cat1_Plot',width="80%")),
          column(3, tableOutput("cat1_Summary"))       
       )
      ),  ## end Input div
    
    div( id = "cat1Test", style = "display: none;", 
         fluidPage(
           h3("Test a single proportion."),       
           div(
             fluidRow(
               column(4, 
                      h4("Original Data"),
                      tableOutput("cat1OriginalData"),
                      
                      h4("Sample from Null Hypothesis"),
                      tableOutput('cat1Test_Table')
                      
               ),
               
               column(7, 
                      div(
                        fluidRow(
                          column(8, offset =1, 
                                 HTML("<h5>True Proportion &nbsp;&nbsp;&nbsp;  H<sub>0</sub>: p = </h5>") ),
                          column(2, tags$div(
                            tags$input(id = "null_p", type = "text", class = "form-control", value = "0.001", width = "20px"))
                          )
                        ),
                        plotOutput('cat1Test_Plot2', click = 'cat1_Test_click', height = "300px")  
                      )
               )
             ), 
             fluidRow(
               column(4, offset = 1, h4("More samples from the null?")),
               column(1,
                      actionButton("cat1_test_shuffle_10", label = "10", class="btn btn-primary")),
               column(1,
                      actionButton("cat1_test_shuffle_100", label = "100", class="btn btn-primary")),
               column(1,
                      actionButton("cat1_test_shuffle_1000", label = "1000", class="btn btn-primary")),
               column(1,
                      actionButton("cat1_test_shuffle_5000", label = "5000", class="btn btn-primary"))
             ),
             br(),
             br(),
             fluidRow(
               column(8, offset = 2,
                      uiOutput("Cat1TestXtremes"),
                      uiOutput("Cat1TestPvalue")
               )
             )
           )
         )
    ),    ## end of Cat 1 testing
    
    div( id = "cat1Estimate", style = "display: none;", 
      fluidRow(
        column(4, 
               h4("Estimate a Single Proportion"),
               h5("Original Data"),
               tableOutput("cat1_CIPrep"),
               h5("One Resampled Dataset"),
               tableOutput('cat1Estimate_Table')
        ),
        column(8, 
               plotOutput('cat1Estimate_Plot2', click = 'cat1_Estimate_click', height = 350)
        )
      ),
      fluidRow(
        column(4, offset = 1, h4("More resamples: ")),
        column(1, actionButton("cat1_estimate_shuffle_10", label = "10", class="btn btn-primary")),
        column(1, actionButton("cat1_estimate_shuffle_100", label = "100", class="btn btn-primary")),
        column(1, actionButton("cat1_estimate_shuffle_1000", label = "1000", class="btn btn-primary")),
        column(1, actionButton("cat1_estimate_shuffle_5000", label = "5000", class="btn btn-primary"))
      ),
      br(),
      fluidRow(
        column(4, offset = 1, 
               h4("Confidence Level (%):")
        ),
        column(5,
               fluidRow(
                 column(3, actionButton('cat1_conf80', label = "80", class="btn btn-primary")),
                 column(3, actionButton('cat1_conf90', label = "90", class="btn btn-primary")),
                 column(3, actionButton('cat1_conf95', label = "95", class="btn btn-primary")),
                 column(3, actionButton('cat1_conf99', label = "99", class="btn btn-primary"))
               ))
      ),
      fluidRow(
        column(8, offset = 2,
               uiOutput("Cat1ShowCI")
        )
      )
 
    ) 
    ) ## close Cat1_triplePlay UI
  }) 

  ######
  output$Cat1ShowCI <- renderText({  
    if(!is.null(cat1Estimate$tailCount)){
      fluidRow(
        column(8, offset = 4, 
             h4(paste(round(100 * cat1Estimate$confLevel), 
                      "% Confidence Interval Estimate: (", 
                      round(sort(cat1Estimate$phat)[cat1Estimate$tailCount],3), ",", 
                      round(sort(cat1Estimate$phat, decreasing = TRUE)[cat1Estimate$tailCount], 3), ")"))
      ) )
    }
  })
  ######
  printMyCI <- reactive({
    req(cat1Estimate$CI)
    h4(paste(round(100 * cat1Estimate$confLevel), "% Confidence Interval Estimate: (", 
       round(cat1Estimate$CI[1], 3), ",", 
       round(cat1Estimate$CI[2], 3), ")"))
  }) 
  ## Descriptives:  plot a bar chart of the successes / failures 
    ## No longer needed?
  output$cat1_Plot <- renderPlot( {
    if(input$cat1_submitButton ==0) return()
    #isolate( { 
      ## print(dataDF)
      ## phat1 <- cat1_dataDF$counts[1]/sum(cat1_dataDF$counts)
      countTable <- as.table(matrix(cat1_data$counts, 2, 1))
      rownames(countTable) =  cat1_data$names
       ## make plot
       par(mar=c(24, 40, 10, 35)/10)
       barplot(prop.table(countTable), ylab = "Proportion", main = "",xaxt="n")
     #})
  }, height=120)

#   output$cat1DataIn <- renderText({
#   if(input$cat1_submitButton ==0) return()
#     "Data are entered, you may now choose to estimate or test one proportion."
#   })


  output$cat1_Summary <- renderTable({
    if(input$cat1_submitButton ==0) return()
    #isolate({
     # cat1_dataDF <- cat1_data
      counts <- as.table( matrix(cat1_data$counts), 1, 2)
      dimnames(counts) = list(cat1_data$names,"Proportions")
      prop.table(counts)
    #})
  }, digits = 3)
} 
    ##  Test for single proportion ------------------------------------------------ cat 1
{ 
#  output$cat1_testUI <- renderUI({
#    if( is.null(cat1_data$counts)){
#      h4(" You must first enter data. Choose 'Enter/Describe Data'.")
#    } else {
#        fluidPage(
#           h3("Test a single proportion."),       
#             div(
#               fluidRow(
#                 column(4, 
#                    h4("Original Data"),
#                    tableOutput("cat1OriginalData"),
#                       
#                    h4("Sample from Null Hypothesis"),
#                    tableOutput('cat1Test_Table')
#                       
#                 ),
#                       
#                 column(7, 
#                     div(
#                       fluidRow(
#                         column(8, offset =1, h4("True Proportion (Null hypothesis for p):")),
#                         column(2, tags$div( 
#                                  tags$input(id = "null_p", type = "text", class = "form-control", value = "0.001", width = "20px"))
#                                )
#                         ),
#                        plotOutput('cat1Test_Plot2', click = 'cat1_Test_click', height = "300px")  
#                       )
# #                     fluidRow(
# #                       column(11, offset = 1,
# #                              HTML(" Click a point to see its counts and proportions."),
# #                              br()
# #                              )
# #                     )
#                  )
#                 
#               ), 
#               #br(),
#               fluidRow(
#                 column(5, offset = 1, h4("How many more samples from the null?")),
#                 column(1,
#                    actionButton("cat1_test_shuffle_10", label = "10", class="btn btn-primary")),
#                 column(1,
#                    actionButton("cat1_test_shuffle_100", label = "100", class="btn btn-primary")),
#                 column(1,
#                    actionButton("cat1_test_shuffle_1000", label = "1000", class="btn btn-primary")),
#                 column(1,
#                    actionButton("cat1_test_shuffle_5000", label = "5000", class="btn btn-primary"))
#                ),
#                br(),
#                br(),
#                fluidRow(
#                  column(8, offset = 2,
#                         uiOutput("Cat1TestXtremes"),
#                         uiOutput("Cat1TestPvalue")
#                  )
#                )
#         )
#        )
#     }
#  })

observeEvent( input$null_p, {
  cat1Test$phat <- NULL  
  cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL
})


output$Cat1TestXtremes <- renderUI({
#  PvalueUI("cat1Pval")
  fluidRow(
    column(4,
           h4("Count values equal to or")
    ),
    column(4,
           tags$div(style="width: 200px",
                    tags$select(id='cat1_testDirection', class="form-control",
                                tags$option( value = "less", "less"),
                                tags$option( value = "more extreme", "more extreme", selected = TRUE),
                                tags$option( value = "greater", "greater"))
           )),
    column(1, h4("than ")),
    column(2,
    #     textInput('cat1_test_cutoff', label = "", value = NA)
           tags$div(
                tags$input(id = "cat1_test_cutoff", type = "text", class = "form-control", value = NA))
    ),
    column(1,
           actionButton('cat1_test_countXtremes', "Go", class="btn btn-success")
    )
  )
})
   ## error in this line -- says cat1Test$phat should be reactive, but if it is, then it says is a closure
#cat1Pvalue <- callModule(Pvalue, "cat1Pval", parms = reactive({cat1Test$phat}), reactive({input$null_p}))

observeEvent( input$cat1_testDirection, {
  cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL
  cat1Test$colors <- rep(blu, length(cat1Test$colors))
})

observeEvent( input$cat1_test_cutoff, {
  cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL
  cat1Test$colors <- rep(blu, length(cat1Test$colors))
})


 output$Cat1TestPvalue <- renderUI({
   req(cat1Test$moreExtremeCount)
     h4(pvalue2print(cat1Test$moreExtremeCount,  cat1Test$sampleCount, cat1Test$direction, cat1Test$cutoff, cat1Test$pvalue))
 })

    output$cat1OriginalData <- renderTable({ 
      if(input$cat1_submitButton ==0) return()
      #print(cat1_data$counts)
      #print(cat1_data$names)
      counts <- data.frame( matrix(as.numeric(c(cat1_data$counts, 0, 0)), 2, 2, 
                                   dimnames = list(cat1_data$names, c("Counts","Proportions"))))
      counts[,2] <- as.numeric(prop.table(as.table(counts[,1])))
      counts[,1] <- as.integer(counts[,1])
      counts
    }, digits = c(0,0,3))
    
    output$cat1Test_Table <- renderTable({
      #if(input$cat2_submitButton == 0) return()
      req(cat1_data$counts)
      
      n1 <- cat1_data$total
      if(is.null(cat1Test$phat)){
        y1_new <- as.matrix(rbinom(1, n1, as.numeric(input$null_p)))
        phat_new <- round(y1_new/n1, 3)
        cat1Test$phat <- phat_new
        cat1Test$colors <- blu
      } else  if(!is.null(input$cat1_Test_click)){
        ##  We already have shuffled data and want to pick the clicked point
        ##  Change to data related to a clicked point.
        closestPoint <- which.min(abs( cat1Test$phat - input$cat1_Test_click$x))
        ##closestPoint <- nearPoints(as.data.frame(cat1Test$test), coordinfo=input$cat1_Test_click,
                                             #xvar = input$cat1_Test_click$x,
                                             #yvar = input$cat1_Test_click$y,
        ##                                     maxpoints=1)[1]
        ##cat("Close to number: ", closestPoint, "\n")
        phat_new <- cat1Test$phat[closestPoint] 
        y1_new <- round(phat_new * n1)
      } else {
        return()
      }
      #print(phat_new)
      counts <- data.frame( matrix(as.numeric(as.numeric(c(y1_new, n1 - y1_new, 0, 0))), 2, 2, 
                                   dimnames = list(cat1_data$names, c("Counts","Proportions"))))
      counts[,2] <- as.numeric(prop.table(as.table(counts[,1])))
      counts[,1] <- as.integer(counts[,1])
      #print(counts)
      counts
    }, digits = c(0,0,3))
    
    
    observeEvent(input$cat1_test_shuffle_10, {
      n1 <- cat1_data$total
      cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL
      y1_new <- rbinom(10, cat1_data$total, as.numeric(input$null_p))
      phat <- round(y1_new/n1, 3)
      cat1Test$phat <- c(cat1Test$phat, phat)
      cat1Test$colors <- rep(blu, length(cat1Test$phat))
    })
    
    observeEvent(input$cat1_test_shuffle_100, {
      n1 <- cat1_data$total
      cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL
      y1_new <- rbinom(100, cat1_data$total, as.numeric(input$null_p))
      phat <- round(y1_new/n1, 3)
      cat1Test$phat <- c(cat1Test$phat, phat)
      cat1Test$colors <- rep(blu, length(cat1Test$phat))
    })
    
    observeEvent(input$cat1_test_shuffle_1000, {
      n1 <- cat1_data$total
      cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL
      y1_new <- rbinom(1000, cat1_data$total, as.numeric(input$null_p))
      phat <- round(y1_new/n1, 3)
      cat1Test$phat <- c(cat1Test$phat, phat)
      cat1Test$colors <- rep(blu, length(cat1Test$phat))
    })
    
    observeEvent(input$cat1_test_shuffle_5000, {
      n1 <- cat1_data$total
      cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL
      y1_new <- rbinom(5000, cat1_data$total, as.numeric(input$null_p))
      phat <- round(y1_new/n1, 3)
      cat1Test$phat <- c(cat1Test$phat, phat)
      cat1Test$colors <- rep(blu, length(cat1Test$phat))
    })
 
    observeEvent(input$cat1_test_countXtremes, {
      x <- sort(as.numeric(cat1Test$phat))
      nsims <- length(x)
      p0 <- as.numeric(input$null_p)
      cat1Test$colors <- rep(blu, nsims)
      cat1Test$cutoff <- threshold <- as.numeric(input$cat1_test_cutoff)
      cat1Test$direction <- input$cat1_testDirection
      if(nsims > 1 & !is.na(input$cat1_testDirection)){
        redValues <-  switch(input$cat1_testDirection,
                             "less" = which(x < threshold + 1.0e-10),
                             "greater" = which(x > threshold - 1.0e-10),
                             "more extreme" = which(abs(x - p0) > abs(threshold - p0) - 1.0e-10 )) 
        cat1Test$colors[redValues] <- rd       
        cat1Test$moreExtremeCount  <- length(redValues)
        cat1Test$pvalue <- cat1Test$moreExtremeCount/nsims
        cat1Test$sampleCount <- length(cat1Test$phat)
      }
    })
    
    
    output$cat1Test_Plot2 <- renderPlot({
      req(input$null_p, cat1Test$phat)
      if(input$cat1_submitButton == 0 ) return()
      
      DF <- sort(cat1Test$phat)
      
      if(length(DF) == 1){
        w <- 1
        radius = 6
      } 
      else {
        #nbreaks <- 0.5*nclass.Sturges(DF)^2
        #z <- cut(DF, breaks = nbreaks)
        #w <- unlist(tapply(z, z, function(V) 1:length(V)))
        w <- newy(DF)
        #print(w)
        #print(max(w))
        nsims <- length(DF)
        radius = pmax(1, 11 - round(log(length(DF))))        
      }
#       qplot(x=DF, y=w, ylab = "", #size = radius/2, shape = 16, 
#            colour = cat1Test$colors,  ylim=c(.5, pmax(10,max(w))),
#            xlab = expression(hat(p)), main = "Sampling Distribution",
#            sub = "Click a point to see its counts") +theme_bw()
       plot(DF, w, ylab = "", cex = radius/2, pch = 16, 
           col = cat1Test$colors,  ylim=c(.5, pmax(10,max(w))),
           xlab = expression(hat(p)), main = "Sampling Distribution",
           sub = "Click a point to see its counts")
       legend("topright", bty = "n", paste(length(DF), "points \n Mean = ", 
                                         round(mean(DF),3), "\n SE = ", round(sd(DF),3)))
  }, height = 300, width = 500)

}

   ###  estimate p.hat  -------------------------------------- cat 1
{
output$cat1_OLDestimateUI <- renderUI({
  if( is.null(cat1_data$counts)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else {
    tabPanel("Estimate", value="1catEstimate",       
             fluidRow(
                 column(4, 
                      h3("Estimate a Single Proportion"),
                      h4("Original Data"),
                      tableOutput("cat1_CIPrep"),
                      h4("One Resampled Dataset"),
                      tableOutput('cat1Estimate_Table')
                 ),
                 column(8, 
                      plotOutput('cat1Estimate_Plot2', click = 'cat1_Estimate_click', height = 350)
                )
             ),
             #br(),
             fluidRow(
               column(4, offset = 1, h4("More resamples: ")),
               column(1,
                      actionButton("cat1_estimate_shuffle_10", label = "10", class="btn btn-primary")),
               column(1,
                      actionButton("cat1_estimate_shuffle_100", label = "100", class="btn btn-primary")),
               column(1,
                      actionButton("cat1_estimate_shuffle_1000", label = "1000", class="btn btn-primary")),
               column(1,
                      actionButton("cat1_estimate_shuffle_5000", label = "5000", class="btn btn-primary"))
             ),
             br(),
             fluidRow(
               column(4, offset = 1, 
                      h4("Select Confidence Level (%)")
                      ),
               column(5,
                 fluidRow(
                    column(3, actionButton('cat1_conf80', label = "80", class="btn btn-primary")),
                    column(3, actionButton('cat1_conf90', label = "90", class="btn btn-primary")),
                    column(3, actionButton('cat1_conf95', label = "95", class="btn btn-primary")),
                    column(3, actionButton('cat1_conf99', label = "99", class="btn btn-primary"))
                ))),
                        
              if(!is.null(cat1Estimate$tailCount)){
                fluidRow(
                  column(8, offset = 4,
                          h4(paste(round(100 * cat1Estimate$confLevel), 
                                   "% Confidence Interval Estimate: (", 
                                   round(sort(cat1Estimate$phat)[cat1Estimate$tailCount],3), ",", 
                                   round(sort(cat1Estimate$phat, decreasing = TRUE)[cat1Estimate$tailCount], 3), ")"))
                  )
                )
                } else {br()}
            )
         
  }
})


output$cat1_CIPrep <- renderTable({ 
  if(input$cat1_submitButton ==0) return()
  #print(cat1_data$counts)
  #print(cat1_data$names)
#   counts <- as.table( matrix(cat1_data$counts), 1, 2)
#   dimnames(counts) = list(cat1_data$names,"Proportions")
#   prop.table(counts) 
  counts <- data.frame( matrix(as.numeric(c(cat1_data$counts, 0, 0)), 2, 2, 
                               dimnames = list(cat1_data$names, c("Counts","Proportions"))))
  counts[,2] <- as.numeric(prop.table(as.table(counts[,1])))
  counts[,1] <- as.integer(counts[,1])
  counts
  
}, digits = c(0,0,3))


output$cat1Estimate_Table <- renderTable({
  #if(input$cat1_submitButton == 0) return()
  req(cat1_data$counts)
  n1 <- cat1_data$total
  ##
  if(is.null(cat1Estimate$phat)){
    y1_new <- as.matrix(rbinom(1, cat1_data$total, cat1_data$counts[1]/cat1_data$total))
    cat1Estimate$phat <-  phat_new <- round(y1_new/n1, 3)
    cat1Estimate$colors <- blu
   } else  if(!is.null(input$cat1_Estimate_click)){
    ##  We already have shuffled data and want to pick the clicked point
    ##  Change to data related to a clicked point.
    closestPoint <- which.min(abs( cat1Estimate$phat - input$cat1_Estimate_click$x))
    #cat("Close to number: ", closestPoint, "\n")
    phat_new <- cat1Estimate$phat[closestPoint] 
    y1_new <- round(phat_new * n1)
  } else{
    return()
  }
  
  counts <- data.frame( matrix(as.numeric(c(y1_new, n1 - y1_new, 0, 0)), 2, 2, 
                               dimnames = list(cat1_data$names, c("Counts","Proportions"))))
  counts[,2] <- as.numeric(prop.table(as.table(counts[,1])))
  counts[,1] <- as.integer(counts[,1])
  counts
}, digits = c(0,0,3))


observeEvent(input$cat1_estimate_shuffle_10, {
  cat1Estimate$CI <- NULL
  y1_new <- as.matrix(rbinom(10, cat1_data$total, cat1_data$counts[1]/cat1_data$total))
  phat <- round(y1_new/cat1_data$total, 3)
  cat1Estimate$phat <- rbind(cat1Estimate$phat, phat)
  cat1Estimate$colors <- rep(blu, length(cat1Estimate$phat))
})

observeEvent(input$cat1_estimate_shuffle_100, {
  cat1Estimate$CI <- NULL
  y1_new <- as.matrix(rbinom(100, cat1_data$total, cat1_data$counts[1]/cat1_data$total))
  phat <- round(y1_new/cat1_data$total, 3)
  cat1Estimate$phat <- rbind(cat1Estimate$phat, phat)
  cat1Estimate$colors <- rep(blu, length(cat1Estimate$phat))
})

observeEvent(input$cat1_estimate_shuffle_1000, {
  cat1Estimate$CI <- NULL
  y1_new <- as.matrix(rbinom(1000, cat1_data$total, cat1_data$counts[1]/cat1_data$total))
  phat <- round(y1_new/cat1_data$total, 3)
  cat1Estimate$phat <- rbind(cat1Estimate$phat, phat)
  cat1Estimate$colors <- rep(blu, length(cat1Estimate$phat))
})

observeEvent(input$cat1_estimate_shuffle_5000, {
  cat1Estimate$CI <- NULL
  y1_new <- as.matrix(rbinom(5000, cat1_data$total, cat1_data$counts[1]/cat1_data$total))
  phat <- round(y1_new/cat1_data$total, 3)
  cat1Estimate$phat <- rbind(cat1Estimate$phat, phat)
  cat1Estimate$colors <- rep(blu, length(cat1Estimate$phat))
})

observeEvent(input$cat1_conf80,{
  req(cat1_data$total) 
  if((nsims <- length(cat1Estimate$phat)) < 10){
    return()
  }
  cat1Estimate$confLevel <- .80
  cat1Estimate$colors <- rep(blu, nsims)
  cat1Estimate$tailCount <- floor(nsims * 0.1)
  cat1Estimate$colors[1:cat1Estimate$tailCount] <- rd
  cat1Estimate$colors[nsims +1 -(1:cat1Estimate$tailCount)] <- rd
  #cat1Estimate$CI <- sort(cat1Estimate$phat)[c(tailCount, nsims + 1 - tailCount)]
}) ## short circuits back to data entry if I set CI here.

observeEvent(input$cat1_conf90,{
  req(cat1_data$total) 
  if((nsims <- length(cat1Estimate$phat)) < 10){
    return()
  }
  cat1Estimate$confLevel <- .90
  cat1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * 0.05)
  cat1Estimate$colors[1:tailCount] <- rd
  cat1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  #cat1Estimate$CI <- sort(cat1Estimate$phat)[c(tailCount, nsims + 1 - tailCount)]
})

observeEvent(input$cat1_conf95,{
  req(cat1_data$total)
  if((nsims <- length(cat1Estimate$phat)) < 10){
    return()
  }
  cat1Estimate$confLevel <- .95
  cat1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * 0.025)
  cat1Estimate$colors[1:tailCount] <- rd
  cat1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  #cat1Estimate$CI <- sort(cat1Estimate$phat)[c(tailCount, nsims + 1 - tailCount)]
  
})

observeEvent(input$cat1_conf99,{
  req(cat1_data$total) 
  if((nsims <- length(cat1Estimate$phat)) < 10){
    return()
  }
  cat1Estimate$confLevel <- .99
  cat1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * 0.005)
  cat1Estimate$colors[1:tailCount] <- rd
  cat1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  #cat1Estimate$CI <- sort(cat1Estimate$phat)[c(tailCount, nsims + 1 - tailCount)]
  
})


output$cat1Estimate_Plot2 <- renderPlot({
  if(input$cat1_submitButton == 0 | is.null(cat1Estimate$phat)) return()
  
  DF <- sort(cat1Estimate$phat)
  
  if(length(DF) == 1){
    w <- 1
    radius = 4
  } 
  else {
    #nbreaks <- 0.5*nclass.Sturges(DF)^2
    #z <- cut(DF, breaks = nbreaks)
    #w <- unlist(tapply(z, z, function(V) 1:length(V)))
    w <- newy(DF)
    #print(w)
    #print(max(w))
    nsims <- length(DF)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(DF, w, ylab = "", ylim = c(0.5, pmax(10, max(w))), cex = radius/2, pch = 16, col = cat1Estimate$colors,  
       xlab = expression(hat(p)), main = "Re-Sampling Distribution",
       sub = "Click a point to see its counts")
  legend("topright", bty = "n", paste(length(DF), "points \n Mean = ", 
                                     round(mean(DF),3), "\n SE = ", round(sd(DF),3)))
}, height = 300, width = 490)

}
  
  ##  confidence interval demo  -------------------------------------- cat 1
{  
  output$inputTrueP <- renderUI({
    n <- input$CIdemo_n
    sliderInput("CIdemo_p", "Choose true proportion of successes ", 
                min=pmin(0.5, round(10/n,2)), max=pmax(.5, round(1- 9.9/n,2)), value = .5)
  })
  
  CIdemoSims <- reactive({
    req(input$CIdemo_p)
    
    nsims <- as.numeric(input$CIdemo_reps)      
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)
    ## exactRule <- -log( (1-upperconf) * 2)/input$CIdemo_n
    ##print(c(input$CIdemo_n, input$CIdemo_p))
    phats <- sort((1 + rbinom(nsims, as.integer(input$CIdemo_n), input$CIdemo_p))/(2 + input$CIdemo_n))
    phat.stack <- unlist(tapply(phats, cut(phats, breaks = nclass.Sturges(phats)^2),
                                function(x) if(length(x) > 0) {1:length(x)} else {NULL}))
    SEs <- sqrt(pmax(.0099, phats * (1-phats) )/ input$CIdemo_n)
    phatDF <- data.frame(
      phat = phats,
      y = phat.stack[!is.na(phat.stack)],
      SE = SEs
      #     LB = pmax(0, phats - zstar * SEs),
      #     UB = pmin(1, phats + zstar * SEs)
    )
    phatDF$row <- 1:nrow(phatDF)
    #   phatDF$colr <- with(phatDF, ifelse(LB < input$CIdemo_p & UB > input$CIdemo_p, 1, 2))
    phatDF
  })
  
  output$CIdemo_Plot1 <- renderPlot({
    ##displayFn <-  reactive({
    req(input$CIdemo_p)

    phatDF <- CIdemoSims()
    nsims <- as.numeric(input$CIdemo_reps)      
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)
    par(mar=c(4,2,1,1))
    isolate({
      plot(y ~ phat, data= phatDF, col = rgb(70, 130, 180, 127, max = 255), pch=16, bty="l",
           cex = radius/2, ylab = "", xlab = expression(hat(p)),
           ylim=c(.5, pmax(10, max(y))), #main = "Sampling Distribution" ,
           sub = "Click a point to see its interval estimate")
    })
  }, height = 225)
  
  output$CIdemo_Plot2 <- renderPlot({
    ##displayFn <-  reactive({
    req(input$CIdemo_p, input$CIdemo_conf)
    phatDF <- CIdemoSims()  
    isolate({
      upperconf = 1- (1 - as.numeric(substr(input$CIdemo_conf,1,2))/100)/2
      zstar <- qnorm(upperconf)
      #print(zstar)
      phatDF$LB = pmax(0, phatDF$phat - zstar * phatDF$SE)
      phatDF$UB = pmin(1, phatDF$phat + zstar * phatDF$SE)
      phatDF$colr <- with(phatDF, ifelse(LB < input$CIdemo_p & UB > input$CIdemo_p, 1, 2))
      #print(summary(phatDF))
      coverage = 2 - mean(phatDF$colr) 
      nsims <- as.numeric(input$CIdemo_reps)      
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)
      par(mar=c(4,1,2,1))
      plot(row ~ phat, data = phatDF, col = "white", bty="l", xlim = c(min(phatDF$LB), max(phatDF$UB)),
           cex = radius/4, ylab = "", # main = "Confidence Intervals",
           xlab = paste("Coverage rate =", coverage))
      with(phatDF, segments(LB, row, UB, row, col = c("green","red")[colr] ))
      points(row ~ phat, data = phatDF, col = rgb(70, 130, 180, 127, max = 255), pch=16)
      abline(v = input$CIdemo_p, lwd = 2, col = "grey")
      mtext(side=3, at = input$CIdemo_p, input$CIdemo_p, line=0)
      text(x = phatDF$UB[nsims/2], y = 0.08 * nsims, paste( sum(phatDF$UB < input$CIdemo_p), "too low"))
      text(x = phatDF$LB[nsims/2], y= .93 * nsims, paste( sum(phatDF$LB > input$CIdemo_p), "too high"))     
    })
    if(!is.null(input$CIplot1_click)){
      myY <- subset(phatDF, abs(phatDF$phat - input$CIplot1_click$x) < .005)[round(input$CIplot1_click$y),]
      if(!is.null(myY) & length(myY) > 1){
        points(x=myY$phat, y = myY$row, cex=2, col = "blue")
        segments(myY$LB, myY$row, myY$UB, myY$row, lwd=4, col = "blue")
      }
    }
    
  }, height = 275)
}
  
 
  ##  Lurking Demo -----------------------------------------  cat 1
   
   ## data input
  {
  
  c1Lurk <- reactiveValues(data = NULL, shuffles = NULL, difprop = NULL, closest = 2,
                           colors = NULL,  names = NULL, p1hat = NULL, p2hat = NULL, y1 =NULL,y2 = NULL)
  
  output$c1_LurkDataUI <- renderUI({
    ## 
  div(    
    fluidRow( 
      column(3, offset = 1, h5("Lurking Variable: Names:")),
      column(2, h5("Counts:"))
    ),
    fluidRow(
      column( 3, offset = 1,
             tags$input(name='c1Lurk_group1', type='text', value='Success', size='10'),
             br(),
             tags$input(name='c2Lurk_group2', type='text', value='Failure', size='10')
       ),
      column(2,
            tags$input(name='c1Lurk_n1', type='text', value='0', size='10'),
            br(),
            tags$input(name='c1Lurk_n2', type='text', value='0', size='10')
      )
    ),
    br(),
    fluidRow(
       column(5,
              h5("Size of Treament group: (the rest are controls)")
              ),
      column(3,
             tags$input(name='c1Lurk_m1', type='text', value='0', size='10')
            ),
      column(2, actionButton("c1Lurk_Go", "Go", class="btn btn-success"))
       
     )
    )
  })
  
  observeEvent(input$c1Lurk_Go, {
    c1Lurk$data <- list(n1 = as.numeric(input$c1Lurk_n1) , 
                        n2 = as.numeric(input$c1Lurk_n2), m1 = as.numeric(input$c1Lurk_m1))
    c1Lurk$data$m2 <- with(c1Lurk$data, n1 + n2 - m1)
    c1Lurk$names <- c(input$c1Lurk_group1, input$c1Lurk_group2)
    DF <- c1Lurk_shuffles(2, c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$phat1 <-  DF[, 1]
    c1Lurk$phat2 <-  DF[, 2]
    c1Lurk$difprop <- DF[, 3]
    c1Lurk$y1 <-  DF[, 4]
    c1Lurk$y2 <-  DF[, 5]
    c1Lurk$colors <- blu
  })
  

  output$c1_LurkingUI <- renderUI({
    req(c1Lurk$data, c1Lurk$names)
    div(
      fluidRow(
        column(4, 
               h5("Randomization 1"),
               tableOutput("c1Lurk_Table1"),
               h5(paste("Difference in proportions: ", 
                        round( as.numeric(c1Lurk$difprop[1]), 3)
               )),
               
               br(),
               
               h5("Randomization 2"),
               tableOutput('c1Lurk_Table2'),
               h5(paste("Difference in proportions: " , 
                        round( as.numeric(c1Lurk$difprop[c1Lurk$closest]), 3)))
              ),
        column(8, 
               plotOutput('c1_LurkPlot2', click = 'c1_Lurk_click')
      )
    ), 
    fluidRow(
      column(4, offset = 1,
             h4("How many more randomizations?")
             ),
      column(1, actionButton("c1_Lurk_shuffle_10", label = "10", class="btn btn-primary")),
      column(1, actionButton("c1_Lurk_shuffle_100", label = "100", class="btn btn-primary")),
      column(1, actionButton("c1_Lurk_shuffle_1000", label = "1000", class="btn btn-primary")),
      column(1, actionButton("c1_Lurk_shuffle_5000", label = "5000", class="btn btn-primary"))
      
    )  
  )
})

  ## -------- 1 cat Lurking tables ------------------
  

  output$c1Lurk_Table1 <- renderTable({
    req(c1Lurk$data)

    y1_new <- c1Lurk$y1[1]
    y2_new <- c1Lurk$y2[1]
    diff.p <- c1Lurk$difprop[1]
    # print(c(y1_new, n1, DF[1,1], y2_new, n2, DF[1,2], diff.p))
    table1 <- data.frame(count = as.integer(c(y1_new, y2_new)), 
                         n = with(c1Lurk$data, as.integer(c(m1, m2))),
                         Prop = c(c1Lurk$phat1[1], c1Lurk$phat2[1]))
    colnames(table1)[1] <- c1Lurk$names[1]
    rownames(table1) <- c("Treatment","Control")
    table1
  }, digits = c(0,0,0,3))
  
  
  output$c1Lurk_Table2 <- renderTable({
    req(c1Lurk$data)

      if(!is.null(input$c1_Lurk_click)){
      ##  Pick the clicked shuffle
      closestPoint <- which.min(abs( c1Lurk$phat1 - c1Lurk$phat2 - input$c1_Lurk_click$x))
      #cat("Close to number: ", closestPoint, "\n")
    } else{
      closestPoint <- 2
    }
    c1Lurk$closest <- closestPoint
    y1_new <- c1Lurk$y1[closestPoint] #* input$c1Lurk_n1
    y2_new <- c1Lurk$y2[closestPoint] #* input$c1Lurk_n2
    diff.p <- c1Lurk$difprop[closestPoint]
    # print(c(y1_new, n1, DF[1,1], y2_new, n2, DF[1,2], diff.p))
    table2 <- data.frame(count = as.integer(c(y1_new, y2_new)), 
                         n = with(c1Lurk$data, as.integer(c(m1, m2))),
                         Prop = c(c1Lurk$phat1[closestPoint], c1Lurk$phat2[closestPoint]))
    colnames(table2)[1] <- c1Lurk$names[1]
    rownames(table2) <- c("Treatment","Control")
    table2
  }, digits = c(0,0,0,3))
  
  observeEvent(input$c1_Lurk_shuffle_10, {
    DF <- c1Lurk_shuffles(shuffles = 10, c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$difprop <- c(c1Lurk$difprop,  DF[,3])
    c1Lurk$phat1 <- c(c1Lurk$phat1,  DF[,1])
    c1Lurk$phat2 <- c(c1Lurk$phat2,  DF[,2])
    c1Lurk$y1 <- c(c1Lurk$y1,  DF[,4])
    c1Lurk$y2 <- c(c1Lurk$y2,  DF[,5])
    c1Lurk$colors <- rep(blu, length(c1Lurk$difprop))
  })
  
  observeEvent(input$c1_Lurk_shuffle_100, {
    DF <- c1Lurk_shuffles(shuffles = 100,  c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$difprop <- c(c1Lurk$difprop,  DF[,3])
    c1Lurk$phat1 <- c(c1Lurk$phat1,  DF[,1])
    c1Lurk$phat2 <- c(c1Lurk$phat2,  DF[,2])
    c1Lurk$y1 <- c(c1Lurk$y1,  DF[,4])
    c1Lurk$y2 <- c(c1Lurk$y2,  DF[,5])
    c1Lurk$colors <- rep(blu, length(c1Lurk$difprop))
  })
  observeEvent(input$c1_Lurk_shuffle_1000, {        
    DF <- c1Lurk_shuffles(shuffles = 1000,  c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$difprop <- c(c1Lurk$difprop,  DF[,3])
    c1Lurk$phat1 <- c(c1Lurk$phat1,  DF[,1])
    c1Lurk$phat2 <- c(c1Lurk$phat2,  DF[,2])
    c1Lurk$y1 <- c(c1Lurk$y1,  DF[,4])
    c1Lurk$y2 <- c(c1Lurk$y2,  DF[,5])
    c1Lurk$colors <- rep(blu, length(c1Lurk$difprop))
  })
  observeEvent(input$c1_Lurk_shuffle_5000, {
    DF <- c1Lurk_shuffles(shuffles = 5000,  c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$difprop <- c(c1Lurk$difprop,  DF[,3])
    c1Lurk$phat1 <- c(c1Lurk$phat1,  DF[,1])
    c1Lurk$phat2 <- c(c1Lurk$phat2, DF[,2])
    c1Lurk$y1 <- c(c1Lurk$y1,  DF[,4])
    c1Lurk$y2 <- c(c1Lurk$y2,  DF[,5])
    c1Lurk$colors <- rep(blu, length(c1Lurk$difprop))
  })
  
  
  output$c1_LurkPlot2 <- renderPlot({
    req(c1Lurk$difprop)

    diffs <- sort(c1Lurk$difprop)
    if(length(diffs) < 4){
      w <- 1 + diffs*0
      radius <- 4 + diffs*0
    } 
    else {
      #nbreaks <- 0.5*nclass.Sturges(diffs)^2
      #z <- cut(diffs, breaks = nbreaks)
      #w <- unlist(tapply(z, z, function(V) 1:length(V)))
      w <- newy(diffs)
      # print(summary(w))
      # print(max(w))
      nsims <- length(diffs)
      radius <- 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(diffs, w, ylab = "", ylim = c(0.5, pmax(10, max(w))), cex = radius/2, pch = 16, col = c1Lurk$colors,  
         xlab = expression(hat(p)[1] - hat(p)[2]), main = "Randomization Distribution")
    legend("topright", bty = "n", paste(length(diffs), "points \n Mean = ", 
                                        round(mean(diffs),3), "\n SE = ", round(sd(diffs),3)))
    #mtext(side = 1, at = 0, adj = 0, line = 0, bquote(p[1] == p[2]))
  }, width = 480)
  

  
  }
     ##    
   ##  Spinner  ## ---------------------------------------------------------------
{
  
  spin_functionList <- c( "Count in Group1","Count in Group 2", "Max Run Length")
  
  reactiveSpin <- function (outputId) {
    HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
  }
  
    output$c1_spinnerUI <- renderUI({
     div(  
       titlePanel("Spinner"),
       fluidRow(
         column(4, 
          div(
            tags$label("Number of Categories",       
                     tags$input(name='spin_nCat', type='number', value= 3, size=10,style="width: 40px" )),
            tags$label("Category names: ", 
                       tags$input(name = "spin_categories", type = 'text', value = "A, B, C", size = 12,style="width: 90px" )),
            h5("(separate with commas)"),
            tags$label( "Percentages: ", 
                        tags$input(name = "spin_probs",type = "text", value = "50, 30, 20",
                                   style="width: 120px" )),
            h5("Separate with commas. Relative size is the key."),
            #hr() ,
            tags$label("Stop after: ",
                tags$div(style="width: 200px", 
                     tags$select(id='spin_stopRule', class="form-control",
                                 tags$option( value = "Fixed number of spins","Fixed number of spins", selected = TRUE ),
                                 tags$option( value = "One spin in 1st category", "One spin in 1st category"),
                                 tags$option( value = "One of each type", "One of each type"))
            )),
          conditionalPanel(
            condition = "input.spin_stopRule =='Fixed number of spins'" ,
            tags$label("Last Spin:",       
                       tags$input(name='spin_nDraws', type='number', value= 5, size=10, style="width: 40px" ))
          ),
          tags$label("Number of Trials: ",       
                     tags$input(name='spin_reps', type='number', value= 1, size=10, style="width: 40px" )),
          #selectInput("spin_reps", "Number of Trials:", c("1","10","100","1000"),1),
           helpText("Start with 1 for an animation"),
           ## hr() ,
           conditionalPanel(
             condition = "input.spin_reps != 1 && input.spin_stopRule =='Fixed number of spins'" ,
             selectInput("spin_fn","Store what result?", spin_functionList)
           ),
           conditionalPanel(
             condition = "input.spin_stopRule !='Fixed number of spins' && input.spin_reps != 1", 
             helpText("Display shows number of spins needed" )
           )
        )), 
        #mainPanel(
        column(1,
               actionButton("spin_RunButton", "Run", class="btn btn-success")  
        ),
       column(6,
          includeHTML("www/spin.js"),
          reactiveSpin(outputId = "spin_Plot")
          ),
       fluidRow(
         column(10, offset = 1,
          conditionalPanel(
            condition = "input.spin_reps == 1",
            tableOutput("spin_Summary")
          ),
          #hr() ,
          conditionalPanel(
            condition = "input.spin_reps != 1",
            plotOutput("spin_Histogrm"),
            tableOutput("spin_Summry2"),
            tableOutput("spin_Summry3")
          )
        )
      )
      )
     )
  })

  spin_data <- reactive( {
    run = input$spin_RunButton
    groups <- sapply(strsplit(input$spin_categories, ","), function(x)
      gsub("[[:space:]]", "", x))##[order(-prob)]
    nCat <-  length(groups)
    prob <- sapply(strsplit(input$spin_probs, ","), as.numeric)
    prob <-  prob/sum(prob)
    if(input$spin_stopRule == "Fixed number of spins"){
      nDraws <- input$spin_nDraws
      spinAngle <- runif(input$spin_nDraws)
      drawColor <- as.numeric(cut( spinAngle, c(0, cumsum(prob)))) 
      nDraws <- as.numeric(input$spin_nDraws)
    } else {
      ## apply a stopping rule
      if(input$spin_stopRule == "One spin in 1st category"){
        drawSumry <- draws2get1( prob, 1)
        if(drawSumry == 1){ ## got it on 1st spin
          drawColor <-  1
          nDraws <- 1
          spinAngle <-  runif(1) * prob[1]
        } else{  ## take a few spins first
          cumProb <-  c(0, cumsum(prob) )
          drawColor <- reconstructSpins( drawSumry, prob)
          nDraws <- drawSumry
          spinAngle <-  runif(drawSumry[1]) * prob[drawColor] + cumProb[drawColor]
          ##  gives random spin in 1st category for last draw,
          ## in other categories for prior draws.
          
        }
      } else{
        ## Stop after we get one of each type
        drawSumry <- draws2get1ofEach( prob, 1, fullOut =TRUE)
        nDraws <- drawSumry$nDraws
        cumProb <-  c(0,cumsum(prob))
        drawColor <- reconstructSpins( drawSumry , prob)
        names(drawColor) <-  NULL
        spinAngle <-  runif(nDraws[1]) * prob[drawColor] + cumProb[drawColor]
      }
    }
    data.df <- list(nCat = input$spin_nCat,
                    nDraws = nDraws[1],
                    pieValues = prob,
                    pieLabels = groups,
                    spinAngle = as.list((spinAngle + 1 ) * 360),
                    drawColor = as.list(drawColor - 1)
    )
    return(data.df)
    
  })
  

  repData <- reactive( {
    run = input$spin_RunButton
    prob <- sapply(strsplit(input$spin_probs, ","), as.numeric)
    prob <-  prob/sum(prob)
    groups <- sapply(strsplit(input$spin_categories, ","), function(x)
      gsub("[[:space:]]", "", x))##[order(-prob)]
    ##prob <- -sort(-prob)
    nCat <-  length(groups)
    nReps <-  ifelse(is.null(input$spin_reps), 1, as.numeric(input$spin_reps))
    fixedN <- (input$spin_stopRule == "Fixed number of spins")
    if(fixedN){ 
      nDraws <- as.numeric(input$spin_nDraws)
      nCat <- as.numeric(input$spin_nCat)
      samplData <- matrix(sample(1:nCat, nReps * nDraws,
                                 prob = prob,replace = TRUE), ncol=nDraws)
      fn <- match(input$spin_fn, spin_functionList)
      if(fn==3){
        return(apply(samplData, 1 , function(x)  max(rle(x)[[1]])))
      } else
        return(apply(samplData, 1 , function(x) table(c(1:fn,x))[fn]-1))
    } else {
      ## apply stopping rule
      if(input$spin_stopRule =="One spin in 1st category"){
        nDraws <- draws2get1( prob, nReps)
        return(nDraws)
      } else {  ## input$spin_stopRule =="One of Each"
        nDraws <-  draws2get1ofEach( prob, nReps)
        return(nDraws)
      }
    }
    
  })
  
  output$spin_Plot <- reactive( {
    if(input$spin_RunButton == 0) return()
    spin_data()
  })  # execute when run is clicked
  
  output$spin_Summary <- renderTable({
    if(input$spin_RunButton ==0) return()
    isolate({
      data.df <- spin_data()
      out1 <- summary(factor(data.df$pieLabels[unlist(data.df$drawColor)+1],
                             levels=data.df$pieLabels))
      if(input$spin_stopRule =="Fixed number of spins"){
        runs <-  rle(unlist(data.df$drawColor)[1:data.df$nDraws])
        out1 <-  c( out1, max(runs[[1]]))
        names(out1)[data.df$nCat + 1] <- "maxRunLength"
      } else{
        out1 <-  c( out1, length(data.df$drawColor))
        names(out1)[data.df$nCat + 1] <- "spins"
      }
      out1 <- t(as.table(out1))
      rownames(out1) <- " "
      out1
    })
  }, width = 120)
  
  ## run more:
  output$spin_Summry2 <- renderTable({
    rData <-  repData()
    as.table(matrix(c(quantile(rData,c(0,.25,.5,.75,1)), mean(rData), sd(rData)),nrow=1,
                    dimnames = list(" ", c("Min","Q1","Median","Q3","Max","Mean","SD") )))
  }, digits = 3)
  
  output$spin_Summry3 <- renderTable({
    ##isolate({
    counts  <- repData()
    if(input$spin_stopRule =="Fixed number of spins"){
      temp <- t(table(counts)) ## c(counts,0:input$spin_nCat))-1)
    } else if(input$spin_stopRule =="One spin in 1st category"){
      temp <-  t(table(c(counts, 1:max(counts)))-1)
    } else  if(input$spin_stopRule =="One of each type"){
      temp <-   t(table(c(counts, input$spin_nCat:max(counts)))-1)
    }
    #print(temp)
    rownames(temp) <- "Counts"
    temp
  }, digits = 0)
  
  output$spin_Histogrm <- renderPlot({
    ##isolate({
    x <- sort(repData())
    y <- unlist(tapply(x, x, function(z) 1:length(z)))
    if(input$spin_stopRule =="Fixed number of spins"){
      stat <-  input$spin_fn
      begin <- 0 + (input$spin_fn == spin_functionList[3]) 
      xlimits = range(x) ## c(begin, input$spin_nDraws)
    }
    if(substr(input$spin_stopRule,1,3) == "One") {
      stat = "Number of Spins"
      xlimits = range(x)
    }
    plot(jitter(x, .3), y, main = paste("Distribution of ", stat), xlab = "", cex=2, 
         ylab = "Frequency",  xlim=xlimits, ylim=c(.5, pmax(10, max(y))), pch = 16, col = blu)
    ##})
  })
} 
  
   ##  Mixer  ## ---------------------------------------------------------------
   {
   mix_functionList <-  c( "Count in Group 1","Count in Group 2", "Max Run Length","Matches")
     
     output$c1_mixerUI <- renderUI({
       div(  
         titlePanel("Mix It Up"),
         fluidRow(
           column(4, 
                  div(
                    tags$label("Number of Categories",       
                               tags$input(name='mix_nCat', type='number', value= 3, size=10,style="width: 40px" )),
                    tags$label("Category names: ", 
                               tags$input(name = "mix_categories", type = 'text', value = "A, B, C", size = 12,style="width: 90px" )),
                    h5("(separate with commas)"),
                    tags$label( "Numbers of balls: ", 
                                tags$input(name = "mix_counts",type = "text", value = "5, 3, 2",style="width: 80px" )),
                    h5("Separate with commas."),
                    tags$label( "Replace each ball? ", 
                                tags$select(id='mix_replace', class="form-control",
                                            tags$option( value = "Yes","Yes", selected = TRUE ),
                                            tags$option( value = "No", "No"))
                    ),
                    #hr() ,
                    tags$label("Stop after: ",
                               tags$div(style="width: 200px", 
                                        tags$select(id='mix_stopRule', class="form-control",
                                                    tags$option( value = "Fixed number of draws","Fixed number of draws", selected = TRUE ),
                                                    tags$option( value = "One in 1st category", "One in 1st category"),
                                                    tags$option( value = "One of each type", "One of each type"))
                               )),
                    conditionalPanel(
                      condition = "input.mix_stopRule =='Fixed number of draws'" ,
                      tags$label("Last Draw:",       
                                 tags$input(name='mix_nDraws', type='number', value= 5, size=10, style="width: 40px" ))
                    ),
                    tags$label("Number of Trials: ",       
                               tags$input(name='mix_reps', type='number', value= 1, size=10, style="width: 40px" )),
                    helpText("Start with 1 for an animation"),
                    conditionalPanel(
                      condition = "input.mix_reps != 1 && input.mix_stopRule =='Fixed number of spins'" ,
                      selectInput("mix_fn","Store what result?", mix_functionList)
                    ),
                    conditionalPanel(
                      condition = "input.mix_stopRule !='Fixed number of draws' && input.mix_reps != 1", 
                      helpText("Display shows number of draws needed" )
                    )
                  )), 
           column(1,
                  actionButton("mix_RunButton", "Run", class="btn btn-success")  
           ),
           column(6,
                  includeHTML("www/mix.js"),
                  reactiveSpin(outputId = "mix_Plot")
           ),
           fluidRow(
             column(10, offset = 1,
                    conditionalPanel(
                      condition = "input.mix_reps == 1",
                      tableOutput("mix_Summary")
                    ),
                    #hr() ,
                    conditionalPanel(
                      condition = "input.mix_reps != 1",
                      plotOutput("mix_Histogrm"),
                      tableOutput("mix_Summry2"),
                      tableOutput("mix_Summry3")
                    )
             )
           )
         )
       )
     })
     
     h <-  330
     mix_data <- reactive( {
       run = input$mix_RunButton
       counts <- sapply(strsplit(input$mix_counts, ","), as.integer)
       groups <- sapply(strsplit(input$mix_categories, ","), function(x)
         gsub("[[:space:]]", "", x))##[order(-prob)]
       nCat <-  length(groups)
       nBalls <-  sum(counts)
       prob <-  counts/nBalls
       ## make radius small enough so that only about half of the
       ##  (h/r)^2 points are utilized
       radius <-  pmin(16, round( h/2 / sqrt(2 * nBalls)))
       gridpoints <- expand.grid( x = seq( 0, h, 2*radius),
                                  y = seq( 0, h, 2*radius))
       gridpoints <- subset( gridpoints, sqrt((x-h/2)^2 + (y-h/2)^2) < h/2 - radius)
       balls <- sample( rep(groups, counts))
       
       ballNums <- as.numeric(factor(balls, levels=groups)) 
       sampleLocs <- gridpoints[sample(length(gridpoints$x), nBalls),]

       ### Fixed number of Draws
       if(input$mix_stopRule == "Fixed number of draws"){
         nDraws <- as.numeric(input$mix_nDraws)
         if(input$mix_replace == "Yes"){
           ## don't need prob, just sample from counted balls
           draws <- sample(1:nBalls, nDraws, replace=TRUE) - 1
           ## zero-initialized indices
         } else {  ## no replacement, use sampled order
           draws <-  1:nDraws - 1
         }  
       } else
         ### Stop on first category
         if(input$mix_stopRule == "One draw in 1st category"){
           if(input$mix_replace == "Yes"){
             drawSumry <- draws2get1( prob, 1)
             if(drawSumry == 1){
               draws <- sample(which(ballNums == 1),1) - 1
               nDraws <- 1
             } else{
               drawGrp <- reconstructSpins( drawSumry, prob) 
               ##  gives category. need to convert to a ball of that category
               draws <-  sapply(drawGrp, function(x) sample(which( ballNums == x),1)) - 1
               
               nDraws <-  drawSumry}
           } else {  ## don't replace balls drawn
             nDraws <- min(which( ballNums == 1))
             if( nDraws > 1){
               draws <-  1:nDraws - 1
             } else  draws = 0 ## stop on first pick
           }
         } else{
           ## Stop after one of each type
           if(input$mix_replace == "No"){
             nUnique <-  sapply(nCat:nBalls, function(x) length(unique(ballNums[1:x])))
             nDraws <- min(which(nUnique == nCat)) + nCat - 1
             draws <-  1:nDraws - 1
           } else {
             ## do replace each drawn ball  (replace == "Yes")
             drawSumry <- draws2get1ofEach( prob, 1, full=TRUE)
             nDraws <-  drawSumry$nDraws
             drawGrp <- reconstructSpins( drawSumry, prob)
             names(drawGrp) <-  1:nDraws
             ##  gives category. need to convert to a ball of that category
             draws <-  sapply(drawGrp, function(x) which( ballNums  == x)[1]) - 1
             names(draws) <-  NULL
           }
         }
       ##cat("# draws: ", nDraws, " draw: ",draws," balls: ",balls[draws] ,"\n")
       if (length(draws) == 1) draws <-  as.list(draws)
       data.df <- list(height = h,
                       run = input$mix_RunButton,
                       nCat = nCat,
                       counts = as.list(counts),
                       nBalls = nBalls, 
                       radius = radius,
                       nDraws = nDraws,
                       x = as.list(sampleLocs$x - h/2), ## sample from a grid of points
                       y = as.list(sampleLocs$y - h/2 - radius) ,
                       labels = groups,
                       replace = input$mix_replace,
                       balls = balls,
                       draws = draws,
                       drawColor = ballNums - 1
                       )
       data.df
} )
     

  mix_repData <- reactive( {
       run = input$mix_RunButton
       counts <- sapply(strsplit(input$mix_counts, ","), as.integer)
       groups <- sapply(strsplit(input$mix_categories, ","), function(x)
         gsub("[[:space:]]", "", x))##[order(-prob)]
       nCat <-  length(groups)
       nBalls <-  sum(counts)
       prob <-  counts/nBalls
       nReps <-  ifelse(is.null(input$mix_reps), 1, as.numeric(input$mix_reps))
       fixedN <- (input$mix_stopRule == "Fixed number of draws")
       if(fixedN){
         fn <- match(input$mix_fn, mix_functionList) 
         nDraws <- as.numeric(input$mix_nDraws)
         if(input$mix_replace == "No"){
           samplData <- t(sapply(1:nReps, function(x) sample(rep(groups,counts))[1:nDraws]))
           ## not working ##
         } else {
           samplData <- matrix(sample(1:input$mix_nCat, nReps * nDraws,
                                      prob = counts/nBalls, replace = TRUE),
                               ncol=nDraws)
         }
         if(fn == 3){
           return(apply(samplData, 1 , function(x)  max(rle(x)[[1]])))
         } else if(fn ==4){
           return(apply(samplData, 1 , function(x) sum( x == groups[1:length(x)])))
         } else
           return(apply(samplData, 1 , function(x) table(x)[fn]))
       } else {
         ## apply stopping rule
         ##if(input$mix_replace == "No" ){
         ##  warning("Please choose to replace each draw.")
         ##}
         if(input$mix_stopRule == "One draw in 1st category"){
           if(input$mix_replace == "No" ){
             nDraws <- sapply(1:nReps, function(x) min(which(sample(rep(groups,counts), nBalls) == groups[1])))
             ##drawColor <- reconstructSpins(nDraws[1], counts/nBalls ) - 1
             return(nDraws)
           } else{  ## input$mix_replace =="Yes"
             nDraws <- draws2get1( counts/nBalls, nReps)
             ##drawColor <- reconstructSpins(  nDraws[1], counts/nBalls ) - 1
             return(nDraws)
           }
           ##  gives random draw in 1st category for last draw,
           ## in other categories for prior draws.
         } else{ ## one of each type
           if( input$mix_replace =="No"){
             tempDraws <- sapply(1:nReps, function(x) sample(rep(groups, counts)))
             nUnique <-  tempDraws[1:(nBalls - nCat),]
             if(nCat > nBalls)
               stop("Need more balls to draw from")
             nDraws <- apply(tempDraws, 2, function(x) max(which(!duplicated(x))))
             return(nDraws)
           } else { ## input$replace =="Yes"
             nDraws <- draws2get1ofEach(prob, rep=nReps)
             return(nDraws)
           }
         }
       }
  })
     
  output$mix_Plot <- reactive( {
    if(input$mix_RunButton ==0) return()
    isolate( mix_data() )
  }) 
  
  output$mix_Summary <- renderTable({
    if(input$mix_RunButton == 0) return()
    isolate({
      data.df <- mix_data()
      out1 <- summary(factor(data.df$balls[unlist(data.df$draws)+1],
                                levels=data.df$labels))
      if(input$mix_stopRule =="Fixed number of draws"){
        runs <-  rle(data.df$balls[unlist(data.df$draws) + 1])
        out1 <-  c( out1, max(runs[[1]]))
        names(out1)[data.df$nCat + 1] <- "maxRunLength"
      } else{
        out1 <-  c( out1, length(data.df$draws))
        names(out1)[data.df$nCat + 1] <- "draws"
      }
      out1 <- t(as.table(out1))
      rownames(out1) <- " "
      out1
    })
  }, digits = 0)
  
  ## run more:
  output$mix_Summry2 <- renderTable({
    rData <-  mix_repData()
    as.table(matrix(c(quantile(rData,c(0,.25,.5,.75,1)), mean(rData), sd(rData)),nrow=1,
                     dimnames = list(" ", c("Min","Q1","Median","Q3","Max","Mean","SD") )))
  })
  
  output$mix_Summry3 <- renderTable({
    ##isolate({
    counts  <- mix_repData()
    if(input$mix_stopRule =="Fixed number of draws"){
      temp <- t(table(counts))
    } else if(input$mix_stopRule =="One draw in 1st category"){
      temp <-  t(table(c(counts, 1:max(counts)))-1)
    } else  if(input$mix_stopRule =="One of each type"){
      temp <-   t(table(c(counts, input$mix_nCat:max(counts)))-1)
    }
    rownames(temp) <- "Counts"
    temp
    ##})
  }, digits = 0)
  

       output$mix_Histogrm <- renderPlot({
       ##isolate({
       x <- sort(mix_repData())
       y <- unlist(tapply(x, x, function(z) 1:length(z)))
       if(input$mix_stopRule =="Fixed number of draws"){
         stat <-  input$mix_fn
         begin <- 0 + (input$mix_fn == mix_functionList[3]) 
         xlimits = range(x) ## c(begin, input$spin_nDraws)
       }
       if(substr(input$mix_stopRule,1,3) == "One") {
         stat = "Number of Draws"
         xlimits = range(x)
       }
       plot(jitter(x, .3), y, main = paste("Distribution of ", stat), xlab = "", 
            ylab = "Frequency",  xlim=xlimits, ylim = c(.5, pmax(10, max(y))), 
            pch = 16, col = blu,cex  = 2)
   })
 } 
   
  ## Normal probability computations  ----------------------- cat 1
{
  ##  Set storage for reactive values

cat1_normalProb <- reactiveValues(prob = NULL, z = NULL, findP = NULL)
  
  observeEvent( input$cat1_z_txt, {
    cat1_normalProb$z <- as.numeric(input$cat1_z_txt) 
    cat1_normalProb$findP <- TRUE
  })

 observeEvent( input$cat1_prob_txt,{
    cat1_normalProb$prob <- as.numeric(input$cat1_prob_txt) 
    cat1_normalProb$findP  <- FALSE
  })

output$normalProbPlot1 <- renderPlot({ 
  req(cat1_normalProb$findP)

  par(mar=c(24,1,1,1)/10)
  z <- absz <- prob <- yrr <- xrr <- NA
  x <- -300:300 / 50

  if(!cat1_normalProb$findP & !is.na(cat1_normalProb$prob)){
    ## given prob, find z
    prob <- cat1_normalProb$prob
    #cat("finding z for p = ", prob, "\n")
    if(input$cat1_area == "Lower"){ ##  left tail
      z <-  qnorm(prob)
      if(z < min(x))  x <- c(1.01 * z, x)
      ## rejection region in x dimension
      xrr <- c(x[x < z], z, z)
      ## density curve over xrr:
      yrr <- c( dnorm(xrr[-length(xrr)]), 0)
    } else if(input$cat1_area == "Upper"){   ## right tail        
      z <- qnorm(1 - prob) 
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(z, z, x[x > z])
      yrr <- c(0, dnorm(xrr[-1]))
    } else if(input$cat1_area == "Center"){
      z <- abs(qnorm( (1 - prob)/2) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- seq(-z, z, length=100)
      yrr <- c(0, dnorm(xrr[2:99]), 0)
    } else if(input$cat1_area == "Extremes"){
      z <- abs(qnorm(1-prob/2) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(-z, x[x < -z], -z)
      yrr <- c(0,  dnorm(xrr[-1]))
    }
    absz <- abs(z)
  }
  if(cat1_normalProb$findP & !is.na(cat1_normalProb$z)){
    z <- cat1_normalProb$z
    ##  find probability
    absz <- abs(z)
    maxX <- pmax(z, 6)
    minX <- pmin(z, -6)
     # cat("finding p ", minX, maxX, "for z = ", z, "\n")
    x <- seq(minX, maxX, length=200)
    prob <-  1 - pnorm(z) 
    xrr <- c(z, z, x[x > z])  ## right tail
    yrr <- c(0,  dnorm(xrr[-1]) )
    if(input$cat1_area == "Lower"){         ##  left tail
      prob =  pnorm(z) 
      xrr <- c(z, x[x < z], z)
      yrr <- c(0,  dnorm(xrr[-1]))
    } else if (input$cat1_area == "Extremes"){ ##  extremes
      xrr <- c( -absz, x[x < -absz], -absz)
      yrr <- c(0, dnorm(xrr[-1]))
      prob = pnorm(-absz) 
      #yrr <- c(yrr, NA,NA, rev(yrr))
      #xrr <- c(xrr, NA,NA, rev(-xrr))
    } else if (input$cat1_area == "Center"){   ##  center
      prob <- cat1_normalProb$prob
      xrr <- seq(-absz, absz, length=100)
      yrr <- c(0, dnorm(xrr), 0)
      xrr <- c(-absz, xrr, absz)
      prob <- diff( pnorm(c(-absz,absz)) )
    }
  } 
  plot(x, dnorm(x), type = "l", bty='n', xlab="Z Score", ylab="", yaxt="n")
  abline(h=0)
  max.height <- dnorm(0) *.9
  text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
  segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
  
  if(input$cat1_area == "Extremes") {  ## extremes
    polygon(xrr, yrr, col = rd)
    polygon(-xrr, yrr, col = rd)
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
         round(prob * ifelse(cat1_normalProb$findP,1,.5), 3), col = "darkblue")
    place.x <- c(-absz, absz)
    if(!is.na(absz) & (absz < 1)) place.x <- place.x/absz * .8
    text(place.x, y = text.height, round(c(-absz,absz),3))
  } else if (input$cat1_area == "Center") {   ## fill & label center
    polygon(xrr, yrr, col = grn)
    text(x=0, y= text.height, round(prob,3))
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y=text.height, round(c(-absz,absz),3))
  } else {          ## show tails
    polygon(xrr, yrr, col = rd)
    text( z, y = text.height, round(z, 3))
    text(x= sign(z) * (absz+4) / 2 , y= max(yrr) / 2 + 0.02, 
         round(prob,3), col = "darkblue")
  }
  
}, height=300)
}
  
  ## 1 Quantitative -----------------------------------------------------------  -- 1 Quant 
  
  q1Test <- reactiveValues(shuffles = NULL, new.xbars = NULL, xbar = NULL, nsims = 0,
                           colors = NULL, moreExtremeCount = NULL, pvalue = NULL, direction = NULL, 
                           cutoff = NULL, sampleCount = NULL)
  
  q1Estimate <- reactiveValues(shuffles = NULL, xbars = NULL, observed = NULL, 
                               confLevel = NULL, colors = NULL, colors = NULL, CI = NULL)
  

  observeEvent(input$q1_InputToggle, {
    shinyjs::show("q1Data")            ##  show data input
    shinyjs::disable("q1_EstimateToggle") ##  disable  Estimate btn
    shinyjs::hide("q1Estimate")         ## hide Estimate page
    shinyjs::hide("q1Test")             ## hide Test page
    shinyjs::disable("q1_TestToggle")   ## disable Test btn
  })
  
  observeEvent(input$q1_TestToggle, {
    shinyjs::hide("q1Data")             ##  hide data input
    shinyjs::enable("q1_InputToggle")   ## disable Input button
    shinyjs::hide("q1Estimate")         ## hide Estimate page
    shinyjs::show("q1Test")             ## show Test page
  })
  
  observeEvent(input$q1_EstimateToggle, {
    shinyjs::hide("q1Data")             ##  hide data input
    shinyjs::enable("q1_InputToggle")   ## disable Input button
    shinyjs::hide("q1Test")             ## hide Test page
    shinyjs::show("q1Estimate")             ## show Estimate page
  })
  
  output$q1_triplePlay <- renderUI({
    fluidPage(
      fluidRow(
        column(3, offset = 1, 
               actionButton("q1_InputToggle", "Input Data", class="btn btn-primary")),
        column(3, offset = 1, 
               actionButton("q1_EstimateToggle", "Estimate", class="btn btn-primary", disabled=TRUE)),
        column(3, offset = 1, 
               actionButton("q1_TestToggle", "Test", class="btn btn-primary", disabled = TRUE))
      ),
      div( id ="q1Data", width = "500px",   
           fluidRow(  
                column(6, 
                       h5("How would you like to input the data?"),
                       selectInput('q1_entry', ' ', 
                                    list(" ", "Pre-Loaded Data","Local CSV File",
                                                        "Type/Paste into Text Box"), #"Type/Paste into Data Sheet"), 
                                    selected = " ",
                                   selectize = FALSE, width = "200px"))
           ),
           uiOutput("q1_inputUI"),
           hr(),
          fluidRow(
              column(6, 
                      plotOutput('q1_Plot', height = "320px") ),
              column(3, 
                    tableOutput('q1_Summary'))
          )
      ),  ## close Input div
      
      div( id = "q1Test", style = "display: none;", 
             fluidPage( 
               fluidRow( 
                 column(4,  
                        h4("Test for a single mean.")
                 ),
                 column(8, 
                        tags$label(HTML("True Mean (Null hypothesis for &mu;):"),  
                                   tags$input(name='null_mu', type='text', value='0', size='10'))
                 )
               ),
               fluidRow( 
                 column(4,  
                        plotOutput("q1_TestPlot1", height = "280px")
                 ),
                 column(8, 
                        plotOutput('q1_TestPlot2', click = 'q1_Test_click', height = '300px')
                 )
               ),
               br(),
               fluidRow(
                 column(5, offset = 1, h4("How many more (shifted) resamples?")),
                 column(1, actionButton("q1_test_shuffle_1", label = "1", class="btn btn-primary")),
                 column(1, actionButton("q1_test_shuffle_10", label = "10", class="btn btn-primary")),
                 column(1, actionButton("q1_test_shuffle_100", label = "100", class="btn btn-primary")),
                 column(1, actionButton("q1_test_shuffle_1000", label = "1000", class="btn btn-primary")),
                 column(1, actionButton("q1_test_shuffle_5000", label = "5000", class="btn btn-primary"))
               ),
               
               br(),
               #         fluidRow(
               #            column(5, offset =6, h4("Click on a point to see that resample."))
               #          ),
               fluidRow(
                 column(8, offset = 1,
                        uiOutput("q1TestXtremes"),
                        uiOutput("q1TestPvalue")
                 )
               )
             )
      ),                                                  ### close q1-testing div
      div( id = "q1Estimate", style = "display: none;", 
             h3("Estimate a single mean."),
             fluidRow(
               column(4,
                      plotOutput("q1_EstPlot1")
               ),
               column(8, 
                      plotOutput('q1_EstimatePlot2', click = 'q1_Estimate_click')
               )
             ),             
             fluidRow(
               column(4, offset = 2, h4("How many more resamples?")),
               column(1,
                      actionButton("q1_resample_10", label = "10", class="btn btn-primary")),
               column(1,
                      actionButton("q1_resample_100", label = "100", class="btn btn-primary")),
               column(1,
                      actionButton("q1_resample_1000", label = "1000", class="btn btn-primary")),
               column(1,
                      actionButton("q1_resample_5000", label = "5000", class="btn btn-primary"))
             ),
             br(),
             br(),
             fluidRow(
               column(4, offset = 3, 
                      h4("Select Confidence Level (%)")
               ),
               column(5,
                      fluidRow(
                        column(2, actionButton('q1_conf80', label = "80", class="btn btn-primary")),
                        column(2, actionButton('q1_conf90', label = "90", class="btn btn-primary")),
                        column(2, actionButton('q1_conf95', label = "95", class="btn btn-primary")),
                        column(2, actionButton('q1_conf99', label = "99", class="btn btn-primary"))
                      ))
             ),
             if(!is.null(q1Estimate$CI)){
               fluidRow( 
                 column(7, offset = 5,
                        h4(paste(q1Estimate$confLevel*100, "% Interval Estimate: (", round(q1Estimate$CI[1],3), ",", 
                                 round(q1Estimate$CI[2], 3), ")"))
                 ))
             }
      )     
    )    ## close q1_triplePlay UI
  })  
  

##  Enter data    ----------------------------------------- quant 1
 {

# output$quant1DataIn <- renderText({ "How would you like to input the data? " 
#  })

 ## user selects an input method.
  ## renderUI changes to get appropriate inputs.

 output$q1_inputUI <- renderUI({
  req(input$q1_entry)

  switch( input$q1_entry,
          "Pre-Loaded Data" ={ 
            fluidRow(  
              column(4, selectInput('q1_data1', 'Available Datasets',  
                                    choices = as.list(quant1_contents))
                     ),
              column(4, actionButton("q1_useLddBtn", "Use These Data", class="btn btn-primary") )
            )
          },
          "Local CSV File" ={
            ## copied from: http://shiny.rstudio.com/gallery/file-upload.html    
            fluidRow(  
              column(4, fileInput('q1_file1', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')) ,
                     br(),
                     actionButton("q1_useCSVBtn", "Use These Data", class="btn btn-primary")
              ),
              column(3, checkboxInput('q1_header', 'Row One is column names', TRUE)
              ),
              column(3, radioButtons('q1_sep', 'Separator',
                                     c(Comma=',', Semicolon=';', Tab='\t'),
                                     ',')
              ),
              column(2, radioButtons('q1_quote', 'Quote',
                                     c(None='', 'Double Quote'='"','Single Quote'="'"),
                                     '"')
              )
            )
            
          },
          "Type/Paste into Text Box" = {
            div(
              HTML('<textarea name="q1_text" cols="30" rows="10"></textarea>'),
              actionButton("q1_useText", "Use These Data", class="btn btn-primary")
            )
           },
        NULL
      )
 })
 
 
                 
 ##  grab data according to input method
 q1 <- reactiveValues(data = NULL, names = NULL)
 
 observeEvent(  input$q1_useLddBtn, {
   DF <- eval(parse( text = input$q1_data1))
   q1$data <- DF
   #print(q1$data)
   q1$names <- names(DF)
   q1Test$nsims <- 0
   q1Test$shuffles <-  q1Test$new.xbars <-  q1Test$xbar <- q1Test$colors <- NULL
   q1Estimate$shuffles <- q1Estimate$xbars <- q1Estimate$observed <- q1Estimate$confLevel <- q1Estimate$colors <- NULL
#    output$quant1DataIn <- renderText({
#           "Data are entered, you may now choose to estimate or test one mean"
#    })
   shinyjs::enable("q1_EstimateToggle") ## enable Estimate
   shinyjs::enable("q1_TestToggle")     ## enable Test 
   shinyjs::disable("q1_InputToggle")   ## disable Input button
 })

 observeEvent(  input$q1_useCSVBtn,{
   DF <-  read.csv(input$q1_file1$datapath, header=input$q1_header, sep=input$q1_sep, quote=input$q1_quote)
   q1$names <- if(is.null(names(DF)) | "V1" %in% names(DF)){ "x"} else {names(DF)[1]}
   q1$data <- data.frame(DF[, 1])
   #print(q1$data)
   q1Test$nsims <- 0
   q1Test$shuffles <-  q1Test$new.xbars <-  q1Test$xbar <-  q1Test$colors <- NULL
   q1Estimate$shuffles <- q1Estimate$xbars <- q1Estimate$observed <- q1Estimate$confLevel <- q1Estimate$colors <- NULL
#    output$quant1DataIn <- renderText({
#      "Data are entered, you may now choose to estimate or test one mean"
#    })
   shinyjs::enable("q1_EstimateToggle") ## enable Estimate
   shinyjs::enable("q1_TestToggle")     ## enable Test 
   shinyjs::disable("q1_InputToggle")   ## disable Input button
 })

#  observeEvent(input$q1_useHotBtn,{
#    DF = data.frame(x=as.numeric(q1_values[["hot"]][,2]))
#    # print(DF)
#    q1$names <- names(DF)
#    q1Test$nsims <- 0
#    q1$data <- data.frame( x = as.numeric(unlist(DF)))
#    q1Test$shuffles <-  q1Test$new.xbars <-  q1Test$xbar <-   q1Test$colors <- NULL
#    q1Estimate$shuffles <- q1Estimate$xbars <- q1Estimate$observed <- q1Estimate$colors <- NULL
#    #print(q1$data)
#    output$quant1DataIn <- renderText({
#      "Data are entered, you may now choose to estimate or test one mean"
#    })
#  })

 
   observeEvent(input$q1_useText,{
    if(nchar(input$q1_text) < 1){
      return()
    }
    #print(input$q1_text)
    if (grepl(",", input$q1_text)){          ## check for commas,
      q1Text <- gsub(","," ",input$q1_text)  ## replace commas with spaces
    } else {
      q1Text <- input$q1_text
    }
    tempData <- scan(text = q1Text, what = "a", quiet = TRUE) ## read in as text
    # print(tempData)
    if(is.na(as.numeric(tempData[1]))){         # is the first "word" numeric or character?
      q1$names <- tempData[1]                   # character becomes name of this column
      tempData <- as.numeric(tempData[-1])      
    } else{
      q1$names <- "x"                           # numeric, so name it "x"
      tempData <- as.numeric(tempData)
    }
    q1Test$nsims <- 0
    q1$data <- data.frame( x = tempData)
    q1Test$shuffles <-  q1Test$new.xbars <-  q1Test$xbar <-   q1Test$colors <- NULL
    q1Estimate$shuffles <- q1Estimate$xbars <- q1Estimate$observed <- q1Estimate$colors <- NULL
    #print(q1$data)
#     output$quant1DataIn <- renderText({
#       "Data are entered, you may now choose to estimate or test one mean"
#     })
    shinyjs::enable("q1_EstimateToggle") ## enable Estimate
    shinyjs::enable("q1_TestToggle")     ## enable Test 
    shinyjs::disable("q1_InputToggle")   ## disable Input button
  })
 
   q1_values = list()
  # q1_setHot = function(x) q1_values[["hot"]] <<- x
  # q1_setHot(read.csv("data/dummyData.csv", stringsAsFactors = FALSE, head = TRUE) )

#   output$q1_hot = renderRHandsontable({
#     if (!is.null(input$q1_hot)) {
#       q1_DF = hot_to_r(input$q1_hot)
#       q1_setHot(q1_DF)
#       rhandsontable(q1_DF) %>%
#         hot_table(highlightCol = TRUE, highlightRow = TRUE, copyPaste = TRUE, pasteMode = "shift_down")
#     } else {
#       ## seems that HOT needs at least 2 columns, so column 1 is just row numbers.
#       q1_DF = read.csv("data/dummyData.csv", stringsAsFactors = FALSE, head = TRUE)
#       #cat("loading \n")
#       #print(q1_DF)
#       q1_setHot(q1_DF)    
#       rhandsontable(q1_values[["hot"]], height = 200) %>%
#         hot_table(highlightCol = TRUE, highlightRow = TRUE)
#     }
#   })



}

  ##  Describe and plot data -------------------------------  quant 1
{
  output$q1_Plot <- renderPlot( {
    req(q1$data)

    DF <- q1$data
    names(DF)[ncol(DF)] <- "x"
    ## make plot
    q1_plot1 <- 
      qplot(x = x, y=x, data = DF,  geom ="boxplot") + theme_bw() + xlab("") + ylab(q1$names) + 
                  scale_x_continuous(breaks = c(-1,1000)) +  coord_flip()
    #par(mar=c(24, 40, 10, 35)/10, mfrow=c(2,1))
    #boxplot(q1_dataDF, horizontal = TRUE, main = "")
    # Plot stacked x values. 
    x <- sort(q1$data[,1])
    ## print(x)
    #z <- cut(x, breaks = nclass.Sturges(x) ^2 )
    w <- newy(x)  #unlist(tapply(x, z, function(x) 1:length(x)))
    tempDF <- data.frame(x, w=w[!is.na(w)])
    myBlue <- rgb(0, 100/256, 224/256, alpha = .8)  
    q1_plot2 <- qplot(data=tempDF, x=x, y=w, colour = I(myBlue), ylim = c(0, pmax(10, max(w))), size = I(4)) + 
      theme_bw() + xlab(q1$names)
    grid.arrange(q1_plot1, q1_plot2, heights = c(1,3)/4, ncol=1)
  #})
}, height=320, width = 340)

  output$q1_Summary <- renderTable({
    req(q1$data)  

    #isolate({
      #q1_dataDF <- q1_data()
      ## print(q1_dataDF)
      DF <- rbind(mean = mean(q1$data[, 1], na.rm = TRUE ),
                   sd = sd(q1$data[, 1], na.rm = TRUE),
                   min = min(q1$data[, 1]),
                   Q1 = quantile(q1$data[, 1], .25),
                   median = median(q1$data[, 1]),
                   Q3 = quantile(q1$data[, 1], .75),
                   max = max(q1$data[, 1]),
                   n = length(q1$data[, 1]))
      colnames(DF) <- q1$names
      DF
    #}
  }, digits = 3)
}


  ###  test 1 mean value  ------------------------------------ quant 1
{

## --------- 1 quant UI ---------------------------


output$q1_testUI <- renderUI({ 
  if(is.null(q1$data)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else{  
  fluidPage( 
    fluidRow( 
      column(4,  
             h4("Test for a single mean.")
      ),
      column(8, 
             tags$label(HTML("True Mean (Null hypothesis for &mu;):"),  
                        tags$input(name='null_mu', type='text', value='0', size='10'))
      )
    ),
    fluidRow( 
      column(4,  
             plotOutput("q1_TestPlot1", height = "280px")
       ),
      column(8, 
               plotOutput('q1_TestPlot2', click = 'q1_Test_click', height = '360px')
              )
        ),
       br(),
       fluidRow(
           column(5, offset = 1, h4("How many more (shifted) resamples?")),
           column(1, actionButton("q1_test_shuffle_1", label = "1", class="btn btn-primary")),
           column(1, actionButton("q1_test_shuffle_10", label = "10", class="btn btn-primary")),
           column(1, actionButton("q1_test_shuffle_100", label = "100", class="btn btn-primary")),
           column(1, actionButton("q1_test_shuffle_1000", label = "1000", class="btn btn-primary")),
           column(1, actionButton("q1_test_shuffle_5000", label = "5000", class="btn btn-primary"))
         ),
             
        br(),
#         fluidRow(
#            column(5, offset =6, h4("Click on a point to see that resample."))
#          ),
        fluidRow(
           column(8, offset = 1,
             uiOutput("q1TestXtremes"),
             uiOutput("q1TestPvalue")
         )
       )
  )

  }
})
  
observeEvent( input$null_mu, {
  q1Test$pvalue <- q1Test$moreExtremeCount <- NULL
  q1Test$colors <- rep(blu, length(q1Test$colors))
})


output$q1TestPvalue <- renderUI({
  req(q1Test$moreExtremeCount)
    h4(pvalue2print(q1Test$moreExtremeCount,  q1Test$sampleCount, q1Test$direction, q1Test$cutoff, q1Test$pvalue))
})

output$q1TestXtremes <- renderUI({
  fluidRow(
    column(4, 
           h4('Count values equal to or ')
    ),
    column(3,
           tags$div(style="width: 160px",
              ##tags$label( "Count values equal to or"),      
               tags$select( id='q1_testDirection', class="form-control",
                      tags$option( value = "less", "less"),
                      tags$option( value = "more extreme", "more extreme", selected = TRUE),
                      tags$option( value = "greater", "greater"))
           )
    ),
    column(1, h4("than ")),
    column(2,
          tags$div( 
             tags$input(id = "q1_test_cutoff", type = "text", class = "form-control", value = NA))
    ),
    column(1,
           actionButton("q1_countXtremes","Go", class="btn btn-success")
    )
  )
})
 

# ----------------------1 quant test plots ------------------------------------------------------------

 
output$q1_TestPlot1 <- renderPlot({
  ## first plot observed data:
  
  q1Test$xbar <- mean(q1$data[,1])
  nullMean <- as.numeric(input$null_mu)
  ## q1Test$mu_diff <- q1Test$xbar  
  nn <- nrow(q1$data)
  par(mfrow = c(2,1), mar = c(4,3.5,3,1))
  ## Plot Original Data
  x <- sort(q1$data[,1])
  #z <- cut(x, breaks = nclass.Sturges(x) ^2 )
  w <- newy(x) #unlist(tapply(x, z, function(x) 1:length(x)))
  tempDF <- data.frame(x=x, w=w[!is.na(w)])
  plot(w ~ x, data = tempDF, col = blu, pch = 16, main = "Original Data", xlab = q1$names, ylab = "Count")
  legend("topleft", bty = "n", paste(" n = ",nn,"\n Mean = ", round(mean(x),3), "\n SD = ", round(sd(x),3)))
  
  ## 2nd plot:  One Shuffle of Shifted Data
   if(is.null(q1Test$new.xbars) | length(q1Test$new.xbars) < 1){
    ## 1st time round shuffle is done here
    shuffle <- sample(x = q1$data[,1] - q1Test$xbar, length(q1$data[,1]), replace = TRUE)
    q1Test$shuffles <- as.matrix(shuffle)
    q1Test$new.xbars <- mean(shuffle)
    q1Test$colors <- blu
    ### stores samples as columns
    #print(q1Test$shuffles)
    DF0 <- sort(shuffle)
  }  else   if(!is.null(input$q1_Test_click)){
     ##  We already have shuffled data and want to pick the clicked point
     ##  Change to data related to a clicked point.
     closestPoint <- which.min(abs( nullMean +  q1Test$new.xbars - input$q1_Test_click$x))
     DF0 <- sort(q1Test$shuffles[,closestPoint] ) 
  }  else { 
    return()
    }

  if(is.null(DF0)){
    return()
  }
  z0 <- cut(DF0, breaks = nclass.Sturges(DF0) ^2 )
  w0 <- unlist(tapply(DF0, z0, function(DF0) 1:length(DF0)))
  tempDF0 <- data.frame(DF0 = DF0 + nullMean, w0=w0[!is.na(w0)])
  plot(w0 ~ DF0, data = tempDF0, col = blu, pch = 16, main = "One Shifted Resample", xlab = q1$names, ylab = "Count")
  legend("topleft", bty = "n", paste(" n = ",nn,"\n Mean = ", round(mean(DF0)+ nullMean,3), "\n SD = ", round(sd(DF0),3)))

},  height = 360, width = 300)

# output$q1_TestPrep2 <- renderTable({
#   if( is.null(q1$data))  return()
#   DF <- rbind(mean = mean(q1$data[, 1], na.rm = TRUE ),
#               sd = sd(q1$data[, 1], na.rm = TRUE),
#               n = length(q1$data[,1]))
#   colnames(DF) <- q1$names
#   DF
# })
# 
# output$q1_TestTable1 <- renderTable({
#   if( is.null(q1$data))  return()
#   DF <- rbind(mean = mean(q1Test$shuffles[,1], na.rm = TRUE ),
#               sd = sd(q1Test$shuffles[,1], na.rm = TRUE),
#               length = length(q1Test$shuffles[,1]))
#   colnames(DF) <- q1$names
#   DF
# })

observeEvent(input$q1_test_shuffle_10, {
  q1Test$moreExtremeCount <- NULL
  newShuffles <- sapply(1:10, function(x) sample(q1$data[,1] - q1Test$xbar, length(q1$data[,1]), replace = TRUE))
  q1Test$shuffles <- cbind(q1Test$shuffles, newShuffles)
  q1Test$new.xbars <- c(q1Test$new.xbars, apply(newShuffles, 2, function(x) mean(x)))
  #print(q1Test$new.xbars)
  q1Test$colors <- rep(blu, length(q1Test$new.xbars))
})

observeEvent(input$q1_test_shuffle_1, {
  q1Test$moreExtremeCount <- NULL
  newShuffles <- sample(q1$data[,1] - q1Test$xbar, length(q1$data[,1]), replace = TRUE)
  q1Test$shuffles <- cbind(q1Test$shuffles, newShuffles)
  q1Test$new.xbars <- c(q1Test$new.xbars, mean(newShuffles))
  #print(q1Test$new.xbars)
  q1Test$colors <- rep(blu, length(q1Test$new.xbars))
})

observeEvent(input$q1_test_shuffle_100, {
  q1Test$moreExtremeCount <- NULL
  newShuffles <- sapply(1:100, function(x) sample(x = q1$data[,1] - q1Test$xbar, 
                                                   length(q1$data[,1]), replace = TRUE))
  #  print(dim(newShuffles))
  q1Test$shuffles <- cbind(q1Test$shuffles, newShuffles)
  q1Test$new.xbars <- c(q1Test$new.xbars, apply(newShuffles, 2, function(x) mean(x)))
  q1Test$colors <- rep(blu, length(q1Test$new.xbars))
  
})
observeEvent(input$q1_test_shuffle_1000, {
  q1Test$moreExtremeCount <- NULL
  newShuffles <- sapply(1:1000, function(x) sample(x = q1$data[,1] - q1Test$xbar, 
                                                   length(q1$data[,1]), replace = TRUE))
  # print(dim(newShuffles))
  q1Test$shuffles <- cbind(q1Test$shuffles, newShuffles)
  q1Test$new.xbars <- c(q1Test$new.xbars, apply(newShuffles, 2, function(x) mean(x)))
  q1Test$colors <- rep(blu, length(q1Test$new.xbars))
  
})
observeEvent(input$q1_test_shuffle_5000, {
  q1Test$moreExtremeCount <- NULL
  newShuffles <- sapply(1:5000, function(x) sample(x = q1$data[,1] - q1Test$xbar, 
                                                   length(q1$data[,1]), replace = TRUE))
  #print(dim(newShuffles))
  q1Test$shuffles <- cbind(q1Test$shuffles, newShuffles)
  q1Test$new.xbars <- c(q1Test$new.xbars, apply(newShuffles, 2, function(x) mean(x)))
  q1Test$colors <- rep(blu, length(q1Test$new.xbars))
})

observeEvent(input$q1_countXtremes, {
  parm <- sort(as.numeric(q1Test$new.xbars)) + as.numeric(input$null_mu)
  nsims <- length(parm)
  mu0 <- as.numeric(input$null_mu)
  q1Test$colors <- rep(blu, nsims)
  q1Test$cutoff <- threshold <- as.numeric(input$q1_test_cutoff)
  q1Test$direction <- input$q1_testDirection
  if(nsims > 9 & !is.na(input$q1_testDirection)){
    redValues <-  switch( input$q1_testDirection,
                          "less" = which(parm <= threshold + 1.0e-10),
                          "greater" = which(parm >= threshold - 1.0e-10),
                          "more extreme" = which(abs(parm - mu0) > abs(threshold - mu0) - 1.0e-10 ))
    q1Test$colors[redValues] <- rd
    #print(q1Test$new.xbars[redValues])
    q1Test$moreExtremeCount  <- length(redValues)
    q1Test$pvalue <- q1Test$moreExtremeCount/nsims
    q1Test$sampleCount <- length(q1Test$new.xbars)
  }
})

observeEvent( input$q1_testDirection, {
  q1Test$pvalue <- q1Test$moreExtremeCount <- NULL  
  q1Test$colors <- rep(blu, length(q1Test$colors))
})

observeEvent( input$q1_test_cutoff, {
  q1Test$pvalue <- q1Test$moreExtremeCount <- NULL  
  q1Test$colors <- rep(blu, length(q1Test$colors))  
})


output$q1_TestPlot2 <- renderPlot({
  req(q1Test$new.xbars)
  
    parm <- as.matrix(q1Test$new.xbars)
    #print(parm)
    parm <- sort(parm)
    if(length(parm) == 1){
      y <- .5
      radius <- 4
    } else {
#       nbreaks <- nclass.Sturges(parm)^2
#       z <- cut(parm, breaks = nbreaks)
#       y <- unlist(tapply(z, z, function(V) 1:length(V)))
      y <- newy(parm) #[!is.na(y)]
      #print(y)
      nsims <- length(parm)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
  plot(x = parm + as.numeric(input$null_mu), y = y, ylim = c(0.5, pmax(10, max(y))), ylab = "", 
       cex = radius/2, pch = 16, col = q1Test$colors,  
       xlab = expression(bar(x)), main = "Shifted Resampling Distribution",
       sub ="Click a point to see its resample")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                     round(mean(parm) + as.numeric(input$null_mu),3), "\n SE = ", round(sd(parm),3)))
  
}, height = 360, width = 480)



}
  ###   estimate 1 mean value  ------------------------------- quant 1
{
 ## --------- 1 quant estimate UI ---------------------------

output$q1_estimateUI <- renderUI({
  shinyjs::useShinyjs()
  if(is.null(q1$data)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else{ 
    fluidPage(
      h3("Estimate a single mean."),
      fluidRow(
        column(4,
                  plotOutput("q1_EstPlot1")
               ),
       column(8, 
            plotOutput('q1_EstimatePlot2', click = 'q1_Estimate_click')
            )
       ),             
      fluidRow(
         column(4, offset = 2, h4("How many more resamples?")),
         column(1, actionButton("q1_resample_10", label = "10", class="btn btn-primary")),
         column(1, actionButton("q1_resample_100", label = "100", class="btn btn-primary")),
         column(1, actionButton("q1_resample_1000", label = "1000", class="btn btn-primary")),
         column(1, actionButton("q1_resample_5000", label = "5000", class="btn btn-primary"))
       ),
      br(),
      br(),
      fluidRow(
        column(4, offset = 3, 
               h4("Select Confidence Level (%)")
        ),
        column(5,
               fluidRow(
                 column(2, actionButton('q1_conf80', label = "80", class="btn btn-primary")),
                 column(2, actionButton('q1_conf90', label = "90", class="btn btn-primary")),
                 column(2, actionButton('q1_conf95', label = "95", class="btn btn-primary")),
                 column(2, actionButton('q1_conf99', label = "99", class="btn btn-primary"))
               ))
        ),
        if(!is.null(q1Estimate$CI)){
                fluidRow( 
                  column(7, offset = 5,
                         h4(paste(q1Estimate$confLevel*100, "% Interval Estimate: (", round(q1Estimate$CI[1],3), ",", 
                                   round(q1Estimate$CI[2], 3), ")"))
                  ))
              }
        )
   }
})


# -------- 1 quant estimate plots ------------------

output$q1_EstPlot1 <- renderPlot({
  req(q1$data)
  q1Estimate$observed <- mean(q1$data[,1])
  nn <- nrow(q1$data)
  par(mfrow = c(2,1), mar = c(4,3.5,3,1))
  
  ## Plot Original Data
  x <- sort(q1$data[,1])
  #z <- cut(x, breaks = nclass.Sturges(x) ^2 )
  #w <- unlist(tapply(x, z, function(x) 1:length(x)))
  w <- newy(x) # w[!is.na(w)]
  #par(mar = c(2,2,2,1))
  plot(x=x, y=w, col = blu, pch = 16, main = "Original Data", xlab = q1$names, ylab = "Count")
  legend("topleft", bty = "n", paste(" n = ", nn, "\n Mean = ", round(mean(x),3), "\n SD = ", round(sd(x),3)))
  
  ## Plot One Resample of Data
  if(is.null(q1Estimate$shuffles)){
    shuffle <- sample(x = q1$data[,1], length(q1$data[,1]), replace = TRUE)
    q1Estimate$shuffles <- as.matrix(shuffle, ncol = 1)
    ### stores samples as columns
    q1Estimate$xbars <- mean(shuffle)
    q1Estimate$colors <- blu
    DF0 <- sort(shuffle)
    #print(q1Estimate$shuffles)
  } else if(!is.null(input$q1_Estimate_click)){
    ##  We already have shuffled data and want to pick the clicked point
    ##  Change to data related to a clicked point.
    closestPoint <- which.min(abs(q1Estimate$xbars - input$q1_Estimate_click$x))
    DF0 <- sort(q1Estimate$shuffles[,closestPoint] ) 
  } else  {
    return()
  }
  #z <- cut(DF0, breaks = nclass.Sturges(DF0) ^2 )
  #w <- unlist(tapply(DF0, z, function(DF0) 1:length(DF0)))
  w <- newy(DF0) #w[!is.na(w)]
  #par(mar = c(2,2,2,1))
  plot(x=DF0, y=w, col = blu, pch = 16, main = "Resampled Data", xlab = q1$names, ylab = "Count")
  legend("topleft", bty = "n", paste(" n = ", nn, "\n Mean = ", round(mean(DF0),3), "\n SD = ", round(sd(DF0),3)))
},  height = 400, width = 300)

observeEvent(input$q1_resample_10, {
  newShuffles <- sapply(1:10, function(x) sample(q1$data[,1], length(q1$data[,1]), replace = TRUE))
  q1Estimate$shuffles <- cbind(q1Estimate$shuffles, newShuffles)
  q1Estimate$xbars <- c(q1Estimate$xbars, apply(newShuffles, 2, function(x) mean(x)))
  #print(q1Estimate$xbars) 
  q1Estimate$colors <- rep(blu, length(q1Estimate$xbars))
})

observeEvent(input$q1_resample_100, {
  newShuffles <- sapply(1:100, function(x) sample(x = q1$data[,1], length(q1$data[,1]), replace = TRUE))
  #print(dim(newShuffles))
  q1Estimate$shuffles <- cbind(q1Estimate$shuffles, newShuffles)
  q1Estimate$xbars <- c(q1Estimate$xbars, apply(newShuffles, 2, function(x) mean(x)))
  q1Estimate$colors <- rep(blu, length(q1Estimate$xbars))
  
})
observeEvent(input$q1_resample_1000, {
  newShuffles <- sapply(1:1000, function(x) sample(x = q1$data[,1], length(q1$data[,1]), replace = TRUE))
  #print(dim(newShuffles))
  q1Estimate$shuffles <- cbind(q1Estimate$shuffles, newShuffles)
  q1Estimate$xbars <- c(q1Estimate$xbars, apply(newShuffles, 2, function(x) mean(x)))
  q1Estimate$colors <- rep(blu, length(q1Estimate$xbars))
  
})
observeEvent(input$q1_resample_5000, {
  newShuffles <- sapply(1:5000, function(x) sample(x = q1$data[,1], length(q1$data[,1]), replace = TRUE))
  #print(dim(newShuffles))
  q1Estimate$shuffles <- cbind(q1Estimate$shuffles, newShuffles)
  q1Estimate$xbars <- c(q1Estimate$xbars, apply(newShuffles, 2, function(x) mean(x)))
  q1Estimate$colors <- rep(blu, length(q1Estimate$xbars))
})

observeEvent(input$q1_conf80,{
  if(is.null(q1Estimate$xbars) | (nsims <- length(q1Estimate$xbars)) < 10){
    return()
  }
  q1Estimate$confLevel <- .80
  q1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .1)
  q1Estimate$colors[1:tailCount] <- rd
  q1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q1Estimate$CI <- sort(q1Estimate$xbars) [c(tailCount, nsims + 1 - tailCount)]
})

observeEvent(input$q1_conf90,{
  if(is.null(q1Estimate$xbars)  | (nsims <- length(q1Estimate$xbars) ) < 10){
    return()
  }
  q1Estimate$confLevel <- .90
  q1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .05)
  q1Estimate$colors[1:tailCount] <- rd
  q1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q1Estimate$CI <- sort(q1Estimate$xbars) [c(tailCount, nsims + 1 - tailCount)]
})

observeEvent(input$q1_conf95,{
  if(is.null(q1Estimate$xbars)  | (nsims <- length(q1Estimate$xbars) ) < 10){
    return()
  }
  q1Estimate$confLevel <- .95
  q1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .025)
  q1Estimate$colors[1:tailCount] <- rd
  q1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q1Estimate$CI <- sort(q1Estimate$xbars) [c(tailCount, nsims + 1 - tailCount)]
})

observeEvent(input$q1_conf99,{
  if(is.null(q1Estimate$xbars)  | (nsims <- length(q1Estimate$xbars) ) < 10){
    return()
  }
  q1Estimate$confLevel <- .99
  q1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .005)
  q1Estimate$colors[1:tailCount] <- rd
  q1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q1Estimate$CI <- sort(q1Estimate$xbars) [c(tailCount, nsims + 1 - tailCount)]
})

output$q1_EstimatePlot2 <- renderPlot({
  req(q1Estimate$xbars) 
  parm <- as.matrix(q1Estimate$xbars) 
  #print(parm)
  parm <- sort(parm)
  if(length(parm) == 1){
    y <- .5
    radius <- 4
  } else {
#     nbreaks <- nclass.Sturges(parm)^2
#     z <- cut(parm, breaks = nbreaks)
#     y <- unlist(tapply(z, z, function(V) 1:length(V)))
    y <- newy(parm)  #[!is.na(y)]
    #print(y)
    #print(max(w))
    nsims <- length(parm)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(x = parm, y = y, ylim = c(0.5, pmax(10, max(y))), ylab = "", cex = radius/2, pch = 16, col = q1Estimate$colors,  
       xlab = expression(bar(x)), main = "Resampling Distribution")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                      round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
}, height = 360, width = 480)

}
  ## Bootstrap Demo  ---------------------------------------
{
  
  observeEvent(input$q1BS_toggle, {
    shinyjs::toggle("bootstrapIntro")
  })
  
  output$q1_bootstrap <- renderUI({
    div( id = 'BootDiv',
    h2("Bootstrap Demo"),
    div(id = "bootstrapIntro",
        HTML('We start with a known "population". In this case our values are textbook
         costs  in tens of dollars for one semester.<BR>
             <ul>
             <li> Click [Sample] and watch values get pulled from the population
             to the sample below.<BR>
             The population then disappears, because in real data collection we
             never know  it.<BR>
             Bootstrap resamples are based on just one sample.
             </li>
             <li> Click [1 Resample].  You will see a re-sample of size 8 get selected
             from the original sample <b>with replacement</b>.  Some of the items in the
             sample get chosen multiple times (giving a darker background), and
             some not at all (background stays white).  The mean of
             the resample is shown as a point in the plot. It disappears after a few seconds.
             Click [slower] or [faster] to vary the speed.
             </li>
             <li> Once you understand how one resample is picked,  click one
             of the "Many Resamples" options. Only the resample means are shown.
             A confidence interval appears on the plot as red boundary
             lines.  It is computed by the "percentile" method with the given 
             percentage of means in the middle and equal numbers of points above and below.  
             </li>
             </ul>
             Note: in real applications, we only get one sample, but for a demo, this is needed to change sample size.<br>
             Because we are using a "known" population, we can compare the bootstrap mean to the true value: 35.54,<br>
             and the standard deviations are actually 9.75 for n=4, 6.745 for n = 8, and 4.56 for n = 16.')
        ),   
    actionButton("q1BS_toggle", "Hide/Show Description", class="btn btn-primary"),
    renderUI(expr=  includeHTML("www/BootDemo.html") )
    )
  })
}  

    ## Lurking Demo  --------------------------------------    quant 1
  {
  
  q1Lurk <- reactiveValues(data = NULL, shuffles = NULL, diff = NULL, 
                           colors = NULL,  name = NULL)
  
  output$q1_LurkDataUI <- renderUI({
    ## choice of normal or skewed data
    fluidRow(
      column(2, actionButton("q1Lurk_IQ", label = "IQ (normal)", class="btn btn-primary")),
      column(2, actionButton("q1Lurk_Salary", label = "Salary (skewed)", class="btn btn-primary"))
    )
  })
  
  observeEvent(input$q1Lurk_IQ, {
    q1Lurk$data <- rnorm(50, 100, 15)
    temp <- gl(2, length(q1Lurk$data)/2)
    q1Lurk$shuffles <- data.frame(s1=sample(temp), s2 = sample(temp))
    q1Lurk$diff <- sapply(q1Lurk$shuffles[,1:2], function(x) -diff(tapply(q1Lurk$data,x,mean)))
    q1Lurk$name <- "IQ"
    q1Lurk$colors <- blu
  })
  
  observeEvent(input$q1Lurk_Salary,{
    q1Lurk$data <- 10^rnorm(50, 4.5, .8)
    temp <- gl(2, length(q1Lurk$data)/2)
    q1Lurk$shuffles <- data.frame(s1=sample(temp), s2 = sample(temp))
    q1Lurk$diff <- sapply(q1Lurk$shuffles[,1:2], function(x) -diff(tapply(q1Lurk$data,x,mean)))
    q1Lurk$name <- "Salary"
    q1Lurk$colors <- blu
  })
  
  output$q1_LurkingUI <- renderUI({
    req(q1Lurk$data, q1Lurk$shuffles)

     div(
       fluidRow(
          column(4, 
                 plotOutput("q1_LurkPlot1")
          ),
          column(3,
                 br(),
                 br(),
                 tableOutput("q1_LurkTable1"),
                 h5(paste("Difference in means for 1st randomization = ", 
                          round(-diff(tapply(q1Lurk$data, q1Lurk$shuffles[, 1], mean, na.rm=TRUE)), 3))),
                 br(),
                 br(),
                tableOutput("q1_LurkTable2"),
                 h5(paste("Difference in means for 2nd randomization = ", 
                          round(-diff(tapply(q1Lurk$data, q1Lurk$shuffles[, 2], mean, na.rm=TRUE)), 3)))    
          ),
          column(5, 
                uiOutput('q1_LurkSampDistPlot'))
        ),
        fluidRow(
          column(4, offset = 1, h4("How many more randomizations?")),
          column(1, actionButton("q1_Lurk_shuffle_10", label = "10", class="btn btn-primary")),
          column(1, actionButton("q1_Lurk_shuffle_100", label = "100", class="btn btn-primary")),
          column(1, actionButton("q1_Lurk_shuffle_1000", label = "1000", class="btn btn-primary")),
          column(1, actionButton("q1_Lurk_shuffle_5000", label = "5000", class="btn btn-primary"))
        )
     )
  })
  
  output$q1_LurkSampDistPlot <- renderUI({ 
    plotOutput('q1_LurkPlot2') #, click = 'q1_Lurk_click')
  })
  

  # -------- 1 quant Lurking plots ------------------
  
  output$q1_LurkPlot1 <- renderPlot({
    req(q1Lurk$shuffles) 

    nLurk <- length(q1Lurk$data)
    ## Original Data
    DF <- cbind( q1Lurk$shuffles[,1:2], q1Lurk$data) 
    names(DF) <- c("group", "group2", "y")
    DF$group <- factor(DF$group)
    #print(summary(DF))
    
    plot1 <- qplot(y=y, x=group, data = DF, geom="boxplot", main = "Randomization 1") +
      theme_bw() + xlab("") +  coord_flip() + ylab(q1Lurk$name)
    
    ## Plot One Shuffle 
    #q1Lurk$diff <- -diff(tapply(DF$y, DF$group, mean))
    #
    ### stores samples as columns
  plot2 <- qplot(y=y, x=group2, data = DF, geom="boxplot", main = "Randomization 2", 
   #ylim = c(.5, pmax(10, max(DF$y))) 
   ) + theme_bw() + xlab("") + coord_flip() + ylab(q1Lurk$name)
  grid.arrange(plot1, plot2, heights = c(3, 3)/6, ncol=1)
  }, height = 360, width = 320)
  
  
  output$q1_LurkTable1 <- renderTable({
    req(q1Lurk$data)  

    #print(q1Lurk$data)
    DF <- data.frame(mean = tapply(q1Lurk$data, q1Lurk$shuffles[,1], mean, na.rm = TRUE ),
                     sd = tapply(q1Lurk$data, q1Lurk$shuffles[,1], sd, na.rm = TRUE ),
                     n = as.integer(tapply(q1Lurk$data, q1Lurk$shuffles[,1], length)))
    rownames(DF) <- levels(q1Lurk$shuffles[,1])
    DF
  })
  
  
  output$q1_LurkTable2 <- renderTable({
    req(q1Lurk$data)

    DF <- data.frame(mean = tapply(q1Lurk$data, q1Lurk$shuffles[, 2], mean, na.rm = TRUE ),
                     sd = tapply(q1Lurk$data, q1Lurk$shuffles[, 2], sd, na.rm = TRUE ),
                     n = as.integer(tapply(q1Lurk$data, q1Lurk$shuffles[, 2], length)))
    rownames(DF) <- levels(q1Lurk$shuffles[,1])
    DF
  })
  
  observeEvent(input$q1_Lurk_shuffle_10, {
    newShuffles <- sapply(1:10, function(x) sample(q1Lurk$shuffles[,1]) )
    q1Lurk$shuffles <- cbind(q1Lurk$shuffles, newShuffles)
    q1Lurk$diff <- c(q1Lurk$diff, apply(newShuffles, 2, 
                                        function(x) -diff(tapply(q1Lurk$data, x, mean, na.rm=TRUE))))
    #print(q1Lurk$diff)
    q1Lurk$colors <- rep(blu, length(q1Lurk$diff))
  })
  
  observeEvent(input$q1_Lurk_shuffle_100, {
    newShuffles <- sapply(1:100, function(x) sample(q1Lurk$shuffles[,1]) )
    q1Lurk$shuffles <- cbind(q1Lurk$shuffles, newShuffles)
    q1Lurk$diff <- c(q1Lurk$diff, apply(newShuffles, 2, function(x) -diff(tapply(q1Lurk$data, x, mean, na.rm=TRUE))))
    #print(q1Lurk$diff)
    q1Lurk$colors <- rep(blu, length(q1Lurk$diff))
  })
  observeEvent(input$q1_Lurk_shuffle_1000, {        
    newShuffles <- sapply(1:1000, function(x) sample(q1Lurk$shuffles[,1]) )
    q1Lurk$shuffles <- cbind(q1Lurk$shuffles, newShuffles)
    q1Lurk$diff <- c(q1Lurk$diff, apply(newShuffles, 2, function(x) -diff(tapply(q1Lurk$data, x, mean, na.rm=TRUE))))
    #print(q1Lurk$diff)
    q1Lurk$colors <- rep(blu, length(q1Lurk$diff))
  })
  observeEvent(input$q1_Lurk_shuffle_5000, {
    newShuffles <- sapply(1:5000, function(x) sample(q1Lurk$shuffles[,1]) )
    q1Lurk$shuffles <- cbind(q1Lurk$shuffles, newShuffles)
    q1Lurk$diff <- c(q1Lurk$diff, apply(newShuffles, 2, function(x) -diff(tapply(q1Lurk$data, x, mean, na.rm=TRUE))))
    #print(q1Lurk$diff)
    q1Lurk$colors <- rep(blu, length(q1Lurk$diff))
  })
  
  
  output$q1_LurkPlot2 <- renderPlot({
    req(q1Lurk$diff) 

        parm <- sort(q1Lurk$diff)
    # print(parm)
    if(length(parm) < 3){
      y <- rep(0.5,length(parm))
      radius <- 4
    } else {
      #nbreaks <- nclass.Sturges(parm)^2
      #z <- cut(parm, breaks = nbreaks)
      #y <- unlist(tapply(z, z, function(V) 1:length(V)))
      y <- newy(parm)
      #print(y)
      #print(max(w))
      nsims <- length(parm)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(parm, y, ylim = c(0.5, pmax(10,max(y))), ylab = "", cex = radius/2, pch = 16, col = q1Lurk$colors,  
         xlab = expression(bar(x)[1] - bar(x)[2]), main = "Randomization Distribution")
    legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                        round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
  }, width = 400)      
  
  }
 
  
  ##  Random Sampling Demo -----------------------------------------  quant 1
  {
  ## input order:
  ## text box allows data editing
  ## [Use Data] after that's clicked, also allow: [Repeat Data Twice]
    ## show population size, mean word length, SD
  ## Change sample size -- blanking out previous samples and values
  ## sample: 1 [Start Over], 10, 100, 1000, 5000
  ## plot -- clickable to allow ID of various samples.
  
  ## data input
  
  
  q1Samp <- reactiveValues(data = NULL, samples = NULL, textIn = NULL, parm = NULL,
                           colors = NULL,  values = NULL, names = NULL, n = NULL,
                           paramFn = NULL, trueValue = NULL)
  
  ##  Use text box for input -- Paste in any text?
  ## set sample size. Allow population to increase.
  output$q1_SampDataUI <- renderUI({
    fluidPage(   
      column(5,  div( 
        ##  Could add a choice here for Gettysburg address vs Joke.
         HTML(paste('<textarea name="q1_SampleText" cols="40" rows="10"> ', joke, ' </textarea>')),
         br(),
         div(
         actionButton("q1_sampUseData", "Use This Text", class="btn btn-primary"),
         if(!is.null(q1Samp$data)){
             div(
                 actionButton("q1_sampPopDouble", "Clone (double) the Text", class="btn btn-primary"),
                 h5(paste("Population size: ", nrow(q1Samp$data))),
                 h5("Choose parameter: "), ##, "Mean word length: ", round(mean(q1Samp$data[,2]), 2) )),           
                 radioButtons("q1_sampParam", label = "", list("Mean "," Median ", "Standard Deviation"),
                              "Mean ", inline = TRUE) ,
                 uiOutput('q1_sampTrueValue')
             )
          } else div()
          ))
      ),
       column(6,
            if(!is.null(q1Samp$data)){
                div(
                  uiOutput('q1_sampDemoSampSize'),
                  uiOutput('q1_sampDemoDraws')
                )
            } 
       )
    )
  })
  observeEvent(input$q1_sampParam, {
    req(q1Samp$data)
    q1Samp$samples <-  q1Samp$values <- NULL
    q1Samp$parm <- switch(input$q1_sampParam,
           "Mean " = mean(q1Samp$data[, 2], na.rm=TRUE),
           " Median " = median(q1Samp$data[, 2], na.rm=TRUE),
           "Standard Deviation" = sd(q1Samp$data[, 2], na.rm=TRUE))
    q1Samp$paramFn <- switch(input$q1_sampParam,
                           "Mean " = mean,
                           " Median " = median,
                           "Standard Deviation" = sd )
    q1Samp$trueValue <- q1Samp$paramFn(q1Samp$data[,2], na.rm=TRUE)
  })
  
  output$q1_sampTrueValue <- renderUI({
    req(q1Samp$trueValue)
    h5(paste("True ", input$q1_sampParam, " word length: ", round(q1Samp$trueValue,2)))
  })
  
  output$q1_sampDemoSampSize <- renderUI({ 
    req(q1Samp$data)

    div(
    tags$label('Sample Size: ',
              tags$input(name='q1_sampSize', type='text', value='10', size='10'))
    
    )
    })  
  
  output$q1_sampDemoDraws <- renderUI({
    req(q1Samp$data)
    div(
    uiOutput('q1_SampDemoSample' ),
    br(),
    div(
      actionButton("q1_SampDemo_1", "Draw one Sample (and Clear)", class="btn btn-primary")
    ), 
    if(length(q1Samp$samples)>1){
      div(
        fluidRow(
          column(4,  h4("More samples: ")),
          column(2, actionButton("q1_SampDemo_10", label = "10", class="btn btn-primary")),
          column(2, actionButton("q1_SampDemo_100", label = "100", class="btn btn-primary")),
          column(2, actionButton("q1_SampDemo_1000", label = "1000", class="btn btn-primary")),
          column(2, actionButton("q1_SampDemo_5000", label = "5000", class="btn btn-primary"))
        ),
        plotOutput('q1_sampDemoPlot', click = 'q1_Samp_click', height = '320px')
      )}
    ) 
  })   
    
  ## get samples
  output$q1_SampDemoSample <- renderUI({
    req(q1Samp$samples)

    if(!is.null(input$q1_Samp_click)){
      ##  We already have values plotted and want to pick the clicked point
      ##  Change to data related to a clicked point.
      closestPoint <- which.min(abs(q1Samp$values - input$q1_Samp_click$x))
      DF0 <- q1Samp$data[q1Samp$samples[, closestPoint], ]
    } else if(is.matrix(q1Samp$samples)){  ## show last row
      DF0 <- q1Samp$data[q1Samp$samples[, length(q1Samp$values)], ]
    } else{   ## take a sample
      DF0 <- q1Samp$data[ sample(1:nrow(q1Samp$data), as.numeric(input$q1_sampSize)),]
    }
     div(
      HTML(paste(c("Sample words: ", as.character(DF0[, 1])))),
      HTML(paste(c("<br>  Lengths:      ", DF0[, 2]), collapse = "   ")),
      HTML(paste( "<br>", input$q1_sampParam  ,":", 
             round(q1Samp$paramFn(DF0[,2], na.rm=TRUE), 2)))
    )
  }) 
  
  observeEvent(input$q1_sampUseData, {
    q1Samp$data <- cleanText(input$q1_SampleText)
    q1Samp$samples <- q1Samp$means <- NULL
  })
  
  observeEvent(input$q1_sampPopDouble,{
    q1Samp$data <- rbind( q1Samp$data, q1Samp$data)
    q1Samp$samples <- q1Samp$values <- NULL
  })
  
  observeEvent(input$q1_sampSize,{
    q1Samp$n <- as.numeric(input$q1_sampSize)
  })
  observeEvent(input$q1_SampDemo_1, {
      newSamples <- matrix(sample(1:nrow(q1Samp$data), q1Samp$n), ncol=1)
      q1Samp$samples <-  newSamples
      q1Samp$values <- q1Samp$paramFn(q1Samp$data[newSamples, 2], na.rm=TRUE) 
  })

  observeEvent(input$q1_SampDemo_10, {
    newSamples <- sapply(1:10, function(x) sample(1:nrow(q1Samp$data), as.numeric(input$q1_sampSize)))
    if(!is.null(q1Samp$samples) & nrow(newSamples) != nrow(q1Samp$samples)){
      q1Samp$samples <-  q1Samp$values <- NULL
    }
    q1Samp$samples <- cbind(q1Samp$samples, newSamples) 
    q1Samp$values <- c(q1Samp$values, apply(newSamples, 2, function(x) q1Samp$paramFn(q1Samp$data[x, 2],na.rm=TRUE)) )
    #q1Samp$colors <- rep(blu, length(q1Samp$mean))
  })
  
  observeEvent(input$q1_SampDemo_100, {
    newSamples <- sapply(1:100, function(x) sample(1:nrow(q1Samp$data), as.numeric(input$q1_sampSize)))
    if(!is.null(q1Samp$samples) & nrow(newSamples) != nrow(q1Samp$samples)){
      q1Samp$samples <-  q1Samp$values <- NULL
    }
    q1Samp$samples <- cbind(q1Samp$samples, newSamples) 
    q1Samp$values <- c(q1Samp$values, apply(newSamples, 2, function(x) q1Samp$paramFn(q1Samp$data[x, 2],na.rm=TRUE)) )
    #q1Samp$colors <- rep(blu, length(q1Samp$mean))
  })

  observeEvent(input$q1_SampDemo_1000, {
    newSamples <- sapply(1:1000, function(x) sample(1:nrow(q1Samp$data), as.numeric(input$q1_sampSize)))
    if(!is.null(q1Samp$samples) & nrow(newSamples) != nrow(q1Samp$samples)){
      q1Samp$samples <-  q1Samp$values <- NULL
    }
    q1Samp$samples <- cbind(q1Samp$samples, newSamples) 
    q1Samp$values <- c(q1Samp$values, apply(newSamples, 2, function(x) q1Samp$paramFn(q1Samp$data[x, 2],na.rm=TRUE)) )
    #q1Samp$colors <- rep(blu, length(q1Samp$mean))
  })

  observeEvent(input$q1_SampDemo_5000, {
    newSamples <- sapply(1:5000, function(x) sample(1:nrow(q1Samp$data), as.numeric(input$q1_sampSize)))
    if(!is.null(q1Samp$samples) & nrow(newSamples) != nrow(q1Samp$samples)){
      q1Samp$samples <-  q1Samp$values <- NULL
    }
    q1Samp$samples <- cbind(q1Samp$samples, newSamples) 
    q1Samp$values <- c(q1Samp$values, apply(newSamples, 2, function(x) q1Samp$paramFn(q1Samp$data[x, 2],na.rm=TRUE)) )
    #q1Samp$colors <- rep(blu, length(q1Samp$mean))
  })

  output$q1_sampDemoPlot <- renderPlot({
    if(is.null(q1Samp$values) | length(q1Samp$values) < 2){ 
      return() }
    parm <- sort(q1Samp$values)
    ## print(parm)
    if(length(parm) < 3){
      y <- rep(0.5,length(parm))
      radius <- 4
    } else {
      y <- newy(parm)
      nsims <- length(parm)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(parm, y, ylim = c(0.5, pmax(10,max(y))), ylab = "", cex = radius/2, 
         pch = 16, col = blu,  
         xlab = paste("Word Length", input$q1_sampParam), 
         main = "Sampling Distribution")
    legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                        round(mean(parm),3), "\n SD = ", round(sd(parm),3)))
  }, width = 400)      
  
    
  }
  
   
  ###  t dist'n option ------------------------------------- quant 1

{
q1_tProb <- reactiveValues(prob = NULL, z = NULL, findP = NULL)

observeEvent( input$q1_z_txt, {
  q1_tProb$z <- as.numeric(input$q1_z_txt) 
  q1_tProb$findP <- TRUE
})

observeEvent( input$q1_prob_txt,{
  q1_tProb$prob <- as.numeric(input$q1_prob_txt) 
  q1_tProb$findP  <- FALSE
})


  output$tProbPlot1 <-    renderPlot({ 
#     print(q1_tProb$prob)
#     print(q1_tProb$z)
#     print(q1_tProb$findP)
#     print(input$q1_df)
#     print(input$q1_area)
    req(q1_tProb$findP, input$q1_df)

    if(is.null(q1_tProb$prob) & is.null(q1_tProb$z))
      return()
    df <- as.numeric(input$q1_df)
    
    par(mar=c(24,1,1,1)/10)
    z <- absz <- prob <- yrr <- xrr <- NA
    x <- -300:300 / 50
    
     if(!q1_tProb$findP & !is.na(q1_tProb$prob)){
      ## given prob, find z
      prob <- q1_tProb$prob
      #cat("finding z for p = ", prob, "\n")
      if(input$q1_area == "Lower"){ ##  left tail
        z <-  qt(prob, df)
        if(z < min(x))  x <- c(1.01 * z, x)
        ## rejection region in x dimension
        xrr <- c(x[x < z], z, z)
        ## density curve over xrr:
        yrr <- c( dt(xrr[-length(xrr)], df), 0)
      } else if(input$q1_area == "Upper"){   ## right tail        
        z <- qt(1 - prob, df) 
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- c(z, z, x[x > z])
        yrr <- c(0, dt(xrr[-1], df))
      } else if(input$q1_area == "Center"){
        z <- abs(qt( (1 - prob)/2, df ))
        if(z < min(x))  x <- c(1.01 * z, x)       
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- seq(-z, z, length=100)
        yrr <- c(0, dt(xrr[2:99], df), 0)
      } else if(input$q1_area == "Extremes"){
        z <- abs(qt(1-prob/2, df) )
        if(z < min(x))  x <- c(1.01 * z, x)       
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- c(-z, x[x < -z], -z)
        yrr <- c(0,  dt(xrr[-1], df))
      }
      absz <- abs(z)
    }
    if(q1_tProb$findP & !is.na(q1_tProb$z)){
      z <- q1_tProb$z
      ##  find probability
      absz <- abs(z)
      maxX <- pmax(z, 6)
      minX <- pmin(z, -6)
      # cat("finding p ", minX, maxX, "for z = ", z, "\n")
      x <- seq(minX, maxX, length=200)
      prob <-  1 - pt(z, df) 
      xrr <- c(z, z, x[x > z])  ## right tail
      yrr <- c(0,  dt(xrr[-1],  df) )
      if(input$q1_area == "Lower"){         ##  left tail
        prob =  pt(z, df) 
        xrr <- c(z, x[x < z], z)
        yrr <- c(0,  dt(xrr[-1], df))
      } else if (input$q1_area == "Extremes"){ ##  extremes
        xrr <- c( -absz, x[x < -absz], -absz)
        yrr <- c(0, dt(xrr[-1], df))
        prob = pt(-absz, df) 
        #yrr <- c(yrr, NA,NA, rev(yrr))
        #xrr <- c(xrr, NA,NA, rev(-xrr))
      } else if (input$q1_area == "Center"){   ##  center
        prob <- q1_tProb$prob
        xrr <- seq(-absz, absz, length=100)
        yrr <- c(0, dt(xrr, df), 0)
        xrr <- c(-absz, xrr, absz)
        prob <- diff( pt(c(-absz,absz), df) )
      }
    }
  if(is.null(df)) return
  plot(x, dt(x, df), type = "l", bty='n', xlab="Z Score", ylab="", yaxt="n")
  abline(h=0)
  max.height <- dt(0, df) *.9
  text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
  segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
  
  if(input$q1_area == "Extremes") {  ## extremes
    polygon(xrr, yrr, col = rd)
    polygon(-xrr, yrr, col = rd)
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
         round(prob * ifelse(q1_tProb$findP,1,.5), 3), col = "darkblue")
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y = text.height, round(c(-absz,absz),3))
  } else if (input$q1_area == "Center") {   ## fill & label center
    polygon(xrr, yrr, col = grn)
    text(x=0, y= text.height, round(prob,3))
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y=text.height, round(c(-absz,absz),3))
  } else {          ## show tails
    polygon(xrr, yrr, col = rd)
    text( z, y = text.height, round(z, 3))
    text(x= sign(z) * (absz+4) / 2 , y= max(yrr) / 2 + 0.02, 
         round(prob,3), col = "darkblue")
  }
}, height=300)
}

    ###  Power Demo ---------------------------------------  quant 1
  {
    sliderValues <- reactive({
      ## Compose data frame
      data.frame(
        Inputs = c("Sample Size", 
                    "Standard Deviation",
                    "Shift in Mean",
                    "Significance Level (alpha)"),
        InValue = as.character(c(input$pwr_n, 
                               input$pwr_sd,
                               input$pwr_altMean, 
                               input$pwr_alpha )),
        Outputs = c( "Effect Size",  "Power", NA, NA   ),
        OutValue = as.character(c(round(input$pwr_altMean/input$pwr_sd, 3),
                               round(
                                 power.t.test(n=input$pwr_n, delta=input$pwr_altMean,
                                              sd=input$pwr_sd, sig.level=input$pwr_alpha,
                                              type="one.sample",
                                              alternative="one")$power,3),NA,NA)), 
        stringsAsFactors=FALSE)
    }) 

      output$powerPlot <- renderPlot({
      sd <- input$pwr_sd
      x <- seq(-8,20,length=200) * sd
      suppressWarnings({
        plot(x, dt(x/sd, input$pwr_n-1), bty='l', type="l", xlab="",ylab="", xlim = c(-8,20))
        lines(x, dt((x -input$pwr_altMean)/sd, input$pwr_n-1, ncp=input$pwr_altMean/input$pwr_sd * sqrt(input$pwr_n)))
        qt1 <- qt(1-input$pwr_alpha, input$pwr_n-1) * sd
        xrr <- c(qt1, qt1, x[x>=qt1],max(x))
        yrr <- c(0, dt(c(qt1,  x[x>=qt1])/sd, input$pwr_n -1),0)
        polygon(xrr,yrr, col = rd)
        xpwr <- c(qt1, x[x>=qt1])
        ypwr <- c(0, dt((xpwr-input$pwr_altMean)/sd, df=input$pwr_n-1, ncp=input$pwr_altMean/input$pwr_sd *sqrt(input$pwr_n) ), 0 )
        xpwr <- c(qt1, xpwr, max(x))
      })
      abline(h=0)
      abline(v = c(0, qt1))
      ##cat(length(xpwr), length(ypwr))
      polygon(xpwr,ypwr, col = grn)
      text(weighted.mean(xrr, w=yrr^2), weighted.mean(yrr,w=yrr^.5),cex=1.5, expression(alpha))
      text(weighted.mean(xpwr, w=ypwr^4), weighted.mean(ypwr, w=ypwr^.5),cex=1.5,"Power")
      mtext(side=1,at=0,line=2, expression(H[0]: mu == 0))
      mtext(side=1, at = xpwr[which.max(ypwr)], line=2, expression(H[a]: mu > 0))
    })
    
    # Show the values using an HTML table
    output$powerValues <- renderTable({
      sliderValues()
    }, include.rownames = FALSE)
  
  
  }  
  
  ## 2 Categorical ------------------------------------------------------------- 2 cat

  ## Data Entry --------------------------------------------------------  cat 2
{
cat2_data <- reactiveValues(counts = NULL, names = NULL, groups = NULL)

cat2Test <- reactiveValues(difprop = NULL, phat1 = NULL, phat2 = NULL, observed = NULL, colors = NULL,
                           cutoff = NULL, direction = NULL, moreExtremeCount = NULL, pvalue = NULL, 
                           sampleCount = NULL, selected = NULL)

cat2Estimate <- reactiveValues(difprop = NULL, phat1 = NULL, phat2 = NULL, observed = NULL, colors = blu,
                               confLevel = NULL, CI = NULL, selected = NULL)


observeEvent(input$cat2_submitButton, {
  ## need to remove old data if user comes back to data entry 
  cat2Test$difprop <- cat2Test$phat1 <- cat2Test$phat2 <- cat2Test$observed <- cat2Test$colors <- cat2Test$moreExtremeCount  <- cat2Test$pvalue <- NULL
  cat2Estimate$difprop  <- cat2Estimate$phat1  <- cat2Estimate$phat2  <- cat2Estimate$observed  <- cat2Estimate$colors  <- cat2Estimate$confLevel  <- cat2Estimate$CI <- NULL
  cat2_data$counts <- as.numeric(c(input$cat2_n11, input$cat2_n12, input$cat2_n21, input$cat2_n22))
  cat2_data$names <- rep(c(input$cat2_name1, input$cat2_name2), 2)
  cat2_data$groups <- rep(c(input$cat2_grp1, input$cat2_grp2), each = 2)
}
)


}

  ## Summary of data --------------------------------------------------- cat 2

{
  output$cat2Summary <- renderTable({ 
  if(input$cat2_submitButton == 0) return()
  #isolate({
    #cat2_dataDF <- cat2_data()
    #print(cat2_data$counts)
    counts <- as.table( matrix(cat2_data$counts, 2, 2))
    #print(counts)
    colnames(counts) <- cat2_data$names[1:2]
    rownames(counts) <- cat2_data$groups[c(1,3)]
    round(t(prop.table(counts, 1)),3)
  #})
 }, digits = 3)
  

 output$cat2Plot <- renderPlot( {
    if(input$cat2_submitButton ==0) return()
   # isolate( { 
    #  cat2_dataDF <- cat2_data()
      #print(cat2_dataDF)
      counts <- as.table( matrix(cat2_data$counts, 2, 2))
      colnames(counts) <- cat2_data$names[1:2]
      rownames(counts) <- cat2_data$groups[c(1,3)]
      props <- t(prop.table(counts, 1))
      #print(props)
      ## make plot
      par(mar=c(24, 40, 10, 35)/10)
      barplot(props, ylab = "Proportion", main = "")
    #})
  }, height=180)


  output$cat2DataIn <- renderText({
    if(input$cat2_submitButton ==0) return()
    "Data are entered, you may now choose to estimate or test the difference in two proportions."
  })
}


  ###  cat2 --  test equality of proportions  -------------------------- cat 2
  
  ## cat2 test UI ---------------------------------------------
  
  {
  
  output$cat2_testUI <- renderUI({
    if(is.null(cat2_data$counts)){
      h4(" You must first enter data. Choose 'Enter/Describe Data'.")
    } else {
      fluidPage(
        h3("Test: 'Are two proportions equal?'"),
        fluidRow(
             column(4, 
                  h4("Original Data"),
                  tableOutput("cat2OriginalData"),
                  h5(paste("Original difference in proportions: ", 
                            round(-diff(prop.table(as.table(matrix(cat2_data$counts, 2, 2)), 1))[1], 3)
                   )),
                  br(),
                  h4("Shuffled Sample"),
                  uiOutput("Cat2TestShuffle")      
                  ),
                 column(8, 
                        h4(HTML("&nbsp;&nbsp;&nbsp;&nbsp; Null hypothesis: p<sub>1</sub> = p<sub>2</sub>")),                     
                        plotOutput('cat2Test_Plot2', click = 'cat2_Test_click')
                        
                 )
          ),
      #br(),
      fluidRow(
        column(4, offset = 1, h4("How many more shuffles?")),
        column(1,  actionButton("cat2_test_shuffle_10", label = "10", class="btn btn-primary")),
        column(1,  actionButton("cat2_test_shuffle_100", label = "100", class="btn btn-primary")),
        column(1,  actionButton("cat2_test_shuffle_1000", label = "1000", class="btn btn-primary")),
        column(1,  actionButton("cat2_test_shuffle_5000", label = "5000", class="btn btn-primary"))
      ),
      br(),      br(),
      fluidRow(
        column(8, offset = 4,
               h4("Click on a point to see its data table."),
               uiOutput("Cat2TestXtremes")    )
      ),
     fluidRow(
        column(8, offset = 4, 
          uiOutput("Cat2TestPvalue")    )
      )
    )          
  }
})

output$Cat2TestShuffle <- renderUI({
  req(cat2Test$selected)
    div(
      tableOutput('cat2Test_Table') , 
      h5(paste("Shuffled difference in proportions: ", round(cat2Test$selected, 3) ))
    )
}) 
  
output$Cat2TestXtremes <- renderUI({
  fluidRow(
    column(3, 
           h4("Count values equal to or")
    ),
    column(4,
           tags$div(style="width: 200px",
                    tags$select(id='cat2_testDirection',class="form-control",
                                tags$option( value = "less", "less"),
                                tags$option( value = "more extreme", "more extreme", selected = TRUE),
                                tags$option( value = "greater", "greater")))
           ),
    column(1, h4("than ")),
    column(2,           
           tags$div( 
             tags$input(id = "cat2_test_cutoff", type = "text", class = "form-control", value = NA))
    ),
    column(1,
           actionButton('cat2_test_countXtremes', "Go", class="btn btn-success")
    )
    )
})
  

observeEvent( input$cat2_testDirection, {
  cat2Test$pvalue <- cat2Test$moreExtremeCount <- NULL  
  cat2Test$colors <- rep(blu, length(cat2Test$colors))
})

observeEvent( input$cat2_test_cutoff, {
  cat2Test$pvalue <- cat2Test$moreExtremeCount <- NULL  
  cat2Test$colors <- rep(blu, length(cat2Test$colors))
})


output$Cat2TestPvalue <- renderUI({
  req(cat2Test$moreExtremeCount)
    h4(pvalue2print(cat2Test$moreExtremeCount,  cat2Test$sampleCount,
                    cat2Test$direction, cat2Test$cutoff, cat2Test$pvalue)
    )
})

## cat2 test plots --------------------------------------------------

  
  output$cat2OriginalData <- renderTable({ 
    if(input$cat2_submitButton ==0) return()
    
    y1 = cat2_data$counts[1]
    y2 = cat2_data$counts[2]
    n1 <- sum(cat2_data$counts[1], cat2_data$counts[3])
    n2 <- sum(cat2_data$counts[2], cat2_data$counts[4])

   if(is.null(cat2Test$difprop) ){
       DF <- cat2_test_shuffles(1, cat2_data$counts[1], cat2_data$counts[2], n1, n2)
       cat2Test$selected <- as.numeric(DF[1,3])
       cat2Test$difprop <- DF[1,3]
       cat2Test$phat1 <- DF[1,1]
       cat2Test$phat2 <- DF[1,2]
       cat2Test$colors <- blu
    }    

    p1 <- round(y1/n1,3)
    p2 = round(y2/n2,3)
    # print(c(y1, n1, y2, n2, p1, p2))
    counts <- data.frame(count = as.integer(c(y1, y2)), 
                         "n" = as.integer(c(n1, n2)),
                         Proportion = c(p1, p2))
    colnames(counts)[1] <- cat2_data$names[1]
    rownames(counts) <- cat2_data$groups[c(1,3)]
    counts
  }, digits = c(0,0,0,3))
  
  observeEvent(input$cat2_test_shuffle_10, {
    cat2Test$moreExtremeCount <-  NULL
    cat2Test$selected <-  NA
    DF <- cat2_test_shuffles(shuffles = 10, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                            n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                            n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Test$difprop <- rbind(cat2Test$difprop, as.matrix(DF[,3]))
    cat2Test$phat1 <- rbind(cat2Test$phat1, as.matrix(DF[,1]))
    cat2Test$phat2 <- rbind(cat2Test$phat2, as.matrix(DF[,2]))
    cat2Test$colors <- rep(blu, length(cat2Test$difprop))
  })
  
  observeEvent(input$cat2_test_shuffle_100, {
    cat2Test$moreExtremeCount <-  NULL
    cat2Test$selected <- NA
    DF <- cat2_test_shuffles(shuffles = 100, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                            n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                            n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Test$difprop <- rbind(cat2Test$difprop, as.matrix(DF[,3]))
    cat2Test$phat1 <- rbind(cat2Test$phat1, as.matrix(DF[,1]))
    cat2Test$phat2 <- rbind(cat2Test$phat2, as.matrix(DF[,2]))
    cat2Test$colors <- rep(blu, length(cat2Test$difprop))

  })
  
  observeEvent(input$cat2_test_shuffle_1000, {
    cat2Test$moreExtremeCount <-  NULL
    cat2Test$selected <- NA
    DF <- cat2_test_shuffles(shuffles = 1000, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                            n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                            n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Test$difprop <- rbind(cat2Test$difprop, as.matrix(DF[,3]))
    cat2Test$phat1 <- rbind(cat2Test$phat1, as.matrix(DF[,1]))
    cat2Test$phat2 <- rbind(cat2Test$phat2, as.matrix(DF[,2]))
    cat2Test$colors <- rep(blu, length(cat2Test$difprop))

  })
  
  observeEvent(input$cat2_test_shuffle_5000, {
    cat2Test$moreExtremeCount <-   NULL
    cat2Test$selected <-  NA
    DF <- cat2_test_shuffles(shuffles = 5000, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                            n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                            n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Test$difprop <- rbind(cat2Test$difprop, as.matrix(DF[,3]))
    cat2Test$phat1 <- rbind(cat2Test$phat1, as.matrix(DF[,1]))
    cat2Test$phat2 <- rbind(cat2Test$phat2, as.matrix(DF[,2]))
    cat2Test$colors <- rep(blu, length(cat2Test$difprop))
    
  })
  
  output$cat2Test_Table <- renderTable({
    #if(input$cat2_submitButton == 0) return()
    req(cat2_data$counts) 
    n1 <- sum(cat2_data$counts[1], cat2_data$counts[3])
    n2 <- sum(cat2_data$counts[2], cat2_data$counts[4])
    if(is.null(cat2Test$difprop) ){
      DF <- cat2_test_shuffles(1, cat2_data$counts[1], cat2_data$counts[2], n1, n2)
      cat2Test$selected <- as.numeric(DF[1,3])
      cat2Test$difprop <- DF[1,3]
      cat2Test$phat1 <- DF[1,1]
      cat2Test$phat2 <- DF[1,2]
      cat2Test$colors <- blu
      props <- DF[1, 1:2]
      y1_new <- as.integer(n1 * DF[1,1])
      y2_new <- as.integer(n2 * DF[1,2])
    } else  if(!is.null(input$cat2_Test_click)){
      ##  We already have shuffled data and want to pick the clicked point
      closestPoint <- which.min(abs( cat2Test$difprop - input$cat2_Test_click$x))
      #cat("Close to number: ", closestPoint, "\n")
      y1_new <- as.integer(n1 * cat2Test$phat1[closestPoint])
      y2_new <- as.integer(n2 * cat2Test$phat2[closestPoint])
      props <- c(cat2Test$phat1[closestPoint], cat2Test$phat2[closestPoint] )
      cat2Test$selected <-  -diff(props)
      cat2Test$colors <- rep( blu, length(cat2Test$difprop))
      #cat2Test$colors[closestPoint] <- grn
      
      ## cat(c(n1, n2, props, y1_new, y2_new), "\n")
    } else {
      return()
    }
    # print(c(y1_new, n1, DF[1,1], y2_new, n2, DF[1,2], diff.p))
    count2 <- data.frame(count = as.integer(c(y1_new, y2_new)), 
                     "n" = as.integer(c(n1, n2)),
                     Proportion = props)
    colnames(count2)[1] <- cat2_data$names[1]
    rownames(count2) <- cat2_data$groups[c(1,3)]
    count2
  }, digits = c(0,0,0,3))

    observeEvent(input$cat2_test_countXtremes, {
    x <- sort(cat2Test$difprop)
    nsims <- length(x)
    cat2Test$colors <- rep(blu, nsims)
    cat2Test$cutoff <- threshold <- as.numeric(input$cat2_test_cutoff)
    cat2Test$direction <- input$cat2_testDirection
    
    if(nsims > 1 & !is.na(input$cat2_testDirection)){
      redValues <-  switch(input$cat2_testDirection,
                           "less" = which(x <= threshold + 1.0e-10),
                           "greater" = which(x >= threshold - 1.0e-10),
                           "more extreme" = c(which(x <= -abs(threshold) + 1.0e-10 ), 
                                              which(x >= abs(threshold) -1.0e-10 ) )  )
      cat2Test$colors[redValues] <- rd       
      cat2Test$moreExtremeCount  <- length(redValues)
      cat2Test$pvalue <- cat2Test$moreExtremeCount/nsims
      cat2Test$sampleCount <- length(cat2Test$difprop)
    }
  })
  
  
  output$cat2Test_Plot2 <- renderPlot({
    ##if(input$cat2_submitButton == 0) return()
    req(cat2Test$difprop)
    DF <- sort(cat2Test$difprop)
    ## print(head(DF))
    if(length(DF) == 1){
      w <- 1
      radius = 4
      } 
    else {
      #nbreaks <- 0.5*nclass.Sturges(DF)^2
      ##z <- cut(DF, breaks = nbreaks)
      #w <- unlist(tapply(z, z, function(V) 1:length(V)))
      w <- newy(DF) #w[!is.na(w)]
      #print(w)
      #print(max(w))
      nsims <- length(DF)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(DF, w, ylab = "", ylim = c(0.5, pmax(10, max(w))), cex = radius/2, pch = 16, col = cat2Test$colors,  
         xlab = expression(hat(p)[1] - hat(p)[2]), main = "Sampling Distribution")
    legend("topright", bty = "n", paste(length(DF), "points \n Mean = ", 
                                       round(mean(DF),3), "\n SE = ", round(sd(DF),3)))
    #mtext(side = 1, at = 0, adj = 0, line = 0, bquote(p[1] == p[2]))
    }, width = 600)
  
  }

  ###  cat2 --  estimate difference in proportions --------------------- cat 2
  {
  output$cat2_estimateUI <- renderUI({
    if( is.null(cat2_data$counts)){
      h4(" You must first enter data. Choose 'Enter/Describe Data'.")
    } else {
      tabPanel("Estimate", value="2catEstimate",
               titlePanel("Estimate a Difference in Proportions"),       
               div(
                 fluidRow(
                    column(4, 
                        h4("Original Data"),
                        tableOutput("cat2_CIPrep"),
                        h5(paste("Original Difference in proportions: ", 
                                 round(-diff(prop.table(as.table(matrix(cat2_data$counts, 2, 2)), 1))[1],3))), 
                        hr(),
                        
                        h4("One Resampled Dataset"),
                        uiOutput("Cat2EstimateShuffle")   
                        #tableOutput('cat2Estimate_Table'),
                        #h5(paste("Difference in proportions for resampled data: " , 
                        #         round(as.numeric(cat2Estimate$difprop[1]), 3)))
                    ),
                 
                   column(8, 
                         plotOutput('cat2Estimate_Plot2', click = 'cat2_Estimate_click')
                   )
                ),
                br(),
                br(),
                fluidRow(
                   column(4, offset = 1, h4("How many more resamples?")),
                   column(1,
                            actionButton("cat2_estimate_shuffle_10", label = "10", class="btn btn-primary")),
                   column(1, 
                          actionButton("cat2_estimate_shuffle_100", label = "100", class="btn btn-primary")),
                   column(1,
                          actionButton("cat2_estimate_shuffle_1000", label = "1000", class="btn btn-primary")),
                   column(1,
                          actionButton("cat2_estimate_shuffle_5000", label = "5000", class="btn btn-primary"))
                   ),
                br(),
                br(),
                fluidRow(
                  column(3, offset = 2, 
                         h4("Select Confidence Level (%)")
                         ),
                  column(6,
                         fluidRow(
                            column(2,  
                                   actionButton('cat2_conf80', label = "80", class="btn btn-primary")),
                            column(2,
                                   actionButton('cat2_conf90', label = "90", class="btn btn-primary")),
                            column(2,
                                   actionButton('cat2_conf95', label = "95", class="btn btn-primary")),
                            column(2,
                                   actionButton('cat2_conf99', label = "99", class="btn btn-primary"))
                            )
                         )
                  ),
                if(!is.null(cat2Estimate$CI)){
                  fluidRow(
                    column(8, offset = 4,
                           h4(paste(round(100 * cat2Estimate$confLevel), "% Confidence Interval Estimate: (", 
                                    round(cat2Estimate$CI[1],3), ",", 
                                     round(cat2Estimate$CI[2], 3), ")"))
                           )
                  )
                  } else {br()}
            )
        )
    }
  })
  

  output$Cat2EstimateShuffle <- renderUI({
    req(cat2Estimate$selected)
      div(
        tableOutput('cat2Estimate_Table') , 
        h5(paste("Difference in resampled proportions: ", round(cat2Estimate$selected, 3) ))
      )
  }) 
  
  
  output$cat2_CIPrep <- renderTable({ 
    req(cat2_data$counts)
    if(input$cat2_submitButton ==0) return()
    
    y1 = cat2_data$counts[1]
    y2 = cat2_data$counts[2]
    n1 <- sum(cat2_data$counts[1], cat2_data$counts[3])
    n2 <- sum(cat2_data$counts[2], cat2_data$counts[4])
    p1 <- round(y1/n1,3)
    p2 = round(y2/n2,3)
    
    if(is.null(cat2Estimate$difprop)  ){
      DF <- cat2_estimate_shuffles(1, y1, y2, n1, n2)
      cat2Estimate$selected <- as.numeric(DF[1,3])
      cat2Estimate$difprop <- DF[1,3]
      cat2Estimate$phat1 <- DF[1,1]
      cat2Estimate$phat2 <- DF[1,2]
      cat2Estimate$colors <- blu
    }
    
    #print(c(y1, n1, y2, n2, p1, p2))
    counts <- as.table(matrix(as.numeric(c(y1, y2, n1, n2, p1, p2)), 2, 3))
    colnames(counts) <- c(cat2_data$names[1], "n", "Proportion")
    rownames(counts) <- cat2_data$groups[c(1,3)]
    counts 

  }, digits = c(0,0,0,3))
  

  observeEvent(input$cat2_estimate_shuffle_10, {
    cat2Estimate$CI <- NULL
    cat2Estimate$selected <- NA
    DF <- cat2_estimate_shuffles(shuffles = 10, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                                 n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                                 n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Estimate$difprop <- rbind(cat2Estimate$difprop, as.matrix(DF[,3]))
    cat2Estimate$phat1 <- rbind(cat2Estimate$phat1, as.matrix(DF[,1]))
    cat2Estimate$phat2 <- rbind(cat2Estimate$phat2, as.matrix(DF[,2]))
    cat2Estimate$colors <- rep(blu, length(cat2Estimate$difprop))

  })
  
  observeEvent(input$cat2_estimate_shuffle_100, {
    cat2Estimate$CI <- NULL
    cat2Estimate$selected <- NA
    DF <- cat2_estimate_shuffles(shuffles = 100, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                                 n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                                 n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Estimate$difprop <- rbind(cat2Estimate$difprop, as.matrix(DF[,3]))
    cat2Estimate$phat1 <- rbind(cat2Estimate$phat1, as.matrix(DF[,1]))
    cat2Estimate$phat2 <- rbind(cat2Estimate$phat2, as.matrix(DF[,2]))
    cat2Estimate$colors <- rep(blu, length(cat2Estimate$difprop))

  })
  
  observeEvent(input$cat2_estimate_shuffle_1000, {
    cat2Estimate$CI <- NULL
    cat2Estimate$selected <- NA
    DF <- cat2_estimate_shuffles(shuffles = 1000, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                                 n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                                 n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Estimate$difprop <- rbind(cat2Estimate$difprop, as.matrix(DF[,3]))
    cat2Estimate$phat1 <- rbind(cat2Estimate$phat1, as.matrix(DF[,1]))
    cat2Estimate$phat2 <- rbind(cat2Estimate$phat2, as.matrix(DF[,2]))
    cat2Estimate$colors <- rep(blu, length(cat2Estimate$difprop))

  })
  
  observeEvent(input$cat2_estimate_shuffle_5000, {
    cat2Estimate$CI <- NULL
    cat2Estimate$selected <- NA
    DF <- cat2_estimate_shuffles(shuffles = 5000, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                                 n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                                 n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Estimate$difprop <- rbind(cat2Estimate$difprop, as.matrix(DF[,3]))
    cat2Estimate$phat1 <- rbind(cat2Estimate$phat1, as.matrix(DF[,1]))
    cat2Estimate$phat2 <- rbind(cat2Estimate$phat2, as.matrix(DF[,2]))
    cat2Estimate$colors <- rep(blu, length(cat2Estimate$difprop))

   })
  
  observeEvent(input$cat2_conf80,{
    if(is.null(cat2Estimate$difprop) | (nsims <- length(cat2Estimate$difprop)) < 10){
      return()
    }
    cat2Estimate$confLevel <- .80
    cat2Estimate$colors <- rep(blu, nsims)
    tailCount <- floor(nsims * .1)
    cat2Estimate$colors[1:tailCount] <- rd
    cat2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
    cat2Estimate$CI <- sort(cat2Estimate$difprop)[c(tailCount, nsims + 1 - tailCount)]
    
    })
  
  observeEvent(input$cat2_conf90,{
    if(is.null(cat2Estimate$difprop) | (nsims <- length(cat2Estimate$difprop)) < 10){
      return()
    }
    cat2Estimate$confLevel <- .90
    cat2Estimate$colors <- rep(blu, nsims)
    tailCount <- floor(nsims * 0.05)
    cat2Estimate$colors[1:tailCount] <- rd
    cat2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
    cat2Estimate$CI <- sort(cat2Estimate$difprop)[c(tailCount, nsims + 1 - tailCount)]
    
  })
  
  observeEvent(input$cat2_conf95,{
    if(is.null(cat2Estimate$difprop) | (nsims <- length(cat2Estimate$difprop)) < 10){
      return()
    }
    cat2Estimate$confLevel <- .95
    cat2Estimate$colors <- rep(blu, nsims)
    tailCount <- floor(nsims * 0.025)
    cat2Estimate$colors[1:tailCount] <- rd
    cat2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
    cat2Estimate$CI <- sort(cat2Estimate$difprop)[c(tailCount, nsims + 1 - tailCount)]
    
  })
  
  observeEvent(input$cat2_conf99,{
    if(is.null(cat2Estimate$difprop) | (nsims <- length(cat2Estimate$difprop)) < 10){
      return()
    }
    cat2Estimate$confLevel <- .99
    cat2Estimate$colors <- rep(blu, nsims)
    tailCount <- floor(nsims * 0.005)
    cat2Estimate$colors[1:tailCount] <- rd
    cat2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
    cat2Estimate$CI <- sort(cat2Estimate$difprop)[c(tailCount, nsims + 1 - tailCount)]
    
  })

  output$cat2Estimate_Table <- renderTable({
    req(cat2_data$counts)
    if(input$cat2_submitButton == 0) return()
    
    n1 <- sum(cat2_data$counts[1], cat2_data$counts[3])
    n2 <- sum(cat2_data$counts[2], cat2_data$counts[4])
    
    if(is.null(cat2Estimate$difprop)  ){
      DF <- cat2_test_shuffles(1, cat2_data$counts[1], cat2_data$counts[2], n1, n2)
      cat2Estimate$selected <- as.numeric(DF[1,3])
      cat2Estimate$difprop <- DF[1,3]
      cat2Estimate$phat1 <- DF[1,1]
      cat2Estimate$phat2 <- DF[1,2]
      cat2Estimate$colors <- blu
      props <- DF[1, 1:2]
      y1_new <- as.integer(n1 * DF[1,1])
      y2_new <- as.integer(n2 * DF[1,2])
    } else  if(!is.null(input$cat2_Estimate_click)){
      ##  We already have shuffled data and want to pick the clicked point
      closestPoint <- which.min(abs( cat2Estimate$difprop - input$cat2_Estimate_click$x))
      #cat("Close to number: ", closestPoint, "\n")
      y1_new <- as.integer(n1 * cat2Estimate$phat1[closestPoint])
      y2_new <- as.integer(n2 * cat2Estimate$phat2[closestPoint])
      props <- c(cat2Estimate$phat1[closestPoint], cat2Estimate$phat2[closestPoint] )
      cat2Estimate$selected <-  -diff(props)
      cat2Estimate$colors <- rep( blu, length(cat2Estimate$difprop))
      #cat2Estimate$colors[closestPoint] <- grn
      
      ## cat(c(n1, n2, props, y1_new, y2_new), "\n")
    } else {
      return()
    }
    
    #diff.p <- DF[1,1] - DF[1,3]
    #print(c(y1_new, n1, DF[1,1], y2_new, n2, DF[1,2], diff.p))
    count2 <- as.table(matrix(as.numeric(c(y1_new, y2_new, n1, n2, props)), 2, 3))
    colnames(count2) <- c(cat2_data$names[1], "n", "Proportion")
    rownames(count2) <- cat2_data$groups[c(1,3)]
    count2
  }, digits = c(0,0,0,3))
  
  
  output$cat2Estimate_Plot2 <- renderPlot({
    req(cat2Estimate$difprop)
    
    DF <- sort(cat2Estimate$difprop)
    
    if(length(DF) == 1){
      w <- 1
      radius = 4
    } 
    else {
      #nbreaks <- 0.5 * nclass.Sturges(DF)^2
      #z <- cut(DF, breaks = nbreaks)
      #w <- unlist(tapply(z, z, function(V) 1:length(V)))
      w <- newy(DF) #w[!is.na(w)]
      #print(w)
      #print(max(w))
      nsims <- length(DF)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(DF, w, ylab = "", ylim = c(0.5, pmax(10, max(w))), cex = radius/2, pch = 16, col = cat2Estimate$colors,  
         xlab = expression(hat(p)[1] - hat(p)[2]), main = "Re-Sampling Distribution")
    #mtext(side = 1, at = cat2Estimate$observed, line = 0, bquote(hat(p)[1] - hat(p)[2]))
    legend("topright", bty = "n", paste(length(DF), "points \n Mean = ", 
                                       round(mean(DF),3), "\n SE = ", round(sd(DF),3)))
    #mtext(side = 3, at = 0, adj = 1, line = 0, bquote(hat(p)[1] - hat(p)[2]))
    }, height = 450, width = 600)

  }

  ### Normal Probs ----------------------------------------------------- cat 2  
{
 cat2_normalProb <- reactiveValues(prob = NULL, z = NULL, findP = NULL)

 observeEvent( input$cat2_z_txt, {
  cat2_normalProb$z <- as.numeric(input$cat2_z_txt) 
  cat2_normalProb$findP <- TRUE
 })

 observeEvent( input$cat2_prob_txt,{
  cat2_normalProb$prob <- as.numeric(input$cat2_prob_txt) 
  cat2_normalProb$findP  <- FALSE
 })

output$normalProbPlot2 <- renderPlot({ 
  #print(cat2_normalProb$prob)
  #print(cat2_normalProb$z)
  #print(cat2_normalProb$findP)
  req(cat2_normalProb$findP)
  par(mar=c(24,1,1,1)/10)
  z <- absz <- prob <- yrr <- xrr <- NA
  x <- -300:300 / 50
  
  if(!cat2_normalProb$findP & !is.na(cat2_normalProb$prob)){
    ## given prob, find z
    prob <- cat2_normalProb$prob
    #cat("finding z for p = ", prob, "\n")
    if(input$cat2_area == "Lower"){ ##  left tail
      z <-  qnorm(prob)
      if(z < min(x))  x <- c(1.01 * z, x)
      ## rejection region in x dimension
      xrr <- c(x[x < z], z, z)
      ## density curve over xrr:
      yrr <- c( dnorm(xrr[-length(xrr)]), 0)
    } else if(input$cat2_area == "Upper"){   ## right tail        
      z <- qnorm(1 - prob) 
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(z, z, x[x > z])
      yrr <- c(0, dnorm(xrr[-1]))
    } else if(input$cat2_area == "Center"){
      z <- abs(qnorm( (1 - prob)/2) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- seq(-z, z, length=100)
      yrr <- c(0, dnorm(xrr[2:99]), 0)
    } else if(input$cat2_area == "Extremes"){
      z <- abs(qnorm(1-prob/2) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(-z, x[x < -z], -z)
      yrr <- c(0,  dnorm(xrr[-1]))
    }
    absz <- abs(z)
  }
  if(cat2_normalProb$findP & !is.na(cat2_normalProb$z)){
    z <- cat2_normalProb$z
    ##  find probability
    absz <- abs(z)
    maxX <- pmax(z, 6)
    minX <- pmin(z, -6)
    # cat("finding p ", minX, maxX, "for z = ", z, "\n")
    x <- seq(minX, maxX, length=200)
    prob <-  1 - pnorm(z) 
    xrr <- c(z, z, x[x > z])  ## right tail
    yrr <- c(0,  dnorm(xrr[-1]) )
    if(input$cat2_area == "Lower"){         ##  left tail
      prob =  pnorm(z) 
      xrr <- c(z, x[x < z], z)
      yrr <- c(0,  dnorm(xrr[-1]))
    } else if (input$cat2_area == "Extremes"){ ##  extremes
      xrr <- c( -absz, x[x < -absz], -absz)
      yrr <- c(0, dnorm(xrr[-1]))
      prob = pnorm(-absz) 
      #yrr <- c(yrr, NA,NA, rev(yrr))
      #xrr <- c(xrr, NA,NA, rev(-xrr))
    } else if (input$cat2_area == "Center"){   ##  center
      prob <- cat2_normalProb$prob
      xrr <- seq(-absz, absz, length=100)
      yrr <- c(0, dnorm(xrr), 0)
      xrr <- c(-absz, xrr, absz)
      prob <- diff( pnorm(c(-absz,absz)) )
    }
  } 
  plot(x, dnorm(x), type = "l", bty='n', xlab="Z Score", ylab="", yaxt="n")
  abline(h=0)
  max.height <- dnorm(0) *.9
  text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
  segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
  
  if(input$cat2_area == "Extremes") {  ## extremes
    polygon(xrr, yrr, col = rd)
    polygon(-xrr, yrr, col = rd)
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
         round(prob * ifelse(cat2_normalProb$findP,1,.5), 3), col = "darkblue")
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y = text.height, round(c(-absz,absz),3))
  } else if (input$cat2_area == "Center") {   ## fill & label center
    polygon(xrr, yrr, col = grn)
    text(x=0, y= text.height, round(prob,3))
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y=text.height, round(c(-absz,absz),3))
  } else {          ## show tails
    polygon(xrr, yrr, col = rd)
    text( z, y = text.height, round(z, 3))
    text(x= sign(z) * (absz+4) / 2 , y= max(yrr) / 2 + 0.02, 
         round(prob,3), col = "darkblue")
  }
  
}, height=300)
}

 ## 2 Quantitative -----------------------------------------------------------  2 quant

  ###  Data Entry ------------------------------------------------------------  q2
{
   q2Test <- reactiveValues( shuffles = NULL, slopes = NULL, corr = NULL, observed = NULL,
                          colors = blu, moreExtremeCount = NULL, direction = NULL, cutoff = NULL, 
                          sampleCount = NULL, pvalue = NULL)


  q2Estimate <- reactiveValues( resamples = NULL,   slopes = NULL,  corr =  NULL,
                              observed = NULL,    confLevel = NULL, colors = blu,
                              CI = NULL, param = NULL)

  ##  grab data according to input method
  q2 <- reactiveValues(data = NULL, names = NULL, intercept = NULL, slope = NULL, 
                       corr = NULL, qr = NULL)
  
  output$quant2DataIn <- renderText({
  "How do you want to input the data?"
 })

 # use  selectInput to grab the 3 types of input
 output$q2_ui <- renderUI({
  req(input$q2_entry)

  switch( input$q2_entry,
          "Pre-Loaded Data" ={ 
            fluidRow(  
              column(4, selectInput('q2_data1', 'Available Datasets',  choices = as.list(quant2_contents))
              ),
              column(4, actionButton("q2_useLddBtn", "Use These Data", class="btn btn-success") )
            )
          },
          "Local CSV File" ={
            ## copied from: http://shiny.rstudio.com/gallery/file-upload.html    
            fluidRow(  
              column(4, fileInput('q2_file1', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')) ,
                     br(),
                     actionButton("q2_useCSVBtn", "Use These Data", class="btn btn-primary")
              ),
              column(3, checkboxInput('q2_header', 'Row One is column names', TRUE)
              ),
              column(3, radioButtons('q2_sep', 'Separator',
                                     c(Comma=',', Semicolon=';', Tab='\t'),
                                     ',')
              ),
              column(2, radioButtons('q2_quote', 'Quote',
                                     c(None='', 'Double Quote'='"','Single Quote'="'"),
                                     '"')
              )
            )
          },
          "Type/Paste into Text Box" = {
            div(
              HTML('<textarea name="q2_text" cols="30" rows="10"></textarea>'),
              actionButton("q2_useText", "Use These Data", class="btn btn-primary")
            )
          },
          
#           "Type/Paste into Data Table" = {
#             #h4("Edit the values in Column 2.  Column 1 will be ignored.  To paste, use Cntrl-V or Cmd-V(on a mac)"), 
#             fluidRow(
#               column(4, 
#                      rHandsontableOutput("q2_hot")) 
#               ,
#               column(4, actionButton("q2_useHotBtn", "Use These Data", class="btn btn-primary"))
#             )            
#           }, 
          NULL
  )
  ##  Need to grab names from the data input
})

observeEvent(  input$q2_useLddBtn, {
  ##  Wipe out any old data  
  q2Test$shuffles <- q2Test$intercepts <- q2Test$slopes <- q2Test$corr <- q2Test$observed <- 
    q2Test$colors <- q2Test$moreExtremeCount <- q2Test$pvalue <- NULL
  q2Estimate$resamples <- q2Estimate$slopes <- q2Estimate$corr <- q2Estimate$observed <-
    q2Estimate$CI <- q2Estimate$colors <- NULL
  q2Estimate$param <- "Slope"
  
  DF <- eval(parse( text = input$q2_data1))
  q2$names <- names(DF) 
  q2$data <- data.frame(DF)
  names(q2$data) <- c("x","y")
  output$quant2DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true slope or correlation"
  })
})

observeEvent(  input$q2_useCSVBtn,{
  ##  Wipe out any old data    
  q2Test$shuffles <- q2Test$intercepts <-q2Test$slopes <- q2Test$corr <- q2Test$observed <- 
    q2Test$colors <- q2Test$moreExtremeCount <- q2Test$pvalue <- NULL
  q2Estimate$resamples <- q2Estimate$slopes <- q2Estimate$corr <- q2Estimate$observed <-
    q2Estimate$intercepts <- q2Estimate$CI <- q2Estimate$colors <- NULL
  q2Estimate$param <- "Slope"
  
  DF <- read.csv(input$q2_file1$datapath, header=input$q2_header,
                      sep=input$q2_sep, quote=input$q2_quote)
  q2$names <- names(DF)
  q2$data <- data.frame(DF)
  names(q2$data) <- c("x","y")
  output$quant2DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true slope or correlation"
  })
  
})

# observeEvent(  input$q2_useHotBtn,{
#   ##  Wipe out any old data  
#   q2Test$shuffles <- q2Test$slopes <- q2Test$corr <- q2Test$observed <- 
#     q2Test$colors <- q2Test$moreExtremeCount <- q2Test$pvalue <- NULL
#   q2Estimate$resamples <- q2Estimate$slopes <- q2Estimate$corr <- q2Estimate$observed <-
#     q2Estimate$CI <- q2Estimate$colors <- NULL
#   
#   DF <- data.frame(q2_values[["hot"]])
#   # print(DF)
#   q2$names <- names(DF) 
#   q2$data <- data.frame(DF)
#   names(q2$data) <- c("x","y")
#   output$quant2DataIn <- renderText({
#     "Data are entered, you may now choose to estimate or test the true slope or correlation"
#   })
# })

q2_values = list()

observeEvent(input$q2_useText,{
  if(nchar(input$q2_text) < 1){
    return()
  }
  #print(input$q2_text)
  if (grepl(",", input$q2_text)){          ## check for commas,
    q2Text <- gsub(","," ",input$q2_text)  ## replace commas with spaces
  } else {
    q2Text <- input$q2_text
  }
  ## tempData <- input$q2_text # readLines(text = ) ## read in as text
  if(is.na(as.numeric(unlist(strsplit(q2Text," "))))){ # is the first "word" numeric or character?
    tempData <- read.table(text = q2Text, head = TRUE)      
    q2$names <- names(tempData)
  } else{
    q2$names <- c("x","y")                           # numeric, so name it "x"
    tempData <- read.table(text = q2Text, head = FALSE)
    names(tempData) <- q2$names
  }
  print(tempData)
  q2Test$nsims <- 0
  q2$data <- tempData
  names(q2$data) <- c("x","y")
  q2Test$shuffles <-  q2Test$new.xbars <-  q2Test$xbar <-   q2Test$colors <- NULL
  q2Estimate$shuffles <- q2Estimate$xbars <- q2Estimate$observed <- q2Estimate$colors <- NULL
  output$quant2DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test slope"
  })
})

#q2_setHot = function(x) q2_values[["hot"]] <<- x

# output$q2_hot = renderRHandsontable({
#   if (!is.null(input$q2_hot)) {
#     q2_DF = hot_to_r(input$q2_hot)
#     q2_setHot(q2_DF)
#     rhandsontable(q2_DF) %>%
#       hot_table(highlightCol = TRUE, highlightRow = TRUE)
#   } else {
#     ## seems that HOT needs at least 2 columns, so column 1 is just row numbers.
#     q2_DF = read.csv("data/dummyData.csv", stringsAsFactors = FALSE, head = TRUE)
#     q2_setHot(q2_DF)    
#     rhandsontable(q2_DF, height = 230) %>%
#       hot_table(highlightCol = TRUE, highlightRow = TRUE)
#   }
# })
}

###  Data Summary ----------------------------------------------------------  q2

{
output$q2_Plot <- renderPlot( {
  req(q2$data)

  #isolate( { 
    ## need to allow user to switch predictor and response
    ## make plot
    DF <- q2$data
    q2_plot1 <- ggplot(data = DF) +geom_boxplot(aes(y= x, x = x)) +
      theme_bw() + ylab(q2$names[1]) + xlab("") + scale_x_continuous(breaks = c(-1,1000)) +  coord_flip()
    q2_plot2 <- ggplot(data = DF) +geom_boxplot(aes(y= y, x = y)) +
    theme_bw() + ylab(q2$names[2]) + xlab("") + scale_x_continuous(breaks = c(-1,1000)) +  coord_flip()
    #par(mar=c(24, 40, 10, 35)/10, mfrow=c(2,1))
    #boxplot(q2_dataDF, horizontal = TRUE, main = "")
    #myBlue <- rgb(0, 100/256, 224/256, alpha = .8)  
    q2_plot3 <- qplot(data= DF, x=x, y=y, colour = I(blu), size = I(4)) + theme_bw() +
                  xlab(q2$names[1]) + ylab(q2$names[2]) + stat_smooth(method ="lm", se = FALSE)
    grid.arrange(q2_plot1, q2_plot2, q2_plot3, heights = c(1.5, 1.5, 5)/8, ncol=1)
  #}
}, height=400)

output$q2_Summary <- renderTable({
  req(q2$data)  
  #isolate({
  fit0 <- lm(y ~ x, data = q2$data)
  q2$intercept <- coef(fit0)[1]
  q2$slope <- coef(fit0)[2]
  q2$corr <- cor(q2$data[,1], q2$data[,2])
  DF <- rbind(mean   = apply(q2$data, 2, mean, na.rm = TRUE ),
                sd     = apply(q2$data, 2, sd, na.rm = TRUE),
                min    = apply(q2$data, 2, min),
                Q1     = apply(q2$data, 2, quantile, .25),
                median = apply(q2$data, 2, median),
                Q3     = apply(q2$data, 2, quantile, .75),
                max    = apply(q2$data, 2, max),
                n = apply(q2$data, 2, length),
                correlation = c(q2$corr, NA))
                #beta.hat = round(coef(fit0),3),
                #"resid SD" = c(summary(fit0)$sigma, NA) )
    colnames(DF) <- q2$names
    DF
  #})
})

output$q2_headRegrLine <- renderText({
  req(q2$data)
  "Least Squares line: "
})

output$q2_SLR_line <- renderText({
  req(q2$data)  
  beta.hat = round(coef(lm(y ~ x, q2$data)),4)
  paste( q2$names[2], " = ", beta.hat[1], " + ", beta.hat[2], " * ", q2$names[1], sep = "")  
})
  

observeEvent(  input$q2_swapXwithY,{
  req(q2$data)

  q2$names <- q2$names[2:1]
  q2$data <- q2$data[, 2:1]
  names(q2$data) <- c("x","y")
  })

output$q2_swap <- renderUI({
  (q2$data)
  actionButton('q2_swapXwithY', "Swap Variables (X goes to Y)", class="btn btn-primary")
})
}

###   TESTING slope / correlation = 0 ---------------------------------------  q2
{

## q2 test UI -------------------------------------
output$q2_testUI <- renderUI({
  if( is.null(q2$data)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else {
    fluidPage(
      fluidRow(
        column(2, offset =2,
          h4('Test either')),
        column(3, 
          radioButtons("q2_TestParam", label = "", list("Slope  OR"," Correlation"),
                       "Slope  OR", inline = TRUE)
          ),
        column(3,  h4('is zero'))
      ),
      fluidRow(
        ## for 1 shuffle, show equal size plots of original and reshuffled x,y data
        ##  for more shuffles, make original data and click --> shuffle plots smaller, large plot of 
        ##  sampling distribution for slope / correlation.
        column(5, 
               plotOutput('q2_TestPlot1')
        ),
        column(6,
               uiOutput('q2_SampDistPlot')
        )
      ),
      fluidRow(
        column(4, offset = 1, 
               h4("How many more shuffles?")),
        column(1,       actionButton("q2_shuffle_10", label = "10", class="btn btn-primary")),
        column(1,       actionButton("q2_shuffle_100", label = "100", class="btn btn-primary")),
        column(1,       actionButton("q2_shuffle_1000", label = "1000", class="btn btn-primary")),
        column(1,       actionButton("q2_shuffle_5000", label = "5000", class="btn btn-primary"))
      ),
      br(),
      fluidRow(
        column(5, offset = 4,  h4("Click on a point to see that shuffle"))
      ),
      fluidRow(
        column(10, offset = 2, 
               uiOutput("slopeTestXtremes")
        )
      ),
      fluidRow(
        column(8, offset = 4, 
               uiOutput("slopeTestPvalue")
        )
      )
    )
  }
})


output$q2_SampDistPlot <- renderUI({ 
  plotOutput('q2_TestPlot2', click = 'q2Test_click')
})

output$slopeTestPvalue <- renderUI({
  req(q2Test$moreExtremeCount)
    h4(pvalue2print(q2Test$moreExtremeCount,  q2Test$sampleCount, q2Test$direction, 
                    q2Test$cutoff, q2Test$pvalue))
})

output$slopeTestXtremes <- renderUI({
  fluidRow(
    column(3, 
           h4("Count values equal to or")  ),
    column(3,
           tags$div(style="width: 200px",
                    tags$select(id='q2_testDirection',class="form-control",
                                tags$option( value = "less", "less"),
                                tags$option( value = "more extreme", "more extreme", selected = TRUE),
                                tags$option( value = "greater", "greater")))  ),
    column(1, h4("than ")),
    column(3,           
           tags$div( 
             tags$input(id = "q2_cutoff", type = "text", class = "form-control", value = NA)) ),
    column(1, actionButton("q2_countXtremes","Go", class="btn btn-success")   )
  )
}) 

output$q2_TestPrep <- renderTable({
#  names(q2$data) <- c("x","y")
  fit0 <- lm( y ~ x, q2$data)
  q2$slope <- coef(fit0)[2]
  q2$qr <- fit0$qr
  q2$corr <- cor(q2$data[,1], q2$data[,2])
  out <- list("Correlation: " =  q2$corr,
              "Slope: " = q2$slope )
  out <- do.call(rbind, out)
  colnames(out) <- c("Estimate")
  out
})


output$q2_TestPlot1 <- renderPlot({
  req(q2$data)

  fit0 <- lm( y ~ x, q2$data)
  q2$slope <- coef(fit0)[2]
  q2$qr <- fit0$qr
  q2$corr <- cor(q2$data[,1], q2$data[,2])
  par(mfrow=c(2,1), mar = c(2.4,2,4,2))
  DF0 <- q2$data
  plot(y ~ x, data=DF0, xlab = q2$names[1], ylab = q2$names[2], col = blu, pch = 16,
       main = "Original Data")
  #lmfit0 <- lm(y ~ x, DF0)
  if(!is.null(input$q2Test_click) ){
    closestPnt <- ifelse( input$q2_TestParam == "Slope  OR", 
                          which.min(abs( q2Test$slopes - input$q2Test_click$x)),
                          which.min(abs( q2Test$corr - input$q2Test_click$x)))
    hiLiteShuffle <- q2Test$shuffles[, closestPnt]
  } else if(!is.null(q2Test$shuffles) ){
    closestPnt <- ncol(q2Test$shuffles)
    ##resample <- q2Test$shuffles[, closestPnt]
  } else {
    shuffle <- q2Test$shuffles <- matrix( sample(1:nrow(q2$data)), ncol = 1)
    newCoef <- coef( lm(q2$data$y[shuffle] ~ q2$data$x))
    q2Test$intercepts <- newCoef[1]
    q2Test$slopes <- newCoef[2]
    q2Test$corr <-  cor(q2$data$x, q2$data$y[shuffle])
    closestPnt <- 1 
  }
  nShuffles <- length(q2Test$slopes) 
  #print(nShuffles)
  if(nShuffles > 1){
    for(ndx in 1:nShuffles)
     abline(q2Test$intercepts[ndx], q2Test$slopes[ndx], col = rgb(0,1,0,1/50))
  }
  
  if(closestPnt > 1){
    abline(q2Test$intercepts[closestPnt], q2Test$slopes[closestPnt], col = rd)
    #print(closestPnt)
  }
  abline(fit0)
  mtext(side = 3, line=.4, at = min(DF0$x)/3 + max(DF0$x)*2/3, bquote(r == .(round(q2$corr,3))))
  mtext(side = 3,   at = min(DF0$x)*2/3 + max(DF0$x)/3, bquote(hat(beta)[1] == .(round(q2$slope,3))))
 
  DF0$newy <- q2$data$y[q2Test$shuffles[, closestPnt] ]
  q2Test$colors <- rep( blu, length(q2Test$slopes))
  
  plot(y ~ x, data = DF0, xlab = q2$names[1], ylab = q2$names[2], col = blu, pch =16,
       main = "Shuffled Data")
  points(newy ~ x, data = DF0, pch = 16, col = "green")
  points(newy ~ x, data = DF0)
  with(subset(DF0, abs(y - newy) > .0001),
    arrows(x, y, x, newy, col = grey(.5), length = .1, angle = 15)
  )
  abline(q2Test$intercepts[closestPnt], q2Test$slopes[closestPnt])
  #abline(lm(y ~ x, data = DF0))
  beta1hat <- round(q2Test$slopes[closestPnt], 3)
  rhat1 <- round(q2Test$corr[closestPnt], 3)
  mtext(side = 3, at = min(DF0$x)*2/3 + max(DF0$x)/3, bquote(hat(beta)[1] == .(beta1hat) ) )
  mtext(side = 3, line=.4, at = min(DF0$x)/3 + max(DF0$x)*2/3, bquote(r == .(rhat1)))
}, height = 400, width = 300)

observeEvent(input$q2_TestParam, {
  q2Test$moreExtremeCount <- NULL
})

observeEvent(input$q2_shuffle_10, {
  q2Test$moreExtremeCount <- NULL
   newShuffles <-  sapply(1:10, function(x) sample(1:nrow(q2$data)))
   q2Test$shuffles <- cbind(q2Test$shuffles, newShuffles)
  newCoef <- qr.coef(q2$qr, matrix(q2$data$y[newShuffles], ncol=10))
  q2Test$intercepts <- c(q2Test$intercepts, newCoef[1,])
  q2Test$slopes <- c(q2Test$slopes, newCoef[2,])
#  q2Test$slopes <- c(q2Test$slopes, apply(newShuffles, 2, function(ndx) qr.coef(q2$qr, q2$data$y[ndx])[2]))
  q2Test$corr <- c(q2Test$corr, apply(newShuffles, 2, function(ndx) cor(q2$data$x, q2$data[ndx, 2])))
  q2Test$colors <- rep(blu, length(q2Test$slopes))
})

observeEvent(input$q2_shuffle_100, {
  q2Test$moreExtremeCount <- NULL
  newShuffles <-  sapply(1:100, function(x) sample(1:nrow(q2$data)))
  q2Test$shuffles <- cbind(q2Test$shuffles, newShuffles)
  newCoef <- apply(newShuffles, 2, function(x) qr.coef(q2$qr, q2$data[x, 2]))
  q2Test$intercepts <- c(q2Test$intercepts, newCoef[1,])
  q2Test$slopes <- c(q2Test$slopes, newCoef[2,])
#  q2Test$slopes <- c(q2Test$slopes, apply(newShuffles, 2, function(ndx) qr.coef(q2$qr, q2$data$y[ndx])[2]))
  q2Test$corr <- c(q2Test$corr, apply(newShuffles, 2, function(ndx) cor(q2$data$x, q2$data[ndx, 2])))
  q2Test$colors <- rep(blu, length(q2Test$slopes))
})
observeEvent(input$q2_shuffle_1000, {
  q2Test$moreExtremeCount <- NULL
  newShuffles <-  sapply(1:1000, function(x) sample(1:nrow(q2$data)))
  q2Test$shuffles <- cbind(q2Test$shuffles, newShuffles)
  newCoef <- apply(newShuffles, 2, function(x) qr.coef(q2$qr, q2$data[x, 2]))
  q2Test$intercepts <- c(q2Test$intercepts, newCoef[1,])
  q2Test$slopes <- c(q2Test$slopes, newCoef[2,])
#  q2Test$slopes <- c(q2Test$slopes, apply(newShuffles, 2, function(ndx) qr.coef(q2$qr, q2$data$y[ndx])[2]))
  q2Test$corr <- c(q2Test$corr, apply(newShuffles, 2, function(ndx) cor(q2$data$x, q2$data[ndx, 2])))
  q2Test$colors <- rep(blu, length(q2Test$slopes))
})
observeEvent(input$q2_shuffle_5000, {
  q2Test$moreExtremeCount <- NULL
  newShuffles <-  sapply(1:5000, function(x) sample(1:nrow(q2$data)))
  q2Test$shuffles <- cbind(q2Test$shuffles, newShuffles)
  newCoef <- apply(newShuffles, 2, function(x) qr.coef(q2$qr, q2$data[x, 2]))
  q2Test$intercepts <- c(q2Test$intercepts, newCoef[1,])
  q2Test$slopes <- c(q2Test$slopes, newCoef[2,])
 # q2Test$slopes <- c(q2Test$slopes, apply(newShuffles, 2, function(ndx) qr.coef(q2$qr, q2$data$y[ndx])[2]))
  q2Test$corr <- c(q2Test$corr, apply(newShuffles, 2, function(ndx) cor(q2$data$x, q2$data[ndx, 2])))
  q2Test$colors <- rep(blu, length(q2Test$slopes))
})

observeEvent(input$q2_countXtremes, {
  if(input$q2_TestParam == "Slope  OR") {
    parm <-  q2Test$slopes
  } else { 
    parm <- q2Test$corr
  }
  parm <- sort(parm)
  nsims <- length(parm)
  q2Test$colors <- rep(blu, nsims)
  q2Test$cutoff <- threshold <- as.numeric(input$q2_cutoff)
  q2Test$direction <- input$q2_testDirection
  if(nsims > 9 & !is.na(input$q2_testDirection)){
    redValues <-  switch( input$q2_testDirection,
                        "less" = which(parm <= threshold + 1.0e-10),
                        "greater" = which(parm >= threshold - 1.0e-10),
                        "more extreme" = c(which(parm <= -abs(threshold) + 1.0e-10 ), 
                                           which(parm >= abs(threshold) -1.0e-10 ) )  )
    q2Test$colors[redValues] <- rd       
    q2Test$moreExtremeCount  <- length(redValues)
    q2Test$pvalue <- q2Test$moreExtremeCount/nsims
    q2Test$sampleCount <- length(q2Test$slopes)
  }
})

observeEvent( input$q2_testDirection, {
  q2Test$pvalue <- q2Test$moreExtremeCount <- NULL  
  q2Test$colors <- rep(blu, length(q2Test$colors))
})

observeEvent( input$q2_test_cutoff, {
  q2Test$pvalue <- q2Test$moreExtremeCount <- NULL  
  q2Test$colors <- rep(blu, length(q2Test$colors))  
})


output$q2_TestPlot2 <- renderPlot({
  req( q2Test$shuffles) 

  if(input$q2_TestParam == "Slope  OR") {
      parm <-  q2Test$slopes
      q2Test$observed <- q2$slope
   } else { 
      parm <- q2Test$corr
      q2Test$observed <- q2$corr
  }
  parm <- sort(parm)
  ## print(summary(parm))
  if(length(parm) == 1){
    y <- .5
    radius <- 4
  } else {
#     z <- cut(parm, breaks = .5 * nclass.Sturges(parm)^2 )
# #  print(summary(z))
#     y <- unlist(tapply(z, z, function(v) 1:length(v)))
#     y <-  y[!is.na(y)]
    q2Test$y <- y <- newy(parm)
    nsims <- length(parm)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(parm, y, ylab = "", cex = radius/2, pch = 16, col = q2Test$colors,   ylim=c(.45, pmax(10,max(y))),
        xlab = ifelse(input$q2_TestParam == "Slope  OR", "Slope", "Correlation"), main = "Sampling Distribution")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                     round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
}, height = 400, width = 400)


}
  ###  Estimate slope / correlation with CI ----------------------------------- q2 
  ##  
{

output$q2_estimateUI <- renderUI({
  if( is.null(q2$data)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else {
    fluidPage(   
      fluidRow(
        column(3,       h3("Estimate either")        ),
        column(4, uiOutput('q2_getEstParam') )
        ),
      fluidRow(
        ##  show plots of original data  and one of resampled x,y data
        ##  by default,show latest resample. Allow user to click on a point to see the data which have that slope
      
        column(5, 
               plotOutput('q2_EstPlot1')
        ),
        column(6,
               uiOutput('q2_EstResampDistn')
        )),
      fluidRow(
        column(4, offset = 1, 
               h4("How many more resamples?")),
        column(1,  actionButton("q2_resample_10", label = "10", class="btn btn-primary")),
        column(1,  actionButton("q2_resample_100", label = "100", class="btn btn-primary")),
        column(1,  actionButton("q2_resample_1000", label = "1000", class="btn btn-primary")),
        column(1,  actionButton("q2_resample_5000", label = "5000", class="btn btn-primary"))
      ),
      fluidRow(
        column(4, offset = 5, h4("Click on a point to see that resample"))
      ),
      br(),
      fluidRow(
        column(4, offset = 2, h4("Select Confidence Level (%)")
        ),
        column(6, uiOutput('q2_confLevels')
        )
      ),
        fluidRow(
          column(8, offset = 4, uiOutput('q2_showCI') )
        )
    )
  }
})

output$q2_showCI <- renderUI({
  req(q2Estimate$CI)

  h4(paste(round(100 * q2Estimate$confLevel), "% Confidence Interval Estimate for ",
           q2Estimate$param,": (", 
           round(q2Estimate$CI[1],3), ",", 
           round(q2Estimate$CI[2], 3), ")"))
}) 

output$q2_getEstParam <- renderUI({
  radioButtons('q2_EstParam', label = "", list("Slope  OR","Correlation"), 
               "Slope  OR", inline = TRUE)
})

output$q2_confLevels <- renderUI({        
  fluidRow(
  column(2,  actionButton('q2_conf80', label = "80", class="btn btn-primary")),
  column(2,  actionButton('q2_conf90', label = "90", class="btn btn-primary")),
  column(2,  actionButton('q2_conf95', label = "95", class="btn btn-primary")),
  column(2,  actionButton('q2_conf99', label = "99", class="btn btn-primary"))
)
})

observeEvent(input$q2_EstParam, {
  temp <- q2Estimate$param 
  if(input$q2_EstParam == "Correlation" ){
    q2Estimate$param <- "Correlation"
  } else{
    q2Estimate$param <- "Slope"
  }
  if( (temp == "Slope" & input$q2_EstParam != "Slope  OR") ||
      (temp == "Correlation" & input$q2_EstParam != "Correlation")) {
       q2Estimate$CI <- NULL
     }
  } 
) 

output$q2_EstResampDistn <- renderUI({
  plotOutput('q2_EstPlot2', click = 'q2Est_click')
})

  output$q2_EstTable1 <- renderTable({
  ##  print("in q2_CIPrep")
    out <- list("Correlation: " =  q2$corr,
                "Slope: " = q2$slope )
    out <- do.call(rbind, out)
    colnames(out) <- c("Estimate")
    out
  })
  
  output$q2_EstPlot1 <- renderPlot({
    req(q2$slope)

    ## change these to look like the "Test" plots with values in subtitle.
    DF0 <- q2$data
    par(mfrow=c(2,1), mar = c(4.1,5.1,3.1,.1) )
    plot(y ~ x, data=DF0, xlab = q2$names[1], ylab = q2$names[2], col = blu, pch = 16,
        main = "Original Data")
    mtext(side = 3, line = .0, at = min(DF0$x)*4/5 + max(DF0$x)/5, bquote(hat(beta)[1] == .(round(q2$slope,3))))
    mtext(side = 3, line = .2, at = min(DF0$x)/5 + max(DF0$x)*4/5, bquote(r == .(round(q2$corr,3))))
   ## Check for click on a point
      if(!is.null(input$q2Est_click) ){
        closestPnt <- ifelse( input$q2_EstParam == "Slope  OR", 
          which.min(abs( q2Estimate$slopes - input$q2Est_click$x)),
          which.min(abs( q2Estimate$corr - input$q2Est_click$x)))
            ## print(closestPnt)
          resample <- q2Estimate$resamples[, closestPnt]
      } else if(!is.null(q2Estimate$resamples) ){
        closestPnt <- ncol(q2Estimate$resamples)
        resample <- q2Estimate$resamples[, closestPnt]
      } else {
        resample <- q2Estimate$resamples <- matrix(
               sample(1:nrow(q2$data), nrow(q2$data), replace = TRUE), ncol = 1)
        newCoef <- coef( lm(y ~ x, data = q2$data[resample, ]))
        q2Estimate$intercepts <- newCoef[1]
        q2Estimate$slopes <- newCoef[2]
        q2Estimate$corr <- with(q2$data[resample, ], cor(x, y))
        closestPnt <- 1 
      }
    nSims <- length(q2Estimate$slopes) 
    if(nSims > 1){
      for(ndx in 1:nSims)
        abline(q2Estimate$intercepts[ndx], q2Estimate$slopes[ndx], col = rgb(0,1,0,1/50))
    }
    abline(q2$intercept, q2$slope)
    
    if(closestPnt > 0){
      abline(q2Estimate$intercepts[closestPnt], q2Estimate$slopes[closestPnt], col = rd)
      #print(closestPnt)
    }
    
    ## Bootstrap resample 
    DF1 <- q2$data[q2Estimate$resamples[, closestPnt], ]
      q2Estimate$colors <- rep( blu, length(q2Estimate$slopes))
      DF1$sizes <- rep(table(resample), table(resample))/2
      ##
      plot(y ~ x, data = DF1, xlab = q2$names[1], ylab = q2$names[2], col = grn, pch =16,
           cex = sizes, main = "Re-sampled Data", xlim = range(DF0$x), ylim = range(DF0$y))
      lmfit1 <- lm(y ~ x, DF1)
      abline(q2Estimate$intercepts[closestPnt], q2Estimate$slopes[closestPnt] )
      beta1hat <- round(q2Estimate$slopes[closestPnt], 3)
      rhat1 <- round(q2Estimate$corr[closestPnt], 3)
      mtext(side = 3, line = 0, at = min(DF0$x)*4/5 + max(DF0$x)/5, bquote(hat(beta)[1] == .(beta1hat) ) )
      mtext(side = 3, line = .2, at = min(DF0$x)/5 + max(DF0$x)*4/5, bquote(r == .(rhat1)))
      corner <- ifelse(beta1hat>=0, "bottomright", "topright")
      repeats <- 1:3
      legend(corner,cex = .8, legend = as.character(repeats), title = "Repeats", pt.cex = repeats/2, pch = 16, col = "green", bty = "n")
    #grid.arrange(plotOrig, plotNew, heights = c(4,  4)/8, ncol=1)
  }, height = 400, width = 300)
  
  observeEvent(input$q2_resample_10, {
    q2Estimate$CI <- NULL
    resamples <-  sapply(1:10, function(x) sample(1:nrow(q2$data), replace = TRUE))
    resamples <- resamples[, apply(resamples, 2, function(ndx) var(ndx) > 1.e-15) ]
    q2Estimate$resamples <- cbind(q2Estimate$resamples, resamples)
    newCoefs <- apply(resamples, 2, function(ndx) coef(lm(y ~ x, q2$data[ndx, ])))
    q2Estimate$intercepts <- c(q2Estimate$intercepts, newCoefs[1, ])
    q2Estimate$slopes <- c(q2Estimate$slopes, newCoefs[2, ])
    q2Estimate$corr <- c(q2Estimate$corr, apply(resamples, 2, function(ndx) cor(q2$data$x[ndx], q2$data$y[ndx])))
    q2Estimate$colors <- rep(blu, length(q2Estimate$slopes))
  })
  
observeEvent(input$q2_resample_100, {
  q2Estimate$CI <- NULL
  resamples <-  sapply(1:100, function(x) sample(1:nrow(q2$data), replace = TRUE))
  resamples <- resamples[, apply(resamples,2, function(ndx) var(ndx) > 1.e-15) ]
  q2Estimate$resamples <- cbind(q2Estimate$resamples, resamples)
  newCoefs <- apply(resamples, 2, function(ndx) coef(lm(y ~ x, q2$data[ndx, ])))
  q2Estimate$intercepts <- c(q2Estimate$intercepts, newCoefs[1, ])
  q2Estimate$slopes <- c(q2Estimate$slopes, newCoefs[2, ])
#  q2Estimate$slopes <- c(q2Estimate$slopes, apply(resamples, 2, function(ndx) coef(lm(y ~ x, q2$data[ndx, ]))[2]))
  q2Estimate$corr <- c(q2Estimate$corr, apply(resamples, 2, function(ndx) cor(q2$data$x[ndx], q2$data$y[ndx])))
  q2Estimate$colors <- rep(blu, length(q2Estimate$slopes))
})

observeEvent(input$q2_resample_1000, {
  q2Estimate$CI <- NULL
  resamples <-  sapply(1:1000, function(x) sample(1:nrow(q2$data), replace = TRUE))
  resamples <- resamples[, apply(resamples,2, function(ndx) var(ndx) > 1.e-15) ]
  q2Estimate$resamples <- cbind(q2Estimate$resamples, resamples)
  newCoefs <- apply(resamples, 2, function(ndx) coef(lm(y ~ x, q2$data[ndx, ])))
  q2Estimate$intercepts <- c(q2Estimate$intercepts, newCoefs[1, ])
  q2Estimate$slopes <- c(q2Estimate$slopes, newCoefs[2, ])
  #q2Estimate$slopes <- c(q2Estimate$slopes, apply(resamples, 2, function(ndx) coef(lm(y ~ x, q2$data[ndx, ]))[2]))
  q2Estimate$corr <- c(q2Estimate$corr, apply(resamples, 2, function(ndx) cor(q2$data$x[ndx], q2$data$y[ndx])))
  q2Estimate$colors <- rep(blu, length(q2Estimate$slopes))
})

observeEvent(input$q2_resample_5000, {
  q2Estimate$CI <- NULL
  resamples <-  sapply(1:5000, function(x) sample(1:nrow(q2$data), replace = TRUE))
  resamples <- resamples[, apply(resamples,2, function(ndx) var(ndx) > 1.e-15) ]
  q2Estimate$resamples <- cbind(q2Estimate$resamples, resamples)
  newCoefs <- apply(resamples, 2, function(ndx) coef(lm(y ~ x, q2$data[ndx, ])))
  q2Estimate$intercepts <- c(q2Estimate$intercepts, newCoefs[1, ])
  q2Estimate$slopes <- c(q2Estimate$slopes, newCoefs[2, ])
  #q2Estimate$slopes <- c(q2Estimate$slopes, apply(resamples, 2, function(ndx) coef(lm(y ~ x, q2$data[ndx, ]))[2]))
  q2Estimate$corr <- c(q2Estimate$corr, apply(resamples, 2, function(ndx) cor(q2$data$x[ndx], q2$data$y[ndx])))
  q2Estimate$colors <- rep(blu, length(q2Estimate$slopes))
})

output$q2_EstPlot2 <- renderPlot({
  req( q2Estimate$resamples) 

  #print(input$q2_EstParam)
  if(input$q2_EstParam == "Slope  OR") {
    parm <-  as.numeric(q2Estimate$slopes)
    q2Estimate$observed <- q2$slope
  } else { 
    parm <- as.numeric(q2Estimate$corr)
    q2Estimate$observed <- q2$corr
  }
  parm <- sort(parm)
  ## print(summary(parm))
  if(length(parm) == 1){
    yy <- .5
    radius <- 4
    nsims <- 1
  } else {
    q2Estimate$y <- yy <- newy(parm)
    nsims <- length(parm)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(parm, yy, ylab = "", cex = radius/2, pch = 16, col = q2Estimate$colors, ylim=c(.5, pmax(10,max(yy))),  
       xlab = ifelse(input$q2_EstParam == "Slope  OR", "Slope", "Correlation"), main = "RE-Sampling Distribution")
  corner <- ifelse(q2$corr >= 0, "topleft", "topright")
  legend(corner, bty = "n", paste(length(parm), "points \n Mean = ", 
                                     round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
}, height = 400, width = 400)

observeEvent(input$q2_conf80,{
  if(is.null(q2Estimate$slopes) | (nsims <- length(q2Estimate$slopes)) < 10){
    return()
  }
  q2Estimate$confLevel <- .80
  q2Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .1)
  q2Estimate$colors[1:tailCount] <- rd
  q2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q2Estimate$CI <- if(input$q2_EstParam == "Slope  OR"){
    sort(q2Estimate$slopes)[c(tailCount, nsims + 1 - tailCount)]
  } else {
    sort(q2Estimate$corr)[c(tailCount, nsims + 1 - tailCount)]
  }
})

observeEvent(input$q2_conf90,{
  if(is.null(q2Estimate$slopes) | (nsims <- length(q2Estimate$slopes)) < 20){
    return()
  }
  q2Estimate$confLevel <- .90
  q2Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .05)
  q2Estimate$colors[1:tailCount] <- rd
  q2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q2Estimate$CI <- if(input$q2_EstParam == "Slope  OR"){
    sort(q2Estimate$slopes)[c(tailCount, nsims+1 -tailCount)]
  } else {
    sort(q2Estimate$corr)[c(tailCount, nsims+1 -tailCount)]
  }
})

observeEvent(input$q2_conf95,{
  if(is.null(q2Estimate$slopes) | (nsims <- length(q2Estimate$slopes)) < 40){
    return()
  }
  q2Estimate$confLevel <- .95
  q2Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .025)
  q2Estimate$colors[1:tailCount] <- rd
  q2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q2Estimate$CI <- if(input$q2_EstParam == "Slope  OR"){
    sort(q2Estimate$slopes)[c(tailCount, nsims+1 -tailCount)]
  } else {
    sort(q2Estimate$corr)[c(tailCount, nsims+1 -tailCount)]
  }
})

observeEvent(input$q2_conf99,{
  if(is.null(q2Estimate$slopes) | (nsims <- length(q2Estimate$slopes)) < 200){
    return()
  }
  q2Estimate$confLevel <- .99
  q2Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .005)
  q2Estimate$colors[1:tailCount] <- rd
  q2Estimate$colors[nsims + 1 -(1:tailCount)] <- rd
  q2Estimate$CI <- if(input$q2_EstParam == "Slope  OR"){
    sort(q2Estimate$slopes)[c(tailCount, nsims+1 -tailCount)]
  } else {
    sort(q2Estimate$corr)[c(tailCount, nsims+1 -tailCount)]
  }
})


}

  ##  Least Squares Line Demo ####################################
  {
    ## Which data?  Arbitrary data makes it hard to predict slider input values
    q2_leastSquaresDemoUI <- renderUI({
      fluidPage(   
        fluidRow(
          column(3, h4("Which Line Fits Best?"))))
    })  
    
  }  

## 1 categorical & 1 quantitative   ---------------------------------------  1 cat 1 quant

  ###  Data entry ------------------------------------------------------------ 1c1q
{
output$c1q1DataIn <- renderText({
  "How do you want to input the data?"
})

output$c1q1_ui <- renderUI({
  req(input$c1q1_entry)

  switch( input$c1q1_entry,
          "Pre-Loaded Data" ={ 
            fluidRow(  
              column(4, selectInput('c1q1_data1', 'Available Datasets',  choices = as.list(c1q1_contents))
              ),
              column(4, actionButton("c1q1_useLddBtn", "Use These Data", class="btn btn-primary") )
            )                           
          },
          "Local CSV File" ={
            ## copied from: http://shiny.rstudio.com/gallery/file-upload.html    
            fluidRow(  
              column(4, fileInput('c1q1_file1', 'Choose CSV File',
                                  accept=c('text/csv', 
                                           'text/comma-separated-values,text/plain', 
                                           '.csv')) ,
                     br(),
                     actionButton("c1q1_useCSVBtn", "Use These Data", class="btn btn-primary")
              ),
              column(3, checkboxInput('c1q1_header', 'Row One is column names', TRUE)
              ),
              column(3, radioButtons('c1q1_sep', 'Separator',
                                     c(Comma=',', Semicolon=';', Tab='\t'),
                                     ',')
              ),
              column(2, radioButtons('c1q1_quote', 'Quote',
                                     c(None='', 'Double Quote'='"','Single Quote'="'"),
                                     '"')
              )
            )
            ## grab names from data?
            ## check which is numeric / character
            
          },
          "Type/Paste into Text Box" = {
            fluidRow(  
              column(4, HTML('<textarea name="c1q1_text" cols="30" rows="10"></textarea>'),
                     actionButton("c1q1_useText", "Use These Data", class="btn btn-primary")
              ),
              column(3, checkboxInput('c1q1_header2', 'Row One is column names', TRUE)
              ),
              column(3, radioButtons('c1q1_sep2', 'Separator',
                                     c(Comma=',', Semicolon=';', Tab='\t'),
                                     ',')
              ),
              column(2, radioButtons('c1q1_quote2', 'Quote',
                                     c(None='', 'Double Quote'='"','Single Quote'="'"),
                                     '"')
              )
            )
          },
#           "Type/Paste into Data Table" = {
#             #h4("Edit the values in Column 2.  Column 1 will be ignored.  To paste, use Cntrl-V or Cmd-V(on a mac)"), 
#             fluidRow(
#               column(4, 
#                      rHandsontableOutput("c1q1_hot")) 
#               ,
#               ## allow user to change names of the columns:
#               # column(4,
#               #       )
#               column(4, actionButton("c1q1_useHotBtn", "Use These Data", class="btn btn-primary"))
#             )
#             
#           }, 
          NULL
  )
})

##  grab data according to input method
c1q1 <- reactiveValues(data = NULL, names = c("groups","y"))

observeEvent(  input$c1q1_useLddBtn, {
  DF <- eval(parse( text = input$c1q1_data1))
  whichIsFactor <- which(sapply(DF, is.factor))
  if(length(whichIsFactor) < 1) {
    whichIsFactor <- which.min(sapply(DF, function(x) length(unique(x))))
    DF[,whichIsFactor] <- factor( DF[,whichIsFactor])
  }
  DF <- DF[, whichIsFactor:(3 - whichIsFactor)]
  c1q1$names <- names(DF)
  c1q1$data <- data.frame(DF)
  c1q1Test$shuffles <- c1q1Test$diff <- NULL
  c1q1Est$shuffles <- c1q1Est$diff <- NULL
  output$c1q1DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true difference in means"
  })
})

observeEvent(  input$c1q1_useCSVBtn,{
  DF <- read.csv(input$c1q1_file1$datapath, header=input$c1q1_header,
                 sep=input$c1q1_sep, quote=input$c1q1_quote)
  whichIsNumeric <- which(sapply(DF, is.numeric))
  whichIsFactor <- which(sapply(DF, is.factor))
  if(length(whichIsFactor) < 1) {
    whichIsFactor <- which.min(sapply(D,  function(x) length(unique(x))))
    DF[,whichIsFactor] <- factor( DF[,whichIsFactor])
  }
  DF <- DF[, whichIsFactor:(3 - whichIsFactor)]
  if("V1" %in% names(DF)[1])    names(DF) <- c("group", "y")
  c1q1$names <- names(DF)
  c1q1$data <- data.frame(DF)
  c1q1Test$shuffles <- c1q1Test$diff <- NULL
  c1q1Est$shuffles <- c1q1Est$diff <- NULL
  output$c1q1DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true difference in means"
  })
})

observeEvent(input$c1q1_useText,{
  DF <- read.csv(text = input$c1q1_text, header=input$c1q1_header2,
                 sep=input$c1q1_sep2, quote=input$c1q1_quote2)
  whichIsNumeric <- which(sapply(DF, is.numeric))
  whichIsFactor <- which(sapply(DF, is.factor))
  if(length(whichIsFactor) < 1) {
    whichIsFactor <- which.min(sapply(D,  function(x) length(unique(x))))
    DF[,whichIsFactor] <- factor( DF[,whichIsFactor])
  }
  DF <- DF[, whichIsFactor:(3 - whichIsFactor)]
  if("V1" %in% names(DF)[1])    names(DF) <- c("group", "y")
  c1q1$names <- names(DF)
  c1q1$data <- data.frame(DF)
  c1q1Test$shuffles <- c1q1Test$diff <- NULL
  c1q1Est$shuffles <- c1q1Est$diff <- NULL
  output$c1q1DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true difference in means"
  })
#   if(nchar(input$c1q1_text) < 1){
#     return()
#   }
#   #print(input$c1q1_text)
#   if (grepl(",", input$c1q1_text)){          ## check for commas,
#     c1q1Text <- gsub(","," ",input$c1q1_text)  ## replace commas with spaces
#   } else {
#     c1q1Text <- input$c1q1_text
#   }
#   if(is.na(as.numeric(unlist(strsplit(c1q1Text," "))[2]))){ # is the 2nd "word" numeric or character?
#     tempData <- read.table(text = c1q1Text, head = TRUE)      
#     c1q1$names <- names(tempData)
#   } else{
#     c1q1$names <- c("group","y")   # numeric, so name it "x"
#     tempData <- read.table(text = q2Text, head = FALSE)
#     names(tempData) <- q2$names
#     whichIsNumeric <- which(sapply(DF, is.numeric))
#     whichIsFactor <- which(sapply(DF, is.factor))
#   } 
#   c1q1Test$nsims <- 0
#   c1q1$data <- data.frame( x = tempData)
#   c1q1Test$shuffles <-  c1q1Test$new.xbars <-  c1q1Test$xbar <-   c1q1Test$colors <- NULL
#   c1q1Est$shuffles <- c1q1Est$xbars <- c1q1Est$observed <- c1q1Est$colors <- NULL
#   #print(c1q1$data)
#   output$c1q1DataIn <- renderText({
#     "Data are entered, you may now choose to estimate or test one mean"
#   })
})

# observeEvent(  input$c1q1_useHotBtn,{
#   DF <- data.frame(c1q1_values[["hot"]])
#   # print(DF)
#   names(DF) <- c("group","y")
#   c1q1$names <- names(DF)
#   c1q1$data <- DF
#   c1q1Test$shuffles <- c1q1Test$diff <- NULL
#   c1q1Est$shuffles <- c1q1Est$diff <- NULL
#   output$c1q1DataIn <- renderText({
#     "Data are entered, you may now choose to estimate or test the true difference in means"
#   })
# })

c1q1_values = list()
#c1q1_setHot = function(x) c1q1_values[["hot"]] <<- x

# output$c1q1_hot = renderRHandsontable({
#   if (!is.null(input$c1q1_hot)) {
#     c1q1_DF = hot_to_r(input$c1q1_hot)
#     c1q1_setHot(c1q1_DF)
#     rhandsontable(c1q1_DF) %>%
#       hot_table(highlightCol = TRUE, highlightRow = TRUE)
#   } else {
#     ## seems that HOT needs at least 2 columns, so column 1 is just row numbers.
#     c1q1_DF = read.csv("data/dummyDatac1q1.csv", stringsAsFactors = FALSE, head = TRUE)
#     c1q1_setHot(c1q1_DF)    
#     rhandsontable(c1q1_DF, height = 230) %>%
#       hot_table(highlightCol = TRUE, highlightRow = TRUE)
#   }
# })
}
  ##  Describe and summarize ------------------------------------------------- 1c1q
{
c1q1Test <- reactiveValues(shuffles = NULL,  observed = NULL, diff = NULL,  colors = NULL, 
                           moreExtremeCount = NULL, pvalue = NULL, direction = NULL, cutoff = NULL,
                           sampleCount = NULL, selected = NULL)

c1q1Est <- reactiveValues(shuffles = NULL,  observed = NULL, diff = NULL, confLevel = NULL, colors = NULL, 
                          ndx1 = NULL, ndx2 = NULL, CI = NULL, selected = NULL )

output$c1q1_Plot <- renderPlot( {
  req(c1q1$data)

  #isolate( { 
  ## make plot
  DF <- c1q1$data
  names(DF) <- c("group","y")
  DF[, 1] <- factor(DF[,1])
  #print(summary(DF))
  c1q1_plot1 <- qplot(y=y, x=group, data = DF, geom="boxplot") +
    theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2]) 
  DF <- DF[order(DF$y), ]
  #nbreaks <- min(c(length(unique(DF$y)), floor(.5*nclass.Sturges(DF$y)^2)))
  #z <- cut(DF$y, breaks =  nbreaks )
  w <- unlist(tapply(DF$y, DF$group, newy))
  ##w <- newy(DF$y)  #w[!is.na(w)]  
  myBlue <- rgb(0, 100/256, 224/256, alpha = .8)  
  c1q1_plot2 <- qplot(data= DF, x=y, y=w , colour = I(myBlue), size = I(4), ylim = 
                        c(.5, pmax(10, max(w)))) + facet_wrap( ~group) + 
                      theme_bw() + ylab("Count") + xlab( c1q1$names[2])
  grid.arrange(c1q1_plot1, c1q1_plot2, heights = c(2,  5)/7, ncol=1)
  #})
}, height=400)


output$c1q1_Summary1 <- renderTable({
  req(c1q1$data)  
  #isolate({
  c1q1Est$ndx1 <- which(unclass(c1q1$data[,1]) == 1 | c1q1$data[,1] == "A")
  c1q1Est$ndx2 <- which(unclass(c1q1$data[,1]) == 2 | c1q1$data[,1] == "B")
  
  tempDF <- c1q1$data
  names(tempDF) <- c("group","y")
  tempDF$group <- factor(tempDF$group)
  #print(tempDF)
  DF <- with(tempDF,
        rbind(mean   = tapply(y, group, mean, na.rm = TRUE ),
              sd     = tapply(y, group, sd, na.rm = TRUE),
              min    = tapply(y, group, min),
              Q1     = tapply(y, group, quantile, .25),
              median = tapply(y, group, median),
              Q3     = tapply(y, group, quantile, .75),
              max    = tapply(y, group, max),
              n = tapply(y, group, length)
        ))
  colnames(DF) <- levels(tempDF$group)
  DF
  #})
})

output$c1q1_Summary2 <- renderTable({
  req(c1q1$data)  
    val <- round( diff(tapply(c1q1$data[, 2], c1q1$data[, 1], mean, na.rm=TRUE)), 3)
    names(val) <- NULL
    c1q1$diff <- -val
  matrix( -val, ncol= 1, dimnames = list("Difference in Means", c1q1$names[2]))
})
}

 ##  test equality of two means   -------------------------------------------- 1c1q
{
  
  output$c1q1_testUI <- renderUI({
        if(is.null(c1q1$data)){
          h4(" You must first enter data. Choose 'Enter/Describe Data'.")
        } else{ 
          fluidPage(
            h3("Test: 'Are Two Means Equal?'"),
            fluidRow(
              column(4, 
                      plotOutput("c1q1_TestPlot1")
                      ),
               column(3,
                      uiOutput("c1q1_TestTables")
                     ),
               column(5, 
                      h4(HTML("Null hypothesis: &mu;<sub>1</sub> = &mu;<sub>2</sub>")),
                      uiOutput('c1q1_SampDistPlot')
                     )
              ),
            fluidRow(
              column(4, offset = 8, HTML("Click a point to see its shuffle."))
            ),
            br(),
            fluidRow(
              column(4, offset = 3, h4("How many more shuffles?")),
              column(1, actionButton("c1q1_test_shuffle_10", label = "10", class="btn btn-primary")),
              column(1, actionButton("c1q1_test_shuffle_100", label = "100", class="btn btn-primary")),
              column(1, actionButton("c1q1_test_shuffle_1000", label = "1000", class="btn btn-primary")),
              column(1, actionButton("c1q1_test_shuffle_5000", label = "5000", class="btn btn-primary"))
            ),
            br(),
            fluidRow(
              column(11, offset = 1,
                     uiOutput("c1q1TestXtremes"))
            ),
            fluidRow(
              column(8, offset = 4,
                     uiOutput("c1q1PrintPvalue")
            )
          )
        )
      }
    })

   output$c1q1_TestTables <- renderUI({
     div(
      tableOutput("c1q1_TestTable1"),
      h5(paste("Original Difference in means = ", 
              round(-diff(tapply(c1q1$data[, 2], c1q1$data[, 1], mean, na.rm=TRUE)), 3))),
      br(),
      tableOutput("c1q1_TestTable2"),
      h5(paste("Shuffled difference in means = ", round(as.numeric(c1q1Test$selected),3)))
    )  
   })
   
    output$c1q1_SampDistPlot <- renderUI({ 
      plotOutput('c1q1_TestPlot2', click = 'c1q1_Test_click')
   })
      
   output$c1q1PrintPvalue <- renderUI({
      req(c1q1Test$moreExtremeCount)
        h4(pvalue2print(c1q1Test$moreExtremeCount,  c1q1Test$sampleCount, c1q1Test$direction, 
                        c1q1Test$cutoff, c1q1Test$pvalue))
   })
      
  output$c1q1TestXtremes <- renderUI({
        fluidRow(
          column(3,
                 h4("Count values equal to or ")
          ),
          column(4,
                 tags$div(style="width: 200px",
                          tags$select(id='c1q1_testDirection', class="form-control",
                                      tags$option( value = "less", "less"),
                                      tags$option( value = "more extreme", "more extreme", selected = TRUE),
                                      tags$option( value = "greater", "greater"))
                 )
          ),
          column(1, h4("than ")),
          column(2,
                 tags$div( 
                   tags$input(id = "c1q1_test_cutoff", type = "text", class = "form-control", value = NA))
          ),
          column(1, 
            actionButton("c1q1_countXtremes","Go", class="btn btn-success")
          )
        )
      })
      
      
      # -------- 1 cat  1 quant test plots ------------------
      
      output$c1q1_TestPlot1  <- renderPlot({
        req(c1q1$data)
        ## Original Data
        DF <- c1q1$data
        names(DF) <- c("group","y")
        DF$group <- factor(DF$group)
        #print(DF)
        par(mfrow = c(2,1), mar = c(4.1,5.1,3.1,.1))
        boxplot( y ~ group, data = DF, horizontal = TRUE,  main = "Original Data", las =1)
      #  plot1 <- qplot(y=y, x=group, data = DF, geom="boxplot", main = "Original Data") +
      #    theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2])

        ## Plot One Shuffle 
        if(!is.null(input$c1q1_Test_click)){
          ##  We already have shuffled data and want to pick the clicked point
          closestPnt <- which.min(abs( c1q1Test$diff - input$c1q1_Test_click$x))
          #cat("Close to number: ", closestPoint, "\n")
          DF$group2 <- c1q1Test$shuffles[, closestPnt]
          c1q1Test$colors <- rep( blu, length(c1q1Test$diff))
        } else if(is.null(c1q1Test$diff)){
          shuffle <- sample(c1q1$data[,1])
          c1q1Test$shuffles <- as.matrix(shuffle, ncol = 1)
          c1q1Test$diff <- -diff(tapply(DF$y, shuffle, mean))
          c1q1Test$colors <- blu
          ### stores samples as columns
          DF$group2 <- shuffle
        } else {
          ## use latests shuffle
          DF$group2 <- c1q1Test$shuffles[, ncol(c1q1Test$shuffles)]
        }
        #print(DF)
      #  plot2 <- qplot(y=y, x=group2, data = DF, geom="boxplot", main = "Shuffled Sample") +
      #    theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2])
       boxplot( y ~ group2, data = DF, horizontal = TRUE,  main = "Shuffled Sample", las =1)
        
        
#         w2 <- unlist(tapply(DF$y, list(z, DF$group2), function(x) 1:length(x)))
#         tempDF <- data.frame(DF, w=w2[!is.na(w2)])
#         plot2 <- qplot(data = tempDF, x = y, y = w, colour = I(blu), size = I(4), main = "Shuffled Data") + 
#                     theme_bw() + xlab(c1q1$names[2]) + ylab("Count") + facet_wrap( ~ group2)
       #plot1
        #         grid.arrange(plot1, plot2, heights = c(3, 3)/6, ncol=1)
      }, height = 360, width = 320)
      
      output$c1q1_TestTable1 <- renderTable({
        req(c1q1$data)
        #print(c1q1$data)
        DF <- data.frame(mean = tapply(c1q1$data[,2], c1q1$data[,1], mean, na.rm = TRUE ),
                    sd = tapply(c1q1$data[,2], c1q1$data[,1], sd, na.rm = TRUE ),
                    n = as.integer(tapply(c1q1$data[,2], c1q1$data[,1], length)))
        rownames(DF) <- levels(c1q1$data[,1])
        DF[nrow(DF):1, ]
      })

      output$c1q1_TestTable2  <- renderTable({
        req(c1q1$data)
        if(!is.null(input$c1q1_Test_click)){
          closestPt <- which.min(abs( c1q1Test$diff - input$c1q1_Test_click$x))
          #cat("Close to number: ", closestPoint, "\n")
        } else {
          closestPt <- ncol(c1q1Test$shuffles)
        }
        DF <- data.frame(mean = tapply(c1q1$data[, 2], c1q1Test$shuffles[, closestPt], mean, na.rm = TRUE ),
              sd = tapply(c1q1$data[, 2], c1q1Test$shuffles[, closestPt], sd, na.rm = TRUE ),
              n = tapply(c1q1$data[, 2], c1q1Test$shuffles[, closestPt], length))
        c1q1Test$selected <- -diff(DF$mean)
        rownames(DF) <- levels(c1q1$data[,1])
        DF[nrow(DF):1, ]
      })
      
      observeEvent(input$c1q1_test_shuffle_10, {
        c1q1Test$moreExtremeCount <- NULL
        c1q1Test$selected <- NA
        newShuffles <- sapply(1:10, function(x) sample(c1q1$data[,1]) )
        c1q1Test$shuffles <- cbind(c1q1Test$shuffles, newShuffles)
        c1q1Test$diff <- c(c1q1Test$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[,2], x, mean, na.rm=TRUE))))
        #print(c1q1Test$diff)
        c1q1Test$colors <- rep(blu, length(c1q1Test$diff))
      })
      
      observeEvent(input$c1q1_test_shuffle_100, {
        c1q1Test$moreExtremeCount <- NULL
        c1q1Test$selected <- NA
        newShuffles <- sapply(1:100, function(x) sample(c1q1$data[,1]) )
        c1q1Test$shuffles <- cbind(c1q1Test$shuffles, newShuffles)
        c1q1Test$diff <- c(c1q1Test$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[, 2], x, mean, na.rm=TRUE))))
        #print(c1q1Test$diff)
        c1q1Test$colors <- rep(blu, length(c1q1Test$diff))
      })
      observeEvent(input$c1q1_test_shuffle_1000, {        
        c1q1Test$selected <- NA
        c1q1Test$moreExtremeCount <- NULL
        newShuffles <- sapply(1:1000, function(x) sample(c1q1$data[,1]) )
        c1q1Test$shuffles <- cbind(c1q1Test$shuffles, newShuffles)
        c1q1Test$diff <- c(c1q1Test$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[, 2], x, mean, na.rm=TRUE))))
        #print(c1q1Test$diff)
        c1q1Test$colors <- rep(blu, length(c1q1Test$diff))
      })
      observeEvent(input$c1q1_test_shuffle_5000, {
        c1q1Test$selected <- NA
        c1q1Test$moreExtremeCount <- NULL
        newShuffles <- sapply(1:5000, function(x) sample(c1q1$data[,1]) )
        c1q1Test$shuffles <- cbind(c1q1Test$shuffles, newShuffles)
        c1q1Test$diff <- c(c1q1Test$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[, 2], x, mean, na.rm=TRUE))))
        #print(c1q1Test$diff)
        c1q1Test$colors <- rep(blu, length(c1q1Test$diff))
      })
      
      observeEvent(input$c1q1_countXtremes, {
        parm <- sort(as.numeric(c1q1Test$diff))
        nsims <- length(parm)
        c1q1Test$colors <- rep(blu, nsims)
        c1q1Test$cutoff <- threshold <- as.numeric(input$c1q1_test_cutoff)
        c1q1Test$direction <- input$c1q1_testDirection
        if(nsims > 9 & !is.na(input$c1q1_testDirection)){
          redValues <-  switch( input$c1q1_testDirection,
                                "less" = which(parm <= threshold + 1.0e-10),
                                "greater" = which(parm >= threshold - 1.0e-10),
                                "more extreme" = which(abs(parm) > abs(threshold) - 1.0e-10 ))
          c1q1Test$colors[redValues] <- rd
          #print(c1q1Test$diff[redValues])
          c1q1Test$moreExtremeCount  <- length(redValues)
          c1q1Test$pvalue <- c1q1Test$moreExtremeCount/nsims
          c1q1Test$sampleCount <- length(c1q1Test$diff)
        }
      })
      observeEvent( input$c1q1_testDirection, {
        c1q1Test$pvalue <- c1q1Test$moreExtremeCount <- NULL  
        c1q1Test$colors <- rep(blu, length(c1q1Test$colors))
      })
      
      observeEvent( input$c1q1_test_cutoff, {
        c1q1Test$pvalue <- c1q1Test$moreExtremeCount <- NULL  
        c1q1Test$colors <- rep(blu, length(c1q1Test$colors))  
      })
      
      output$c1q1_TestPlot2 <- renderPlot({
        req(c1q1Test$diff)
        
        parm <- sort(as.matrix(c1q1Test$diff))
        # print(parm)
        if(length(parm) == 1){
          y <- 0.5
          radius <- 4
        } else {
#           nbreaks <- nclass.Sturges(parm)^2
#           z <- cut(parm, breaks = nbreaks)
#           y <- unlist(tapply(z, z, function(V) 1:length(V)))
          y <- newy(parm)  #[!is.na(y)]
          #print(y)
          #print(max(w))
          nsims <- length(parm)
          radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
        }
        plot(parm, y, ylim = c(0.5, pmax(10, max(y))), ylab = "", cex = radius/2, pch = 16, col = c1q1Test$colors,  
             xlab = expression(bar(x)[1] - bar(x)[2]), main = "Sampling Distribution")
        legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                            round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
      }, width = 400)      

}  
  

##  estimate difference between two means  ----------------------------------- 1c1q
{
  output$c1q1_estimateUI <- renderUI({
    if( is.null(c1q1$data)){
      h4(" You must first enter data. Choose 'Enter/Describe Data'.")
    } else {
      fluidPage(
        h3("Estimate the difference between two means."),
        fluidRow(
          column(4, 
                 uiOutput("c1q1_dataPlot1")
          ),
          column(3,
                 uiOutput('c1q1_EstTables')
          ),
          column(5,
                uiOutput('c1q1_ReSampDistPlot')
           )
          ),
        fluidRow(
          column(4, offset = 8, HTML("&nbsp;&nbsp; Click a point to see its resample."))
        ),
        br(),
        uiOutput('c1q1Shuffles'),
        br(),
        uiOutput('c1q1_CI')          
      )
  }
})

output$c1q1_EstTables <- renderUI({
  div(
    tableOutput("c1q1_EstTable1"),
    h5(paste("Original Difference in means = ", 
             round(-diff(tapply(c1q1$data[, 2], c1q1$data[, 1], mean, na.rm=TRUE)), 3))),
    br(),
    tableOutput("c1q1_EstTable2"),
    h5(paste("Resampled difference in means = ", round(as.numeric(c1q1Est$selected),3)))
  )  
})


output$c1q1_dataPlot1 <- renderUI({ 
  plotOutput('c1q1_EstPlot1')
})

  
output$c1q1_CI <- renderUI({ 
 div(
   fluidRow(
    column(3, offset = 3, 
         h5("Select Confidence Level (%)")
    ),
    column(6,
         fluidRow(
           column(3,  actionButton('c1q1_conf80', label = "80", class="btn btn-primary")),
           column(3,  actionButton('c1q1_conf90', label = "90", class="btn btn-primary")),
           column(3,  actionButton('c1q1_conf95', label = "95", class="btn btn-primary")),
           column(3,  actionButton('c1q1_conf99', label = "99", class="btn btn-primary"))
         )
    )
  ),
  if(!is.null(c1q1Est$CI)){
   fluidRow( 
     column(7, offset = 6,
           h4(paste(c1q1Est$confLevel*100, "% Interval Estimate: (", 
                    round(c1q1Est$CI[1],3), ",", 
                    round(c1q1Est$CI[2], 3), ")"))
    ))
  }
 )
})

output$c1q1Shuffles <- renderUI({ 
fluidRow(
  column(4, offset = 3, 
         h4("How many more shuffles?")),
  column(1, actionButton("c1q1_Est_shuffle_10", label = "10", class="btn btn-primary")),
  column(1, actionButton("c1q1_Est_shuffle_100", label = "100", class="btn btn-primary")),
  column(1, actionButton("c1q1_Est_shuffle_1000", label = "1000", class="btn btn-primary")),
  column(1, actionButton("c1q1_Est_shuffle_5000", label = "5000", class="btn btn-primary"))
)
})

output$c1q1_ReSampDistPlot <- renderUI({ 
  plotOutput('c1q1_EstPlot2', click = 'c1q1_Est_click')
})

  ## why does this redraw when I click a CI?

output$c1q1_EstPlot2 <- renderPlot({
  req(c1q1Est$diff)
  parm <- sort(c1q1Est$diff)
  #print(parm)
  #parm <- sort(parm)
  if(length(parm) == 1){
    y <- .5
    radius <- 4
  } else {
#     nbreaks <- nclass.Sturges(parm)^2
#     z <- cut(parm, breaks = nbreaks)
#     y <- unlist(tapply(z, z, function(V) 1:length(V)))
    y <- newy(parm) #[!is.na(y)]
    #print(y)
    #print(max(w))
    nsims <- length(parm)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(x = parm, y = y, ylim = c(0.5, pmax(10, max(y))), ylab = "", cex = radius/2, pch = 16, col = c1q1Est$colors,  
       xlab = expression(bar(x)[1] - bar(x)[2]), main = "Resampling Distribution")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                      round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
}, width = 400)      

output$c1q1_EstPlot1 <- renderPlot({
  req(c1q1$diff)

  DF <- c1q1$data
  names(DF) <- c("group","y")
  DF[, 1] <- factor(DF[,1])
  #print(summary(DF))
  
  plot1 <- qplot(y=y, x=group, data = DF, geom="boxplot", main = "Original Data") +
    theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2])
  
  ## Plot One resample 
  if(!is.null(input$c1q1_Est_click)){
    closestPt <- which.min(abs( c1q1Est$diff - input$c1q1_Est_click$x))
    #cat("Close to number: ", closestPoint, "\n")
  } else if (!is.null(c1q1Est$shuffles)){
    closestPt <- ncol(c1q1Est$shuffles)
  } else {
    closestPt <- 1
    n1 <- length(c1q1Est$ndx1)
    n2 <- length(c1q1Est$ndx2)
    c1q1Est$shuffles <- c1q1_estimate_shuffles(1, c1q1Est$ndx1, c1q1Est$ndx2)
    c1q1Est$diff <- -with(DF[c1q1Est$shuffles[, 1] , ], diff(tapply(y, group, mean)))
    c1q1Est$colors <- blu
  }
    #print(c1q1Est$shuffles)
  DF2 <- c1q1$data[c1q1Est$shuffles[, closestPt], ] 
  names(DF2) <- names(DF)
  DF2 <- DF2[order(DF2$y), ]
  ### stores samples as columns

  plot3 <- qplot(y=y, x=group, data = DF2, geom="boxplot", main = "Resampled Data") +
                theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2])

  grid.arrange(plot1, plot3, heights = c( 3,3)/6, ncol=1)
}, height = 360)

output$c1q1_EstTable1 <- renderTable({
  req(c1q1$data)
  DF <- data.frame(mean = tapply(c1q1$data[,2], c1q1$data[, 1], mean, na.rm = TRUE ),
                   sd = tapply(c1q1$data[,2], c1q1$data[, 1], sd, na.rm = TRUE ),
                   n = as.integer(tapply(c1q1$data[,2], c1q1$data[, 1], length)))
  rownames(DF) <- levels(c1q1$data[,1])
  DF[nrow(DF):1, ]
})


output$c1q1_EstTable2 <- renderTable({
  req(c1q1$data)  
  if(!is.null(input$c1q1_Est_click)){
    closestPt <- which.min(abs( c1q1Est$diff - input$c1q1_Est_click$x))
    #cat("Close to number: ", closestPoint, "\n")
  } else {
    closestPt <- ncol(c1q1Est$shuffles)
  }
  resamp1 <- c1q1$data[c1q1Est$shuffles[, closestPt], ]
  names(resamp1) <- c("group","y")
  #print(resamp1)
  DF <- with(resamp1, data.frame(mean = tapply(y, group, mean, na.rm = TRUE ),
                   sd = tapply(y, group, sd, na.rm = TRUE ),
                   n = tapply(y, group, length)))
  rownames(DF) <- levels(c1q1$data[,1])
  c1q1Est$selected <- -diff(DF$mean)
  #print(DF)
  DF[nrow(DF):1, ]
})

observeEvent(input$c1q1_Est_shuffle_10, {
  c1q1Est$CI <- NULL
  c1q1Est$selected <- NA
  newShuffles <- c1q1_estimate_shuffles(10, c1q1Est$ndx1, c1q1Est$ndx2)
  c1q1Est$shuffles <- cbind(c1q1Est$shuffles, newShuffles)
  c1q1Est$diff <- c(c1q1Est$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[x,2], c1q1$data[x,1], mean, na.rm=TRUE))))
  #print(c1q1Est$diff)
  c1q1Est$colors <- rep(blu, length(c1q1Est$diff))
})

observeEvent(input$c1q1_Est_shuffle_100, {
  c1q1Est$CI <- NULL
  c1q1Est$selected <- NA
  newShuffles <- c1q1_estimate_shuffles(100, c1q1Est$ndx1, c1q1Est$ndx2)
  c1q1Est$shuffles <- cbind(c1q1Est$shuffles, newShuffles)
  c1q1Est$diff <- c(c1q1Est$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[x, 2],  c1q1$data[x,1], mean, na.rm=TRUE))))
  #print(c1q1Est$diff)
  c1q1Est$colors <- rep(blu, length(c1q1Est$diff))
})
observeEvent(input$c1q1_Est_shuffle_1000, {        
  c1q1Est$CI <- NULL
  c1q1Est$selected <- NA
  newShuffles <- c1q1_estimate_shuffles(1000, c1q1Est$ndx1, c1q1Est$ndx2)
  c1q1Est$shuffles <- cbind(c1q1Est$shuffles, newShuffles)
  c1q1Est$diff <- c(c1q1Est$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[x, 2],  c1q1$data[x,1], mean, na.rm=TRUE))))
  #print(c1q1Est$diff)
  c1q1Est$colors <- rep(blu, length(c1q1Est$diff))
})
observeEvent(input$c1q1_Est_shuffle_5000, {
  c1q1Est$CI <- NULL
  c1q1Est$selected <- NA
  newShuffles <- c1q1_estimate_shuffles(5000, c1q1Est$ndx1, c1q1Est$ndx2)
  c1q1Est$shuffles <- cbind(c1q1Est$shuffles, newShuffles)
  c1q1Est$diff <- c(c1q1Est$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[x, 2],  c1q1$data[x,1], mean, na.rm=TRUE))))
  #print(c1q1Est$diff)
  c1q1Est$colors <- rep(blu, length(c1q1Est$diff))
})

observeEvent(input$c1q1_conf80,{
  req(c1q1$diff)

  nsims <- length(c1q1Est$diff)
  c1q1Est$confLevel <- .80
  c1q1Est$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .1)
  c1q1Est$colors[1:tailCount] <- rd
  c1q1Est$colors[nsims +1 -(1:tailCount)] <- rd
  c1q1Est$CI <- sort(c1q1Est$diff)[c(tailCount, nsims + 1 - tailCount)]
})


observeEvent(input$c1q1_conf90,{
  req(c1q1$diff)
  nsims <- length(c1q1Est$diff)
  c1q1Est$confLevel <- .90
  c1q1Est$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .05)
  c1q1Est$colors[1:tailCount] <- rd
  c1q1Est$colors[nsims +1 -(1:tailCount)] <- rd
  c1q1Est$CI <- sort(c1q1Est$diff)[c(tailCount, nsims + 1 - tailCount)]
})

observeEvent(input$c1q1_conf95,{
  req(c1q1$diff)

  nsims <- length(c1q1Est$diff)
  c1q1Est$confLevel <- .95
  c1q1Est$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .025)
  c1q1Est$colors[1:tailCount] <- rd
  c1q1Est$colors[nsims +1 -(1:tailCount)] <- rd
  c1q1Est$CI <- sort(c1q1Est$diff)[c(tailCount, nsims + 1 - tailCount)]
})

observeEvent(input$c1q1_conf99,{
  req(c1q1$diff)
  nsims <- length(c1q1Est$diff)
  c1q1Est$confLevel <- .99
  c1q1Est$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .005)
  c1q1Est$colors[1:tailCount] <- rd
  c1q1Est$colors[nsims +1 -(1:tailCount)] <- rd
  c1q1Est$CI <- sort(c1q1Est$diff)[c(tailCount, nsims + 1 - tailCount)]
})


}

  ##  t distributions -------------------------------------------------------- 1c1q

{
c1q1_tProb <- reactiveValues(prob = NULL, z = NULL, findP = NULL)

observeEvent( input$c1q1_z_txt, {
  c1q1_tProb$z <- as.numeric(input$c1q1_z_txt) 
  c1q1_tProb$findP <- TRUE
})

observeEvent( input$c1q1_prob_txt,{
  c1q1_tProb$prob <- as.numeric(input$c1q1_prob_txt) 
  c1q1_tProb$findP  <- FALSE
})


output$tProbPlot2 <-    renderPlot({ 
  #     print(c1q1_tProb$prob)
  #     print(c1q1_tProb$z)
  #     print(c1q1_tProb$findP)
  #     print(input$c1q1_df)
  #     print(input$c1q1_area)
  req(c1q1_tProb$findP)

  if(is.null(c1q1_tProb$prob) & is.null(c1q1_tProb$z))
    return()
  
  df <- as.numeric(input$c1q1_df)
  
  par(mar=c(24,1,1,1)/10)
  z <- absz <- prob <- yrr <- xrr <- NA
  x <- -300:300 / 50
  
  if(!c1q1_tProb$findP & !is.na(c1q1_tProb$prob)){
    ## given prob, find z
    prob <- c1q1_tProb$prob
    #cat("finding z for p = ", prob, "\n")
    if(input$c1q1_area == "Lower"){ ##  left tail
      z <-  qt(prob, df)
      if(z < min(x))  x <- c(1.01 * z, x)
      ## rejection region in x dimension
      xrr <- c(x[x < z], z, z)
      ## density curve over xrr:
      yrr <- c( dt(xrr[-length(xrr)], df), 0)
    } else if(input$c1q1_area == "Upper"){   ## right tail        
      z <- qt(1 - prob, df) 
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(z, z, x[x > z])
      yrr <- c(0, dt(xrr[-1], df))
    } else if(input$c1q1_area == "Center"){
      z <- abs(qt( (1 - prob)/2, df ))
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- seq(-z, z, length=100)
      yrr <- c(0, dt(xrr[2:99], df), 0)
    } else if(input$c1q1_area == "Extremes"){
      z <- abs(qt(1-prob/2, df) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(-z, x[x < -z], -z)
      yrr <- c(0,  dt(xrr[-1], df))
    }
    absz <- abs(z)
  }
  if(c1q1_tProb$findP & !is.na(c1q1_tProb$z)){
    z <- c1q1_tProb$z
    ##  find probability
    absz <- abs(z)
    maxX <- pmax(z, 6)
    minX <- pmin(z, -6)
    # cat("finding p ", minX, maxX, "for z = ", z, "\n")
    x <- seq(minX, maxX, length=200)
    prob <-  1 - pt(z, df) 
    xrr <- c(z, z, x[x > z])  ## right tail
    yrr <- c(0,  dt(xrr[-1],  df) )
    if(input$c1q1_area == "Lower"){         ##  left tail
      prob =  pt(z, df) 
      xrr <- c(z, x[x < z], z)
      yrr <- c(0,  dt(xrr[-1], df))
    } else if (input$c1q1_area == "Extremes"){ ##  extremes
      xrr <- c( -absz, x[x < -absz], -absz)
      yrr <- c(0, dt(xrr[-1], df))
      prob = pt(-absz, df) 
      #yrr <- c(yrr, NA,NA, rev(yrr))
      #xrr <- c(xrr, NA,NA, rev(-xrr))
    } else if (input$c1q1_area == "Center"){   ##  center
      prob <- c1q1_tProb$prob
      xrr <- seq(-absz, absz, length=100)
      yrr <- c(0, dt(xrr, df), 0)
      xrr <- c(-absz, xrr, absz)
      prob <- diff( pt(c(-absz,absz), df) )
    }
  }
  
  plot(x, dt(x, df), type = "l", bty='n', xlab="Z Score", ylab="", yaxt="n")
  abline(h=0)
  max.height <- dt(0, df) *.9
  text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
  segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
  
  if(input$c1q1_area == "Extremes") {  ## extremes
    polygon(xrr, yrr, col = rd)
    polygon(-xrr, yrr, col = rd)
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
         round(prob * ifelse(c1q1_tProb$findP,1,.5), 3), col = "darkblue")
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y = text.height, round(c(-absz,absz),3))
  } else if (input$c1q1_area == "Center") {   ## fill & label center
    polygon(xrr, yrr, col = grn)
    text(x=0, y= text.height, round(prob,3))
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y=text.height, round(c(-absz,absz),3))
  } else {          ## show tails
    polygon(xrr, yrr, col = rd)
    text( z, y = text.height, round(z, 3))
    text(x= sign(z) * (absz+4) / 2 , y= max(yrr) / 2 + 0.02, 
         round(prob,3), col = "darkblue")
  }
}, height=300)

}


})

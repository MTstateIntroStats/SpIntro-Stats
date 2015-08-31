

includeScript("www/helper.js")
source("helpers.R")

quant1_contents <- load("data/quant1.RData")
quant2_contents <- load("data/quant2.RData")
c1q1_contents <- load("data/cat1quant1.RData")
load("data/quant1.RData")
load("data/quant2.RData")
load("data/cat1quant1.RData")

 ##  These were created to hold sample data with:
 ##  save(birthWeights, REDvsCntrl, REDvsREDA, REDAvsCntrl, file = "data/cat1quant1.RData")
 ##  save(bodytemp, birthweights, arsenic, geyser2011, file="data/quant1.RData")
 ##  save(shuttle, womenRateMen, menRateWomen, Dental, file = "data/quant2.RData")

## setup transparent colors
grn <- rgb(0, 1, 0, alpha=.4)
rd  <- rgb(1, 0, 0, alpha=.5)
blu <- rgb(0, 0, 1, alpha=.4)

shinyServer(function(input, output, session) {

  ## 1 Categorical  -----------------------------------------------------------
 
    ##  Enter and Describe ---------------------------------- cat 1

{
    ##  Using Submit Button to keep plots from changing too soon
  cat1_data <- reactiveValues(counts = NULL, names = NULL, total = NULL)
  
  cat1Test <- reactiveValues(phat = NULL, colors = NULL, cutoff = NULL, moreExtremeCount = NULL, 
                             sampleCount = NULL, pvalue = NULL)
  
  cat1Estimate <- reactiveValues(phat = NULL, observed = NULL, colors = blu, confLevel = NULL, CI = NULL)
  
  observeEvent(input$cat1_submitButton, {
      cat1Test$phat <- cat1Test$colors <-   cat1Test$sampleCount <- NULL
      cat1Estimate$phat <- cat1Estimate$observed <- cat1Estimate$colors <- NULL
      cat1_data$counts <- as.numeric(c(input$cat1_n1, input$cat1_n2))
      cat1_data$names <- c(input$cat1_name1, input$cat1_name2)
      cat1_data$total <- sum(as.numeric(c(input$cat1_n1, input$cat1_n2)))
  }
 )
  
  ## Descriptives:  plot a bar chart of the successes / failures
  
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

  output$cat1DataIn <- renderText({
  if(input$cat1_submitButton ==0) return()
    "Data are entered, you may now choose to estimate or test one proportion."
  })


  output$cat1_Summary <- renderTable({
    if(input$cat1_submitButton ==0) return()
    #isolate({
     # cat1_dataDF <- cat1_data
      counts <- as.table( matrix(cat1_data$counts), 1, 2)
      dimnames(counts) = list(cat1_data$names,"Proportions")
      prop.table(counts)
    #})
  })
} 
    ##  Test for single proportion ------------------------------------------------ cat 1
{ 
 output$cat1_testUI <- renderUI({
   if( is.null(cat1_data$counts)){
     h4(" You must first enter data. Choose 'Enter/Describe Data'.")
   } else {
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
                        column(9, offset =1, h4("True Proportion (Null hypothesis for p):")),
                        column(2, tags$div( 
                                 tags$input(id = "null_p", type = "text", class = "form-control", value = "0.5"))
                               )
                        ),
                       plotOutput('cat1Test_Plot2', click = 'cat1_Test_click', height = "300px")  
                    )
                )
              ), 
              #br(),
              fluidRow(
                column(5, offset = 1, h4("How many more samples from the null?")),
                column(1,
                   actionButton("cat1_test_shuffle_10", label = "10")),
                column(1,
                   actionButton("cat1_test_shuffle_100", label = "100")),
                column(1,
                   actionButton("cat1_test_shuffle_1000", label = "1000")),
                column(1,
                   actionButton("cat1_test_shuffle_5000", label = "5000"))
               ),
               br(),
               br(),
               fluidRow(
                 column(8, offset = 4,
                        h4(" Click on a point to see its counts and proportions."), br(), 
                        uiOutput("Cat1TestXtremes"),
                        uiOutput("Cat1TestPvalue")
                 )
               )
        )
       )
    }
 })

observeEvent( input$null_p, {
  cat1Test$phat <- NULL  
  cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL
})


output$Cat1TestXtremes <- renderUI({
  fluidRow(
    column(3,  
           h4("Count values")
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
#           textInput('cat1_test_cutoff', label = "", value = NA)
           tags$div( 
                tags$input(id = "cat1_test_cutoff", type = "text", class = "form-control", value = NA))
    ),
    column(1,
           actionButton('cat1_test_countXtremes', "Go")
    )
  )
})

observeEvent( input$cat1_testDirection, {
  cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL    
  cat1Test$colors <- rep(blu, length(cat1Test$colors))
})

observeEvent( input$cat1_test_cutoff, {
  cat1Test$pvalue <- cat1Test$moreExtremeCount <- NULL    
  cat1Test$colors <- rep(blu, length(cat1Test$colors))
})


output$Cat1TestPvalue <- renderUI({
  if(!is.null(cat1Test$moreExtremeCount)){
    #     fluidRow(
    #       column(8, offset = 3, 
    h4(paste(cat1Test$moreExtremeCount, " of ", cat1Test$sampleCount, "values are ",
             cat1Test$direction," than", as.numeric(cat1Test$cutoff),",  p-value =  ", round(cat1Test$pvalue,5))
    )
    #      ))
  }
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
    })
    
    output$cat1Test_Table <- renderTable({
      #if(input$cat2_submitButton == 0) return()
      if(is.null(cat1_data$counts)) return()
      
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
        #cat("Close to number: ", closestPoint, "\n")
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
    })
    
    
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
      if(input$cat1_submitButton == 0 | is.na(input$null_p) | is.null(cat1Test$phat)) return()
      
      DF <- sort(cat1Test$phat)
      
      if(length(DF) == 1){
        w <- 1
        radius = 4
      } 
      else {
        nbreaks <- 0.5*nclass.Sturges(DF)^2
        z <- cut(DF, breaks = nbreaks)
        w <- unlist(tapply(z, z, function(V) 1:length(V)))
        w <- w[!is.na(w)]
        #print(w)
        #print(max(w))
        nsims <- length(DF)
        radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
      }
      plot(DF, w, ylab = "", ylim = c(0.5, max(w)), cex = radius/2, pch = 16, col = cat1Test$colors,  
           xlab = expression(hat(p)), main = "Sampling Distribution")
      legend("topright", bty = "n", paste(length(DF), "points \n Mean = ", 
                                         round(mean(DF),3), "\n SE = ", round(sd(DF),3)))
  }, height = 300, width = 500)

}

   ###  estimate phat  -------------------------------------- cat 1
{
output$cat1_estimateUI <- renderUI({
  if( is.null(cat1_data$counts)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else {
    tabPanel("Estimate", value="1catEstimate",
             titlePanel("Estimate a Single Proportion"),       
             div(
               fluidRow(
                 column(4, 
                      h3("Original Data"),
                      tableOutput("cat1_CIPrep"),
                      
                      h3("One Resampled Dataset"),
                      tableOutput('cat1Estimate_Table')
                      
                 ),
                 column(8, 
                      plotOutput('cat1Estimate_Plot2', click = 'cat1_Estimate_click')
                 )
             ),
             fluidRow(
               column(4, offset = 1, h4("How many more resamples?")),
               column(1,
                      actionButton("cat1_estimate_shuffle_10", label = "10")),
               column(1,
                      actionButton("cat1_estimate_shuffle_100", label = "100")),
               column(1,
                      actionButton("cat1_estimate_shuffle_1000", label = "1000")),
               column(1,
                      actionButton("cat1_estimate_shuffle_5000", label = "5000"))
             ),
             br(),
             fluidRow(
               column(5, offset = 3, 
                     h4("Click on a point to see its count."))
               ),
             br(),
             fluidRow(
               column(4, offset = 2, 
                      h4("Select Confidence Level (%)")
                      ),
               column(6,
                 fluidRow(
                    column(2,  
                      actionButton('cat1_conf80', label = "80")),
                    column(2,
                      actionButton('cat1_conf90', label = "90")),
                    column(2,
                      actionButton('cat1_conf95', label = "95")),
                    column(2,
                      actionButton('cat1_conf99', label = "99"))
                ))),
                        
              if(!is.null(cat1Estimate$CI)){
                fluidRow(
                  column(8, offset = 4,
                          h4(paste(round(100 * cat1Estimate$confLevel), "% Confidence Interval Estimate: (", round(cat1Estimate$CI[1],3), ",", 
                                   round(cat1Estimate$CI[2], 3), ")"))
                  )
                )
                } else {br()}
            )
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
  
})


output$cat1Estimate_Table <- renderTable({
  #if(input$cat1_submitButton == 0) return()
  if(is.null(cat1_data$counts)) return()
  
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
})


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
  if(is.null(cat1Estimate$phat) | (nsims <- length(cat1Estimate$phat)) < 10){
    return()
  }
  cat1Estimate$confLevel <- .80
  cat1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * 0.1)
  cat1Estimate$colors[1:tailCount] <- rd
  cat1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  cat1Estimate$CI <- sort(cat1Estimate$phat)[c(tailCount, nsims + 1 - tailCount)]
  
})

observeEvent(input$cat1_conf90,{
  if(is.null(cat1Estimate$phat) | (nsims <- length(cat1Estimate$phat)) < 10){
    return()
  }
  cat1Estimate$confLevel <- .90
  cat1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * 0.05)
  cat1Estimate$colors[1:tailCount] <- rd
  cat1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  cat1Estimate$CI <- sort(cat1Estimate$phat)[c(tailCount, nsims + 1 - tailCount)]
  
})

observeEvent(input$cat1_conf95,{
  if(is.null(cat1Estimate$phat) | (nsims <- length(cat1Estimate$phat)) < 10){
    return()
  }
  cat1Estimate$confLevel <- .95
  cat1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * 0.025)
  cat1Estimate$colors[1:tailCount] <- rd
  cat1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  cat1Estimate$CI <- sort(cat1Estimate$phat)[c(tailCount, nsims + 1 - tailCount)]
  
})

observeEvent(input$cat1_conf99,{
  if(is.null(cat1Estimate$phat) | (nsims <- length(cat1Estimate$phat)) < 10){
    return()
  }
  cat1Estimate$confLevel <- .99
  cat1Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * 0.005)
  cat1Estimate$colors[1:tailCount] <- rd
  cat1Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  cat1Estimate$CI <- sort(cat1Estimate$phat)[c(tailCount, nsims + 1 - tailCount)]
  
})


output$cat1Estimate_Plot2 <- renderPlot({
  if(input$cat1_submitButton == 0 | is.null(cat1Estimate$phat)) return()
  
  DF <- sort(cat1Estimate$phat)
  
  if(length(DF) == 1){
    w <- 1
    radius = 4
  } 
  else {
    nbreaks <- 0.5*nclass.Sturges(DF)^2
    z <- cut(DF, breaks = nbreaks)
    w <- unlist(tapply(z, z, function(V) 1:length(V)))
    w <- w[!is.na(w)]
    #print(w)
    #print(max(w))
    nsims <- length(DF)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(DF, w, ylab = "", ylim = c(0.5, max(w)), cex = radius/2, pch = 16, col = cat1Estimate$colors,  
       xlab = expression(hat(p)), main = "Re-Sampling Distribution")
  legend("topright", bty = "n", paste(length(DF), "points \n Mean = ", 
                                     round(mean(DF),3), "\n SE = ", round(sd(DF),3)))
}, height = 340, width = 440)

}
  
  ##  confidence interval demo  -------------------------------------- cat 1
{  
  output$inputTrueP <- renderUI({
    n <- input$CIdemo_n
    sliderInput("CIdemo_p", "Choose true proportion of successes ", 
                min=pmin(0.5, round(10/n,2)), max=pmax(.5, round(1- 9.9/n,2)), value = .5)
  })
  
  CIdemoSims <- reactive({
    if(is.null(input$CIdemo_p)) return()
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
    if(is.null(input$CIdemo_p)) return()  
    phatDF <- CIdemoSims()
    nsims <- as.numeric(input$CIdemo_reps)      
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)
    par(mar=c(4,2,1,1))
    isolate({
      plot(y ~ phat, data= phatDF, col = rgb(70, 130, 180, 127, max = 255), pch=16, bty="l",
           cex = radius/2, ylab = "", xlab = expression(hat(p)))#, main = "Sampling Distribution")
    })
  }, height = 275)
  
  output$CIdemo_Plot2 <- renderPlot({
    ##displayFn <-  reactive({
    if(is.null(input$CIdemo_p) || is.null(input$CIdemo_conf)) return() 
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
  {
  
  c1Lurk <- reactiveValues(data = NULL, shuffles = NULL, difprop = NULL, 
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
      column(2, actionButton("c1Lurk_Go", "Go"))
       
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
    if (is.null(c1Lurk$data) | is.null(c1Lurk$names))
      return()
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
                        round( as.numeric(c1Lurk$difprop[2]), 3)))
              ),
        column(8, 
               plotOutput('c1_LurkPlot2')#, click = 'c1_Lurk_click')
      )
    ),
    fluidRow(
      column(4, offset = 1,
             h4("How many more randomizations?")
             ),
      column(1, actionButton("c1_Lurk_shuffle_10", label = "10")),
      column(1, actionButton("c1_Lurk_shuffle_100", label = "100")),
      column(1, actionButton("c1_Lurk_shuffle_1000", label = "1000")),
      column(1, actionButton("c1_Lurk_shuffle_5000", label = "5000"))
      
    )  
  )
})
  
#   output$c1_LurkSampDistPlot <- renderUI({ 
#     plotOutput('c1_LurkPlot2') #, click = 'c1_Lurk_click')
#   })
  
  
  ## -------- 1 cat Lurking tables ------------------
  

  output$c1Lurk_Table1 <- renderTable({
    if(is.null(c1Lurk$data)) 
      return()
    y1_new <- c1Lurk$y1[1]
    y2_new <- c1Lurk$y2[1]
    diff.p <- c1Lurk$difprop[1]
    # print(c(y1_new, n1, DF[1,1], y2_new, n2, DF[1,2], diff.p))
    table1 <- data.frame(count = as.integer(c(y1_new, y2_new)), 
                         N = with(c1Lurk$data, as.integer(c(m1, m2))),
                         Prop = c(c1Lurk$phat1[1], c1Lurk$phat2[1]))
    colnames(table1)[1] <- c1Lurk$names[1]
    rownames(table1) <- c("Treatment","Control")
    table1
  })
  
  
  output$c1Lurk_Table2 <- renderTable({
    if(is.null(c1Lurk$data)) 
      return()
    y1_new <- c1Lurk$y1[2]
    y2_new <- c1Lurk$y2[2]
    diff.p <- c1Lurk$difprop[2]
    # print(c(y1_new, n1, DF[1,1], y2_new, n2, DF[1,2], diff.p))
    table2 <- data.frame(count = as.integer(c(y1_new, y2_new)), 
                         N = with(c1Lurk$data, as.integer(c(m1, m2))),
                         Prop = c(c1Lurk$phat1[2], c1Lurk$phat2[2]))
    colnames(table2)[1] <- c1Lurk$names[1]
    rownames(table2) <- c("Treatment","Control")
    table2
  })
  
  observeEvent(input$c1_Lurk_shuffle_10, {
    DF <- c1Lurk_shuffles(shuffles = 10, c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$difprop <- c(c1Lurk$difprop,  DF[,3])
    c1Lurk$phat1 <- c(c1Lurk$phat1,  DF[,1])
    c1Lurk$phat2 <- c(c1Lurk$phat2,  DF[,2])
    c1Lurk$colors <- rep(blu, length(c1Lurk$difprop))
  })
  
  observeEvent(input$c1_Lurk_shuffle_100, {
    DF <- c1Lurk_shuffles(shuffles = 100,  c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$difprop <- c(c1Lurk$difprop,  DF[,3])
    c1Lurk$phat1 <- c(c1Lurk$phat1,  DF[,1])
    c1Lurk$phat2 <- c(c1Lurk$phat2,  DF[,2])
    c1Lurk$colors <- rep(blu, length(c1Lurk$difprop))
  })
  observeEvent(input$c1_Lurk_shuffle_1000, {        
    DF <- c1Lurk_shuffles(shuffles = 1000,  c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$difprop <- c(c1Lurk$difprop,  DF[,3])
    c1Lurk$phat1 <- c(c1Lurk$phat1,  DF[,1])
    c1Lurk$phat2 <- c(c1Lurk$phat2,  DF[,2])
    c1Lurk$colors <- rep(blu, length(c1Lurk$difprop))
  })
  observeEvent(input$c1_Lurk_shuffle_5000, {
    DF <- c1Lurk_shuffles(shuffles = 5000,  c1Lurk$data$n1 , c1Lurk$data$n2, c1Lurk$data$m1)
    c1Lurk$difprop <- c(c1Lurk$difprop,  DF[,3])
    c1Lurk$phat1 <- c(c1Lurk$phat1,  DF[,1])
    c1Lurk$phat2 <- c(c1Lurk$phat2, DF[,2])
    c1Lurk$colors <- rep(blu, length(c1Lurk$difprop))
  })
  
  
  output$c1_LurkPlot2 <- renderPlot({
    if(is.null(c1Lurk$difprop)) 
      return()
    diffs <- sort(c1Lurk$difprop)
    if(length(diffs) < 4){
      w <- 1 + diffs*0
      radius <- 4 + diffs*0
    } 
    else {
      nbreaks <- 0.5*nclass.Sturges(diffs)^2
      z <- cut(diffs, breaks = nbreaks)
      w <- unlist(tapply(z, z, function(V) 1:length(V)))
      w <- w[!is.na(w)]
      # print(summary(w))
      # print(max(w))
      nsims <- length(diffs)
      radius <- 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(diffs, w, ylab = "", ylim = c(0.5, max(w)), cex = radius/2, pch = 16, col = c1Lurk$colors,  
         xlab = expression(hat(p)[1] - hat(p)[2]), main = "Randomization Distribution")
    legend("topright", bty = "n", paste(length(diffs), "points \n Mean = ", 
                                        round(mean(diffs),3), "\n SE = ", round(sd(diffs),3)))
    #mtext(side = 1, at = 0, adj = 0, line = 0, bquote(p[1] == p[2]))
  }, width = 480)
  

  
  }
  

  ## Normal probability computations  ----------------------- cat 1
{
  ##  Set storage for reactive values

cat1_normalProb <- reactiveValues(prob = NULL, z = NULL, findP = NULL)
  
  observeEvent( input$cat1_z_txt, {
    #if(is.null(input$cat1_z_txt)) 
    #  return
    cat1_normalProb$z <- as.numeric(input$cat1_z_txt) 
    cat1_normalProb$findP <- TRUE
  })

 observeEvent( input$cat1_prob_txt,{
    #if(is.null(input$cat1_p_txt)) 
    #  return
    cat1_normalProb$prob <- as.numeric(input$cat1_prob_txt) 
    cat1_normalProb$findP  <- FALSE
  })

output$normalProbPlot1 <- renderPlot({ 
  #print(cat1_normalProb$prob)
  #print(cat1_normalProb$z)
  #print(cat1_normalProb$findP)
  if(is.null(cat1_normalProb$findP))
    return()
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
    if(absz < 1) place.x <- place.x/absz * .8
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
  
  
 
##  Enter data    ----------------------------------------- quant 1

{

output$quant1DataIn <- renderText({ "How would you like to input the data? " 
  })

 ## user selects an input method.
  ## renderUI changes to get appropriate inputs.

 output$q1_inputUI <- renderUI({
  if (is.null(input$q1_entry))
    return()
  switch( input$q1_entry,
          "Pre-Loaded Data" ={ 
            fluidRow(  
              column(4, selectInput('q1_data1', 'Available Datasets',  choices = as.list(quant1_contents))
                     ),
              column(4, actionButton("q1_useLddBtn", "Use These Data") )
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
                     actionButton("q1_useCSVBtn", "Use These Data")
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
          "Type/Paste into Data Table" = {
            #h4("Edit the values in Column 2.  Column 1 will be ignored.  To paste, use Cntrl-V or Cmd-V(on a mac)"), 
            ## take inputs here for number of rows (& columns?)
            fluidRow(
              column(4, 
                     rHandsontableOutput("q1_hot")) 
              ,
               column(4, actionButton("q1_useHotBtn", "Use These Data"))
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
   output$quant1DataIn <- renderText({
          "Data are entered, you may now choose to estimate or test one mean"
   })
 })

 observeEvent(  input$q1_useCSVBtn,{
   DF <-  read.csv(input$q1_file1$datapath, header=input$q1_header, sep=input$q1_sep, quote=input$q1_quote)
   q1$names <- if(is.null(names(DF)) | "V1" %in% names(DF)){ "x"} else {names(DF)[1]}
   q1$data <- data.frame(DF[, 1])
   #print(q1$data)
   q1Test$nsims <- 0
   q1Test$shuffles <-  q1Test$new.xbars <-  q1Test$xbar <-  q1Test$colors <- NULL
   q1Estimate$shuffles <- q1Estimate$xbars <- q1Estimate$observed <- q1Estimate$confLevel <- q1Estimate$colors <- NULL
   output$quant1DataIn <- renderText({
     "Data are entered, you may now choose to estimate or test one mean"
   })
 })

 observeEvent(input$q1_useHotBtn,{
   DF = data.frame(x=as.numeric(q1_values[["hot"]][,2]))
   # print(DF)
   q1$names <- names(DF)
   q1Test$nsims <- 0
   q1$data <- data.frame( x = as.numeric(unlist(DF)))
   q1Test$shuffles <-  q1Test$new.xbars <-  q1Test$xbar <-   q1Test$colors <- NULL
   q1Estimate$shuffles <- q1Estimate$xbars <- q1Estimate$observed <- q1Estimate$colors <- NULL
   #print(q1$data)
   output$quant1DataIn <- renderText({
     "Data are entered, you may now choose to estimate or test one mean"
   })
 })

   q1_values = list()
   q1_setHot = function(x) q1_values[["hot"]] <<- x
   q1_setHot(read.csv("data/dummyData.csv", stringsAsFactors = FALSE, head = TRUE) )

  output$q1_hot = renderRHandsontable({
    if (!is.null(input$q1_hot)) {
      q1_DF = hot_to_r(input$q1_hot)
      q1_setHot(q1_DF)
      rhandsontable(q1_DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE, copyPaste = TRUE, pasteMode = "shift_down")
    } else {
      ## seems that HOT needs at least 2 columns, so column 1 is just row numbers.
      q1_DF = read.csv("data/dummyData.csv", stringsAsFactors = FALSE, head = TRUE)
      #cat("loading \n")
      #print(q1_DF)
      q1_setHot(q1_DF)    
      rhandsontable(q1_values[["hot"]], height = 200) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })



}

  ##  Describe and plot data -------------------------------  quant 1
{
  output$q1_Plot <- renderPlot( {
    if( is.null(q1$data))  return()
     #if(input$q1_useLddBtn == 0 && input$q1_useHotBtn == 0)  ## && input$q1_useFileBtn == 0) 
       #isolate( { 
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
    z <- cut(x, breaks = nclass.Sturges(x) ^2 )
    w <- unlist(tapply(x, z, function(x) 1:length(x)))
    tempDF <- data.frame(x, w=w[!is.na(w)])
    myBlue <- rgb(0, 100/256, 224/256, alpha = .8)  
    q1_plot2 <- qplot(data=tempDF, x=x, y=w, colour = I(myBlue), size = I(4)) + theme_bw() + xlab(q1$names)
    grid.arrange(q1_plot1, q1_plot2, heights = c(1,3)/4, ncol=1)
  #})
}, height=360)


  output$q1_Summary <- renderTable({
    if( is.null(q1$data))  
      return()
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
                   length = length(q1$data[, 1]))
      colnames(DF) <- q1$names
      DF
    #}
  }, digits = 3)
}


  ###  test mean value  ------------------------------------ quant 1
{

## --------- 1 quant UI ---------------------------


output$q1_testUI <- renderUI({
  if(is.null(q1$data)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else{ 
  fluidPage(
    h3("Test for a single mean."),
      fluidRow(
        column(4, 
               plotOutput("q1_TestPlot1")
              ),
        column(8, 
               tags$label(HTML("True Mean (Null hypothesis for &mu;):"), 
                          tags$input(name='null_mu', type='text', value='0', size='10')),
              ##uiOutput('q1_SampDistPlot')),
               plotOutput('q1_TestPlot2', click = 'q1_Test_click', height = '360px')
              )
        ),
       br(), 
       br(),
       fluidRow(
           column(5, offset = 1, h4("How many more (shifted) resamples?")),
           column(1, actionButton("q1_test_shuffle_1", label = "1")),
           column(1, actionButton("q1_test_shuffle_10", label = "10")),
           column(1, actionButton("q1_test_shuffle_100", label = "100")),
           column(1, actionButton("q1_test_shuffle_1000", label = "1000")),
           column(1, actionButton("q1_test_shuffle_5000", label = "5000"))
         ),
             
        br(),
        fluidRow(
           column(5, offset =6, h4("Click on a point to see that resample."))
         ),
        fluidRow(
           column(8, offset = 4,
             uiOutput("q1TestXtremes"),
             uiOutput("q1TestPvalue")
             #  need some kind of reactive connection to data and the form inputs just above
         )
       )
  )

  }
})
  
# output$q1_SampDistPlot <- renderUI({ 
#   plotOutput('q1_TestPlot2', click = 'q1_Test_click')
# })


observeEvent( input$null_mu, {
  q1Test$pvalue <- q1Test$moreExtremeCount <- NULL
  q1Test$colors <- rep(blu, length(q1Test$colors))
})


output$q1TestPvalue <- renderUI({
  if(!is.null(q1Test$moreExtremeCount)){
    h4(paste(q1Test$moreExtremeCount, " of ", q1Test$sampleCount, "values are ",
                                    q1Test$direction," than", q1Test$cutoff, ",  p-value =  ", round(q1Test$pvalue,5))
    )
  }
})

output$q1TestXtremes <- renderUI({
  fluidRow(offset = 1,
    column(3,  
           h4("Count values")
    ),
    column(4,
           tags$div(style="width: 200px",
                    tags$select(id='q1_testDirection', class="form-control",
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
           actionButton("q1_countXtremes","Go")
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
  z <- cut(x, breaks = nclass.Sturges(x) ^2 )
  w <- unlist(tapply(x, z, function(x) 1:length(x)))
  tempDF <- data.frame(x, w=w[!is.na(w)])
  z <- cut(x, breaks = nclass.Sturges(x) ^2 )
  w <- unlist(tapply(x, z, function(x) 1:length(x)))
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

output$q1_TestPrep2 <- renderTable({
  if( is.null(q1$data))  return()
  DF <- rbind(mean = mean(q1$data[, 1], na.rm = TRUE ),
              sd = sd(q1$data[, 1], na.rm = TRUE),
              n = length(q1$data[,1]))
  colnames(DF) <- q1$names
  DF
})

output$q1_TestTable1 <- renderTable({
  if( is.null(q1$data))  return()
  DF <- rbind(mean = mean(q1Test$shuffles[,1], na.rm = TRUE ),
              sd = sd(q1Test$shuffles[,1], na.rm = TRUE),
              length = length(q1Test$shuffles[,1]))
  colnames(DF) <- q1$names
  DF
})

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
  if(is.null(q1Test$new.xbars)) return() 
  
    parm <- as.matrix(q1Test$new.xbars)
    #print(parm)
    parm <- sort(parm)
    if(length(parm) == 1){
      y <- .5
      radius <- 4
    } else {
      nbreaks <- nclass.Sturges(parm)^2
      z <- cut(parm, breaks = nbreaks)
      y <- unlist(tapply(z, z, function(V) 1:length(V)))
      y <- y[!is.na(y)]
      #print(y)
      #print(max(w))
      nsims <- length(parm)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
  plot(x = parm + as.numeric(input$null_mu), y = y, ylim = c(0.5, max(y)), ylab = "", cex = radius/2, pch = 16, col = q1Test$colors,  
       xlab = expression(bar(x)), main = "Shifted Resampling Distribution")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                     round(mean(parm) + as.numeric(input$null_mu),3), "\n SE = ", round(sd(parm),3)))
  
}, height = 360, width = 480)



}
  ###   estimate mean value  ------------------------------- quant 1
{
 ## --------- 1 quant estimate UI ---------------------------

output$q1_estimateUI <- renderUI({
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
         column(1,
                actionButton("q1_resample_10", label = "10")),
         column(1,
                actionButton("q1_resample_100", label = "100")),
         column(1,
                actionButton("q1_resample_1000", label = "1000")),
         column(1,
                actionButton("q1_resample_5000", label = "5000"))
       ),
      br(),
      br(),
      fluidRow(
        column(4, offset = 3, 
               h4("Select Confidence Level (%)")
        ),
        column(5,
               fluidRow(
                 column(2,  
                        actionButton('q1_conf80', label = "80")),
                 column(2,
                        actionButton('q1_conf90', label = "90")),
                 column(2,
                        actionButton('q1_conf95', label = "95")),
                 column(2,
                        actionButton('q1_conf99', label = "99"))
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
  if(is.null(q1$data)) return()
  q1Estimate$observed <- mean(q1$data[,1])
  nn <- nrow(q1$data)
  par(mfrow = c(2,1), mar = c(4,3.5,3,1))
  
  ## Plot Original Data
  x <- sort(q1$data[,1])
  z <- cut(x, breaks = nclass.Sturges(x) ^2 )
  w <- unlist(tapply(x, z, function(x) 1:length(x)))
  w <- w[!is.na(w)]
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
  z <- cut(DF0, breaks = nclass.Sturges(DF0) ^2 )
  w <- unlist(tapply(DF0, z, function(DF0) 1:length(DF0)))
  w <- w[!is.na(w)]
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
  if(is.null(q1Estimate$xbars) ) return() 
  parm <- as.matrix(q1Estimate$xbars) 
  #print(parm)
  parm <- sort(parm)
  if(length(parm) == 1){
    y <- .5
    radius <- 4
  } else {
    nbreaks <- nclass.Sturges(parm)^2
    z <- cut(parm, breaks = nbreaks)
    y <- unlist(tapply(z, z, function(V) 1:length(V)))
    y <- y[!is.na(y)]
    #print(y)
    #print(max(w))
    nsims <- length(parm)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(x = parm, y = y, ylim = c(0.5, max(y)), ylab = "", cex = radius/2, pch = 16, col = q1Estimate$colors,  
       xlab = expression(bar(x)), main = "Resampling Distribution")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                      round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
}, height = 360, width = 480)

}

  ## Lurking Demo  --------------------------------------    quant 1
  {
  
  q1Lurk <- reactiveValues(data = NULL, shuffles = NULL, diff = NULL, 
                           colors = NULL,  name = NULL)
  
  output$q1_LurkDataUI <- renderUI({
    ## choice of normal or skewed data
    fluidRow(
      column(2, actionButton("q1Lurk_IQ", label = "IQ (normal)")),
      column(2, actionButton("q1Lurk_Salary", label = "Salary (skewed)"))
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
    if (is.null(q1Lurk$data) | is.null(q1Lurk$shuffles))
      return()
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
          column(1, actionButton("q1_Lurk_shuffle_10", label = "10")),
          column(1, actionButton("q1_Lurk_shuffle_100", label = "100")),
          column(1, actionButton("q1_Lurk_shuffle_1000", label = "1000")),
          column(1, actionButton("q1_Lurk_shuffle_5000", label = "5000"))
        )
     )
  })
  
  output$q1_LurkSampDistPlot <- renderUI({ 
    plotOutput('q1_LurkPlot2') #, click = 'q1_Lurk_click')
  })
  

  # -------- 1 quant Lurking plots ------------------
  
  output$q1_LurkPlot1 <- renderPlot({
    if(is.null(q1Lurk$shuffles)) 
      return()
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
    plot2 <- qplot(y=y, x=group2, data = DF, geom="boxplot", main = "Randomization 2") +
      theme_bw() + xlab("") +  coord_flip() + ylab(q1Lurk$name)
    
    
    grid.arrange(plot1, plot2, heights = c(3, 3)/6, ncol=1)
  }, height = 360, width = 320)
  
  
  output$q1_LurkTable1 <- renderTable({
    if(is.null(q1Lurk$data))  
      return()
    #print(q1Lurk$data)
    DF <- data.frame(mean = tapply(q1Lurk$data, q1Lurk$shuffles[,1], mean, na.rm = TRUE ),
                     sd = tapply(q1Lurk$data, q1Lurk$shuffles[,1], sd, na.rm = TRUE ),
                     n = as.integer(tapply(q1Lurk$data, q1Lurk$shuffles[,1], length)))
    rownames(DF) <- levels(q1Lurk$shuffles[,1])
    DF
  })
  
  
  output$q1_LurkTable2 <- renderTable({
    if( is.null(q1Lurk$data))  
      return()
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
    if(is.null(q1Lurk$diff)) 
      return() 
    
    parm <- sort(q1Lurk$diff)
    # print(parm)
    if(length(parm) < 3){
      y <- rep(0.5,length(parm))
      radius <- 4
    } else {
      nbreaks <- nclass.Sturges(parm)^2
      z <- cut(parm, breaks = nbreaks)
      y <- unlist(tapply(z, z, function(V) 1:length(V)))
      y <- y[!is.na(y)]
      #print(y)
      #print(max(w))
      nsims <- length(parm)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(parm, y, ylim = c(0.5, max(y)), ylab = "", cex = radius/2, pch = 16, col = q1Lurk$colors,  
         xlab = expression(bar(x)[1] - bar(x)[2]), main = "Randomization Distribution")
    legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                        round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
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
    if(is.null(q1_tProb$findP))
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
      x <- seq(-4,10,length=200)
      plot(x, dt(x, input$pwr_n-1),bty='l', type="l", xlab="",ylab="")
      lines(x,dt(x,input$pwr_n-1,ncp=input$pwr_altMean/input$pwr_sd *sqrt(input$pwr_n)))
      abline(h=0)
      qt1 <- qt(1-input$pwr_alpha, input$pwr_n-1)
      xrr <- c(qt1, qt1, x[x>=qt1],max(x))
      yrr <- c(0, dt(c(qt1,  x[x>=qt1]), input$pwr_n -1),0)
      polygon(xrr,yrr, col = rd)
      abline(v = c(0, qt1))
      xpwr <- c(qt1,x[x>=qt1])
      ypwr <- c(0, dt(xpwr,df=input$pwr_n-1, ncp=input$pwr_altMean/input$pwr_sd *sqrt(input$pwr_n) ),0)
      xpwr <- c(qt1, xpwr, max(x))
      ##cat(length(xpwr), length(ypwr))
      polygon(xpwr,ypwr, col = grn)
      text(weighted.mean(xrr,w=yrr^2),weighted.mean(yrr,w=yrr^.5),cex=1.5, expression(alpha))
      text(weighted.mean(xpwr,w=ypwr^4),weighted.mean(ypwr,w=ypwr^.5),cex=1.5,"Power")
      mtext(side=1,at=0,line=2, expression(H[0]: mu == 0))
      mtext(side=1,at= xpwr[which.max(ypwr)], line=2, expression(H[a]: mu > 0))
    })
    
    # Show the values using an HTML table
    output$powerValues <- renderTable({
      sliderValues()
    }, include.rownames = FALSE)
  
  
  }  
  
  ## 2 Categorical ------------------------------------------------------------- 2 cat

  ## Data Entry --------------------------------------------------------  cat 2
## need to remove old data if user comes back to data entry 
{
cat2_data <- reactiveValues(counts = NULL, names = NULL, groups = NULL)

observeEvent(input$cat2_submitButton, {
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
 })
  

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
                            round(- diff(prop.table(as.table(matrix(cat2_data$counts, 2, 2)), 1))[1], 3)
                   )),
                        
                  br(),
                        
                  h4("Shuffled Sample"),
                  tableOutput('cat2Test_Table') , 
                  h5(paste("Shuffled difference in proportions: ", round(tail(as.numeric(cat2Test$difprop), 1), 3)
                   ))
                  ),
                 column(8, 
                        h4(HTML("&nbsp;&nbsp;&nbsp;&nbsp; Null hypothesis: p<sub>1</sub> = p<sub>2</sub>")),                     
                        plotOutput('cat2Test_Plot2', click = 'cat2_Test_click')
                        
                 )
          ),
      #br(),
      fluidRow(
        column(4, offset = 1, h4("How many more shuffles?")),
        column(1,
               actionButton("cat2_test_shuffle_10", label = "10")),
        column(1,
               actionButton("cat2_test_shuffle_100", label = "100")),
        column(1,
               actionButton("cat2_test_shuffle_1000", label = "1000")),
        column(1,
               actionButton("cat2_test_shuffle_5000", label = "5000"))
      ),
      br(),
      br(),
      fluidRow(
        column(8, offset = 4, 
               uiOutput("Cat2TestXtremes")
        )
      ),
     fluidRow(
        column(8, offset = 4, 
          uiOutput("Cat2TestPvalue")
       )
    )
   )          
  }
})

output$Cat2TestXtremes <- renderUI({
  fluidRow(
    column(3, 
           h4("Count values")
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
           actionButton('cat2_test_countXtremes', "Go")
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
  if(!is.null(cat2Test$moreExtremeCount)){
    h4(paste(cat2Test$moreExtremeCount, " of ", cat2Test$sampleCount, "values are ",
                      cat2Test$direction," than", as.numeric(cat2Test$cutoff),",  p-value =  ", round(cat2Test$pvalue,5))
    )
  }
})

## cat2 test plots --------------------------------------------------

  cat2Test <- reactiveValues(difprop = NULL, phat1 = NULL, phat2 = NULL, observed = NULL, colors = NULL,
                             cutoff = NULL, direction = NULL, moreExtremeCount = NULL, pvalue = NULL, 
                             sampleCount = NULL)
  
  output$cat2OriginalData <- renderTable({ 
    if(input$cat2_submitButton ==0) return()
    y1 = cat2_data$counts[1]
    y2 = cat2_data$counts[2]
    n1 <- sum(cat2_data$counts[1], cat2_data$counts[3])
    n2 <- sum(cat2_data$counts[2], cat2_data$counts[4])
    p1 <- round(y1/n1,3)
    p2 = round(y2/n2,3)
    # print(c(y1, n1, y2, n2, p1, p2))
    counts <- data.frame(count = as.integer(c(y1, y2)), 
                         "SampleSize" = as.integer(c(n1, n2)),
                         Proportion = c(p1, p2))
    colnames(counts)[1] <- cat2_data$names[1]
    rownames(counts) <- cat2_data$groups[c(1,3)]
    counts
  })
  
  observeEvent(input$cat2_test_shuffle_10, {
    cat2Test$moreExtremeCount <- NULL
    DF <- cat2_test_shuffles(shuffles = 10, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                            n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                            n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Test$difprop <- rbind(cat2Test$difprop, as.matrix(DF[,3]))
    cat2Test$phat1 <- rbind(cat2Test$phat1, as.matrix(DF[,1]))
    cat2Test$phat2 <- rbind(cat2Test$phat2, as.matrix(DF[,2]))
    cat2Test$colors <- rep(blu, length(cat2Test$difprop))
  })
  
  observeEvent(input$cat2_test_shuffle_100, {
    cat2Test$moreExtremeCount <- NULL
    DF <- cat2_test_shuffles(shuffles = 100, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                            n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                            n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Test$difprop <- rbind(cat2Test$difprop, as.matrix(DF[,3]))
    cat2Test$phat1 <- rbind(cat2Test$phat1, as.matrix(DF[,1]))
    cat2Test$phat2 <- rbind(cat2Test$phat2, as.matrix(DF[,2]))
    cat2Test$colors <- rep(blu, length(cat2Test$difprop))

  })
  
  observeEvent(input$cat2_test_shuffle_1000, {
    cat2Test$moreExtremeCount <- NULL
    DF <- cat2_test_shuffles(shuffles = 1000, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                            n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                            n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Test$difprop <- rbind(cat2Test$difprop, as.matrix(DF[,3]))
    cat2Test$phat1 <- rbind(cat2Test$phat1, as.matrix(DF[,1]))
    cat2Test$phat2 <- rbind(cat2Test$phat2, as.matrix(DF[,2]))
    cat2Test$colors <- rep(blu, length(cat2Test$difprop))

  })
  
  observeEvent(input$cat2_test_shuffle_5000, {
    cat2Test$moreExtremeCount <- NULL
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
    if(is.null(cat2_data$counts)) return()
    n1 <- sum(cat2_data$counts[1], cat2_data$counts[3])
    n2 <- sum(cat2_data$counts[2], cat2_data$counts[4])
    DF <- cat2_test_shuffles(1, cat2_data$counts[1], cat2_data$counts[2], n1, n2)
    cat2Test$difprop <- DF[1,3]
    cat2Test$phat1 <- DF[1,1]
    cat2Test$phat2 <- DF[1,2]
    cat2Test$colors <- blu
    y1_new <- as.integer(n1 * DF[1,1])
    y2_new <- as.integer(n2 * DF[1,2])
    # print(c(y1_new, n1, DF[1,1], y2_new, n2, DF[1,2], diff.p))
    count2 <- data.frame(count = as.integer(c(y1_new, y2_new)), 
                     "Sample Size" = as.integer(c(n1, n2)),
                     Proportion = DF[1, 1:2])
    colnames(count2)[1] <- cat2_data$names[1]
    rownames(count2) <- cat2_data$groups[c(1,3)]
    count2
  })

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
    if(input$cat2_submitButton == 0) return()
    if(is.null(cat2Test$difprop)) return()
    
    DF <- sort(cat2Test$difprop)
    
    if(length(DF) == 1){
      w <- 1
      radius = 4
      } 
    else {
      nbreaks <- 0.5*nclass.Sturges(DF)^2
      z <- cut(DF, breaks = nbreaks)
      w <- unlist(tapply(z, z, function(V) 1:length(V)))
      w <- w[!is.na(w)]
      #print(w)
      #print(max(w))
      nsims <- length(DF)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(DF, w, ylab = "", ylim = c(0.5, max(w)), cex = radius/2, pch = 16, col = cat2Test$colors,  
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
                        h3("Original Data"),
                        tableOutput("cat2_CIPrep"),
                        h5(paste("Original Difference in proportions: ", 
                                 round(-diff(prop.table(as.table(matrix(cat2_data$counts, 2, 2)), 1))[1],3))), 
                        br(),
                        
                        h3("One Resampled Dataset"),
                        tableOutput('cat2Estimate_Table'),
                        h5(paste("Difference in proportions for resampled data: " , 
                                 round(as.numeric(cat2Estimate$difprop[1]), 3)))
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
                            actionButton("cat2_estimate_shuffle_10", label = "10")),
                   column(1, 
                          actionButton("cat2_estimate_shuffle_100", label = "100")),
                   column(1,
                          actionButton("cat2_estimate_shuffle_1000", label = "1000")),
                   column(1,
                          actionButton("cat2_estimate_shuffle_5000", label = "5000"))
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
                                   actionButton('cat2_conf80', label = "80")),
                            column(2,
                                   actionButton('cat2_conf90', label = "90")),
                            column(2,
                                   actionButton('cat2_conf95', label = "95")),
                            column(2,
                                   actionButton('cat2_conf99', label = "99"))
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
  
  cat2Estimate <- reactiveValues(difprop = NULL, phat1 = NULL, phat2 = NULL, observed = NULL, colors = blu,
                             confLevel = NULL, CI = NULL)
  
  output$cat2_CIPrep <- renderTable({ 
    if(input$cat2_submitButton ==0) return()
    y1 = cat2_data$counts[1]
    y2 = cat2_data$counts[2]
    n1 <- sum(cat2_data$counts[1], cat2_data$counts[3])
    n2 <- sum(cat2_data$counts[2], cat2_data$counts[4])
    p1 <- round(y1/n1,3)
    p2 = round(y2/n2,3)
    #print(c(y1, n1, y2, n2, p1, p2))
    counts <- as.table(matrix(as.numeric(c(y1, y2, n1, n2, p1, p2)), 2, 3))
    colnames(counts) <- c("Success", "Sample Size", "Proportion")
    rownames(counts) <- cat2_data$groups[c(1,3)]
    counts 

  })
  

  observeEvent(input$cat2_estimate_shuffle_10, {
    cat2Estimate$CI <- NULL
    DF <- cat2_estimate_shuffles(shuffles = 10, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                                 n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                                 n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Estimate$difprop <- rbind(cat2Estimate$difprop, as.matrix(DF[,3]))
    cat2Estimate$phat1 <- rbind(cat2Estimate$phat1, as.matrix(DF[,1]))
    cat2Estimate$phat2 <- rbind(cat2Estimate$phat2, as.matrix(DF[,2]))
    cat2Estimate$colors <- rep(blu, length(cat2Estimate$difprop))
    cat2Estimate$observed <- (cat2_data$counts[1]/sum(cat2_data$counts[1], cat2_data$counts[3])) - 
      (cat2_data$counts[2]/sum(cat2_data$counts[2], cat2_data$counts[4]))
    
  })
  
  observeEvent(input$cat2_estimate_shuffle_100, {
    cat2Estimate$CI <- NULL
    DF <- cat2_estimate_shuffles(shuffles = 100, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                                 n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                                 n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Estimate$difprop <- rbind(cat2Estimate$difprop, as.matrix(DF[,3]))
    cat2Estimate$phat1 <- rbind(cat2Estimate$phat1, as.matrix(DF[,1]))
    cat2Estimate$phat2 <- rbind(cat2Estimate$phat2, as.matrix(DF[,2]))
    cat2Estimate$colors <- rep(blu, length(cat2Estimate$difprop))
    cat2Estimate$observed <- (cat2_data$counts[1]/sum(cat2_data$counts[1], cat2_data$counts[3])) - 
      (cat2_data$counts[2]/sum(cat2_data$counts[2], cat2_data$counts[4]))
    
  })
  
  observeEvent(input$cat2_estimate_shuffle_1000, {
    cat2Estimate$CI <- NULL
    DF <- cat2_estimate_shuffles(shuffles = 1000, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                                 n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                                 n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Estimate$difprop <- rbind(cat2Estimate$difprop, as.matrix(DF[,3]))
    cat2Estimate$phat1 <- rbind(cat2Estimate$phat1, as.matrix(DF[,1]))
    cat2Estimate$phat2 <- rbind(cat2Estimate$phat2, as.matrix(DF[,2]))
    cat2Estimate$colors <- rep(blu, length(cat2Estimate$difprop))
    cat2Estimate$observed <- (cat2_data$counts[1]/sum(cat2_data$counts[1], cat2_data$counts[3])) - 
      (cat2_data$counts[2]/sum(cat2_data$counts[2], cat2_data$counts[4]))
    
  })
  
  observeEvent(input$cat2_estimate_shuffle_5000, {
    cat2Estimate$CI <- NULL
    DF <- cat2_estimate_shuffles(shuffles = 5000, y1 = cat2_data$counts[1], y2 = cat2_data$counts[2], 
                                 n1= sum(cat2_data$counts[1], cat2_data$counts[3]), 
                                 n2= sum(cat2_data$counts[2], cat2_data$counts[4]))
    cat2Estimate$difprop <- rbind(cat2Estimate$difprop, as.matrix(DF[,3]))
    cat2Estimate$phat1 <- rbind(cat2Estimate$phat1, as.matrix(DF[,1]))
    cat2Estimate$phat2 <- rbind(cat2Estimate$phat2, as.matrix(DF[,2]))
    cat2Estimate$colors <- rep(blu, length(cat2Estimate$difprop))
    cat2Estimate$observed <- (cat2_data$counts[1]/sum(cat2_data$counts[1], cat2_data$counts[3])) - 
      (cat2_data$counts[2]/sum(cat2_data$counts[2], cat2_data$counts[4]))
    
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
    if(input$cat2_submitButton == 0) return()
    if(is.null(cat2_data$counts)) return()
    
    n1 <- sum(cat2_data$counts[1], cat2_data$counts[3])
    n2 <- sum(cat2_data$counts[2], cat2_data$counts[4])
    DF <- cat2_estimate_shuffles(1, cat2_data$counts[1], cat2_data$counts[2], n1, n2)
    cat2Estimate$difprop <- DF[1,3]
    cat2Estimate$phat1 <- DF[1,1]
    cat2Estimate$phat2 <- DF[1,2]
    cat2Estimate$colors <- blu
    y1_new <- as.integer(n1 * DF[1,1])
    y2_new <- as.integer(n2 * DF[1,2])
    diff.p <- DF[1,1] - DF[1,3]
    #print(c(y1_new, n1, DF[1,1], y2_new, n2, DF[1,2], diff.p))
    count2 <- as.table(matrix(as.numeric(c(y1_new, y2_new, n1, n2, DF[1:2])), 2, 3))
    colnames(count2) <- c("Success", "Sample Size", "Proportion")
    rownames(count2) <- cat2_data$groups[c(1,3)]
    count2
  })
  
  
  output$cat2Estimate_Plot2 <- renderPlot({
    if(input$cat2_submitButton == 0) return()
    if(is.null(cat2Estimate$difprop)) return()
    
    DF <- sort(cat2Estimate$difprop)
    
    if(length(DF) == 1){
      w <- 1
      radius = 4
    } 
    else {
      nbreaks <- 0.5 * nclass.Sturges(DF)^2
      z <- cut(DF, breaks = nbreaks)
      w <- unlist(tapply(z, z, function(V) 1:length(V)))
      w <- w[!is.na(w)]
      #print(w)
      #print(max(w))
      nsims <- length(DF)
      radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
    }
    plot(DF, w, ylab = "", ylim = c(0.5, max(w)), cex = radius/2, pch = 16, col = cat2Estimate$colors,  
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
  #if(is.null(input$cat2_z_txt)) 
  #  return
  cat2_normalProb$z <- as.numeric(input$cat2_z_txt) 
  cat2_normalProb$findP <- TRUE
 })

 observeEvent( input$cat2_prob_txt,{
  #if(is.null(input$cat2_p_txt)) 
  #  return
  cat2_normalProb$prob <- as.numeric(input$cat2_prob_txt) 
  cat2_normalProb$findP  <- FALSE
 })

output$normalProbPlot2 <- renderPlot({ 
  #print(cat2_normalProb$prob)
  #print(cat2_normalProb$z)
  #print(cat2_normalProb$findP)
  if(is.null(cat2_normalProb$findP))
    return()
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
                              CI = NULL)

  ##  grab data according to input method
  q2 <- reactiveValues(data = NULL, names = NULL, intercept = NULL, slope = NULL, 
                       corr = NULL, qr = NULL)
  
  output$quant2DataIn <- renderText({
  "How do you want to input the data?"
 })

 # use  selectInput to grab the 3 types of input
 output$q2_ui <- renderUI({
  if (is.null(input$q2_entry))
    return()
  switch( input$q2_entry,
          "Pre-Loaded Data" ={ 
            fluidRow(  
              column(4, selectInput('q2_data1', 'Available Datasets',  choices = as.list(quant2_contents))
              ),
              column(4, actionButton("q2_useLddBtn", "Use These Data") )
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
                     actionButton("q2_useCSVBtn", "Use These Data")
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
          "Type/Paste into Data Table" = {
            #h4("Edit the values in Column 2.  Column 1 will be ignored.  To paste, use Cntrl-V or Cmd-V(on a mac)"), 
            fluidRow(
              column(4, 
                     rHandsontableOutput("q2_hot")) 
              ,
              column(4, actionButton("q2_useHotBtn", "Use These Data"))
            )            
          }, 
          NULL
  )
  ##  Need to grab names from the data input
})

observeEvent(  input$q2_useLddBtn, {
  ##  Wipe out any old data  
  q2Test$shuffles <- q2Test$slopes <- q2Test$corr <- q2Test$observed <- 
    q2Test$colors <- q2Test$moreExtremeCount <- q2Test$pvalue <- NULL
  q2Estimate$resamples <- q2Estimate$slopes <- q2Estimate$corr <- q2Estimate$observed <-
    q2Estimate$CI <- q2Estimate$colors <- NULL
  
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
  q2Test$shuffles <- q2Test$slopes <- q2Test$corr <- q2Test$observed <- 
    q2Test$colors <- q2Test$moreExtremeCount <- q2Test$pvalue <- NULL
  q2Estimate$resamples <- q2Estimate$slopes <- q2Estimate$corr <- q2Estimate$observed <-
    q2Estimate$CI <- q2Estimate$colors <- NULL
  
  DF <- read.csv(input$q2_file1$datapath, header=input$q2_header,
                      sep=input$q2_sep, quote=input$q2_quote)
  q2$names <- names(DF)
  q2$data <- data.frame(DF)
  names(q2$data) <- c("x","y")
  output$quant2DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true slope or correlation"
  })
  
})

observeEvent(  input$q2_useHotBtn,{
  ##  Wipe out any old data  
  q2Test$shuffles <- q2Test$slopes <- q2Test$corr <- q2Test$observed <- 
    q2Test$colors <- q2Test$moreExtremeCount <- q2Test$pvalue <- NULL
  q2Estimate$resamples <- q2Estimate$slopes <- q2Estimate$corr <- q2Estimate$observed <-
    q2Estimate$CI <- q2Estimate$colors <- NULL
  
  DF <- data.frame(q2_values[["hot"]])
  # print(DF)
  q2$names <- names(DF) 
  q2$data <- data.frame(DF)
  names(q2$data) <- c("x","y")
  output$quant2DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true slope or correlation"
  })
})

q2_values = list()
q2_setHot = function(x) q2_values[["hot"]] <<- x

output$q2_hot = renderRHandsontable({
  if (!is.null(input$q2_hot)) {
    q2_DF = hot_to_r(input$q2_hot)
    q2_setHot(q2_DF)
    rhandsontable(q2_DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  } else {
    ## seems that HOT needs at least 2 columns, so column 1 is just row numbers.
    q2_DF = read.csv("data/dummyData.csv", stringsAsFactors = FALSE, head = TRUE)
    q2_setHot(q2_DF)    
    rhandsontable(q2_DF, height = 230) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  }
})
}

###  Data Summary ----------------------------------------------------------  q2

{
output$q2_Plot <- renderPlot( {
  if( is.null(q2$data)) 
    return()
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
                  xlab(q2$names[1]) + ylab(q2$names[2]) 
    grid.arrange(q2_plot1, q2_plot2, q2_plot3, heights = c(1.5, 1.5, 5)/8, ncol=1)
  #}
}, height=400)

output$q2_Summary <- renderTable({
  if( is.null(q2$data))  
    #if(input$q2_useHotBtn == 0 && input$q2_useExistingBtn == 0 && input$q2_useFileBtn == 0) 
    return()
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
                correlation = c(q2$corr, NA),
                beta.hat = round(coef(fit0),3),
                "resid SD" = c(summary(fit0)$sigma, NA) )
    colnames(DF) <- q2$names
    DF
  #})
})


observeEvent(  input$q2_swapXwithY,{
  if(is.null(q2$data))
    return()
  q2$names <- q2$names[2:1]
  q2$data <- q2$data[, 2:1]
  names(q2$data) <- c("x","y")
  })

output$q2_swap <- renderUI({
  if (is.null(q2$data))
    return()
  actionButton('q2_swapXwithY', "Swap Variables (X goes to Y)")
})
}

###   TESTING slope / correlation = 0 ---------------------------------------  q2
{
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
  if(is.null(q2$slope))
    return()
  par(mfrow=c(2,1), mar = c(2.4,2,4,2))
  DF0 <- q2$data
 # colnames(DF0) <- c("x","y")
  plot(y ~ x, data=DF0, xlab = q2$names[1], ylab = q2$names[2], col = blu, pch = 16,
       main = "Original Data")
  #lmfit0 <- lm(y ~ x, DF0)
  abline(q2$intercept, q2$slope)
  mtext(side = 3, line=.4, at = min(DF0$x)/3 + max(DF0$x)*2/3, bquote(r == .(round(q2$corr,3))))
  mtext(side = 3,   at = min(DF0$x)*2/3 + max(DF0$x)/3, bquote(hat(beta)[1] == .(round(q2$slope,3))))
  q2Test$shuffles <- shuffle <- sample(1:nrow(q2$data))
  if(!is.null(input$q2_Test_click) & FALSE ){
    ## grab clicked sample
    print(input$q2_Test_click)
    clickX <- input$q2_Test_click$x
    clickY <- input$q2_Test_click$y
    nearest <- which( abs(q2Test$slope - clickX) < diff(range(q2Test$slope))/30 & abs(clickY - q2Test$y) < .5 )[1]
    shuffle <- q2Test$shuffle[nearest,]
  }         ##  ^^ onclick changes plot1, but also resets to a single shuffle, destroying plot2
  DF0$newy <- DF0$y[shuffle]
  plot(y ~ x, data = DF0, xlab = q2$names[1], ylab = q2$names[2], col = blu, pch =16,
       main = "Shuffled Data")
  points(newy ~ x, data = DF0, pch = 16, col = "green")
  points(newy ~ x, data = DF0)
  with(subset(DF0, abs(y - newy) > .0001),
    arrows(x, y, x, newy, col = grey(.8), length = .1, angle = 15)
  )
  lmfit1 <- lm(newy ~ x, DF0)
  abline(lmfit1)
  q2Test$slopes <- beta <- round(coef(lmfit1)[2], 3)
  q2Test$corr <- rhat1 <- round(cor(DF0$x, DF0$newy), 3)
  q2Test$colors <- blu
  mtext(side = 3, at = min(DF0$x)*2/3 + max(DF0$x)/3, bquote(hat(beta)[1] == .(beta) ) )
  mtext(side = 3, line=.4, at = min(DF0$x)/3 + max(DF0$x)*2/3, bquote(r == .(rhat1)))
}, height = 400, width = 300)

observeEvent(input$q2_shuffle_10, {
  q2Test$moreExtremeCount <- NULL
  newShuffles <- t( sapply(1:10, function(x) sample(1:nrow(q2$data))))
  q2Test$shuffles <- rbind(q2Test$shuffles, newShuffles)
 q2Test$slopes <- c(q2Test$slopes, apply(newShuffles, 1, function(x) qr.coef(q2$qr, q2$data[x, 2])[2]))
 q2Test$corr <- c(q2Test$corr, apply(newShuffles, 1, function(ndx) cor(q2$data$x, q2$data[ndx, 2])))
 q2Test$colors <- rep(blu, length(q2Test$slopes))
})

observeEvent(input$q2_shuffle_100, {
  q2Test$moreExtremeCount <- NULL
  newShuffles <- t( sapply(1:100, function(x) sample(1:nrow(q2$data))))
#  print(dim(newShuffles))
  q2Test$shuffles <- rbind(q2Test$shuffles, newShuffles)
  q2Test$slopes <- c(q2Test$slopes, apply(newShuffles, 1, function(x) qr.coef(q2$qr, q2$data[x, 2])[2]))
  q2Test$corr <- c(q2Test$corr, apply(newShuffles, 1, function(x) cor(q2$data[,1],q2$data[x, 2])))
  q2Test$colors <- rep(blu, length(q2Test$slopes))

})
observeEvent(input$q2_shuffle_1000, {
  q2Test$moreExtremeCount <- NULL
  newShuffles <- t( sapply(1:1000, function(x) sample(1:nrow(q2$data))))
 # print(dim(newShuffles))
  q2Test$shuffles <- rbind(q2Test$shuffles, newShuffles)
  q2Test$slopes <- c(q2Test$slopes, apply(newShuffles, 1, function(x) qr.coef(q2$qr, q2$data[x, 2])[2]))
  q2Test$corr <- c(q2Test$corr, apply(newShuffles, 1, function(x) cor(q2$data[,1],q2$data[x, 2])))
  q2Test$colors <- rep(blu, length(q2Test$slopes))

})
observeEvent(input$q2_shuffle_5000, {
  q2Test$moreExtremeCount <- NULL
  newShuffles <- t( sapply(1:5000, function(x) sample(1:nrow(q2$data))))
#  print(dim(newShuffles))
  q2Test$shuffles <- rbind(q2Test$shuffles, newShuffles)
  q2Test$slopes <- c(q2Test$slopes, apply(newShuffles, 1, function(x) qr.coef(q2$qr, q2$data[x, 2])[2]))
  q2Test$corr <- c(q2Test$corr, apply(newShuffles, 1, function(shf) cor(q2$data[,1],q2$data[shf, 2])))
  q2Test$colors <- rep(blu, length(q2Test$slopes))
})

observeEvent(input$q2_countXtremes, {
  if(input$q2_TestParam == "Slope") {
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
  if(is.null( q2Test$shuffles) )
    return() 
  if(input$q2_TestParam == "Slope") {
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
    z <- cut(parm, breaks = .5 * nclass.Sturges(parm)^2 )
#  print(summary(z))
    y <- unlist(tapply(z, z, function(v) 1:length(v)))
    y <-  y[!is.na(y)]
    q2Test$y <- y
    nsims <- length(parm)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(parm, y, ylab = "", cex = radius/2, pch = 16, col = q2Test$colors,  
        xlab = ifelse(input$q2_TestParam == "Slope", "Slope", "Correlation"), main = "Sampling Distribution")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                     round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
}, height = 400, width = 400)

  ## q2 test UI -------------------------------------
output$q2_testUI <- renderUI({
  if( is.null(q2$data)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else {
    fluidPage(
      h3("Test: is slope (or correlation) zero?"),
     fluidRow(
       column(3, 
              tableOutput('q2_TestPrep'),
#               h4("We start showing one shuffle."),
#               h4("How many more?"),
#               actionButton("q2_shuffle_10", label = "10"),
#               actionButton("q2_shuffle_100", label = "100"),
#               actionButton("q2_shuffle_1000", label = "1000"),
#               actionButton("q2_shuffle_5000", label = "5000"),
#               br(),    ########## why does this not add a new line????
#               br(),    ########## why does this not add a new line????
              radioButtons('q2_TestParam', label = "Parameter: ", list("Slope","Correlation"), inline = TRUE)
       ),
       ## for 1 shuffle, show equal size plots of original and reshuffled x,y data
       ##  for more shuffles, make original data and click --> shuffle plots smaller, large plot of 
       ##  sampling distribution for slope / correlation.
       column(4, 
              plotOutput('q2_TestPlot1')
       ),
       column(5,
              # h5("Click on a point to see that shuffle"),
              uiOutput('q2_SampDistPlot')
       ###
       )
     ),
     fluidRow(
        column(4, offset = 1, 
               h4("How many more shuffles?")),
        column(1,
          actionButton("q2_shuffle_10", label = "10")),
        column(1,
               actionButton("q2_shuffle_100", label = "100")),
        column(1,
               actionButton("q2_shuffle_1000", label = "1000")),
        column(1,
               actionButton("q2_shuffle_5000", label = "5000"))
     ),
     br(),
     br(),
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
  plotOutput('q2_TestPlot2')#, click = 'q2_Test_click')
  })

output$slopeTestPvalue <- renderUI({
  if(!is.null(q2Test$moreExtremeCount)){
     h4(paste(q2Test$moreExtremeCount, " of ", q2Test$sampleCount, "values are ",
        q2Test$direction," than", q2Test$cutoff, ",  p-value =  ", round(q2Test$pvalue,5)))
  }
})
                                
output$slopeTestXtremes <- renderUI({
fluidRow(
  column(3, 
         h4("Count values")  ),
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
  column(1, actionButton("q2_countXtremes","Go")   )
 )
}) 
}
  ###  Estimate slope / correlation with CI ----------------------------------- q2 
  ##  
{
  output$q2_CIPrep <- renderTable({
  ##  print("in q2_CIPrep")
    out <- list("Correlation: " =  q2$corr,
                "Slope: " = q2$slope )
    out <- do.call(rbind, out)
    colnames(out) <- c("Estimate")
    out
  })
  
  output$q2_EstPlot1 <- renderPlot({
    if(is.null(q2$slope))
      return()
   b1hat <- round(q2$slope, 3)
   rhat0 <- round(q2$corr, 3)
   plotOrig <- qplot(x=x, y=y, data=q2$data, xlab = q2$names[1], ylab = q2$names[2], col = I(blu), 
         main = "Original Data") + theme_bw() + 
         geom_abline(intercept = q2$intercept, slope = b1hat, colour = blu)
#    mtext(side = 3,  at = min(q2$data$x)*2/3 + max(q2$data$x)/3, bquote(hat(beta)[1] == .(b1hat) ))
#    mtext(side = 3,  line = .3, at = min(q2$data$x)/3 + max(q2$data$x)*2/3, bquote(r == .(rhat0)))
      ## Bootstrap resample 
#      if(!is.null(input$q2Est_click) ){
#        ## grab clicked resample
#        #print(input$q2Est_click$x)
#        clickX <- input$q2Est_click$x
#        nearest <- which.min( abs(q2Estimate$slopes - clickX) )
#        resample <- q2Estimate$resample[nearest,]
#        DF1 <- q2$data[resample, ]
#        lmfit1 <- lm(y ~ x, DF1)    
#      }  else {
       resample <- sort(sample(1:nrow(q2$data), replace=TRUE))
       q2Estimate$resamples <-  matrix(resample, nrow=1)
       DF1 <- q2$data[resample, ]
       lmfit1 <- lm(y ~ x, DF1)    
       q2Estimate$slopes <- beta <- round(coef(lmfit1)[2], 3)
       q2Estimate$corr <- rhat1 <- round(cor(DF1$x, DF1$y), 3)
       q2Estimate$colors <- blu
#      }
    DF1$sizes <- rep(table(resample), table(resample))
    plotNew <- qplot(x=x, y=y, data = DF1,  colour = I(grn), size = factor(DF1$sizes)) + 
                    labs(x = q2$names[1], y=q2$names[2], title="One Resampled Dataset") + 
                    theme_bw() + scale_size_discrete( "repeats", range=c(3, 6))+
                    geom_abline(intercept = coef(lmfit1)[1], slope=coef(lmfit1)[2]) 
     
    #mtext(side = 3, at = min(DF1$x)*2/3 + max(DF1$x)/3, bquote(hat(beta)[1] == .(beta) ) )
    #mtext(side = 3, line = .3,  at = min(DF1$x)/3 + max(DF1$x)*2/3, bquote(r == .(rhat1)))
    grid.arrange(plotOrig, plotNew, heights = c(4,  4)/8, ncol=1)
  }, height = 400, width = 300)
  
  observeEvent(input$q2_resample_10, {
    q2Estimate$CI <- NULL
    resamples <- t( sapply(1:10, function(x) sample(1:nrow(q2$data), replace = TRUE)))
    resamples <- resamples[apply(resamples,1, function(ndx) var(q2$data[ndx, 2])) > 1.e-15, ]
    q2Estimate$resamples <- rbind(q2Estimate$resamples, resamples)
    q2Estimate$slopes <- c(q2Estimate$slopes, apply(resamples, 1, function(ndx) coef(lm(y ~ x, q2$data[ndx, ]))[2]))
    q2Estimate$corr <- c(q2Estimate$corr, apply(resamples, 1, function(ndx) cor(q2$data$x[ndx], q2$data$y[ndx])))
    q2Estimate$colors <- rep(blu, length(q2Estimate$slopes))
  })
  
observeEvent(input$q2_resample_100, {
  q2Estimate$CI <- NULL
  resamples <- t( sapply(1:100, function(x) sample(1:nrow(q2$data), replace = TRUE)))
  resamples <- resamples[apply(resamples,1, function(ndx) var(q2$data[ndx,2])) > 1.e-15, ]
  q2Estimate$resamples <- rbind(q2Estimate$resamples, resamples)
  q2Estimate$slopes <- c(q2Estimate$slopes, apply(resamples, 1, function(ndx) coef(lm(y ~ x, q2$data[ndx, ]))[2]))
  q2Estimate$corr <- c(q2Estimate$corr, apply(resamples, 1, function(ndx) cor(q2$data$x[ndx], q2$data$y[ndx])))
  q2Estimate$colors <- rep(blu, length(q2Estimate$slopes))
})

observeEvent(input$q2_resample_1000, {
  q2Estimate$CI <- NULL
  resamples <- t( sapply(1:1000, function(x) sample(1:nrow(q2$data), replace = TRUE)))
  resamples <- resamples[apply(resamples,1, function(ndx) var(q2$data[ndx,2])) > 1.e-15, ]
  q2Estimate$resamples <- rbind(q2Estimate$resamples, resamples)
  q2Estimate$slopes <- c(q2Estimate$slopes, apply(resamples, 1, function(ndx) coef(lm(y ~ x, q2$data[ndx, ]))[2]))
  q2Estimate$corr <- c(q2Estimate$corr, apply(resamples, 1, function(ndx) cor(q2$data$x[ndx], q2$data$y[ndx])))
  q2Estimate$colors <- rep(blu, length(q2Estimate$slopes))
})

observeEvent(input$q2_resample_5000, {
  q2Estimate$CI <- NULL
  resamples <- t( sapply(1:5000, function(x) sample(1:nrow(q2$data), replace = TRUE)))
  resamples <- resamples[apply(resamples,1, function(ndx) var(q2$data[ndx,2])) > 1.e-15, ]
  q2Estimate$resamples <- rbind(q2Estimate$resamples, resamples)
  q2Estimate$slopes <- c(q2Estimate$slopes, apply(resamples, 1, function(ndx) coef(lm(y ~ x, q2$data[ndx, ]))[2]))
  q2Estimate$corr <- c(q2Estimate$corr, apply(resamples, 1, function(ndx) cor(q2$data$x[ndx], q2$data$y[ndx])))
  q2Estimate$colors <- rep(blu, length(q2Estimate$slopes))
})

output$q2_EstPlot2 <- renderPlot({
  if(is.null( q2Estimate$resamples) )
    return() 
  #print(input$q2_EstParam)
  if(input$q2_EstParam == "Slope") {
    parm <-  as.numeric(q2Estimate$slopes)
    q2Estimate$observed <- q2$slope
  } else { 
    parm <- as.numeric(q2Estimate$corr)
    q2Estimate$observed <- q2$corr
  }
  parm <- sort(parm)
  ## print(summary(parm))
  if(length(parm) == 1){
    y <- .5
    radius <- 4
    nsims <- 1
  } else {
    z <- cut(parm, breaks = .5 * nclass.Sturges(parm)^2 )
    #  print(summary(z))
    y <- unlist(tapply(z, z, function(v) 1:length(v)))
    y <-  y[!is.na(y)]
    q2Estimate$y <- y
    nsims <- length(parm)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(parm, y, ylab = "", cex = radius/2, pch = 16, col = q2Estimate$colors,  
       xlab = ifelse(input$q2_EstParam == "Slope", "Slope", "Correlation"), main = "RE-Sampling Distribution")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
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
  q2Estimate$CI <- if(input$q2_EstParam == "Slope"){
    sort(q2Estimate$slopes)[c(tailCount, nsims + 1 - tailCount)]
  } else {
    sort(q2Estimate$corr)[c(tailCount, nsims + 1 - tailCount)]
  }
})

observeEvent(input$q2_conf90,{
  if(is.null(q2Estimate$slopes) | (nsims <- length(q2Estimate$slopes)) < 10){
    return()
  }
  q2Estimate$confLevel <- .90
  q2Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .05)
  q2Estimate$colors[1:tailCount] <- rd
  q2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q2Estimate$CI <- if(input$q2_EstParam == "Slope"){
    sort(q2Estimate$slopes)[c(tailCount, nsims+1 -tailCount)]
  } else {
    sort(q2Estimate$corr)[c(tailCount, nsims+1 -tailCount)]
  }
})

observeEvent(input$q2_conf95,{
  if(is.null(q2Estimate$slopes) | (nsims <- length(q2Estimate$slopes)) < 10){
    return()
  }
  q2Estimate$confLevel <- .95
  q2Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .025)
  q2Estimate$colors[1:tailCount] <- rd
  q2Estimate$colors[nsims +1 -(1:tailCount)] <- rd
  q2Estimate$CI <- if(input$q2_EstParam == "Slope"){
    sort(q2Estimate$slopes)[c(tailCount, nsims+1 -tailCount)]
  } else {
    sort(q2Estimate$corr)[c(tailCount, nsims+1 -tailCount)]
  }
})

observeEvent(input$q2_conf99,{
  if(is.null(q2Estimate$slopes) | (nsims <- length(q2Estimate$slopes)) < 10){
    return()
  }
  q2Estimate$confLevel <- .99
  q2Estimate$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .005)
  q2Estimate$colors[1:tailCount] <- rd
  q2Estimate$colors[nsims + 1 -(1:tailCount)] <- rd
  q2Estimate$CI <- if(input$q2_EstParam == "Slope"){
    sort(q2Estimate$slopes)[c(tailCount, nsims+1 -tailCount)]
  } else {
    sort(q2Estimate$corr)[c(tailCount, nsims+1 -tailCount)]
  }
})



output$q2_estimateUI <- renderUI({
  if( is.null(q2$data)){
    h4(" You must first enter data. Choose 'Enter/Describe Data'.")
  } else {
    fluidPage(
      fluidRow(
        column(3, tableOutput('q2_CIPrep'),
               br(),
               br(),
               radioButtons('q2_EstParam', label = "Parameter: ", list("Slope","Correlation"), 
                            "Slope", inline = TRUE)
        ),
        ## for 1 resample, show equal size plots of original and resampled x,y data
        ##  for more resamples, make original data and click --> resample plots smaller, large plot of 
        ##  resampling distribution for slope / correlation.
        column(4, 
               plotOutput('q2_EstPlot1')
        ),
        column(5,
               # h5("Click on a point to see that shuffle")
               plotOutput('q2_EstPlot2', click = 'q2Est_click')
        )),
       fluidRow(
           column(4, offset = 1, 
                  h4("How many more shuffles?")),
           column(1,
                  actionButton("q2_resample_10", label = "10")),
           column(1,
                  actionButton("q2_resample_100", label = "100")),
           column(1,
                  actionButton("q2_resample_1000", label = "1000")),
           column(1,
                  actionButton("q2_resample_5000", label = "5000"))
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
                           actionButton('q2_conf80', label = "80")),
                    column(2,
                           actionButton('q2_conf90', label = "90")),
                    column(2,
                           actionButton('q2_conf95', label = "95")),
                    column(2,
                           actionButton('q2_conf99', label = "99"))
                  )
           )
         ),
         if(!is.null(q2Estimate$CI)){
           fluidRow(
             column(8, offset = 4,
                    h4(paste(round(100 * q2Estimate$confLevel), "% Confidence Interval Estimate: (", 
                             round(q2Estimate$CI[1],3), ",", 
                             round(q2Estimate$CI[2], 3), ")"))
             )
           )
         } else {br()}
      )
    }
})

}

## 1 categorical & 1 quantitative   ---------------------------------------  1 cat 1 quant

  ###  Data entry ------------------------------------------------------------ 1c1q
{
output$c1q1DataIn <- renderText({
  "How do you want to input the data?"
})

output$c1q1_ui <- renderUI({
  if (is.null(input$c1q1_entry))
    return()
  switch( input$c1q1_entry,
          "Pre-Loaded Data" ={ 
            fluidRow(  
              column(4, selectInput('c1q1_data1', 'Available Datasets',  choices = as.list(c1q1_contents))
              ),
              column(4, actionButton("c1q1_useLddBtn", "Use These Data") )
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
                     actionButton("c1q1_useCSVBtn", "Use These Data")
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
          "Type/Paste into Data Table" = {
            #h4("Edit the values in Column 2.  Column 1 will be ignored.  To paste, use Cntrl-V or Cmd-V(on a mac)"), 
            fluidRow(
              column(4, 
                     rHandsontableOutput("c1q1_hot")) 
              ,
              ## allow user to change names of the columns:
              # column(4,
              #       )
              column(4, actionButton("c1q1_useHotBtn", "Use These Data"))
            )
            
          }, 
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
  output$c1q1DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true difference in means"
  })
  
})

observeEvent(  input$c1q1_useHotBtn,{
  DF <- data.frame(c1q1_values[["hot"]])
  # print(DF)
  names(DF) <- c("group","y")
  c1q1$names <- names(DF)
  c1q1$data <- DF
  output$c1q1DataIn <- renderText({
    "Data are entered, you may now choose to estimate or test the true difference in means"
  })
})

c1q1_values = list()
c1q1_setHot = function(x) c1q1_values[["hot"]] <<- x

output$c1q1_hot = renderRHandsontable({
  if (!is.null(input$c1q1_hot)) {
    c1q1_DF = hot_to_r(input$c1q1_hot)
    c1q1_setHot(c1q1_DF)
    rhandsontable(c1q1_DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  } else {
    ## seems that HOT needs at least 2 columns, so column 1 is just row numbers.
    c1q1_DF = read.csv("data/dummyDatac1q1.csv", stringsAsFactors = FALSE, head = TRUE)
    c1q1_setHot(c1q1_DF)    
    rhandsontable(c1q1_DF, height = 230) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  }
})
}
  ##  Describe and summarize ------------------------------------------------- 1c1q
{

c1q1Test <- reactiveValues(shuffles = NULL,  observed = NULL, diff = NULL, confLevel = NULL, colors = NULL, 
                           moreExtremeCount = NULL, pvalue = NULL, direction = NULL, cutoff = NULL,
                           sampleCount = NULL)

c1q1Est <- reactiveValues(shuffles = NULL,  observed = NULL, diff = NULL, confLevel = NULL, colors = NULL, 
                          ndx1 = NULL, ndx2 = NULL, CI = NULL )

output$c1q1_Plot <- renderPlot( {
  if( is.null(c1q1$data)) 
    return()
  #isolate( { 
  ## make plot
  DF <- c1q1$data
  names(DF) <- c("group","y")
  DF[, 1] <- factor(DF[,1])
  #print(summary(DF))
  c1q1_plot1 <- qplot(y=y, x=group, data = DF, geom="boxplot") +
    theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2]) 
  DF <- DF[order(DF$y), ]
  nbreaks <- min(c(length(unique(DF$y)), floor(.5*nclass.Sturges(DF$y)^2)))
  z <- cut(DF$y, breaks =  nbreaks )
  w <- unlist(tapply(DF$y, list(z, DF$group), function(x) 1:length(x)))
  w <- w[!is.na(w)]  
  myBlue <- rgb(0, 100/256, 224/256, alpha = .8)  
  c1q1_plot2 <- qplot(data= DF, x=y, y=w , colour = I(myBlue), size = I(4))+ facet_wrap( ~group) + 
    theme_bw() + ylab("Count") + xlab( c1q1$names[2])
  grid.arrange(c1q1_plot1, c1q1_plot2, heights = c(2,  5)/7, ncol=1)
  #})
}, height=400)


output$c1q1_Summary1 <- renderTable({
  if( is.null(c1q1$data))  
    #if(input$c1q1_useHotBtn == 0 && input$c1q1_useExistingBtn == 0 && input$c1q1_useFileBtn == 0) 
    return()
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
              length = tapply(y, group, length)
        ))
  colnames(DF) <- levels(tempDF$group)
  DF
  #})
})

output$c1q1_Summary2 <- renderTable({
  if( is.null(c1q1$data))  
    #if(input$c1q1_useHotBtn == 0 && input$c1q1_useExistingBtn == 0 && input$c1q1_useFileBtn == 0) 
    return()
    val <- round( -diff(tapply(c1q1$data[, 2], c1q1$data[, 1], mean, na.rm=TRUE)), 3)
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
                      plotOutput("c1q1_TestPrep1")
                      ),
               column(3,
                      br(),
                      br(),
                      tableOutput("c1q1_TestPrep2"),
                      h5(paste("Original Difference in means = ", 
                               round(-diff(tapply(c1q1$data[, 2], c1q1$data[, 1], mean, na.rm=TRUE)), 3))),
                      
                      br(),
                      br(),
                      tableOutput("c1q1_TestTable1"),
                      h5(paste("Shuffled difference in means = ", round(as.numeric(c1q1Test$diff[1]),3)))
                     ),
               column(5, 
                      h4(HTML("Null hypothesis: &mu;<sub>1</sub> = &mu;<sub>2</sub>")),
                      uiOutput('c1q1_SampDistPlot')
                     )
              ),
            fluidRow(
              column(4, offset = 1, h4("How many more shuffles?")),
              column(1, actionButton("c1q1_test_shuffle_10", label = "10")),
              column(1, actionButton("c1q1_test_shuffle_100", label = "100")),
              column(1, actionButton("c1q1_test_shuffle_1000", label = "1000")),
              column(1, actionButton("c1q1_test_shuffle_5000", label = "5000"))
            ),
            
            br(),
            fluidRow(
              column(8, offset = 4,
                     uiOutput("c1q1TestXtremes"),
                     uiOutput("c1q1TestPvalue")
              )
            )
          )
      }
    })
      
      output$c1q1_SampDistPlot <- renderUI({ 
        plotOutput('c1q1_TestPlot2') #, click = 'c1q1_Test_click')
      })
      
      output$c1q1TestPvalue <- renderUI({
        if(!is.null(c1q1Test$moreExtremeCount)){
          #fluidRow(
          #  column(9,  
          h4(paste(c1q1Test$moreExtremeCount, " of ", c1q1Test$sampleCount, "values are ",
                                            c1q1Test$direction," than", c1q1Test$cutoff, ",  p-value =  ", round(c1q1Test$pvalue,5)))
          #  ))
        }
      })
      
      output$c1q1TestXtremes <- renderUI({
        fluidRow(
          column(2,
                 h4("Count values")
          ),
          column(5,
                 tags$div(style="width: 200px",
                          tags$select(id='c1q1_testDirection', class="form-control",
                                      tags$option( value = "less", "less"),
                                      tags$option( value = "more extreme", "more extreme", selected = TRUE),
                                      tags$option( value = "greater", "greater"))
                 )
          ),
          column(2, h4("than ")),
          column(2,
                 tags$div( 
                   tags$input(id = "c1q1_test_cutoff", type = "text", class = "form-control", value = NA))
          ),
          column(1, 
            actionButton("c1q1_countXtremes","Go")
          )
        )
      })
      
      
      # -------- 1 cat  1 quant test plots ------------------
      
      output$c1q1_TestPrep1 <- renderPlot({
        if(is.null(c1q1$data)) return()
        ## Original Data
        DF <- c1q1$data
        names(DF) <- c("group","y")
        DF$group <- factor(DF$group)
        #print(DF)
        
        plot1 <- qplot(y=y, x=group, data = DF, geom="boxplot", main = "Original Data") +
          theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2])

                ## Plot One Shuffle 
     
        shuffle <- sample(c1q1$data[,1])
        c1q1Test$shuffles <- as.matrix(shuffle, ncol = 1)
        c1q1Test$diff <- -diff(tapply(DF$y, shuffle, mean))
        c1q1Test$colors <- blu
        ### stores samples as columns
        
        DF$group2 <- shuffle
        #print(DF)
        plot2 <- qplot(y=y, x=group2, data = DF, geom="boxplot", main = "Shuffled Sample") +
          theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2])
        
        
#         w2 <- unlist(tapply(DF$y, list(z, DF$group2), function(x) 1:length(x)))
#         tempDF <- data.frame(DF, w=w2[!is.na(w2)])
#         plot2 <- qplot(data = tempDF, x = y, y = w, colour = I(blu), size = I(4), main = "Shuffled Data") + 
#                     theme_bw() + xlab(c1q1$names[2]) + ylab("Count") + facet_wrap( ~ group2)
         grid.arrange(plot1, plot2, heights = c(3, 3)/6, ncol=1)
      }, height = 360, width = 320)
      
      output$c1q1_TestPrep2 <- renderTable({
        if(is.null(c1q1$data))  return()
        #print(c1q1$data)
        DF <- data.frame(mean = tapply(c1q1$data[,2], c1q1$data[,1], mean, na.rm = TRUE ),
                    sd = tapply(c1q1$data[,2], c1q1$data[,1], sd, na.rm = TRUE ),
                    n = as.integer(tapply(c1q1$data[,2], c1q1$data[,1], length)))
        rownames(DF) <- levels(c1q1$data[,1])
        DF
      })
      
      
      output$c1q1_TestTable1 <- renderTable({
        if( is.null(c1q1$data))  return()
        DF <- data.frame(mean = tapply(c1q1$data[, 2], c1q1Test$shuffles[, 1], mean, na.rm = TRUE ),
                    sd = tapply(c1q1$data[, 2], c1q1Test$shuffles[, 1], sd, na.rm = TRUE ),
                    n = tapply(c1q1$data[, 2], c1q1Test$shuffles[, 1], length))
        rownames(DF) <- levels(c1q1$data[,1])
        DF
      })
      
      observeEvent(input$c1q1_test_shuffle_10, {
        c1q1Test$moreExtremeCount <- NULL
        newShuffles <- sapply(1:10, function(x) sample(c1q1$data[,1]) )
        c1q1Test$shuffles <- cbind(c1q1Test$shuffles, newShuffles)
        c1q1Test$diff <- c(c1q1Test$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[,2], x, mean, na.rm=TRUE))))
        #print(c1q1Test$diff)
        c1q1Test$colors <- rep(blu, length(c1q1Test$diff))
      })
      
      observeEvent(input$c1q1_test_shuffle_100, {
        c1q1Test$moreExtremeCount <- NULL
        newShuffles <- sapply(1:100, function(x) sample(c1q1$data[,1]) )
        c1q1Test$shuffles <- cbind(c1q1Test$shuffles, newShuffles)
        c1q1Test$diff <- c(c1q1Test$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[, 2], x, mean, na.rm=TRUE))))
        #print(c1q1Test$diff)
        c1q1Test$colors <- rep(blu, length(c1q1Test$diff))
      })
      observeEvent(input$c1q1_test_shuffle_1000, {        
        c1q1Test$moreExtremeCount <- NULL
        newShuffles <- sapply(1:1000, function(x) sample(c1q1$data[,1]) )
        c1q1Test$shuffles <- cbind(c1q1Test$shuffles, newShuffles)
        c1q1Test$diff <- c(c1q1Test$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[, 2], x, mean, na.rm=TRUE))))
        #print(c1q1Test$diff)
        c1q1Test$colors <- rep(blu, length(c1q1Test$diff))
      })
      observeEvent(input$c1q1_test_shuffle_5000, {
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
        if(is.null(c1q1Test$diff)) return() 
        
        parm <- sort(as.matrix(c1q1Test$diff))
        # print(parm)
        if(length(parm) == 1){
          y <- 0.5
          radius <- 4
        } else {
          nbreaks <- nclass.Sturges(parm)^2
          z <- cut(parm, breaks = nbreaks)
          y <- unlist(tapply(z, z, function(V) 1:length(V)))
          y <- y[!is.na(y)]
          #print(y)
          #print(max(w))
          nsims <- length(parm)
          radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
        }
        plot(parm, y, ylim = c(0.5, max(y)), ylab = "", cex = radius/2, pch = 16, col = c1q1Test$colors,  
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
                 br(),
                 br(),
                 tableOutput("c1q1_EstPrep2"),
                 h5(paste("Original Difference in means = ", 
                          round(-diff(tapply(c1q1$data[, 2], c1q1$data[, 1], mean, na.rm=TRUE)), 3))),
                 
                 br(),
                 br(),
                 tableOutput("c1q1_EstTable1"),
                 h5(paste("Resampled difference in means = ", round(as.numeric(c1q1Est$diff[1]),3)))
          ),
          column(5,
                uiOutput('c1q1_ReSampDistPlot')
          )
          ),
          uiOutput('c1q1Shuffles'),
        br(),
        br(),
          uiOutput('c1q1_CI')          
      )
      
  }
  })

output$c1q1_dataPlot1 <- renderUI({ 
  plotOutput('c1q1_EstPlot1') #, click = 'c1q1_Est_click')
})

  
output$c1q1_CI <- renderUI({ 
 div(
   fluidRow(
  column(3, offset = 3, 
         h5("Select Confidence Level (%)")
  ),
  column(6,
         fluidRow(
           column(2,  actionButton('c1q1_conf80', label = "80")),
           column(2,  actionButton('c1q1_conf90', label = "90")),
           column(2,  actionButton('c1q1_conf95', label = "95")),
           column(2,  actionButton('c1q1_conf99', label = "99"))
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
  column(4, offset = 1, 
         h4("How many more shuffles?")),
  column(1, actionButton("c1q1_Est_shuffle_10", label = "10")),
  column(1, actionButton("c1q1_Est_shuffle_100", label = "100")),
  column(1, actionButton("c1q1_Est_shuffle_1000", label = "1000")),
  column(1, actionButton("c1q1_Est_shuffle_5000", label = "5000"))
)
})

output$c1q1_ReSampDistPlot <- renderUI({ 
  plotOutput('c1q1_EstPlot2') #, click = 'c1q1_Est_click')
})


output$c1q1_EstPlot2 <- renderPlot({
  if(is.null(c1q1Est$diff)) return() 
  parm <- as.matrix(c1q1Est$diff)
  #print(parm)
  parm <- sort(parm)
  if(length(parm) == 1){
    y <- .5
    radius <- 4
  } else {
    nbreaks <- nclass.Sturges(parm)^2
    z <- cut(parm, breaks = nbreaks)
    y <- unlist(tapply(z, z, function(V) 1:length(V)))
    y <- y[!is.na(y)]
    #print(y)
    #print(max(w))
    nsims <- length(parm)
    radius = 2 + (nsims < 5000) + (nsims < 1000) + (nsims < 500) + (nsims < 100)         
  }
  plot(x = parm, y = y, ylim = c(0.5, max(y)), ylab = "", cex = radius/2, pch = 16, col = c1q1Est$colors,  
       xlab = expression(bar(x)[1] - bar(x)[2]), main = "Resampling Distribution")
  legend("topright", bty = "n", paste(length(parm), "points \n Mean = ", 
                                      round(mean(parm),3), "\n SE = ", round(sd(parm),3)))
}, width = 400)      

output$c1q1_EstPlot1 <- renderPlot({
  if(is.null(c1q1$diff))
     return()
  DF <- c1q1$data
  names(DF) <- c("group","y")
  DF[, 1] <- factor(DF[,1])
  #print(summary(DF))
  
  c1q1_plot1 <- qplot(y=y, x=group, data = DF, geom="boxplot", main = "Original Data") +
    theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2])
  
#   DF <- DF[order(DF$y), ]
#   nbreaks <- min(c(length(unique(DF$y)), floor(.5*nclass.Sturges(DF$y)^2)))
#   z <- cut(DF$y, breaks =  nbreaks )
#   w <- unlist(tapply(DF$y, list(z, DF$group), function(x) 1:length(x)))
#   w <- w[!is.na(w)]  
#   c1q1_plot2 <- qplot(data= DF, x=y, y=w , colour = I(blu), size = I(4), main = "Original Data")+ facet_wrap( ~group) + 
#     theme_bw() + ylab("Count") + xlab( c1q1$names[2])
  
  #mtext(side = 3, at = min(x)*2/3 + max(x)/3, bquote(diff == c1q1Test$observed))
  
  ## Plot One resample
  n1 <- length(c1q1Est$ndx1)
  n2 <- length(c1q1Est$ndx2)
  resample <-  c1q1Est$shuffles <- c1q1_estimate_shuffles(1, c1q1Est$ndx1, c1q1Est$ndx2)
  #print(c1q1Est$shuffles)
  DF2 <- c1q1$data[resample, ] 
  names(DF2) <- names(DF)
  DF2 <- DF2[order(DF2$y), ]
  c1q1Est$diff <- -diff(tapply(DF2$y, DF2$group, mean))
  c1q1Est$colors <- blu
  ### stores samples as columns
  #print(c1q1Est$shuffles)
  #z2 <- cut(DF2$y, breaks =  c(sapply(strsplit(substr(levels(z),2,20),","), function(str) as.numeric(str[1])), max(DF$y +1)) )
  #w2 <- unlist(tapply(DF2$y, list(z2, DF2$group), function(x) 1:length(x)))
  #tempDF2 <- data.frame(DF2, w=w2[!is.na(w2)])
  
  c1q1_plot3 <- qplot(y=y, x=group, data = DF2, geom="boxplot", main = "Resampled Data") +
                theme_bw() + xlab("") +  coord_flip() + ylab(c1q1$names[2])
#c1q1_plot3 <- qplot(data = DF2, x = y, y = w2, colour = I(blu), size = I(4), main = "Resampled Data") + 
  #  theme_bw() + xlab(c1q1$names[2]) + ylab("Count") + facet_wrap( ~ group)
  
  grid.arrange(c1q1_plot1, c1q1_plot3, heights = c( 3,3)/6, ncol=1)
}, height = 360)

output$c1q1_EstPrep2 <- renderTable({
  if( is.null(c1q1$data))  return()
  DF <- data.frame(mean = tapply(c1q1$data[,2], c1q1$data[, 1], mean, na.rm = TRUE ),
                   sd = tapply(c1q1$data[,2], c1q1$data[, 1], sd, na.rm = TRUE ),
                   n = as.integer(tapply(c1q1$data[,2], c1q1$data[, 1], length)))
  rownames(DF) <- levels(c1q1$data[,1])
  DF
})


output$c1q1_EstTable1 <- renderTable({
  if( is.null(c1q1$data)  )  return()
  resamp1 <- c1q1$data[c1q1Est$shuffles[,1], 2]
  #print(resamp1)
  DF <- data.frame(mean = tapply(resamp1, c1q1$data[, 1], mean, na.rm = TRUE ),
                   sd = tapply(resamp1, c1q1$data[, 1], sd, na.rm = TRUE ),
                   n = tapply(resamp1, c1q1$data[, 1], length))
  rownames(DF) <- levels(c1q1$data[,1])
  #print(DF)
  DF
})

observeEvent(input$c1q1_Est_shuffle_10, {
  c1q1Est$CI <- NULL
  newShuffles <- c1q1_estimate_shuffles(10, c1q1Est$ndx1, c1q1Est$ndx2)
  c1q1Est$shuffles <- cbind(c1q1Est$shuffles, newShuffles)
  c1q1Est$diff <- c(c1q1Est$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[x,2], c1q1$data[x,1], mean, na.rm=TRUE))))
  #print(c1q1Est$diff)
  c1q1Est$colors <- rep(blu, length(c1q1Est$diff))
})

observeEvent(input$c1q1_Est_shuffle_100, {
  c1q1Est$CI <- NULL
  newShuffles <- c1q1_estimate_shuffles(100, c1q1Est$ndx1, c1q1Est$ndx2)
  c1q1Est$shuffles <- cbind(c1q1Est$shuffles, newShuffles)
  c1q1Est$diff <- c(c1q1Est$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[x, 2],  c1q1$data[x,1], mean, na.rm=TRUE))))
  #print(c1q1Est$diff)
  c1q1Est$colors <- rep(blu, length(c1q1Est$diff))
})
observeEvent(input$c1q1_Est_shuffle_1000, {        
  c1q1Est$CI <- NULL
  newShuffles <- c1q1_estimate_shuffles(1000, c1q1Est$ndx1, c1q1Est$ndx2)
  c1q1Est$shuffles <- cbind(c1q1Est$shuffles, newShuffles)
  c1q1Est$diff <- c(c1q1Est$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[x, 2],  c1q1$data[x,1], mean, na.rm=TRUE))))
  #print(c1q1Est$diff)
  c1q1Est$colors <- rep(blu, length(c1q1Est$diff))
})
observeEvent(input$c1q1_Est_shuffle_5000, {
  c1q1Est$CI <- NULL
  newShuffles <- c1q1_estimate_shuffles(5000, c1q1Est$ndx1, c1q1Est$ndx2)
  c1q1Est$shuffles <- cbind(c1q1Est$shuffles, newShuffles)
  c1q1Est$diff <- c(c1q1Est$diff, apply(newShuffles, 2, function(x) -diff(tapply(c1q1$data[x, 2],  c1q1$data[x,1], mean, na.rm=TRUE))))
  #print(c1q1Est$diff)
  c1q1Est$colors <- rep(blu, length(c1q1Est$diff))
})

observeEvent(input$c1q1_conf80,{
  if(is.null(c1q1$diff)) {
    return()
  }
  nsims <- length(c1q1Est$diff)
  c1q1Est$confLevel <- .80
  c1q1Est$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .1)
  c1q1Est$colors[1:tailCount] <- rd
  c1q1Est$colors[nsims +1 -(1:tailCount)] <- rd
  c1q1Est$CI <- sort(c1q1Est$diff)[c(tailCount, nsims + 1 - tailCount)]
})


observeEvent(input$c1q1_conf90,{
  if(is.null(c1q1$diff)) {
    return()
  }
  nsims <- length(c1q1Est$diff)
  c1q1Est$confLevel <- .90
  c1q1Est$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .05)
  c1q1Est$colors[1:tailCount] <- rd
  c1q1Est$colors[nsims +1 -(1:tailCount)] <- rd
  c1q1Est$CI <- sort(c1q1Est$diff)[c(tailCount, nsims + 1 - tailCount)]
})

observeEvent(input$c1q1_conf95,{
  if(is.null(c1q1$diff)) {
    return()
  }
  nsims <- length(c1q1Est$diff)
  c1q1Est$confLevel <- .95
  c1q1Est$colors <- rep(blu, nsims)
  tailCount <- floor(nsims * .025)
  c1q1Est$colors[1:tailCount] <- rd
  c1q1Est$colors[nsims +1 -(1:tailCount)] <- rd
  c1q1Est$CI <- sort(c1q1Est$diff)[c(tailCount, nsims + 1 - tailCount)]
})

observeEvent(input$c1q1_conf99,{
  if(is.null(c1q1$diff)) {
    return()
  }
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
  if(is.null(c1q1_tProb$findP))
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

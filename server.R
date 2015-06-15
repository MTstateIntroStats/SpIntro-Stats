includeScript("www/helper.js")
source("helpers.R")

quant1_contents <- load("data/quant1.RData")
quant2_contents <- load("data/quant2.RData")
c1q1_contents <- load("data/cat1quant1.RData")

for(ff in system("ls data/*.RData", intern=T)) load(ff)

 ##  These were created to hold sample data with:
 ##  save(birthWeights, REDvsCntrl, REDvsREDA, REDAvsCntrl, file = "data/cat1quant1.RData")
 ##  save(birthweights, geyser2011, file="data/quant1.RData")
 ##  save(shuttle, womenRateMen, menRateWomen, file = "data/quant2.RData")

## setup transparent colors
grn <- rgb(0, 1, 0, alpha=.4)
rd  <- rgb(1, 0, 0, alpha=.5)
blu <- rgb(0, 0, 1, alpha=.4)

shinyServer(function(input, output, session) {

##  code from https://github.com/jrowen/rhandsontable/blob/master/inst/examples/shiny.R
#   output$dataTable <- renderRHandsontable({
#     if (is.null(input$dataTable)) {
#       DF = data.frame(val = 1:10, bool = TRUE, nm = LETTERS[1:10],
#                       dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#                       stringsAsFactors = F)
#     } else {
#       DF = hot_to_r(input$dataTable)
#     }
#     rhandsontable(DF, useTypes = as.logical(input$useType))
#   })
#   

  ## 1 Categorical  -----------------------------------------------------------
 
    ##  Using Submit Button to keep plots from changing too soon
  cat1_data <- reactive({
      data.frame(#run = input$submitButton,
                counts = as.numeric(c(input$cat1_n1, input$cat1_n2)),
                names = c(input$cat1_name1, input$cat1_name2)
      )
  })
  
  ## Descriptives:  plot a bar chart of the successes / failures
  
  output$cat1_Plot <- renderPlot( {
    if(input$cat1_submitButton ==0) return()
    isolate( { 
      cat1_dataDF <- cat1_data() 
      ## print(dataDF)
      ## phat1 <- cat1_dataDF$counts[1]/sum(cat1_dataDF$counts)
      countTable <- as.table(matrix(cat1_dataDF$counts, 2, 1))
      rownames(countTable) =  cat1_dataDF$names
       ## make plot
       par(mar=c(24, 40, 10, 35)/10)
       barplot(prop.table(countTable), ylab = "Proportion", main = "",xaxt="n")
     })
  }, height=120)

  output$cat1DataIn <- renderText({
  if(input$cat1_submitButton ==0) return()
  "Data is entered, you may now choose to estimate or test one proportion."
})


  output$cat1_Summary <- renderTable({
    if(input$cat1_submitButton ==0) return()
    isolate({
      cat1_dataDF <- cat1_data()
      counts <- as.table( matrix(cat1_dataDF$counts), 1, 2)
      dimnames(counts) = list(cat1_dataDF$names,"Proportions")
      prop.table(counts)
    })
  })
  
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
    text(x = max(phatDF$UB) * .85, y = 0.08 * nsims, paste( sum(phatDF$UB < input$CIdemo_p), "too low"))
    text(x = min(phatDF$UB) * 0.4 + .1, y= .93 * nsims, paste( sum(phatDF$LB > input$CIdemo_p), "too high"))     
  })
  if(!is.null(input$CIplot1_hover)){
    myY <- subset(phatDF, abs(phatDF$phat - input$CIplot1_hover$x) < .005)[round(input$CIplot1_hover$y),]
    if(!is.null(myY) & length(myY) > 1){
      points(x=myY$phat, y = myY$row, cex=2, col = "blue")
      segments(myY$LB, myY$row, myY$UB, myY$row, lwd=4, col = "blue")
    }
  }
  
}, height = 275)

  ## Normal probability computations
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

  
  ## 1 Quantitative -----------------------------------------------------------  -- 1 Quant 
 output$quant1DataIn <- renderText({ "How would you like to input the data? " 
  })

 ## user selects an input method.
  ## renderUI changes to get appropriate inputs.

 output$q1_ui <- renderUI({
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
   q1$names <- names(DF)
   output$quant1DataIn <- renderText({
          "Data is entered, you may now choose to estimate or test one mean"
   })
 })

 observeEvent(  input$q1_useCSVBtn,{
   DF <-  read.csv(input$q1_file1$datapath, header=input$q1_header, sep=input$q1_sep, quote=input$q1_quote)
   q1$names <- if(is.null(names(DF)) | "V1" %in% names(DF)){ "x"} else {names(DF)[1]}
   q1$data <- data.frame(DF[, 1])
   output$quant1DataIn <- renderText({
     "Data is entered, you may now choose to estimate or test one mean"
   })
 })

 observeEvent(input$q1_useHotBtn,{
   DF = data.frame(x=as.numeric(q1_values[["hot"]][,2]))
   # print(DF)
   q1$names <- names(DF)
   q1$data <- data.frame( x = as.numeric(unlist(DF)))
   output$quant1DataIn <- renderText({
     "Data is entered, you may now choose to estimate or test one mean"
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
    #})
  })

 # output$boot_demo <- renderUI(
#     includeScript("www/d3.v3.min.js"),
#     includeScript("www/costs.js"),
#     includeHTML("www/BootDemo.html")
 #   )

  ##  t dist'n option --------------------------------------------


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


  ## 2 Categorical ------------------------------------------------------------- 2 categ

  ## Descriptives:  plot a bar chart of the successes / failures
 cat2_data <- reactive({
    data.frame(
     counts = as.numeric(c(input$cat2_n11, input$cat2_n12, input$cat2_n21, input$cat2_n22)),
     names  = rep(c(input$cat2_name1, input$cat2_name2), 2),
     groups = rep(c(input$cat2_grp1, input$cat2_grp2), each = 2)
   )
 })

 output$cat2Summary <- renderTable({ 
  if(input$cat2_submitButton == 0) return()
  isolate({
    cat2_dataDF <- cat2_data()
    #print(cat2_dataDF)
    counts <- as.table( matrix(cat2_dataDF$counts, 2, 2))
    #print(counts)
    colnames(counts) <- cat2_dataDF$names[1:2]
    rownames(counts) <- cat2_dataDF$groups[c(1,3)]
    round(t(prop.table(counts, 1)),3)
  })
 })
  

 output$cat2Plot <- renderPlot( {
    if(input$cat2_submitButton ==0) return()
    isolate( { 
      cat2_dataDF <- cat2_data()
      #print(cat2_dataDF)
      counts <- as.table( matrix(cat2_dataDF$counts, 2, 2))
      colnames(counts) <- cat2_dataDF$names[1:2]
      rownames(counts) <- cat2_dataDF$groups[c(1,3)]
      props <- t(prop.table(counts, 1))
      #print(props)
      ## make plot
      par(mar=c(24, 40, 10, 35)/10)
      barplot(props, ylab = "Proportion", main = "")
    })
  }, height=180)


  output$cat2DataIn <- renderText({
    if(input$cat2_submitButton ==0) return()
    "Data is entered, you may now choose to estimate or test the difference in two proportions."
  })
  
  output$cat2OriginalData <- renderTable({ 
    if(input$cat2_submitButton ==0) return()
    isolate({
      cat2_dataDF <- cat2_data()
      counts <- as.table( matrix(cat2_dataDF$counts, 2, 2))
      colnames(counts) <- cat2_dataDF$names[1:2]
      rownames(counts) <- cat2_dataDF$groups[c(1,3)]
      round(t(prop.table(counts, 1)), 3)
    })
  })
  
  cat2 <- reactiveValues(data=NULL, names=NULL)
  
#   observeEvent(input$shuffles, {
#     isolate({
#       cat2_dataDF <- cat2_data()
#       counts <- as.table( matrix(cat2_dataDF$counts, 2, 2))
#       y1 <- counts[1,1]
#       n1 <- counts[1,1] + counts[2,1]
#       y2 <- counts[1,2]
#       n2 <- counts[1,2] + counts[2,2]
#       phat_m <- (y1 + y2)/(n1 + n2)
#     })
#     DF <- generate_shuffles(input$shuffles, phat_m = phat_m,
#                             y1=y1, y2=y2, n1=n1, n2=n2)
#     cat2$data <- rbind(cat2$data, DF)
#   })
#   
#   head(cat2$data)

  output$cat2Test <- renderPlot({
    if(input$cat2_submitButton == 0) return()
    ##  Make plot
      #x <- sort(cat2$data[,1])
     }, height=360)


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

# 
# 
# 
#   output$normalProbPlot2 <-    renderPlot({ 
#   par(mar=c(24,1,1,1)/10)
#   z <- absz <- prob <- yrr <- xrr <- NA
#   x <- -300:300 / 50
#   
# 
#   ## convert text to numeric  
#   prob <- as.numeric(input$cat2_prob_txt)
#   z <- as.numeric(input$cat2_z_txt)
#   
#   ##  this loop should run when user changes prob
#   if(!is.na(prob) ){
#     ## given prob, find z
#     if(input$cat2_area == "Lower"){ ##  left tail
#       z <-  qnorm(prob)
#       if(z < min(x))  x <- c(1.01 * z, x)
#       ## rejection region in x dimension
#       xrr <- c(x[x < z], z, z)
#       ## density curve over xrr:
#       yrr <- c( dnorm(xrr[-length(xrr)]), 0)
#     } else if(input$cat2_area == "Upper"){   ## right tail        
#       z <- qnorm(1 - prob) 
#       if(z > max(x))  x <- c(x, 1.01 * z)
#       xrr <- c(z, z, x[x > z])
#       yrr <- c(0, dnorm(xrr[-1]))
#     } else if(input$cat2_area == "Center"){
#       z <- abs(qnorm( (1 - prob)/2) )
#       if(z < min(x))  x <- c(1.01 * z, x)       
#       if(z > max(x))  x <- c(x, 1.01 * z)
#       xrr <- seq(-z, z, length=100)
#       yrr <- c(0, dnorm(xrr[2:99]), 0)
#     } else if(input$cat2_area == "Extremes"){
#       z <- abs(qnorm(1-prob) )
#       if(z < min(x))  x <- c(1.01 * z, x)       
#       if(z > max(x))  x <- c(x, 1.01 * z)
#       xrr <- c(-z, x[x < -z], -z)
#       yrr <- c(0,  dnorm(xrr[-1]))
#     }
#     absz <- abs(z)
#     
#   } else if(!is.na(z)){
#     ##  find probability
#     absz <- abs(z)
#     maxX <- pmax(z, 6)
#     minX <- pmin(z, -6)
#     x <- seq(minX, maxX, length=200)
#     prob <-  1 - pnorm(z) 
#     xrr <- c(z, z, x[x > z])  ## right tail
#     yrr <- c(0,  dnorm(xrr[-1]) )
#     if(input$cat2_area == "Lower"){         ##  left tail
#       prob =  pnorm(z) 
#       xrr <- c(z, x[x < z], z)
#       yrr <- c(0,  dnorm(xrr[-1]))
#     } else if (input$cat2_area == "Extremes"){ ##  extremes
#       xrr <- c( -absz, x[x < -absz], -absz)
#       yrr <- c(0, dnorm(xrr[-1]))
#       prob = pnorm(-absz) 
#       #yrr <- c(yrr, NA,NA, rev(yrr))
#       #xrr <- c(xrr, NA,NA, rev(-xrr))
#     } else if (input$cat2_area == "Center"){   ##  center
#       xrr <- seq(-absz, absz, length=100)
#       yrr <- c(0, dnorm(xrr), 0)
#       xrr <- c(-absz, xrr, absz)
#       prob <- diff( pnorm(c(-absz,absz)) )
#     }
#   } else {
#     ## do nothing.  Wait for user to supply prob or z.
#   }
#   plot(x, dnorm(x), type = "l", bty='n', xlab="Z Score", ylab="", yaxt="n")
#   abline(h=0)
#   max.height <- dnorm(0) *.9
#   text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
#   segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
#   
#   if(input$cat2_area == "Extremes") {  ## extremes
#     polygon(xrr, yrr, col = rd)
#     polygon(-xrr, yrr, col = rd)
#     segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
#     text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
#          round(prob, 3), col = "darkblue")
#     place.x <- c(-absz, absz)
#     if(absz < 1) place.x <- place.x/absz * .8
#     text(place.x, y = text.height, round(c(-absz,absz),3))
#   } else if (input$cat2_area == "Center") {   ## fill & label center
#     polygon(xrr, yrr, col = grn)
#     text(x=0, y= text.height, round(prob,3))
#     segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
#     place.x <- c(-absz, absz)
#     if(absz < 1) place.x <- place.x/absz * .8
#     text(place.x, y=text.height, round(c(-absz,absz),3))
#   } else {          ## show tails
#     polygon(xrr, yrr, col = rd)
#     text( z, y = text.height, round(z, 3))
#     text(x= sign(z) * (absz+4) / 2 , y= max(yrr) / 2 + 0.02, 
#          round(prob,3), col = "darkblue")
#   }
#   
# }, height=300)

  ## 2 Quantitative -----------------------------------------------------------  2 quant

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

##  grab data according to input method
q2 <- reactiveValues(data = NULL, names = NULL)

observeEvent(  input$q2_useLddBtn, {
  DF <- eval(parse( text = input$q2_data1))
  q2$names <- names(DF) 
  q2$data <- data.frame(DF)
  output$quant2DataIn <- renderText({
    "Data is entered, you may now choose to estimate or test the true slope or correlation"
  })
})

observeEvent(  input$q2_useCSVBtn,{
  DF <- read.csv(input$q2_file1$datapath, header=input$q2_header,
                      sep=input$q2_sep, quote=input$q2_quote)
  q2$names <- names(DF)
  q2$data <- data.frame(DF)
  output$quant2DataIn <- renderText({
    "Data is entered, you may now choose to estimate or test the true slope or correlation"
  })
  
})

observeEvent(  input$q2_useHotBtn,{
  DF <- data.frame(q2_values[["hot"]])
  # print(DF)
  q2$names <- names(DF) 
  q2$data <- data.frame(DF)
  output$quant2DataIn <- renderText({
    "Data is entered, you may now choose to estimate or test the true slope or correlation"
  })
})

observeEvent(  input$q2_swapXwithY,{
  if(is.null(q2$data))
    return()
  q2$names <- q2$names[2:1]
  q2$data <- q2$data[, 2:1]
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

output$q2_Plot <- renderPlot( {
  if( is.null(q2$data)) 
    return()
  #isolate( { 
    ## need to allow user to switch predictor and response
    ## make plot
    DF <- q2$data
    names(DF) <- c("x","y")
    q2_plot1 <- ggplot(data = DF) +geom_boxplot(aes(y= x, x = x)) +
      theme_bw() + ylab(q2$names[1]) + xlab("") + scale_x_continuous(breaks = c(-1,1000)) +  coord_flip()
    q2_plot2 <- ggplot(data = DF) +geom_boxplot(aes(y= y, x = y)) +
    theme_bw() + ylab(q2$names[2]) + xlab("") + scale_x_continuous(breaks = c(-1,1000)) +  coord_flip()
    #par(mar=c(24, 40, 10, 35)/10, mfrow=c(2,1))
    #boxplot(q2_dataDF, horizontal = TRUE, main = "")
    myBlue <- rgb(0, 100/256, 224/256, alpha = .8)  
    q2_plot3 <- qplot(data= DF, x=x, y=y, colour = I(myBlue), size = I(4)) + theme_bw() +
                  xlab(q2$names[1]) + ylab(q2$names[2])
    grid.arrange(q2_plot1, q2_plot2, q2_plot3, heights = c(1, 1, 3)/5, ncol=1)
  #})
}, height=400)

output$q2_Summary <- renderTable({
  if( is.null(q2$data))  
    #if(input$q2_useHotBtn == 0 && input$q2_useExistingBtn == 0 && input$q2_useFileBtn == 0) 
    return()
  #isolate({
  DF0 <- q2$data
  names(DF0) <- c("x","y")
    ##  Allow switch in predictor/response
    DF <- rbind(mean   = apply(q2$data, 2, mean, na.rm = TRUE ),
                sd     = apply(q2$data, 2, sd, na.rm = TRUE),
                min    = apply(q2$data, 2, min),
                Q1     = apply(q2$data, 2, quantile, .25),
                median = apply(q2$data, 2, median),
                Q3     = apply(q2$data, 2, quantile, .75),
                max    = apply(q2$data, 2, max),
                length = apply(q2$data, 2, length),
                correlation = c(cor(q2$data[,1], q2$data[,2]), NA),
                beta.hat = round(coef(lm(y ~ x, data = DF0)),3))
    colnames(DF) <- q2$names
    DF
  #})
})

  ## 1 categorical & 1 quantitative   ---------------------------------------  1 cat 1 quant

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
    "Data is entered, you may now choose to estimate or test the true difference in means"
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
    "Data is entered, you may now choose to estimate or test the true difference in means"
  })
  
})

observeEvent(  input$c1q1_useHotBtn,{
  DF <- data.frame(c1q1_values[["hot"]])
  # print(DF)
  names(DF) <- c("group","y")
  c1q1$names <- names(DF)
  c1q1$data <- DF
  output$c1q1DataIn <- renderText({
    "Data is entered, you may now choose to estimate or test the true difference in means"
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
  tempDF <- c1q1$data
  names(tempDF) <- c("group","y")
  tempDF$group <- factor(tempDF$group)
  # print(summary(tempDF))
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
    val <- round( diff(tapply(c1q1$data[, 2], c1q1$data[, 1], mean, na.rm=TRUE)), 3)
    names(val) <- NULL
  matrix( -val, ncol= 1, dimnames = list("Difference in Means", c1q1$names[2]))
})

  ## Plot t distributions -----------------------------------------

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


  ##  Other Tools -----------------------------------------------------------
  ## Probability plot for t & normal distributions  -------------------------


prb_tProb <- reactiveValues(prob = NULL, z = NULL, findP = NULL)

observeEvent( input$prb_z_txt, {
  prb_tProb$z <- as.numeric(input$prb_z_txt) 
  prb_tProb$findP <- TRUE
})

observeEvent( input$prb_prob_txt,{
  prb_tProb$prob <- as.numeric(input$prb_prob_txt) 
  prb_tProb$findP  <- FALSE
})



  output$probPlot <-    renderPlot({ 
    par(mar=c(24,1,1,1)/10)
    z <- absz <- prob <- yrr <- xrr <- NA
    x <- -300:300 / 50
    df <- input$prb_df
   
    ##  this loop should run when user changes prob
    if(!prb_tProb$findP & !is.na(prb_tProb$prob)){
      prob <- prb_tProb$prob
      ## given prob, find z
      if(input$prb_area == "Lower"){ ##  left tail
        z <- if(input$prb_dist=='Normal') qnorm(prob)else qt(prob, df)
        if(z < min(x))  x <- c(1.01 * z, x)
        ## rejection region in x dimension
        xrr <- c(x[x < z], z, z)
        ## density curve over xrr:
        yrr <- c( if(input$prb_dist=='Normal') dnorm(xrr[-length(xrr)]) else 
          dt(xrr[-length(xrr)], df = df), 0)
      } else if(input$prb_area == "Upper"){   ## right tail        
        z <- if(input$prb_dist=='Normal') qnorm(1 - prob) else qt(1-prob, df)
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- c(z, z, x[x > z])
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      } else if(input$prb_area == "Center"){
        z <- abs(if(input$prb_dist=='Normal') qnorm( (1 - prob)/2) else qt((1-prob)/2,df))
        if(z < min(x))  x <- c(1.01 * z, x)       
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- seq(-z, z, length=100)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[2:99]) else dt(xrr[2:99],df), 0)
      } else if(input$prb_area == "Extremes"){
        z <- abs(if(input$prb_dist=='Normal') qnorm(1-prob/2) else qt(1-prob/2,df))
        if(z < min(x))  x <- c(1.01 * z, x)       
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- c(-z, x[x < -z], -z)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      }
      absz <- abs(z)
      
    } 
    if(prb_tProb$findP & !is.na(prb_tProb$z)){
      z <- prb_tProb$z
      ##  find probability
      absz <- abs(z)
      maxX <- pmax(z, 6)
      minX <- pmin(z, -6)
      x <- seq(minX, maxX, length=200)
      prob <-  1 - if(input$prb_dist=='Normal') pnorm(z) else pt(z, df)
      xrr <- c(z, z, x[x > z])  ## right tail
      yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1], df))
      if(input$prb_area == "Lower"){         ##  left tail
        prob =  if(input$prb_dist=='Normal') pnorm(z) else pt(z, df)
        xrr <- c(z, x[x < z], z)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      } else if (input$prb_area == "Extremes"){ ##  extremes
        xrr <- c( -absz, x[x < -absz], -absz)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1], df))
        prob = if(input$prb_dist=='Normal') pnorm(-absz) else pt(-absz, df)
        #yrr <- c(yrr, NA,NA, rev(yrr))
        #xrr <- c(xrr, NA,NA, rev(-xrr))
      } else if (input$prb_area == "Center"){   ##  center
        xrr <- seq(-absz, absz, length=100)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr) else dt(xrr, df), 0)
        xrr <- c(-absz, xrr, absz)
        prob <- diff(if(input$prb_dist=='Normal') pnorm(c(-absz,absz)) else pt(c(-absz,absz), df))
      }
    } 
    plot(x, if(input$prb_dist=='Normal') dnorm(x) else dt(x, df), type = "l", bty='n', xlab="Z Score",
         ylab="", yaxt="n")
    abline(h=0)
    max.height <- ( if(input$prb_dist=='Normal') dnorm(0) else dt(0, df) ) *.9
    text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
    segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
    
    if(input$prb_area == "Extremes") {  ## extremes
      polygon(xrr, yrr, col = rd)
      polygon(-xrr, yrr, col = rd)
      segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
      text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
           round(prob * ifelse(prb_tProb$findP,1,.5), 3), col = "darkblue")
      place.x <- c(-absz, absz)
      if(absz < 1) place.x <- place.x/absz * .8
      text(place.x, y = text.height, round(c(-absz,absz),3))
    } else if (input$prb_area == "Center") {   ## fill & label center
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
  
  ## End of code for z-t probabilities and quantiles. ------------------------

  ##  Start of code for Power web app  ---------------------------------------
  ## Reactive expression to create a data frame containing all of the values for POWER
 sliderValues <- reactive({
  # Create output
   data.frame(
     Setting = c("Sample Size", 
                "Standard Deviation",
                "Shift in Mean"),
     Value = as.character(c(input$pwr_n, 
                           input$pwr_sd,
                           input$pwr_altMean)),
     Name = c( "Significance Level (alpha)",
              "Effect Size",  "Power"
     ),
     Output =as.character(c(input$pwr_alpha, 
                           round(input$pwr_altMean/input$pwr_sd, 3),
                           round(power.t.test(n=input$pwr_n, 
                                              delta=input$pwr_altMean,
                                              sd=input$pwr_sd, 
                                              sig.level=input$pwr_alpha,
                                              type="one.sample",
                                              alternative="one")$power,
                             3))), 
     stringsAsFactors=FALSE)
  })

  ## set colors with transparency
 grn <- rgb(0, 1, 0, alpha=.4)
 rd <- rgb(1, 0, 0, alpha=.5)

## Plot of power to send to window
output$powerPlot <- renderPlot({
  x <- seq(-4, 10, length=200)
  plot(x, dt(x, input$pwr_n-1),bty='l', type="l", xlab="", ylab="")
  lines(x,dt(x, input$pwr_n-1, ncp=input$pwr_altMean/input$pwr_sd *sqrt(input$pwr_n)))
  abline(h=0)
  qt1 <- qt(1-input$pwr_alpha, input$pwr_n-1)
  ## rejection region in x and y (density)
  xrr <- c(qt1, qt1, x[x>=qt1], max(x))
  yrr <- c(0, dt(c(qt1,  x[x>=qt1]), input$pwr_n -1), 0)
  
  polygon(xrr, yrr, col = rd)
  abline(v = c(0, qt1))
  
  xpwr <- c(qt1, x[x>=qt1])
  ypwr <- c(0, dt(xpwr, df=input$pwr_n-1, ncp=input$pwr_altMean/input$pwr_sd *
                    sqrt(input$pwr_n) ), 0)
  xpwr <- c(qt1, xpwr, max(x))
  ## add alternative density
  polygon(xpwr, ypwr, col=grn)
  ## add alpha in center of rejection region
  text(weighted.mean(xrr, w=yrr^2), weighted.mean(yrr,w=yrr^.5), cex=1.5, 
       expression(alpha))
  ## add 'Power' in center of its region
  text(weighted.mean(xpwr, w=ypwr^4), weighted.mean(ypwr, w=ypwr^.5), cex=1.5, "Power")
  mtext(side=1, at=0, line=2, expression(H[0]: mu == 0))
  mtext(side=1, at= xpwr[which.max(ypwr)], line=2, expression(H[a]: mu > 0))
})

# Show the power values using an HTML table
output$values <- renderTable({
  sliderValues()
})    

 ## End of power code ---------------------------------------------------------
})


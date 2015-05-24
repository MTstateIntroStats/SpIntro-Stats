includeScript("www/helper.js")

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

  output$cat1Plot <- renderPlot( {
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


  output$cat1Summary <- renderTable({
    if(input$cat1_submitButton ==0) return()
    isolate({
      cat1_dataDF <- cat1_data()
      counts <- as.table( matrix(cat1_dataDF$counts), 1, 2)
      dimnames(counts) = list(cat1_dataDF$names,"Proportions")
      prop.table(counts)
    })
  })
  
  ## Descriptives:  plot a bar chart of the successes / failures
  
  output$normalProbPlot1 <-    renderPlot({ 
  par(mar=c(24,1,1,1)/10)
  z <- absz <- prob <- yrr <- xrr <- NA
  x <- -300:300 / 50
    
  ## setup transparent colors
  grn <- rgb(0, 1, 0, alpha=.4)
  rd  <- rgb(1, 0, 0, alpha=.5)
  blu <- rgb(0, 0, 1, alpha=.4)
  
  ## convert text to numeric  
  prob <- as.numeric(input$cat1_prob_txt)
  z <- as.numeric(input$cat1_z_txt)
  
  ##  this loop should run when user changes prob
  if(!is.na(prob) ){
    ## given prob, find z
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
      z <- abs(qnorm(1-prob) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(-z, x[x < -z], -z)
      yrr <- c(0,  dnorm(xrr[-1]))
    }
    absz <- abs(z)
    
  } else if(!is.na(z)){
    ##  find probability
    absz <- abs(z)
    maxX <- pmax(z, 6)
    minX <- pmin(z, -6)
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
      xrr <- seq(-absz, absz, length=100)
      yrr <- c(0, dnorm(xrr), 0)
      xrr <- c(-absz, xrr, absz)
      prob <- diff( pnorm(c(-absz,absz)) )
    }
  } else {
    ## do nothing.  Wait for user to supply prob or z.
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
         round(prob, 3), col = "darkblue")
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

  output$q1_dataDF <- renderTable({
  
    inFile <- input$q1_file1
  
    if (is.null(inFile))
      return(NULL)
  
    read.csv(inFile$datapath, header=input$q1_header, sep=input$q1_sep, quote=input$q1_quote)
  })

  ##  t dist'n option --
  output$tProbPlot1 <-    renderPlot({ 
  par(mar=c(24,1,1,1)/10)
  z <- absz <- prob <- yrr <- xrr <- NA
  x <- -300:300 / 50
  
  ## setup transparent colors
  grn <- rgb(0, 1, 0, alpha=.4)
  rd  <- rgb(1, 0, 0, alpha=.5)
  blu <- rgb(0, 0, 1, alpha=.4)
  
  ## convert text to numeric  
  prob <- as.numeric(input$quant1_prob_txt)
  z <- as.numeric(input$quant1_z_txt)
  df <- as.numeric(input$quant1_df)
  
  ##  this loop should run when user changes prob
  if(!is.na(prob) ){
    ## given prob, find z
    if(input$quant1_area == "Lower"){ ##  left tail
      z <-  qt(prob, df)
      if(z < min(x))  x <- c(1.01 * z, x)
      ## rejection region in x dimension
      xrr <- c(x[x < z], z, z)
      ## density curve over xrr:
      yrr <- c( dt(xrr[-length(xrr)], df), 0)
    } else if(input$quant1_area == "Upper"){   ## right tail        
      z <- qt(1 - prob, df) 
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(z, z, x[x > z])
      yrr <- c(0, dt(xrr[-1], df))
    } else if(input$quant1_area == "Center"){
      z <- abs(qt( (1 - prob)/2, df) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- seq(-z, z, length=100)
      yrr <- c(0, dt(xrr[2:99], df), 0)
    } else if(input$quant1_area == "Extremes"){
      z <- abs(qt(1-prob, df) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(-z, x[x < -z], -z)
      yrr <- c(0,  dt(xrr[-1], df))
    }
    absz <- abs(z)
    
  } else if(!is.na(z)){
    ##  find probability
    absz <- abs(z)
    maxX <- pmax(z, 6)
    minX <- pmin(z, -6)
    x <- seq(minX, maxX, length=200)
    prob <-  1 - pt(z, df) 
    xrr <- c(z, z, x[x > z])  ## right tail
    yrr <- c(0,  dt(xrr[-1], df) )
    if(input$quant1_area == "Lower"){         ##  left tail
      prob =  pt(z, df) 
      xrr <- c(z, x[x < z], z)
      yrr <- c(0,  dt(xrr[-1], df))
    } else if (input$quant1_area == "Extremes"){ ##  extremes
      xrr <- c( -absz, x[x < -absz], -absz)
      yrr <- c(0, dt(xrr[-1], df))
      prob = pt(-absz, df) 
      #yrr <- c(yrr, NA,NA, rev(yrr))
      #xrr <- c(xrr, NA,NA, rev(-xrr))
    } else if (input$quant1_area == "Center"){   ##  center
      xrr <- seq(-absz, absz, length=100)
      yrr <- c(0, dt(xrr, df), 0)
      xrr <- c(-absz, xrr, absz)
      prob <- diff( pt(c(-absz,absz), df) )
    }
  } else {
    ## do nothing.  Wait for user to supply prob or z.
  }
  plot(x, dt(x, df), type = "l", bty='n', xlab="Z Score", ylab="", yaxt="n")
  abline(h=0)
  max.height <- dt(0, df) *.9
  text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
  segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
  
  if(input$quant1_area == "Extremes") {  ## extremes
    polygon(xrr, yrr, col = rd)
    polygon(-xrr, yrr, col = rd)
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
         round(prob, 3), col = "darkblue")
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y = text.height, round(c(-absz,absz),3))
  } else if (input$quant1_area == "Center") {   ## fill & label center
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


  ## 2 Categorical -----------------------------------------------------------  -- 2 categ

  ## Descriptives:  plot a bar chart of the successes / failures
 cat2_data <- reactive({
    data.frame(
     counts = as.numeric(c(input$cat2_n11, input$cat2_n12, input$cat2_n21, input$cat2_n22)),
     names  = rep(c(input$cat2_name1, input$cat2_name2), 2),
     groups = rep(c(input$cat2_grp1, input$cat2_grp2), each = 2)
   )
 })

 output$cat2Summary <- renderTable({ 
  if(input$cat2_submitButton ==0) return()
  isolate({
    cat2_dataDF <- cat2_data()
    #print(cat2_dataDF)
    counts <- as.table( matrix(cat2_dataDF$counts, 2, 2))
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

  output$normalProbPlot2 <-    renderPlot({ 
  par(mar=c(24,1,1,1)/10)
  z <- absz <- prob <- yrr <- xrr <- NA
  x <- -300:300 / 50
  
  ## setup transparent colors
  grn <- rgb(0, 1, 0, alpha=.4)
  rd  <- rgb(1, 0, 0, alpha=.5)
  blu <- rgb(0, 0, 1, alpha=.4)
  
  ## convert text to numeric  
  prob <- as.numeric(input$cat2_prob_txt)
  z <- as.numeric(input$cat2_z_txt)
  
  ##  this loop should run when user changes prob
  if(!is.na(prob) ){
    ## given prob, find z
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
      z <- abs(qnorm(1-prob) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(-z, x[x < -z], -z)
      yrr <- c(0,  dnorm(xrr[-1]))
    }
    absz <- abs(z)
    
  } else if(!is.na(z)){
    ##  find probability
    absz <- abs(z)
    maxX <- pmax(z, 6)
    minX <- pmin(z, -6)
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
      xrr <- seq(-absz, absz, length=100)
      yrr <- c(0, dnorm(xrr), 0)
      xrr <- c(-absz, xrr, absz)
      prob <- diff( pnorm(c(-absz,absz)) )
    }
  } else {
    ## do nothing.  Wait for user to supply prob or z.
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
         round(prob, 3), col = "darkblue")
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

  ## 2 Quantitative -----------------------------------------------------------  2 quant


  ## 1 categorical & 1 quantitative   ---------------------------------------  1 cat 1 quant
  
  ## Plot t distributions
  output$tProbPlot2 <-    renderPlot({ 
  par(mar=c(24,1,1,1)/10)
  z <- absz <- prob <- yrr <- xrr <- NA
  x <- -300:300 / 50
  
  ## setup transparent colors
  grn <- rgb(0, 1, 0, alpha=.4)
  rd  <- rgb(1, 0, 0, alpha=.5)
  blu <- rgb(0, 0, 1, alpha=.4)
  
  ## convert text to numeric  
  prob <- as.numeric(input$quant2_prob_txt)
  z <- as.numeric(input$quant2_z_txt)
  df <- as.numeric(input$quant2_df)
  
  ##  this loop should run when user changes prob
  if(!is.na(prob) ){
    ## given prob, find z
    if(input$quant2_area == "Lower"){ ##  left tail
      z <-  qt(prob, df)
      if(z < min(x))  x <- c(1.01 * z, x)
      ## rejection region in x dimension
      xrr <- c(x[x < z], z, z)
      ## density curve over xrr:
      yrr <- c( dt(xrr[-length(xrr)], df), 0)
    } else if(input$quant2_area == "Upper"){   ## right tail        
      z <- qt(1 - prob, df) 
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(z, z, x[x > z])
      yrr <- c(0, dt(xrr[-1], df))
    } else if(input$quant2_area == "Center"){
      z <- abs(qt( (1 - prob)/2, df) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- seq(-z, z, length=100)
      yrr <- c(0, dt(xrr[2:99], df), 0)
    } else if(input$quant2_area == "Extremes"){
      z <- abs(qt(1-prob, df) )
      if(z < min(x))  x <- c(1.01 * z, x)       
      if(z > max(x))  x <- c(x, 1.01 * z)
      xrr <- c(-z, x[x < -z], -z)
      yrr <- c(0,  dt(xrr[-1], df))
    }
    absz <- abs(z)
    
  } else if(!is.na(z)){
    ##  find probability
    absz <- abs(z)
    maxX <- pmax(z, 6)
    minX <- pmin(z, -6)
    x <- seq(minX, maxX, length=200)
    prob <-  1 - pt(z, df) 
    xrr <- c(z, z, x[x > z])  ## right tail
    yrr <- c(0,  dt(xrr[-1], df) )
    if(input$quant2_area == "Lower"){         ##  left tail
      prob =  pt(z, df) 
      xrr <- c(z, x[x < z], z)
      yrr <- c(0,  dt(xrr[-1], df))
    } else if (input$quant2_area == "Extremes"){ ##  extremes
      xrr <- c( -absz, x[x < -absz], -absz)
      yrr <- c(0, dt(xrr[-1], df))
      prob = pt(-absz, df) 
      #yrr <- c(yrr, NA,NA, rev(yrr))
      #xrr <- c(xrr, NA,NA, rev(-xrr))
    } else if (input$quant2_area == "Center"){   ##  center
      xrr <- seq(-absz, absz, length=100)
      yrr <- c(0, dt(xrr, df), 0)
      xrr <- c(-absz, xrr, absz)
      prob <- diff( pt(c(-absz,absz), df) )
    }
  } else {
    ## do nothing.  Wait for user to supply prob or z.
  }
  plot(x, dt(x, df), type = "l", bty='n', xlab="Z Score", ylab="", yaxt="n")
  abline(h=0)
  max.height <- dt(0, df) *.9
  text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
  segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
  
  if(input$quant2_area == "Extremes") {  ## extremes
    polygon(xrr, yrr, col = rd)
    polygon(-xrr, yrr, col = rd)
    segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
    text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
         round(prob, 3), col = "darkblue")
    place.x <- c(-absz, absz)
    if(absz < 1) place.x <- place.x/absz * .8
    text(place.x, y = text.height, round(c(-absz,absz),3))
  } else if (input$quant2_area == "Center") {   ## fill & label center
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
  output$probPlot <-    renderPlot({ 
    par(mar=c(24,1,1,1)/10)
    z <- absz <- prob <- yrr <- xrr <- NA
    x <- -300:300 / 50
    df <- input$prb_df
    
    ## setup transparent colors
    grn <- rgb(0, 1, 0, alpha=.4)
    rd  <- rgb(1, 0, 0, alpha=.5)
    blu <- rgb(0, 0, 1, alpha=.4)

    ## convert text to numeric  
    prob <- as.numeric(input$prb_prob_txt)
    z <- as.numeric(input$prb_z_txt)
    
    ##  this loop should run when user changes prob
    if(!is.na(prob) ){
      ## given prob, find z
      if(input$prb_area == "Lower"){ ##  left tail
        z <- if(input$prb_dist=='Normal') qnorm(prob)else qt(prob, df)
        if(z < min(x))  x <- c(1.01 * z, x)
        ## rejection region in x dimension
        xrr <- c(x[x < z], z, z)
        ## denstiy curve over xrr:
        yrr <- c( if(input$prb_dist=='Normal') dnorm(xrr[-length(x)]) else 
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
      
    } else if(!is.na(z)){
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
    } else {
      ## do nothing.  Wait for user to supply prob or z.
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
           round(prob, 3), col = "darkblue")
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


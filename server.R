includeScript("www/helper.js")

quant1_contents <- load("data/quant1.RData")
quant2_contents <- load("data/quant2.RData")

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


  # use  selectInput to grab the 2 types of input
   q1_values = list()
   q1_setHot = function(x) q1_values[["hot"]] <<- x
   q1_setHot(read.csv("data/dummyData.csv", stringsAsFactors = FALSE, head = TRUE) )

  output$q1_hot = renderRHandsontable({
    if (!is.null(input$q1_hot)) {
      q1_DF = hot_to_r(input$q1_hot)
      q1_setHot(q1_DF)
      rhandsontable(q1_DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
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

#  q1_source <- reactive({ 
#     if (input$q1_useLddBtn != 0){
#         "existing"
#     } else 
# #         if (input$q1_useFileBtn != 0){
# #           "file"
# #     } else 
#       if(input$q1_useHotBtn != 0){
#         "hot"
#     } else  NULL
#   })

q1_data <- reactive({
  if(input$q1_entry == "Pre-Loaded Data") {
    DF <- eval(parse( text = input$q1_data1))
    data.frame(x=DF)
  } else if(input$q1_entry == "Type/Paste into Data Table"){
    DF = data.frame(x=as.numeric(q1_values[["hot"]][,2]))
    print(DF)
    data.frame( x = as.numeric(unlist(DF)))
  } else  NULL
})


#     if(input$q1_useHotBtn != 0){
#       as.numeric(q1_values[["hot"]][,2])
#     } else 
#       if (input$q1_useExistingBtn != 0){
#         as.numeric(eval(parse( text = input$q1_data1)))
#       } else 
#         if (input$q1_useFileBtn != 0){
#           ##print(input$q1_file1)
#           as.numeric(read.csv(input$q1_file1$datapath, header=input$q1_header, sep=input$q1_sep, quote=input$q1_quote)[,1])
#         } else  NULL
  
  
  output$q1_Plot <- renderPlot( {
    if( input$q1_entry == " ")  return()
     #if(input$q1_useLddBtn == 0 && input$q1_useHotBtn == 0)  ## && input$q1_useFileBtn == 0) 
       #isolate( { 
    q1_dataDF <- q1_data() 
    ## make plot
    q1_plot1 <- 
      qplot(x = x, y=x, data = q1_dataDF,  geom ="boxplot") + theme_bw() + xlab("") + ylab("") + 
                  scale_x_continuous(breaks = c(-1,1000)) +  coord_flip()
    #par(mar=c(24, 40, 10, 35)/10, mfrow=c(2,1))
    #boxplot(q1_dataDF, horizontal = TRUE, main = "")
    # Plot stacked x values. 
    x <- sort(q1_dataDF$x)
    ## print(x)
    z <- cut(x, breaks = nclass.Sturges(x) ^2 )
    y <- unlist(tapply(x, z, function(x) 1:length(x)))
    tempDF <- data.frame(x, y=y[!is.na(y)])
    myBlue <- rgb(0, 100/256, 224/256, alpha = .8)  
    q1_plot2 <- qplot(data=tempDF, x=x, y=y, colour = I(myBlue), size = I(4)) + theme_bw() 
    grid.arrange(q1_plot1, q1_plot2, heights = c(1,3)/4, ncol=1)
  #})
}, height=360)


  output$q1_Summary <- renderTable({
    if( input$q1_entry == " ")  return()
    #if(input$q1_useLddBtn == 0 && input$q1_useHotBtn == 0) ## && input$q1_useFileBtn == 0) 
    # return()
    #isolate({
      q1_dataDF <- q1_data()
      ## print(q1_dataDF)
      DF <- rbind(mean = mean(q1_dataDF$x, na.rm = TRUE ),
                   sd = sd(q1_dataDF$x, na.rm = TRUE),
                   min = min(q1_dataDF$x),
                   Q1 = quantile(q1_dataDF$x, .25),
                   median = median(q1_dataDF$x),
                   Q3 = quantile(q1_dataDF$x, .75),
                   max = max(q1_dataDF$x))
      colnames(DF) <- NULL
      DF
    #})
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

# use  actionButtons to grab the 3 types of input
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

q2_data <- reactive({
  if(input$q2_useHotBtn != 0){
    as.numeric(q2_values[["hot"]][,2])
  } else 
    if (input$q2_useExistingBtn != 0){
      as.numeric(eval(parse( text = input$q2_data1)))
    } else 
      if (input$q2_useFileBtn != 0){
        ##print(input$q2_file1)
        as.numeric(read.csv(input$q2_file1$datapath, header=input$q2_header, sep=input$q2_sep, quote=input$q2_quote)[,1])
      } else  NULL
})

output$q2_Plot <- renderPlot( {
  if(input$q2_useHotBtn == 0 && input$q2_useExistingBtn == 0 && input$q2_useFileBtn == 0) 
    return()
  isolate( { 
    q2_dataDF <- q2_data() 
    ## print(q2_dataDF)
    ## make plot
    q2_plot1 <- ggplot() +geom_boxplot(aes(y= q2_dataDF, x = q2_dataDF)) +
      theme_bw() + xlab("") + ylab("") + scale_x_continuous(breaks = c(-1,1000)) +  coord_flip()
    #par(mar=c(24, 40, 10, 35)/10, mfrow=c(2,1))
    #boxplot(q2_dataDF, horizontal = TRUE, main = "")
    # Plot stacked x values. 
    x <- sort(q2_dataDF)
    z <- cut(x, breaks = nclass.Sturges(x) ^2 )
    y <- unlist(tapply(x, z, function(x) 1:length(x)))
    tempDF <- data.frame(x, y=y[!is.na(y)])
    myBlue <- rgb(0, 100/256, 224/256, alpha = .8)  
    q2_plot2 <- qplot(data=tempDF, x=x, y=y, colour = I(myBlue), size = I(4)) + theme_bw() 
    grid.arrange(q2_plot1, q2_plot2, heights = c(1,3)/4, ncol=1)
  })
}, height=360)


output$q2_Summary <- renderTable({
  if(input$q2_useHotBtn == 0 && input$q2_useExistingBtn == 0 && input$q2_useFileBtn == 0) 
    return()
  isolate({
    q2_dataDF <- q2_data()
    DF <- rbind(mean = mean(q2_dataDF, na.rm = TRUE ),
                sd = sd(q2_dataDF, na.rm = TRUE),
                min = min(q2_dataDF),
                q2 = quantile(q2_dataDF, .25),
                median = median(q2_dataDF),
                Q3 = quantile(q2_dataDF, .75),
                max = max(q2_dataDF))
    colnames(DF) <- NULL
    DF
  })
})

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


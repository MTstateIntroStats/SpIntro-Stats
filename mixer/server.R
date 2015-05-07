#thanks to https://www.trestletechnology.net/2012/12/reconstruct-gene-networks/
require(shiny)

shinyServer(function(input, output) {
  ## sample locations for the balls from a grid of points.
  ## spacing and ball radius depend on window size and numbers of points.
  ## we want to use about half the possible points,
  ## and limit max radius to 16 pixels.
  ## assume height is the limiting dimension
  h <-  330
  

  draws2get1 <-  function(prob, reps){
    ## randomly spins til we get one of the first category
    ## returns the number of spins needed
    ## this samples "with replacement"
    nCat <- length(prob)
    prob <-  prob/sum(prob)
    if(nCat < 2)
      stop("Must have at least 2 categories")
    rgeom(reps, prob = prob[1]) + 1
  } 

  draws2get1ofEach <- function( prob, reps, fullOut=FALSE){
    ## randomly draw til we get one of each category
    ## returns the number of draws needed
    ## if fullOut = TRUE, gives info to trace the critical steps
    ##  of each sequence of spins: Category seen, and
    ##  draws to the next new category
    nCat <- length(prob)
    prob <-  prob/sum(prob)
    reps <- as.numeric(reps)
    if(nCat < 2)
      stop("Must have at least 2 categories")
    tempDraw <-  matrix(sample(1:nCat, reps * round(nCat/min(prob)) * 4, replace=TRUE),
                        nrow= reps)
    noDups <- !t( apply(tempDraw, 1, duplicated))
    check <- any(apply(noDups, 1, function(x) sum(as.numeric(x)) < nCat))
    if(check)
      stop("Whoops, we missed some large runs")
    nDraws <- apply(noDups, 1, function(x) max(which(x)))
    
    if(!fullOut)
     return(nDraws)

    draws <-  t(sapply(1:reps, function(i)
                       tempDraw[i,][noDups[i,]]))
    cols <- t(apply(noDups, 1, which))
    return(data.frame(nDraws, draws, cols))
  }

 
  reconstructSpinsWithRep <- function(output, prob){
    ## uses fullOut from 'draws2get1ofEach()' or a count from 'draws2get1()'
    ## and reconstructs a history of spins.  With more than 2 categories,
    ## the sequence is not unique, as intermediate draws could have come
    ## from any of several sequences which have the same 'new categories'
    ## in the same positions, but differ in the "filler" spots.
    ##
    ## returns indices 1:nCategories
    output <- unlist(output)
    nCat = length(prob)
    prob <-  prob/sum(prob)
    if(length(output) == 1){
      if(output == 1) return(1)
      if(nCat == 2) return(c(rep(2, output-1),1))
      return(c(sample(2:nCat, output - 1, prob[-1], replace=TRUE), 1))
      ## returns output for stopping after "1 in 1st category"
    }
    ## else we're drawing to get 1 of each

    if(abs(nCat - (length(output) -1) / 2) > .01)
      stop("Output dimensions don't match length(prob)")

    catObs <- output[ 2:(nCat + 1)]
    trial <-  output[ -(1 + 0:nCat)]
    
    val <- rep(catObs[1:2], c(diff(trial[1:2]),1))
           ## uses first 2 categories observed
    if(nCat > 2){
      for(ndx in 3:nCat){
        covered <- catObs[1:(ndx-1)]
        val <-  c(val, sample(covered,  trial[ndx] - trial[ndx-1] -1 ,  prob = prob[covered], replace = TRUE),
                      catObs[ndx])
      }
    }
    val
  }


  data <- reactive( {
    run = input$runButton
    counts <- sapply(strsplit(input$counts, ","), as.integer)
    groups <- sapply(strsplit(input$categories, ","), function(x)
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
    ## nBalls comes from entered counts
    ## nDraws from the stopping rule
    ##  balls are the randomly ordered originals
    ## draws are the indices of the sample --
    ## referring to which of nBalls is picked in turn
  ### Fixed number of Draws
    if(input$stopRule == "Fixed number of draws"){
        nDraws <- as.numeric(input$nDraws)
        if(input$replace == "yes"){
            ## don't need prob, just sample from counted balls
            draws <- sample(1:nBalls, nDraws, replace=TRUE) - 1
            ## zero-initialized indices
         } else {  ## no replacement, use sampled order
            draws <-  1:nDraws - 1
         }  
    } else
  ### Stop on first category
    if(input$stopRule == "One draw in 1st category"){
        if(input$replace == "yes"){
           drawSumry <- draws2get1( prob, 1)
           if(drawSumry == 1){
             draws <- sample(which(ballNums == 1),1) - 1
             nDraws <- 1
           } else{
             drawGrp <- reconstructSpinsWithRep( drawSumry, prob) 
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
        if(input$replace == "no"){
           nUnique <-  sapply(nCat:nBalls, function(x) length(unique(ballNums[1:x])))
           nDraws <- min(which(nUnique == nCat)) + nCat - 1
           draws <-  1:nDraws - 1
         } else {
           ## do replace each drawn ball  (replace == "yes")
            drawSumry <- draws2get1ofEach( prob, 1, full=TRUE)
            nDraws <-  drawSumry$nDraws
            drawGrp <- reconstructSpinsWithRep( drawSumry, prob)
            names(drawGrp) <-  1:nDraws
            ##  gives category. need to convert to a ball of that category
            draws <-  sapply(drawGrp, function(x) which( ballNums  == x)[1]) - 1
            names(draws) <-  NULL
          }
      }
      ##cat("# draws: ", nDraws, " draw: ",draws," balls: ",balls[draws] ,"\n")
      if (length(draws) == 1) draws <-  as.list(draws)
    
      data.df <- list(height = h,
                      run = input$runButton,
                    nCat = input$nCat,
            counts = as.list(counts),
            nBalls = nBalls, 
            radius = radius,
            nDraws = nDraws,
            x = as.list(sampleLocs$x - h/2), ## sample from a grid of points
            y = as.list(sampleLocs$y - h/2 - radius) ,
            labels = groups,
            replace = input$replace,
            balls = balls,
            draws = draws,
            drawColor = ballNums - 1)

    data.df
  } )
  
  functionList <-  c( "Count in Group 1","Count in Group 2", "Max Run Length","Matches")
  
  repData <- reactive( {
     run = input$runButton
     counts <- sapply(strsplit(input$counts, ","), as.integer)
     groups <- sapply(strsplit(input$categories, ","), function(x)
                       gsub("[[:space:]]", "", x))##[order(-prob)]
     nCat <-  length(groups)
     nBalls <-  sum(counts)
     prob <-  counts/nBalls
     nReps <-  ifelse(is.null(input$reps), 1, as.numeric(input$reps))
     fixedN <- (input$stopRule == "Fixed number of draws")
     if(fixedN){
       fn <- match(input$fn, functionList) 
       nDraws <- as.numeric(input$nDraws)
       if(input$replace == "no"){
          samplData <- t(sapply(1:nReps, function(x) sample(rep(groups,counts))[1:nDraws]))
          ## not working ##
        } else {
         samplData <- matrix(sample(1:input$nCat, nReps * nDraws,
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
        ##if(input$replace == "no" ){
        ##  warning("Please choose to replace each draw.")
        ##}
        if(input$stopRule == "One draw in 1st category"){
          if(input$replace == "no" ){
            nDraws <- sapply(1:nReps, function(x) min(which(sample(rep(groups,counts), nBalls) == groups[1])))
            ##drawColor <- reconstructSpinsWithRep(nDraws[1], counts/nBalls ) - 1
            return(nDraws)
          } else{  ## input$replace =="yes"
            nDraws <- draws2get1( counts/nBalls, nReps)
            ##drawColor <- reconstructSpinsWithRep(  nDraws[1], counts/nBalls ) - 1
            return(nDraws)
          }
          ##  gives random draw in 1st category for last draw,
          ## in other categories for prior draws.
      } else{ ## one of each type
         if( input$replace =="no"){
             tempDraws <- sapply(1:nReps, function(x) sample(rep(groups, counts)))
             nUnique <-  tempDraws[1:(nBalls - nCat),]
             if(nCat > nBalls)
               stop("Need more balls to draw from")
             nDraws <- apply(tempDraws, 2, function(x) max(which(!duplicated(x))))
            return(nDraws)
          } else { ## input$replace =="yes"
            nDraws <- draws2get1ofEach(prob, rep=nReps)
            return(nDraws)
          }
       }
     }
    
   })
  
  output$mixPlot <- reactive( {
    if(input$runButton ==0) return()
        isolate( data() )
  }) 

  output$summary <- renderPrint({
   if(input$runButton ==0) return()
      isolate({
        data.df <- data()
         out1 <-   summary( factor(data.df$balls[unlist(data.df$draws)+1],
                                levels=data.df$labels))
        if(input$stopRule =="Fixed number of draws"){
           runs <-  rle(data.df$balls[unlist(data.df$draws) + 1])
           out1 <-  c( out1, max(runs[[1]]))
           names(out1)[data.df$nCat + 1] <- "maxRunLength"
         } else{
            out1 <-  c( out1, length(data.df$draws))
            names(out1)[data.df$nCat + 1] <- "draws"
          }
        out1
      })
  })
    ## run more:
   output$summry2 <- renderPrint({
      rData <-  repData()
      unlist(list( c(summary(rData), spread = sd(rData))))
   })
  
    output$summry3 <- renderTable({
      ##isolate({
        counts  <- repData() 
        if(input$stopRule =="Fixed number of draws"){
          output <- t(table(c(counts,0:input$nDraws))-1)
        } else if(input$stopRule =="One draw in 1st category"){
            output <-  t(table(c(counts, 1:max(counts)))-1)
        } else  if(input$stopRule =="One of each type"){
            output <-   t(table(c(counts, input$nCat:max(counts)))-1)
        }
        rownames(output) <- "Counts"
        output
      ##})
    })
   
   output$histogrm <- renderPlot({
      x <- sort(repData())
      y <- unlist(tapply(x, x, function(z) 1:length(z)))
      if(input$stopRule =="Fixed number of draws"){
          stat <-  input$fn
          begin <- 0 + (input$fn == functionList[3]) 
          xlimits = c(begin, input$nDraws)
      }
      if(substr(input$stopRule,1,3) == "One") {
          stat = "Number of Draws"
          xlimits = range(x)
      }
      plot(jitter(x, .3), y, main = paste("Distribution of ", stat), xlab = "", ylab = "Frequency",  xlim=xlimits)
    })

  

})



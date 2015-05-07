#thanks to https://www.trestletechnology.net/2012/12/reconstruct-gene-networks/
##options(error = browser)

shinyServer(function(input, output) {

  options(width=122) 
  
  draws2get1 <-  function( prob, reps){
    ## randomly spins til we get one of the first category
    ## returns the number of spins needed
    nCat <- length(prob)
    prob <-  prob/sum(prob)
    if(nCat < 2)
      stop("Must have at least 2 categories")
    rgeom(reps, prob = prob[1]) +1
  } 

  draws2get1ofEach <- function( prob, reps, fullOut=FALSE){
    ## randomly spin til we get one of each category
    ## returns the number of spins needed
    ## if fullOut = TRUE, gives info to trace the critical steps
    ##  of each sequence of spins: Category seen, and
    ##  spins to the next new category
    nCat <- length(prob)
    prob <-  prob/sum(prob)
    reps <- as.numeric(reps)
    if(nCat < 2)
      stop("Must have at least 2 categories")
    tempDraw <- matrix(sample(1:nCat, reps * nCat * round(4/min(prob)), prob = prob, replace=TRUE), nrow = reps)
    noDups <- !t( apply(tempDraw, 1, duplicated))
    check <- any(apply(noDups, 1, function(x) sum(as.numeric(x)) < nCat))
    if(check)
      stop("Whoops, we missed some large runs")
    nDraws <- apply(noDups, 1, function(x) max(which(x)))
    if(!fullOut)
      return(nDraws)
    draws <-  t(sapply(1:reps, function(i) tempDraw[i,][noDups[i,]]))
    cols <- t(apply(noDups, 1, which))
    return(data.frame(nDraws, draws, cols))
   }
 
  reconstructSpins <- function( output, prob){
    ## uses fullOut from 'draws2get1ofEach()' or a count from 'draws2get1()'
    ## and reconstructs a history of spins.  With more than 2 categories,
    ## the sequence is not unique, as intermediate draws could have come
    ## from any of several sequences which have the same 'new categories'
    ## in the same positions, but differ in the "filler" spots.
    nCat <-  length(prob)
    prob <-  prob/sum(prob)
    output <- unlist(output)
    if(length(output) == 1){
      if(output == 1) return(1)
      if(nCat == 2) return(rep(2:1, c(output-1,1)))
      return(c(sample(2:nCat, output - 1, prob[-1], replace=TRUE), 1))
      ## returns output for 'draws2get1()'
    }
    ## else we're doing 'draws2get1ofEach()
    
     ## output <- unlist(output[1,])
    
    if(abs(nCat - (length(output) -1) / 2) > .01)
      stop("Output dimensions don't match length(prob)")
 
    catObs <- output[ 2:(nCat + 1)]
    trial <-  output[ -(1 + 0:nCat)]
   
    val <- rep(catObs[1:2], c(trial[2]-trial[1],1))
           ## uses first 2 categories observed
    if(nCat > 2){
      for(ndx in 3:nCat){
        covered <- catObs[1:(ndx-1)]
       ## print(covered)
       ## print( trial[ndx] - trial[ndx-1] )
        val <-  c(val, sample(covered, trial[ndx] - trial[ndx-1] -1,  prob = prob[covered], replace = TRUE),
                      catObs[ndx])
      }
    }
    val
  }

  data <- reactive( {
     run = input$runButton
     groups <- sapply(strsplit(input$categories, ","), function(x)
                       gsub("[[:space:]]", "", x))##[order(-prob)]
     nCat <-  length(groups)
     prob <- sapply(strsplit(input$probs, ","), as.numeric)
     prob <-  prob/sum(prob)
     if(input$stopRule == "Fixed number of spins"){
        nDraws <- input$nDraws
        spinAngle <- runif(input$nDraws)
        drawColor <- as.numeric(cut( spinAngle, c(0, cumsum(prob)))) 
        nDraws <- as.numeric(input$nDraws)
     } else {
      ## apply a stopping rule
         if(input$stopRule == "One spin in 1st category"){
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
      data.df <- list(nCat = input$nCat,
                      nDraws = nDraws[1],
                      pieValues = prob,
                      pieLabels = groups,
                      spinAngle = as.list((spinAngle + 1 ) * 360),
                      drawColor = as.list(drawColor - 1)
                      )
      return(data.df)
       
  })

  functionList <- c( "Count in Group1","Count in Group 2", "Max Run Length")
  
  repData <- reactive( {
    run = input$runButton
    prob <- sapply(strsplit(input$probs, ","), as.numeric)
    prob <-  prob/sum(prob)
    groups <- sapply(strsplit(input$categories, ","), function(x)
                       gsub("[[:space:]]", "", x))##[order(-prob)]
    ##prob <- -sort(-prob)
    nCat <-  length(groups)
    nReps <-  ifelse(is.null(input$reps), 1, as.numeric(input$reps))
    fixedN <- (input$stopRule == "Fixed number of spins")
    if(fixedN){ 
      nDraws <- as.numeric(input$nDraws)
      samplData <- matrix(sample(1:input$nCat, nReps * nDraws,
                                 prob = prob,replace = TRUE), ncol=nDraws)
      fn <- match(input$fn, functionList)
      if(fn==3){
         return(apply(samplData, 1 , function(x)  max(rle(x)[[1]])))
       } else
      return(apply(samplData, 1 , function(x) table(c(1:fn,x))[fn]-1))
     } else {
     ## apply stopping rule
       if(input$stopRule =="One spin in 1st category"){
         nDraws <- draws2get1( prob, nReps)
         return(nDraws)
       } else {  ## input$stopRule =="One of Each"
         nDraws <-  draws2get1ofEach( prob, nReps)
         return(nDraws)
       }
     }
       
  })
  
  output$spinPlot <- reactive( {
      if(input$runButton == 0) return()
      data()
      })  # execute when run is clicked
  
  output$summary <- renderPrint({
      if(input$runButton ==0) return()
        isolate({
          data.df <- data()
          out1 <-   summary( factor(data.df$pieLabels[unlist(data.df$drawColor)+1],  levels=data.df$pieLabels))
          
          if(input$stopRule =="Fixed number of spins"){
            runs <-  rle(unlist(data.df$drawColor)[1:data.df$nDraws])
            out1 <-  c( out1, max(runs[[1]]))
            names(out1)[data.df$nCat + 1] <- "maxRunLength"
          }else{
            out1 <-  c( out1, length(data.df$drawColor))
            names(out1)[data.df$nCat + 1] <- "spins"
          }
          out1
        })
    })
    ## run more:
    output$summry2 <- renderPrint({
      ##  isolate({
          rData <-  repData()
          unlist(list( c(summary(rData), stdDev = sd(rData))))
      ## })
    })
  
    output$summry3 <- renderTable({
      ##isolate({
        counts  <- repData()
        if(input$stopRule =="Fixed number of spins"){
          output <- t(table(c(counts,0:input$nDraws))-1)
        } else if(input$stopRule =="One spin in 1st category"){
            output <-  t(table(c(counts, 1:max(counts)))-1)
        } else  if(input$stopRule =="One of each type"){
            output <-   t(table(c(counts, input$nCat:max(counts)))-1)
        }
        rownames(output) <- "Counts"
        output
      ##})
    })
  
    output$histogrm <- renderPlot({
       ##isolate({
         x <- sort(repData())
         y <- unlist(tapply(x, x, function(z) 1:length(z)))
      if(input$stopRule =="Fixed number of spins"){
          stat <-  input$fn
          begin <- 0 + (input$fn == functionList[3]) 
          xlimits = c(begin, input$nDraws)
      }
      if(substr(input$stopRule,1,3) == "One") {
          stat = "Number of Spins"
          xlimits = range(x)
      }
      plot(jitter(x, .3), y, main = paste("Distribution of ", stat), xlab = "", ylab = "Frequency",  xlim=xlimits)
       ##})
     })

})




## Function to draw and store difference in proportions for randomization

cat1_estimate_shuffles <- function(shuffles, y1, n1){
  shuffles <- as.numeric(shuffles)
  y1new <- NULL
  
  for(i in 1:shuffles){
    y1_new[i] <- sum(rbinom(n1, 1, y1/n1))
  }
  phat <- y1new/n1
  return(phat)
}

cat2_test_shuffles <- function(shuffles, y1, y2, n1, n2){
  shuffles <- as.numeric(shuffles)
  y1_new <- rhyper(shuffles, y1+y2,(n1 + n2) - (y1 + y2), n1)
  y2_new <- (y1 + y2) - y1_new
  
  phat.1 <- y1_new/n1
  phat.2 <- y2_new/n2
  diff.p <- phat.1 - phat.2
  data <- as.matrix(cbind(phat.1, phat.2, diff.p), ncol = 3, nrow = shuffles)
  
  return(data)
}

c1Lurk_shuffles <- function(shuffles, m, n, k){
  y1_new <- rhyper(as.numeric(shuffles), m, n, k)
  y2_new <- m  - y1_new
  phat.1 <- y1_new/k
  phat.2 <- y2_new/(m + n - k)
  diff.p <- phat.1 - phat.2
  cbind( phat.1, phat.2, diff.p, y1_new, y2_new)
}

## Function to draw and store difference in proportions for bootstrap
cat2_estimate_shuffles <- function(shuffles, y1, y2, n1, n2){
  
    shuffles <- as.numeric(shuffles)
    y1new <- y2new <- NULL
    
    for(i in 1:shuffles){
    y1new[i] <- sum(rbinom(n1, 1, y1/n1))
    y2new[i] <- sum(rbinom(n2, 1, y2/n2))
    }
    phat.1 <- y1new/n1
    phat.2 <- y2new/n2
    diff.p <- phat.1 - phat.2
    data <- as.matrix(cbind(phat.1, phat.2, diff.p), ncol = 3, nrow = shuffles)
  
  return(data)
}

c1q1_estimate_shuffles <- function(shuffles, ndx1, ndx2){
  ## create a matrix of shuffled index values.
  ##  top n1 rows are a resamle from group 1,
  ##  btm n2 rows, same from group 2
  rbind( matrix(sample(ndx1, length(ndx1) * shuffles, replace = TRUE), ncol = shuffles), 
         matrix(sample(ndx2, length(ndx2) * shuffles, replace = TRUE), ncol = shuffles))
}

 ## finding break points for dot plots
newy  <- function(simStats){
  nbreaks <- 0.5*nclass.Sturges(simStats)^2
  z <- cut(simStats, breaks = nbreaks) 
  checkBreaks <- (length(simStats) < 3000)
  ## look at center 30 bins to see if we have lots of variation
  ## if so, try more bins
  oldDifQuant <- 100
  while(nbreaks > 30 & checkBreaks){
    zt <- as.numeric(table(z))
    hipt <- which.max(zt)
    zt <- zt[pmax(1, hipt -10):pmin(length(zt), hipt+10)]
    #print(
    difquant <- diff(quantile(zt,c(.75,1)))/ median(zt) #)
    if(difquant > .8 & difquant < oldDifQuant){
      nbreaks <- nbreaks * 1.1
      z <- cut(simStats, breaks = nbreaks) 
      oldDifQuant <- difquant
    } else { break()}
  }
  w <- unlist(tapply(z, z, function(V) 1:length(V)))
  w[!is.na(w)]
}

  ## control the way p-values are printed  ##  

pvalue2print <- function(extremes, nreps, direction, cutoff, pvalue){
  if(extremes > 0){
    paste(extremes," of ",nreps," values are ",direction," than ", cutoff,
          ". P-value = ", round(pvalue,5))
  } else {
    paste(extremes," of ",nreps," values are ",direction," than ", cutoff,
          ". P-value < 1/", nreps, " or P-value < ", round(1/nreps, ceiling(log10(nreps))+1))    
  }
}

## functions for SPINNERS

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

cleanText <- function(txt){
 ## function to take a string of text, converrt to just letters, and split into words. 
  txt <- tolower(txt) ## convert all to lower case
  txt <- gsub("[:;(),.?']", "", trimws(txt))
  words <- unlist(strsplit(txt, " "))
  data.frame(words, count =nchar(words))
}

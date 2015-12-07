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

gettysbrg <- "   Four score and seven years ago our fathers brought forth on this continent a new nation, conceived in liberty, and dedicated to the proposition that all men are created equal.

    Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battlefield of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

    But, in a larger sense, we can not dedicate, we can not consecrate, we can not hallow this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion - that we here highly resolve that these dead shall not have died in vain - that this nation, under God, shall have a new birth of freedom - and that government of the people, by the people, for the people, shall not perish from the earth.
"

joke <- "Four college friends were so confident that the weekend before finals, they decided to go to a
city several hours away to party with some friends. They had a great time. However, after all
the partying, they slept all day Sunday and didnt make it back to school until early Monday
morning.
Rather than taking the final then, they decided to find their professor after the final and explain
to him why they missed it.
They explained that they had gone to the city for the weekend with the plan to come back
and study but, unfortunately, they had a flat tire on the way back, didnt have a spare, and
couldnt get help for a long time. As a result, they missed the final.
The professor thought it over and then agreed they could make up the final the following day.
The four were elated and relieved.
They studied that night and went in the next day at the time the professor had told them.
The professor placed them in separate rooms and handed each of them a test booklet, and told them to
begin.
They looked at the first problem, worth 5 points. It was something simple about exploratory
data analysis. 'Cool,' they thought at the same time, each one in his separate room. 'This is
going to be easy.'
Each finished the problem and then turned the page. On the second page was written:
For 95 points: Which tire?"

cleanText <- function(atxt){
 ## function to take a string of text, convert to just letters, and split into words. 
  atxt <- gsub("[:;(),.?'-]", "", trimws(atxt)) ## trim white space, remove punctuation
  atxt <- gsub("[[:space:]]"," ", atxt)        ## convert linefeed & tab to space
  atxt <- stringi::stri_enc_toascii(atxt)     ## convert all to  ascii
  atxt <- gsub('[\032]'," ",atxt)             ## removes non-ascii characters
  words <- unlist(strsplit(atxt, " "))      ## split into words
  words <- words[nchar(words)>0]            ## remove NA's
  shorts <- which((nchar(words) == 1) &( words !="a" & words !="I"))
   if(length(shorts)){
     for(wd in shorts){
        words[wd-1] <- paste(words[wd + -1:0], collapse = "")
     }
      words <- words[-shorts]
   }
  data.frame(words, count =nchar(words), hasAnE = grepl("e", words)+0 )
}

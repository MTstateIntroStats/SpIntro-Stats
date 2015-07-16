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
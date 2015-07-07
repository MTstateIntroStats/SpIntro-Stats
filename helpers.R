## Function to draw and store difference in proportions for randomization

cat2_test_shuffles <- function(shuffles, y1, y2, n1, n2){

shuffles <- as.numeric(shuffles)
y1_new <- rhyper(shuffles, y1 + y2, (n1 + n2) - (y1 + y2), n1)
y2_new <- (y1 + y2) - y1_new

phat.1 <- y1_new/n1
phat.2 <- y2_new/n2
diff.p <- phat.1 - phat.2
diff.p <- as.matrix(diff.p, ncol = 1, nrow = shuffles)

return(diff.p)
}

## Function to draw and store difference in proportions for bootstrap
cat2_estimate_shuffles <- function(shuffles, y1, y2, n1, n2){
  
  shuffles <- as.numeric(shuffles)
  for(i in 1:shuffles){
    y1new[i] <- rbinom(shuffles, n1, (y1/n1))
    y2new[i] <- rbinom(shuffles, n2, (y2/n2))
    
    phat.1[i] <- y1new[i]/n1
    phat.2[i] <- y2new[i]/n2
    diff.p[i] <- phat.1[i] - phat.2[i]
    
  }
  return(diff.p)
}
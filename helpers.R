## Function to draw and store difference in proportions

generate_shuffles <- function(shuffles, phat_m, y1, y2, n1, n2){

shuffles <- as.numeric(shuffles)
n1_new <- rbinom(shuffles, n1, phat_m)
n2_new <- (y1 + y2) - n1_new

phat.1 <- n1_new/n1
phat.2 <- n2_new/n2
diff.p <- phat.1 - phat.2
diff.p <- as.matrix(diff.p, ncol = 1, nrow = shuffles)

return(diff.p)
}
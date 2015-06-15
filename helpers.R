## Function to draw and store difference in proportions

cat2_gen_shuffles <- function(shuffles, phat_m, y1, y2, n1, n2){

shuffles <- as.numeric(shuffles)
y1_new <- pmin(y1 + y2, rbinom(shuffles, n1, phat_m))
y2_new <- (y1 + y2) - y1_new

phat.1 <- y1_new/n1
phat.2 <- y2_new/n2
diff.p <- phat.1 - phat.2
diff.p <- as.matrix(diff.p, ncol = 1, nrow = shuffles)

return(diff.p)
}
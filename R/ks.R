ks_test <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  w <- c(x, y)
  o <- order(w)
  z <- cumsum(ifelse(o <= nx, 1 / nx, -1 / ny))
  i <- which.max(abs(z))
  return(list("D" = max(abs(z)), "l" = w[o[i]]))
}

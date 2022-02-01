get_ks <- function(x, y) {
  suppressWarnings(capture.output(ks <- ks.test(x, y)))
  unname(ks$statistic)
}

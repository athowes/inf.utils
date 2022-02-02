tmb_summary <- function(sd_out) {
  sd_summary <- summary(sd_out)
  tab <- table(rownames(sd_summary))

  parameter_names <- sapply(split(tab, names(tab)), function(x) {
    if(x > 1) paste0(names(x), "[", 1:x, "]")
    else(names(x))
  }) %>%
    unlist() %>%
    as.vector()

  row.names(sd_summary) <- NULL

  sd_summary %>%
    as.data.frame() %>%
    dplyr::mutate(parameter = parameter_names) %>%
    dplyr::rename(
      "mean" = "Estimate",
      "sd" = "Std. Error"
    ) %>%
    dplyr::mutate(method = "TMB")
}

sample_tmb <- function(sd_out, obj, M) {
  prec <- sd_out$jointPrecision
  cov <- solve(prec)
  samples <- mvtnorm::rmvnorm(n = M, obj$env$last.par, as.matrix(cov))
  as.data.frame(samples)
}

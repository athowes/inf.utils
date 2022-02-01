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
    mutate(parameter = parameter_names) %>%
    rename(
      "mean" = "Estimate",
      "sd" = "Std. Error"
    ) %>%
    mutate(method = "TMB")
}
aghq_summary <- function(quad) {
  aghq_hyper <- summary(quad, max_print = 10^3)$aghqsummary$summarytable %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(parameter = rowname) %>%
    dplyr::select(parameter, mean, sd)

  aghq_random <- summary(quad, max_print = 10^3)$randomeffectsummary
  names(aghq_random) <- c("mean", "median", "mode", "sd", "2.5%", "97.5%", "variable")

  aghq_random %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    # Warning: this rownumber as index only works when you have one random effect!
    dplyr::mutate(variable = paste0(variable, "[", rowname, "]")) %>%
    dplyr::rename(parameter = variable) %>%
    dplyr::select(parameter, mean, sd)

  dplyr::bind_rows(aghq_hyper, aghq_random) %>%
    dplyr::mutate(method = "aghq")
}

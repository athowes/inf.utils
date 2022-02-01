aghq_summary <- function(quad) {
  aghq_hyper <- aghq::summary(quad)$aghqsummary$summarytable %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(parameter = rowname) %>%
    dplyr::select(parameter, mean, sd)

  aghq_random <- aghq::summary(quad)$randomeffectsummary %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    # Warning: this rownumber as index only works when you have one random effect!
    dplyr::mutate(variable = paste0(variable, "[", rowname, "]")) %>%
    dplyr::rename(parameter = variable) %>%
    dplyr::select(parameter, mean, sd)

  dplyr::bind_rows(aghq_hyper, aghq_random) %>%
    dplyr::mutate(method = "aghq")
}

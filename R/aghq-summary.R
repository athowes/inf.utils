aghq_summary <- function(quad) {
  aghq_hyper <- aghq::summary(quad)$aghqsummary$summarytable %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    rename(parameter = rowname) %>%
    select(parameter, mean, sd)

  aghq_random <- aghq::summary(quad)$randomeffectsummary %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    # Warning: this rownumber as index only works when you have one random effect!
    mutate(variable = paste0(variable, "[", rowname, "]")) %>%
    rename(parameter = variable) %>%
    select(parameter, mean, sd)

  bind_rows(aghq_hyper, aghq_random) %>%
    mutate(method = "aghq")
}

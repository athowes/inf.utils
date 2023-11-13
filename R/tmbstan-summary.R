tmbstan_summary <- function(fit) {
  rstan::summary(fit)[["summary"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("parameter") %>%
    dplyr::select(parameter, mean, sd) %>%
    dplyr::mutate(method = "tmbstan")
}

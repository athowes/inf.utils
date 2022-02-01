tmbstan_summary <- function(fit) {
  rstan::summary(fit)[["summary"]] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("parameter") %>%
    select(parameter, mean, sd) %>%
    mutate(method = "tmbstan")
}

replace_duplicate_colnames <- function(samples) {
  unique_names <- unique(colnames(samples))

  lapply(unique_names, function(name) {
    length <- table(colnames(samples))[[name]]
    cols <- which(colnames(samples) == name)
    if(length > 1) {
      setNames(samples[, cols], paste0(name, "[", 1:length, "]"))
    } else {
      setNames(as.data.frame(samples[, cols]), name)
    }
  }) %>%
    bind_cols()
}

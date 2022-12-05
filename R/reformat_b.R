#' Reformat the data for bayesian cNMA
#'
#' @param df Dataframe to reformat
#' @param maxna Maximum number of arms
#' @param ns Number of studies
#'
#' @return List of variables for Bayesian cNMA
#' @export
reformat_b <- function(df, maxna, ns) {
  variables <- list()
  for (i in 1:ncol(df)) {
    variables[[i]] <- df[, i]
  }
  todf <- function(df, nrow, ncol) {
    output <- matrix(df, nrow, ncol, byrow = TRUE) %>%
      data.frame()
    colnames(output) <- 1:ncol
    return(output)
  }
  variables <- lapply(variables, todf, nrow = ns, ncol = maxna)
  names(variables) <- colnames(df)
  return(variables)
}

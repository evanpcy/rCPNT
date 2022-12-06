#' Reformat the data for Frequentist NMA
#'
#' @param df Dataframe to reformat
#' @param maxna Maximum number of arms
#' @param ns Number of studies
#'
#' @return Dataframe for Frequentist NMA
#' @export
#'
#' @examples data(example)
#' reformat_nma(example, 3, 5)
reformat_nma <- function(df, maxna, ns){

  df[df == ""] <- NA

  #pairwise
  studlab <- df$StudyN %>% unique

  output <- df %>%
    select(Treatment) %>%
    t() %>%
    unlist() %>%
    matrix(byrow = TRUE, nrow = {{ns}}) %>%
    data.frame(studlab, .)

  output <- df %>%
    select(n, mean, SD) %>%
    t() %>%
    unlist() %>%
    matrix(byrow = TRUE, nrow = {{ns}}) %>%
    data.frame(output, .)

  names <- apply(expand.grid(c("n", "mean", "sd"), 1:maxna), 1, paste, collapse="")

  colnames(output) <- c("stublab", paste0("t", 1:maxna), names)

  return(output)
}

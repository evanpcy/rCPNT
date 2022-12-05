#' Reformat the data for frequentist cNMA
#'
#' @param df Dataframe to reformat
#' @param range Columns to reformat (components only)
#' @param maxna Maximum number of arms
#' @param ns Number of studies
#'
#' @return Dataframe for Frequentist cNMA
#' @export
reformat_f <- function(df, range, maxna, ns){
  df_comp <- df %>%
    mutate(across({{range}}, ~ case_when(. == 1 ~ cur_column()), .names = "new_{col}")) %>%
    unite(arm, starts_with("new"), na.rm = TRUE, sep = " + ")
  df_comp[df_comp == ""] <- NA

  #pairwise
  studlab <- df_comp$StudyN

  output <- df_comp %>%
    select(arm) %>%
    t() %>%
    unlist() %>%
    matrix(byrow = TRUE, nrow = {{ns}}) %>%
    data.frame(studlab, .)

  output <- df_comp %>%
    select(n, mean, SD) %>%
    t() %>%
    unlist() %>%
    matrix(byrow = TRUE, nrow = {{ns}}) %>%
    data.frame(output, .)

  names <- apply(expand.grid(c("n", "mean", "sd"), 1:maxna), 1, paste, collapse="")

  colnames(output) <- c("stublab", paste0("t", 1:maxna), names)

  return(output)
}

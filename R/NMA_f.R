#' Perform Frequentist NMA using netmeta
#'
#' @param df Dataframe for analysis
#' @param maxna Maximum number of arms
#' @param ns Number of studies
#'
#' @return Results of Frequentist NMA
#' @export
#'
#' @examples data(example)
#' NMA_f(example, 3, 5)
NMA_f <- function(df, maxna, ns){
  formatted <- reformat_nma(df, maxna, ns)

  target <- c("t", "n", "mean", "sd")
  listed <- lapply(target, function(x) as.list(select(formatted, starts_with(x))))

  pairwised <- netmeta::pairwise(treat = listed[[1]],
                                 n = listed[[2]],
                                 mean = listed[[3]],
                                 sd = listed[[4]],
                                 data = formatted,
                                 studlab = formatted$studlab)

  trts <- df %>%
    select(Treatment) %>%
    unlist %>%
    .[!is.na(.)] %>%
    unique

  result <- netmeta(TE, seTE, treat1, treat2, studlab,
          data = pairwised, reference.group = "Therapy A",
          sm = "MD", comb.fixed = FALSE, comb.random = TRUE,
          seq = trts, nchar.trts = 8)

  return(result)
}

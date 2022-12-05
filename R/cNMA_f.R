#' Perform Frequentist cNMA using netmeta
#'
#' @param df Dataframe for analysis
#' @param range Columns with the components
#' @param maxna Maximum number of arms
#' @param ns Number of studies
#' @param chkident Boolean for details.chkident
#'
#' @return Results of Frequentist cNMA
#' @export
cNMA_f <- function(df, range, maxna, ns, chkident = FALSE){
  formatted <- reformat_f(df, {{range}}, maxna, ns)

  target <- c("t", "n", "mean", "sd")
  listed <- lapply(target, function(x) as.list(select(formatted, starts_with(x))))

  pairwised <- netmeta::pairwise(treat = listed[[1]],
                                 n = listed[[2]],
                                 mean = listed[[3]],
                                 sd = listed[[4]],
                                 data = formatted,
                                 studlab = formatted$studlab)

  pairwised %$%
    netconnection(treat1, treat2, studlab) %>% print


  trts <- formatted %>%
    select(starts_with("t")) %>%
    unlist %>%
    .[!is.na(.)] %>%
    unique

  result <- pairwised %$%
    discomb(TE, seTE, treat1, treat2, studlab,
            sm = "MD", comb.random = TRUE, seq = trts, details.chkident = {{chkident}})

  print(result$C.matrix)
  return(result)
}

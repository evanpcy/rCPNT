#' Perform Bayesian cNMA using rjags
#'
#' @param df Dataframe for analysis
#' @param range Columns with the values and components
#' @param maxna Maximum number of arms
#' @param ns Number of studies
#' @param model Model for cNMA
#' @param nc Number of components
#'
#' @return Results of Bayesian cNMA
#' @export
cNMA_b <- function(df, range, maxna, ns, model, nc){
  parameters <- reformat_b(select(df, {{range}}), maxna, ns)

  names(parameters) <- c("n", "y", "sd", "se", paste0("c", 1:nc))

  na <- df[1:ns * maxna, "ArmNo"]
  parameters <- c(parameters, c(Ns = ns, Nc = nc, na = list(na)))

  model.spec <- textConnection(model)

  parameters$se <- NULL
  jags.m <- 0
  jags.m <- jags.model(model.spec, data = parameters, n.chains = 2, n.adapt = 5000)

  params <- c("tau", "d")
  closeAllConnections()

  samps <- coda.samples(jags.m, params, n.iter = 25000)
  MCMCtrace(samps, pdf = FALSE, params = "d")
  result <- MCMCsummary(samps, probs = c(0.5, 0.05, 0.95))

  rownames(result)[1:nc] <- tail(colnames(df), nc)
  round(result[, c(3:5)], digits = 3) %>% print

  return(result)
}

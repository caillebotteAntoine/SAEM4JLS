
#' Burn-in with Metropolis Hastings
#'
#' @param x object return by a MH function
#' @param n number of samples being discarded
#'
#' @return the MCMC without the n first samples
MH_burnin <- function(x, n)
{
  if('value' %in% names( attributes(x) ) )
    attr(x,'value') <- attr(x,'value') %>% filter(iter > n)

  if('acceptation' %in% names( attributes(x) ) )
    attr(x,'acceptation') <- attr(x,'acceptation') %>% filter(iter > n)

  return(x)
}

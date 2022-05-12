
#' Burn-in with Metropolis Hastings
#'
#' @param x object return by a MH function
#' @param n number of samples being discarded
#'
#' @return the MCMC without the n first samples
MH_burnin <- function(x, n)
{
  if('value' %in% names( attributes(x) ) )
  {
    v <- attr(x,'value')
    if('matrix' %in% class(x))
    {
      if(max(v$iter) > n)
        return(v %>% filter(iter > n ))
      #( n:length(v) %>% lapply(function(i)v[[i]] ) )
    }
    else{
      if(nrow(v) > n)
        return( v[n:nrow(v),] )
    }
  }
  return(NULL)
}

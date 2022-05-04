#' Metropolis Hastings MCMC
#'
#' @param niter number of iteration
#' @param x the initial value of the MCMC
#' @param transition function that apply the transition between x and the new proposal
#' @param loglik function that return the acceptation rate
#' @param ... the parameter for loglik function
#'
#' @export
#'
#' @examples
MH <- function(niter, x, transition, loglik, ...)
{
  args <- list(...)
  x_lik <- do.call(loglik, c(list(x), args))
  
  a <- attributes(x)
  a$naccept <- ifelse(is.null(a$naccept), 0, a$naccept)
  a$niter <- ifelse(is.null(a$niter), 0, a$niter) + niter
  
  for(i in 1:niter)
  {
    #transition de x vers x_new
    x_new <- transition(x)
    x_new_lik <- do.call(loglik, c(list(x_new), args))
    
    if(x_new_lik > x_new_lik || runif(1) < exp(x_new_lik - x_lik) )
    {
      x <- x_new
      x_lik <-x_new_lik
      a$naccept <- a$naccept + 1
    }
  }
  
  a$acceptation_rate <- a$naccept/a$niter
  a$accept_rate <- x_lik
  attributes(x) <- a
  return(x)
}
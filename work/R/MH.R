#' Metropolis Hastings MCMC
#'
#' @param niter number of iteration
#' @param x the initial value of the MCMC
#' @param transition function that apply the transition between x and the new proposal
#' @param loglik function that return the acceptation rate, the first argument must be the proposition value in the algorithm
#' @param ... the parameter for loglik function
#'
#' @export
MH <- function(niter, x, transition, loglik, ..., verbatim = F)
{
  args <- list(...) # initialization of the parameters for the calculation of the acceptance rate
  x_lik <- do.call(loglik, c(list(x), args))

  a <- attributes(x)
  a$naccept <- ifelse(is.null(a$naccept), 0, a$naccept)
  a$niter <- ifelse(is.null(a$niter), 0, a$niter) + niter

  if(verbatim)#saving the values of the chain
  {
    if('matrix' %in% class(x))
      a$value <- list(x)
    else
      a$value <- matrix(x, ncol = length(x))
  }

  #start of the algorithm of metropolis hastings
  for(i in 1:niter)
  {
    #transition de x vers x_new
    x_new <- transition(x)
    #calculation of the new acceptance rate
    x_new_lik <- do.call(loglik, c(list(x_new), args))
    if(x_new_lik > x_lik || runif(1) < exp(x_new_lik - x_lik) ) #min(1,\rho(x,xnew)) avec
    {
      x <- x_new
      x_lik <- x_new_lik
      a$naccept <- a$naccept + 1
    }
    if(verbatim) #memorization of the chain{
    {
      if('matrix' %in% class(x))
        a$value <- append(a$value, list(x))
      else
        a$value <- rbind(a$value, x)
    }

  }

  a$acceptation_rate <- a$naccept/a$niter
  attributes(x) <- a
  return(x)
}




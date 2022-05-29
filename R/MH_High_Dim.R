#' Metropolis Hastings MCMC
#'
#' Will run the metropolis hasting algorithm on the rows of x.
#' For example if we have x = (x_{i,j}), we will do an iteration of MH for each x_{i,.}
#'
#' @param niter number of iteration
#' @param x the initial value of the MCMC
#' @param transition function that apply the transition between x and the new proposal
#' @param loglik function that return the acceptation rate, the first argument must be the proposition value in the algorithm
#' @param ... the parameter for loglik function
#' @param verbatim boolean value, true will return the whole chain, false will return by default only the last value
#'
#' @export
MH_High_Dim <- function(niter, x, transition, loglik, ..., verbatim = F)
{
  #check that x have a good shape for the high dimension
  stopifnot('matrix' %in% class(x) || 'MH_res' %in% class(x))

  args <- list(...) # initialization of the parameters for the calculation of the acceptance rate
  x_lik <- do.call(loglik, c(list(x), args)) #log-Likelihood of the initiale value x
  dim <- nrow(x) #dimension of the vector x

  a <- attributes(x)
  if(is.null(a$acceptation))
    a$acceptation <- data.frame(iter = 0, naccept = 0) #nombre d'iteration et d'acceptation

  if(verbatim && is.null(a$value))#saving the values of the chain
    a$value <- data.frame(x, iter = 0, id = factor(1:nrow(x)))

  #start of the algorithm of metropolis hastings
  for(i in 1:niter)
  {
    #calculation of the new acceptance rate for each component
    x_new <- x
    naccept <- last(a$acceptation$naccept)
    for(k in 1:dim)
    {
      #transition de x[k] vers x_new[k]
      x_new[k,] <- transition(x[k,])
      x_new_lik <- do.call(loglik, c(list(x_new), args))
      if(x_new_lik > x_lik || log(runif(1)) < x_new_lik - x_lik ) #min(1,\rho(x,xnew))
      {
        x[k,] <- x_new[k,]
        x_lik <- x_new_lik
        naccept <- naccept + 1
      }
    }
    a$acceptation <- rbind(a$acceptation, c(last(a$acceptation$iter) + dim, naccept))

    if(verbatim) #memorization of the chain
      a$value <- rbind(a$value, data.frame(x, iter = i, id = 1:nrow(x)))
  }

  attributes(x) <- a
  class(x) <- c('MH_res', 'matrix')
  return(x)
}

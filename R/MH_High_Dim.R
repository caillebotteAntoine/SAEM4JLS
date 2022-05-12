#' Metropolis Hastings MCMC
#'
#' Will run the metropolis hasting algorithm on the columns of x.
#' For example if we have x = (x_{i,j}), we will do an iteration of MH for each x_{.,j}
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
  stopifnot('matrix' %in% class(x))

  args <- list(...) # initialization of the parameters for the calculation of the acceptance rate
  x_lik <- do.call(loglik, c(list(x), args))
  dim <- ncol(x) #dimension of the vector x

  a <- attributes(x)
  a$naccept <- ifelse(is.null(a$naccept), 0, a$naccept) #Nombre d'acceptation
  a$niter <- ifelse(is.null(a$niter), 0, a$niter) + niter*dim #nombre d'iteration

  if(verbatim)#saving the values of the chain
    a$value <- data.frame(x, iter = 0)

  #start of the algorithm of metropolis hastings
  for(i in 1:niter)
  {
    #calculation of the new acceptance rate for each component
    x_new <- x
    for(k in 1:dim)
    {
      #transition de x[k] vers x_new[k]
      x_new[,k] <- transition(x[,k])
      x_new_lik <- do.call(loglik, c(list(x_new), args))
      if(x_new_lik > x_lik || log(runif(1)) < x_new_lik - x_lik ) #min(1,\rho(x,xnew))
      {
        x[,k] <- x_new[,k]
        x_lik <- x_new_lik
        a$naccept <- a$naccept + 1
      }
    }
    if(verbatim) #memorization of the chain
      a$value <- rbind(a$value, data.frame(x, iter = i))
  }

  a$acceptation_rate <- a$naccept/a$niter
  a$acceptation_score <- x_lik
  attributes(x) <- a
  return(x)
}

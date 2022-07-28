



Gibbs_sampler_step <- function(x0, ..., verbatim = F)
{
  #check that x have a good shape for the high dimension
  #stopifnot('chain' %in% class(x0))

  #x_lik <- do.call(loglik, c(list(x), args)) #log-Likelihood of the initiale value x

  x_eval <- get_rate(x0, x0, ...)

  x_new <- as.matrix(x0)
  dim <- length(x0)

  naccept <- x0@acceptation[length(x0@acceptation)]
  for(j in 1:dim)
  {
    #Sampling
    x_new[j] <- rnorm(1, x0[j], x0@sd)
    x_new_eval <- get_rate(x0, x_new, ...)

    if(x_new_eval > x_eval || log(runif(1)) < x_new_eval - x_eval ) #min(1,\rho(x,xnew))
    {
      x0[j] <- x_new[j]
      x_eval <- x_new_eval
      naccept <- naccept + 1
    }else{
      x_new[j] <- x0[j]
    }
  }

  #update : number of acceptation
  x0@acceptation <- append(x0@acceptation, naccept)

  if(verbatim)
    x0@chain <- rbind(x0@chain, c(x0, iteration = nrow(x0@chain)))

  return(x0)
}





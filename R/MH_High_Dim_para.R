#' Metropolis Hastings MCMC
#'
#' Will run the metropolis hasting algorithm on the rows of x.
#' For example if we have x = (x_{i,j}), we will do an iteration of MH for each x_{i,.}
#'
#' !!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!
#'This code is paralized! Your variables must be arranged in columns and the individuals must be on the rows!
#'Otherwise the first variable to be calculated by the algorithm will be forced to take absurd values to counterbalance the absence of update of the other variables!
#'Keep in mind that the variables must be able to balance each other and this is possible IF AND ONLY IF they are on the columns!
#'on this, good programming and live long#'
#' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#' @param niter number of iteration
#' @param x the initial value of the MCMC
#' @param transition function that apply the transition between x and the new proposal
#' @param loglik function that return the acceptation rate, the first argument must be the proposition value in the algorithm
#' @param ... the parameter for loglik function
#' @param verbatim boolean value, true will return the whole chain, false will return by default only the last value
#'
#' @export
MH_High_Dim_para <- function(niter, x, sd, loglik, ..., verbatim = F, cores = 1)
{
  #check that x have a good shape for the high dimension
  stopifnot('matrix' %in% class(x) || 'MH_res' %in% class(x))

  args <- list(...) # initialization of the parameters for the calculation of the acceptance rate
  x_lik <- do.call(loglik, c(list(x), args)) #log-Likelihood of the initiale value x
  dim <- nrow(x) #dimension of the vector x

  stopifnot(length(sd) == 1 || length(sd) == ncol(x) ) #Varification qu'il y a bien assez de variance

  #start of the algorithm of metropolis hastings
  #On considere chaque composante une part une
  #On cree une fct pour que furrr puis parallelisé sur la dimension
  x_new <- x #initialisation de la nouvelle proposition

  f <- function(k)
  {
    #Initialisation des variables de retour
    if(verbatim)
      value <- c(x[k,], id = k, iter = 0)
    acceptation <- rep(0, niter)#nombre d'iteration et d'acceptation
    naccept <- 0

    for(i in 1:niter)
    {
      #Transition en partant de x_new selon les ecart type sd
      x_new[k,] <- rnorm(length(x[k,]), x[k,], sd)
      x_new_lik <- do.call(loglik, c(list(x_new), args))
      if(x_new_lik > x_lik || log(runif(1)) < x_new_lik - x_lik ) #min(1,\rho(x,xnew))
      {
        x[k,] <- x_new[k,]
        x_lik <- x_new_lik
        naccept <- naccept + 1
      }
      #mise a jour du nombre d'acceptation
      acceptation[i] <- naccept
      if(verbatim)
        value <- rbind(value, c(x[k,], id = k, iter = i))
    }

    res <- x[k,] #on retourne uniquement la kème composante (l'unique modifier)
    attr(res, 'acceptation') <- acceptation
    if(verbatim)
      attr(res, 'value') <- as.data.frame(value, row.names = F)

    return(res)
  }

  #switch to multisession mode if there are available cores
  if(cores != 1) plan(multisession, workers = cores)
  res <- future_map(1:dim, f, .options = furrr_options(seed = T))
  plan(sequential)

  #unlist of the paralized result ... a little boring

  # === Acceptation rate === #
  #If there is already an acceptance rate of calculate we want to keep it
  a <- attr(x, 'acceptation')
  if(is.null(a))
    a <- data.frame(naccept = 0, iter = 0)

  #same for the value (verbatim mode)
  v <- attr(x, 'value')

  #Get only the final values to structure the variable to return
  x <- res %>% sapply(function(x) as.numeric(x)) %>% matrix(ncol = dim) %>% t

  attr(x, 'acceptation') <- res %>% sapply(function(x) attr(x, 'acceptation')) %>% apply(1, sum) %>%
    { data.frame(naccept = c(a$naccept, last(a$naccept) + .), iter = c(a$iter, last(a$iter) + 1:niter*dim) ) }

  if(verbatim)
  {
    value <- do.call(rbind , res %>% lapply(function(x) attr(x, 'value')))  %>% as.data.frame

    if(!is.null(v))
      value <- rbind(v, value %>% mutate(iter = last(v$iter) + iter))

    attr(x, 'value') <- value
  }

  class(x) <- c('MH_res', 'matrix') #it is a matrix and also a result of a function MH
  return(x)
}









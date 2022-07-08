
#' Gibbs Sampler MCMC
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
#'
#' @param niter number of iteration
#' @param loglik function that return the acceptation rate, the first argument must be the proposition value in the algorithm
#' @param ... the parameter for loglik function
#' @param verbatim boolean value, true will return the whole chain, false will return by default only the last value
#' @param sd
#' @param cores
#' @param x0
#'
#' @example
#'n <- 10000# nombre d'observation
#'x <- c(3,6) #moyenne et variance
#'obs <- rnorm(n, x[1], sqrt(x[2])) #tirage dans une loi normale
#'
#'transi = function(x) rnorm(length(x), x, 0.1) #fonction de transition entre x et xnew
#'loglik <- function(x, obs) sum( -1/2*log(2*pi*x[2]) -1/(2*x[2])* (obs-x[1])^2 ) #vraisemblence
#'
#'#Metropolis hasting
#'x0 <- matrix(c(1,1), nrow = 1)
#'res <- MH_High_Dim_para_future(500, x0, sd = 0.1 , loglik, obs, verbatim = T, cores = 1)
#'plot(res, nrow = 2)
#'
#'#On peut relancer l'algorithme tout en gardant les informations déjà compiler
#'MH_High_Dim_para_future(100, res, sd = 0.1 , loglik, obs, verbatim = T, cores = 1) %>%
#'  plot(nrow = 2)
MH_Gibbs_Sampler_future <- function(niter, x0, sd, loglik, ..., verbatim = F, cores = 1)
{
  #check that x have a good shape for the high dimension
  stopifnot('matrix' %in% class(x0) || 'MH_res' %in% class(x0))
  x <- as.matrix(x0)

  args <- list(...) # initialization of the parameters for the calculation of the acceptance rate
  x_lik <- do.call(loglik, c(list(x), args)) #log-Likelihood of the initiale value x
  dim <- nrow(x) #dimension of the vector x
  ncomp <- ncol(x) #number of composante

  stopifnot(length(sd) == 1 || length(sd) == ncol(x)) #Vérification qu'il y a bien assez de variance

  #start of the algorithm of metropolis hastings
  #On considere chaque composante une part une
  #On cree une fct pour que furrr puis parallelisé sur la dimension
  x_new <- x #initialisation de la nouvelle proposition

  f <- function(k)
  {
    #Initialisation des variables de retour
    if(verbatim)
      value <- c(x[k,], id = k, iteration = 0)
    acceptation <- matrix(rep(0, niter*ncomp), ncol = ncomp)#nombre d'iteration et d'acceptation
    naccept <- rep(0, ncomp)

    for(i in 1:niter)
    {
      #Transition en partant de x_new selon les ecart type sd
      x_prop <- rnorm(length(x[k,]), x[k,], sd)
      for(h in 1:ncomp)
      {
        x_new[k,h] <- x_prop[h]
        x_new_lik <- do.call(loglik, c(list(x_new), args))
        if(x_new_lik > x_lik || log(runif(1)) < x_new_lik - x_lik ) #min(1,\rho(x,xnew))
        {
          x[k,h] <- x_new[k,h]
          x_lik <- x_new_lik
          naccept[h] <- naccept[h] + 1
        }else{
          x_new[k,h] <- x[k,h]
        }
      }
      #mise a jour du nombre d'acceptation
      acceptation[i,] <- naccept
      if(verbatim)
        value <- rbind(value, c(x[k,], id = k, iteration = i))
    }

    res <- x[k,] #on retourne uniquement la kème composante (l'unique modifier)
    attr(res, 'acceptation') <- acceptation
    if(verbatim)
      attr(res, 'value') <- value

    return(res)
  }

  #switch to multisession mode if there are available cores
  if(cores >= 2)
  {
    plan(multisession, workers = cores)
    res <- future_map(1:dim, f, .options = furrr_options(seed = T))
    plan(sequential)
  }else{
    res <- purrr::map(1:dim, f)
  }
  #unlist of the paralized result ... a little boring

  #Get only the final values to structure the variable to return
  x <-  MH_res( t(matrix( sapply(res, function(x) as.numeric(x)), ncol = dim )) )


  # === Acceptation rate === #
  x@acceptation <- lapply(res, function(x) attr(x, 'acceptation'))  %>%
    reduce(`+`)

  if(verbatim)
    x@chain <- do.call(rbind , lapply(res, function(x) attr(x, 'value')))

  return(SAEM4JLS::link(x0, x) )
}




#' Stochastic Approximation of Expectation-Maximization
#'
#' @param niter
#' @param niter.MH
#' @param M number of MCMC to compute with MH
#' @param u
#' @param param
#' @param P
#' @param exhaustive
#' @param simulation function for simulation step
#' @param maximisation function for maximisation step
#'
#' @return
#' @export
#'
#' @examples
SAEM <- function(niter, niter.MH, u, param, Phi, exhaustive, Z, simulation, maximisation, eps = 1e-3,verbatim = F)
{
  #Initialisation des listes des parametres
  parameter <- function(i) { para %>% lapply(function(x) x[i,])}

  para <- param %>%
    lapply(function(x)
    { y <- matrix(NA, ncol = length(x), nrow = niter+1)
    y[1,] <- x ; return(y) })

  Sh <- do.call(exhaustive, Z %>% lapply(function(z) z[[1]])) ;
  Sh[1:length(Sh)] <- 0

  M <- length(Z[[1]])
  if(verbatim)
    value <- Z %>% lapply(function(z) list(z[[1]]))


  h <- 2
  stopCondi <- function(h) para %>% lapply(function(p) abs(p[h-1] - p[h]) < eps) %>% as.logical %>% prod
  cmp <- 0
  while(h < niter+1 && cmp < 20 ) #  for(h in 1:niter+1)
  {

    # --- Step S : simulation --- #
    Phih <-  do.call(Phi, parameter(h-1) )

    # --- Metropolis Hastings --- #
    Z <- do.call(simulation, c(list(niter.MH, Phih), Z ))
    if(verbatim)
    {
      for(i in 1:length(Z))
      {
        value[[i]] <- append(value[[i]], list(Z[[i]][[1]])) #On mémorise que la premier chaine de chaque Z_i
      }
    }

    # --- Step A : approximation --- #
    Sh <- (1-u(h))*Sh+u(h)*(1:M %>%
                              sapply(function(i) do.call(exhaustive, Z %>% lapply(function(z)z[[i]]) )) %>%
                              apply(1, mean)) #mean of { exhaustive(Z_i) ; i \in \{1, ..., niter.MJ\} }

    # --- Step M : maximisation --- #
    res <- maximisation(Sh)
    for( i in names(para)) para[[i]][h,] <- res[[i]] #mise à jour de chaque ligne de chaque parametre

    cmp <- ifelse(stopCondi(h), cmp + 1, 0) #compteur après convergence des para pour verifier la CV
    h <- h + 1
  }

  if(verbatim)
    attr(Z, 'value') <- value

  if(cmp >= 20)
  {
    attr(para, 'stop') <- h - cmp
  }

  res <- list(parameter = para, Z = Z)
  class(res) <- 'SAEM'

  return(res)
}







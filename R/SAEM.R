
#' Stochastic Approximation of Expectation-Maximization
#'
#' @param niter number of iterations for SAEM loop
#' @param niter.MH number of iterations for metropolis hastings algorithm
#' @param param
#' @param exhaustive
#' @param simulation function for simulation step
#' @param maximisation function for maximisation step
#' @param Phi
#' @param Z list of latent variables (with var names)
#' @param eps Stop criterion
#' @param verbatim 1 message during the main loop and estimation of the remaining execution time; 2 same as 1 with in return values of the simulations of the latent variables
#' @param burnin step at which burn-in period ends
#' @param coef.burnin 1/(step+1-burnin)^coef.burnin
#'
#' @return
#' @export
#'
#' @examples
SAEM <- function(niter, niter.MH, param, Phi, exhaustive, Z, simulation, maximisation, burnin = NULL, coef.burnin = 1, eps = 1e-3, verbatim = F)
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
  if(verbatim == 2)
    value <- Z %>% lapply(function(z) list(z[[1]]))

  #Burn-in
  if(is.null(burnin)) burnin <- niter
  u <- function(k) ifelse(k<burnin, 1, 1/((k+1-burnin)^coef.burnin ))

  h <- 2
  stopCondi <- function(h) para %>% lapply(function(p) abs(p[h-1] - p[h]) < eps) %>% as.logical %>% prod
  cmp <- 0

  start <- Sys.time() #execution time
  message('--- SAEM started ! ---')

  while(h <= niter+1 && cmp < 20 ) #  for(h in 1:niter+1)
  {
    if(verbatim >= 1)
    {
      elasped <- difftime(Sys.time(), start , units = "secs") %>% round(2)
      step.estimation <- elasped/(h-2)
      message(paste0('SAEM step = ', h-1, ', remaining = ', niter+1-h,
                     ', times elasped = ', round(elasped,1),
                     's, estimated time for one step = ', round(step.estimation,1),
                     's, estimated time remaining = ', round(step.estimation * (niter+2-h),1), 's' ))
    }
    # --- Step S : simulation --- #
    Phih <-  do.call(Phi, parameter(h-1) )

    # --- Metropolis Hastings --- #
    Z <- do.call(simulation, c(list(niter.MH, Phih), Z ))
    if(verbatim == 2)
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

  message('--- SAEM ended ! ---')

  if(verbatim==2)
    attr(Z, 'value') <- value

  if(cmp >= 20)
  {
    attr(para, 'stop') <- h - cmp
  }

  res <- list(parameter = para, Z = Z, times_elasped = difftime(Sys.time(), start , units = "secs"))
  class(res) <- 'SAEM'

  return(res)
}







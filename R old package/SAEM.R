

#' Stochastic Approximation Expectation Maximization (SAEM) algorithm
#'
#' SAEM algorithm perform parameter estimation for nonlinear mixed effects


#' @param niter number of iterations for SAEM loop
#'
#' @param niter.MH number of iterations for metropolis hastings algorithm
#' @param param
#' @param exhaustive
#' @param simulation function for simulation step
#' @param maximisation function for maximisation step
#' @param Phi
#' @param Z list of latent variables (with var names) MUST BE MATRIX
#' @param eps Stop criterion
#' @param verbatim 1 message during the main loop and estimation of the remaining execution time; 2 same as 1 with in return values of the simulations of the latent variables
#' @param burnin step at which burn-in period ends
#' @param coef.burnin 1/(step+1-burnin)^coef.burnin
#'
#' @return
#' @export
#'
#' @examples
#'
#'# Y_i,j = a_i*t_j + eps_{i,j}
#'# a_i ~ N(mu, omega2)
#'m <- function(t,a) a*t
#'
#'n <- 10
#'a <- rnorm(n, 4, sqrt(0.1))
#'nt <- 10
#' t <- seq(0,1, length.out = nt)
#' eps <- rnorm(nt*n, 0, 0.5)
#' Y <- data.frame(id = rep(1:n, each = nt),
#'                 obs = m(rep(a, each = nt), rep(t, n)) + eps)
#'
#' #log(f(Y,a)) = log(f(Y|a)) + log(f(a)) = <S(a),Phi(sigma^2,mu,omega^2)> + psi
#'
#' Phi <- function(sigma2, mu, omega2) c(-n*nt/(2*sigma2),  -n/(2*omega2), n*mu/omega2)
#'
#' S <- function(a)
#'   c(mean( (Y$obs - m(rep(a, each = nt), rep(t, n)) )^2 ),
#'     mean(a),
#'     mean(a^2))
#'
#' loglik <- function(a, Phih) Phih*S(a)
#'
#' simulation <- function(niter,iter, Phih, a)
#'   list(a = list(MH_High_Dim_para_future(niter, a[[1]], sd = 0.05, loglik, Phih, cores = 1 )))
#'
#' maximisation <- function(S) list(sigma2 = S[1], mu = S[2], omega2 = S[3]-S[2]^2)
#'
#' res <- SAEM(100, 10, list(sigma2 = 0.1, mu = 1, omega2 = 0.1), Phi, S,
#'             Z = list(a = list(matrix(rep(1,n), ncol = 1))), simulation, maximisation, verbatim = 2)
#'
#' plot(res)
SAEM <- function(niter, niter.MH, param, Phi, exhaustive, Z, simulation, maximisation,
                 burnin = NULL, coef.burnin = 1, eps, verbatim = F)
{

  Sh <- do.call(exhaustive, Z %>% lapply(function(z) z[[1]])) ;
  Sh[1:length(Sh)] <- 0

  #Initialisation des listes des parametres
  para <- SAEM_res(param = param, niter = niter)

  M <- length(Z[[1]])
  if(verbatim >= 2) #Si on veut garder en mémoire la chaine on founrie les variables Z
    para <- SAEM4JLS::addchain(para, Z)

  #Burn-in
  u <- burnin_fct(ifelse(is.null(burnin), niter, burnin), coef.burnin, scale = 1)

  MH.iter <- niter.MH
  if(!is.function(niter.MH)) MH.iter <- function(k) niter.MH

  h <- 2
  step.before.stop <- 20
  if(missing(eps)){
    eps = 1e-3
    step.before.stop = 10000
  }
  stopCondi <- function(h) prod(as.logical(lapply(para, function(p) abs(p[h-1] - p[h]) < eps) ))
  cmp <- 0

  start <- Sys.time() #execution time
  last.msg <- 0
  if(verbatim == 1 || verbatim == 2) message('--- SAEM started ! ---')

  while(h <= niter+1 && cmp < step.before.stop ) #  for(h in 1:niter+1)
  {
    if(verbatim == 1 || verbatim == 2)
    {
      if(Sys.time() - last.msg > 5){
        last.msg <- Sys.time()
        elasped <- round( difftime(Sys.time(), start , units = "secs"), 2)
        step.estimation <- elasped/(h-2)
        message(paste0('step = ', h-1, ', remaining = ', niter+1-h,
                       ', times elasped = ', round(elasped,1),
                       's, estimated time : for one step = ', round(step.estimation,1),
                       's, remaining = ', round(step.estimation * (niter+2-h),1), 's' ))
      }
    }
    # --- Step S : simulation --- #
    Phih <-  do.call(Phi, para[h-1] )

    # --- Metropolis Hastings --- #
    Z <- do.call(simulation, c(list(MH.iter(h-1), h-1, Phih), Z ))
    #On mémorise que la premier chaine de chaque Z_i
    if(verbatim >= 2) para <- SAEM4JLS::addchain(para, Z)

    # --- Step A : approximation --- #
    Sh <- (1-u(h))*Sh+u(h)*( apply(#mean of { exhaustive(Z_i) ; i \in \{1, ..., niter.MJ\} }
                                    sapply(1:M, function(i) do.call(exhaustive, Z %>% lapply(function(z)z[[i]]) )),
                                    1, mean) )

    # --- Step M : maximisation --- #
    res <- maximisation(Sh)
    for( i in names(res)) para[[i]][h,] <- res[[i]] #mise à jour de chaque ligne de chaque parametre

    cmp <- ifelse(stopCondi(h), cmp + 1, 0) #compteur après convergence des para pour verifier la CV
    h <- h + 1
  }

  if(verbatim == 1 || verbatim == 2) message('--- SAEM ended ! ---')

  if(cmp >= 20) attr(para, 'stop') <- h - cmp

  para@Z <- Z
  para@times_elasped = difftime(Sys.time(), start , units = "secs")

  return(para)
}



# SAEM <- function(niter, niter.MH, param, Phi, exhaustive, Z, simulation, maximisation,
#                  burnin = NULL, coef.burnin = 1, eps = 1e-3, verbatim = F)
# {
#   #Initialisation des listes des parametres
#   parameter <- function(i) { para %>% lapply(function(x) x[i,])}
#
#   para <- param %>%
#     lapply(function(x)
#     { y <- matrix(NA, ncol = length(x), nrow = niter+1)
#     y[1,] <- x ; return(y) })
#
#   Sh <- do.call(exhaustive, Z %>% lapply(function(z) z[[1]])) ;
#   Sh[1:length(Sh)] <- 0
#
#   M <- length(Z[[1]])
#   if(verbatim >= 2)
#     value <- Z %>% lapply(function(z) list(z[[1]]))
#
#   #Burn-in
#   if(is.null(burnin)) burnin <- niter
#   u <- function(k) ifelse(k<burnin, 1, 1/((k+1-burnin)^coef.burnin ))
#
#   MH.iter <- niter.MH
#   if(!is.function(niter.MH)) MH.iter <- function(k) niter.MH
#
#   h <- 2
#   stopCondi <- function(h) para %>% lapply(function(p) abs(p[h-1] - p[h]) < eps) %>% as.logical %>% prod
#   cmp <- 0
#
#   start <- Sys.time() #execution time
#   if(verbatim == 1 || verbatim == 2) message('--- SAEM started ! ---')
#
#   while(h <= niter+1 && cmp < 20 ) #  for(h in 1:niter+1)
#   {
#     if(verbatim == 1 || verbatim == 2)
#     {
#       elasped <- difftime(Sys.time(), start , units = "secs") %>% round(2)
#       step.estimation <- elasped/(h-2)
#       message(paste0('SAEM step = ', h-1, ', remaining = ', niter+1-h,
#                      ', times elasped = ', round(elasped,1),
#                      's, estimated time for one step = ', round(step.estimation,1),
#                      's, estimated time remaining = ', round(step.estimation * (niter+2-h),1), 's' ))
#     }
#     # --- Step S : simulation --- #
#     Phih <-  do.call(Phi, parameter(h-1) )
#
#     # --- Metropolis Hastings --- #
#     Z <- do.call(simulation, c(list(MH.iter(h-1), h-1, Phih), Z ))
#     if(verbatim >= 2)
#     {
#       for(i in 1:length(Z))
#       {
#         value[[i]] <- append(value[[i]], list(Z[[i]][[1]])) #On mémorise que la premier chaine de chaque Z_i
#       }
#     }
#
#     # --- Step A : approximation --- #
#     Sh <- (1-u(h))*Sh+u(h)*(1:M %>%
#                               sapply(function(i) do.call(exhaustive, Z %>% lapply(function(z)z[[i]]) )) %>%
#                               apply(1, mean)) #mean of { exhaustive(Z_i) ; i \in \{1, ..., niter.MJ\} }
#
#     # --- Step M : maximisation --- #
#     res <- maximisation(Sh)
#     for( i in names(para)) para[[i]][h,] <- res[[i]] #mise à jour de chaque ligne de chaque parametre
#
#     cmp <- ifelse(stopCondi(h), cmp + 1, 0) #compteur après convergence des para pour verifier la CV
#     h <- h + 1
#   }
#
#   if(verbatim == 1 || verbatim == 2) message('--- SAEM ended ! ---')
#   if(verbatim >= 2)
#     attr(Z, 'value') <- value
#
#   if(cmp >= 20)
#   {
#     attr(para, 'stop') <- h - cmp
#   }
#
#   res <- list(parameter = para, Z = Z, times_elasped = difftime(Sys.time(), start , units = "secs"))
#   class(res) <- 'SAEM'
#
#   return(res)
# }
#
#
#
#
#
#




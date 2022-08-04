

#' verbatim
#' 1 end mcmc
#' 2 full mcmc
simulation <- function(niter, var, Phi, parameter, adptative.sd = NULL, verbatim = F)
{
  Phih <-  do.call(Phi, parameter )

  ver <- verbatim == 2

  if(is.null(adptative.sd))
  {
    for(i in 1:niter)
    {
      for(i in 1:length(var))
        var[[i]] <- do.call(Gibbs_sampler_step, c(list(var[[i]]), var[-i], list(Phi = Phih, verbatim = ver)))
    }
  }else{

    for(i in 1:niter)
    {
      for(i in 1:length(var))
      {
        var[[i]] <- do.call(Gibbs_sampler_step, c(list(var[[i]]), var[-i], list(Phi = Phih, verbatim = ver)))
        var[[i]]@sd <- adaptive_sd(var[[i]], target = adptative.sd)
      }
    }
  }
  if(verbatim == 1)
  {
    for(i in 1:length(var))
      var[[i]]@chain <- rbind(var[[i]]@chain, c(var[[i]], iteration = nrow(var[[i]]@chain)))
  }
  return(var)
}
#=============================================#

#' Stochastic Approximation Expectation Maximization (SAEM) algorithm
#'
#' SAEM algorithm perform parameter estimation for nonlinear mixed effects
#'
#' @param niter  number of iterations for SAEM loop
#' @param sim.iter number of iterations for Gibbs sampler algorithm
#' @param parameter0
#' @param var0 list of latent variables (with var names) MUST BE VECTOR
#' @param Phi
#' @param exhaustive
#' @param maximisation function for maximisation step
#' @param simulation function for simulation step
#' @param adptative.sd target sd value
#' @param burnin  step at which burn-in period ends
#' @param coef.burnin 1/(step+1-burnin)^coef.burnin
#' @param eps  Stop criterion
#' @param verbatim 1 = message only, 2 add MCMC, 3 add full MCMC
#'
#' @return
#' @export
#'
#' @examples
SAEM <- function(niter, sim.iter, parameter0, var0, Phi, exhaustive, maximisation, simulation,
                 adptative.sd = NULL,
                 burnin = NULL, coef.burnin = 1, eps = NULL, verbatim = F)
{
  #Initialisation de l'approximation de S
  Sh <- do.call(exhaustive, c(var0, parameter0))
  Sh[1:length(Sh)] <- 0

  #Initialisation des listes des parametres
  para <- SAEM4JLS::SAEM_res(param = parameter0, niter = niter, Z = var0)

  #Burn-in
  u <- burnin_fct(ifelse(is.null(burnin), niter, burnin), coef.burnin, scale = 1)

  MH.iter <- ifelse(is.function(sim.iter), sim.iter, function(k) sim.iter)
  sd <- ifelse(is.function(adptative.sd), adptative.sd, function(k) adptative.sd)


  h <- 2
  step.before.stop <- 20
  if(is.null(eps)){
    eps = 1e-3
    step.before.stop = 10000
  }
  stopCondi <- function(h) prod(as.logical(lapply(para, function(p) abs(p[h-1] - p[h]) < eps) ))
  cmp <- 0

  start <- Sys.time() #execution time
  last.msg <- 0
  if(verbatim != 0) message('--- SAEM started ! ---')


  #=======================#
  # === SAEM function === #
  #=======================#

  while(h <= niter+1 && cmp < step.before.stop ) #  for(h in 1:niter+1)
  {
    if(verbatim != 0)
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
    parameter = para[h-1]
    # --- Step S : simulation --- #
    para@Z <- simulation(MH.iter(h), para@Z, Phi, parameter = parameter, adptative.sd = sd(h), verbatim = verbatim - 1)
    # --- Step A : approximation --- #

    Sh <- (1-u(h))*Sh+u(h)*do.call(exhaustive, c(para@Z, parameter))
    # --- Step M : maximisation --- #
    res <- maximisation(Sh,parameter, para@Z)
    for( i in names(res)) para[[i]][h,] <- res[[i]] #mise à jour de chaque ligne de chaque parametre

    cmp <- ifelse(stopCondi(h), cmp + 1, 0) #compteur après convergence des para pour verifier la CV
    h <- h + 1
  }

  if(verbatim != 0) message('--- SAEM ended ! ---')

  if(cmp >= 20) attr(para, 'stop') <- h - cmp

  para@times_elasped = difftime(Sys.time(), start , units = "secs")

  return(para)
}


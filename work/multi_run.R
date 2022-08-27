


rm(list = ls())


require(future)
require(furrr)

cores <- future::availableCores()-1
require(SAEM4JLS)


#==============================================================================#
# Exemple d'optimalité entre les 3 fct MH, MH_high_dim et MH_high_dim_para sur le modèle non lineaire mixte
m <- function(t, phi1, phi2, phi3) (phi1  )/(1+exp((phi2-t)/phi3))
#=======================================#
p <- 1000
parameter <- list(sigma2 = .05^2,
                  mu = c(0.9,90,5),
                  omega2 = c(0.005, 40, 1),
                  bara = 90,
                  barb = 30,
                  baralpha = 0.5,
                  beta = rep(0,p))
parameter$beta[1:4] <-  c(-.8, -.2 , .3 , .9)
#=======================================#
t <- seq(60,120, length.out = 10) #time values
set.seed(123)

G <- 40 ; ng = 4
link = m

dt <- create_JLS_HD_data(G, ng, t, m, link, parameter)

var.true <- dt$var.true
a <- var.true$a ; var.true$a <- NULL #a fixé (et retiré des variables latentes)
S.data <- dt$survival
U <- dt$U

Y <- do.call(get_obs, var.true) + rnorm(n, 0, sqrt(parameter$sigma2))
S.data.time <- S.data$obs
S.data.time.log.sum <- sum(log(S.data.time))

#==============================================================================#
sigma2_b <- sigma2_alpha <- 0.1

h <- fct_vector(function(b, ...) N*log(b*a^-b),
                function(b, ...) (b-1)*S.data.time.log.sum,
                function(phi1, phi2, phi3, ...) sum(link(S.data.time, rep(phi1, ng), rep(phi2, ng), rep(phi3, ng) ))
)

zeta <- function(beta, alpha, phi1, phi2, phi3,b)
{
  link.fct <- function(t,g) link(t, phi1[g], phi2[g], phi3[g])

  P1 <- as.vector(exp(beta %*% t(U)))

  P2 <- sapply(1:nrow(S.data),
               function(i)integrate(
                 function(t) t^(b-1)*exp(alpha*link.fct(t, S.data$gen[i])),
                 0, S.data$obs[i])$value
  )

  sum(beta%*%t(U) - b*a^-b * P2*P1)
}

zeta.der.B <- function(beta, i, alpha, phi1, phi2, phi3, b)
{
  g <- S.data$gen[i]
  link.fct <- function(t) link(t, phi1[g], phi2[g], phi3[g])

  P1 <- U[i,] * exp(sum(beta * U[i,]))

  P2 <- integrate(function(t) t^(b-1)*exp(alpha*link.fct(t) ),
                  0, S.data$obs[i])$value

  -(U[i,] - b*a^-b * P1*P2)
}

#==============================================================================#
#==============================================================================#
#==============================================================================#

model_lasso <- SAEM_model(
  function(sigma2, ...) -n/(2*sigma2),
  function(phi1, phi2, phi3, ...) mean((Y - get_obs(phi1, phi2, phi3) )^2 ), 'sigma2',

  # === Variable Latente === #
  latent_vars = list(
    # === Non linear model === #
    latent_variable('phi', dim = G, size = 3, prior = list(mean = 'mu', variance = 'omega2'),
                    add_on = c('zeta(phi1 = phi1, phi2 = phi2, phi3 = phi3, ...)' )),

    # === S.data model === #
    latent_variable('b', prior = list(mean = 'barb', variance.hyper = 'sigma2_b'),
                    add_on = c('zeta(b = b, ...) +',
                               'sum(h$eval(b = b, ..., i = c(1,2)))' )),
    latent_variable('alpha', prior = list(mean = 'baralpha', variance.hyper = 'sigma2_alpha'),
                    add_on = c('zeta(alpha = alpha, ...) +',
                               'alpha*h$eval(alpha = alpha,..., i = 3)'))
  ),

  # === Paramètre de regression === #
  regression.parameter = list(
    regression_parameter('beta', 1, function(...) SPGD(5, theta0 = beta,
                                                       step = 0.05, lambda = 1/sqrt(N), alpha = 0,
                                                       normalized.grad = T,
                                                       zeta.der.B, N, zeta.B,
                                                       Z$alpha,  Z$phi1, Z$phi2, Z$phi3,Z$b) )
  )
)

model <- SAEM_model(
  function(sigma2, ...) -n/(2*sigma2),
  function(phi1, phi2, phi3, ...) mean((Y - get_obs(phi1, phi2, phi3) )^2 ), 'sigma2',

  # === Variable Latente === #
  latent_vars = list(
    # === Non linear model === #
    latent_variable('phi', dim = G, size = 3, prior = list(mean = 'mu', variance = 'omega2'),
                    add_on = c('zeta(phi1 = phi1, phi2 = phi2, phi3 = phi3, ...)' )),

    # === S.data model === #
    latent_variable('b', prior = list(mean = 'barb', variance.hyper = 'sigma2_b'),
                    add_on = c('zeta(b = b, ...) +',
                               'sum(h$eval(b = b, ..., i = c(1,2)))' )),
    latent_variable('alpha', prior = list(mean = 'baralpha', variance.hyper = 'sigma2_alpha'),
                    add_on = c('zeta(alpha = alpha, ...) +',
                               'alpha*h$eval(alpha = alpha,..., i = 3)'))
  ),

  # === Paramètre de regression === #
  regression.parameter = list(
    regression_parameter('beta', 1, function(...) SPGD(5, theta0 = beta,
                                                       step = 0.05, lambda = 1/sqrt(N), alpha = 1,
                                                       normalized.grad = T,
                                                       zeta.der.B, N, zeta.B,
                                                       Z$alpha,  Z$phi1, Z$phi2, Z$phi3,Z$b) )
  )
)

#==============================================================================#
#==============================================================================#
#==============================================================================#

#===============================================#
init.options <- list(x0 = list(phi = c(1,80,4), b = 20, alpha = 0.1),
                     sd = list(phi = c(.05, 1.5, .5), b = 1, alpha = .1) )

load.SAEM(model)

SAEM.options <- list(niter = 200, sim.iter = 5, burnin = 190,
                     adptative.sd = 0.6)

rds_filename <- paste0('data/res', gsub(' ','_',  gsub('-','_', gsub(':','_', Sys.time()) )), '_')
#==============================================================================#

load.SAEM(model)
oracle <- maximisation(1, do.call(S$eval, var.true), parameter, var.true)


f <- function(i)
{
  # ---  Initialisation des paramètres --- #
  parameter0 <- parameter %>% sapply(function(x) x* runif(1, 1.2,1.4))



  res_lasso <- SAEM4JLS::run(model_lasso, parameter0, init.options, SAEM.options, verbatim = 3*(i==1) )

  parameter0 <- res_lasso[SAEM.options$niter]
  SAEM.options$niter <- 5
  SAEM.options$burnin <- 4

  res <- SAEM4JLS::run(model, parameter0, init.options, SAEM.options, verbatim = 3*(i==1) )
  return(list(res = res, res_lasso = res_lasso))
  # saveRDS(res, paste0(rds_filename, i, '.rds'))
}

setwd('work/')
message(getwd())

SAEM.options$niter <- 5
SAEM.options$burnin <- 4

plan(multisession, workers = cores)
res <- future_map(1:cores, f, .options = furrr_options(seed = T, globals = ls()) )
plan(sequential)



saveRDS(list(res = res, oracle = oracle, parameter = parameter, data = dt, G = G, ng = ng, p = p, t = t, link = m, Y = Y),
        file = paste0('multi_run_data_', gsub(' ','_',  gsub('-','_', gsub(':','_', Sys.time()) )), '.rds'))




rm(list = ls())

require(SAEM4JLS)

param <- list(sigma2 = .05^2,
              mu = c(0.05,90,5),
              lbd = c(1,0,1) )


p <- 1
param <- list(sigma2 = .05^2,
              mu = c(5),
              lbd = c(0) )

omega2 = rep(0, p)



N <- 20
t <- seq(60,120, length.out = 10) #time values
set.seed(123)

n <- N*length(t)


time <- rep(t, N)

var.true <- list()
var.true$phi <- rnorm(N, param$mu[1], sqrt(omega2[1]))

var.true$lambda <- param$lbd[1]



m <- function(t, phi1, phi2, phi3) (phi1  )/(1+exp((phi2-t)/phi3))
m <- function(t, phi) t*(1+phi)^2

get_obs <- function(phi, lambda, ...){
  m( time,
     lambda*rep(phi, each = 1*length(t))
  )
}

Y <- do.call(get_obs, var.true) + rnorm(n, 0, sqrt(param$sigma2))


gamma2 <- rep(0.1, p)


model <- SAEM_model(
  function(sigma2, ...) -n/(2*sigma2),
  function(phi, lambda, ...) mean((Y - get_obs(phi, lambda) )^2 ),
  'sigma2',

  # === Variable Latente === #
  latent_vars = list(
    # === Non linear model === #
    latent_variable('phi', dim = N, size = p, prior = list(mean = 'mu', variance.hyper = 'omega2'),
                    ),

    latent_variable('lambda', dim = 1, size = p, prior = list(mean = 'lbd', variance.hyper = 'gamma2'),
                    )
  ),

  # === Paramètre de regression === #
  regression.parameter = list()
)





# ---  Initialisation des paramètres --- #
param0 <- param %>% sapply(function(x) x* runif(1, 1.1,1.4))
#===============================================#
load.SAEM(model)
var0 <- init.SAEM(model,
                  x0 = list(phi = c(4),
                            lambda = c(1)),
                  sd = list(phi = c(.5),
                            lambda = c(0.5) ))
#===================================================================================#
S.tmp <- do.call(S$eval, var.true)
oracle <- max(do.call(S$eval, var.true), param, var.true)
oracle %>% unlist



load.SAEM(model)#, exclude.simulation = c('phi1'))

sim(20, var0, Phi, param, adptative.sd = 0.6, verbatim = 2) %>% plot(nrow = 2)
# sim(20, var0, Phi, param, adptative.sd = 0.6, verbatim = 2) %>% plot(nrow = 2)

res <- SAEM(100, 5, param0, var0, Phi, S$eval, max, sim,  eps = 1e-3,
            burnin = 150,
            adptative.sd = 0.6, verbatim = 3, RDS = T )


plot(res, true.value = oracle, var = 'special')

plot(res, true.value = oracle, var = 'summary', time = F )



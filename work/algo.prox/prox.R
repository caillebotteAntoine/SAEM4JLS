
#' Y_ij = \beta^T U_i + Z_i *tj + eps
#' Z_i ~ N(mu, rho^2)
#' eps ~ N(0, sigma^2)
#'
#' U_i \in \setr^p
#' \beta \in \setr^p
#'
#' (Y_i - \beta U_i - Z_i) \sim N(0, \sigma^2)
#'
#' L_comp = f(Y|Z) f(Z)
#' l_comp  = n*J/2*log(2pi*sigma2) +1/2sigma2 \som i1n \som j1J(Y_ij- \beta^T U_i - Z_i*tj)^2
#'           + n/2*log(2pi*rho^2) + 1/2rho2 \som i1n (Z_i-mu)^2
#'
#' f_i = 1/2*sigma2 * \som j1J (Y_{i,j} - \beta^T U_i - Z_i tj)^2
#'
#' \nabla (\beta^T U_i) = U_i
#'
#' \nabla f_i = 1\sigma2 * \som j1J (Y_{i,j} - \beta^T U_i - Z_i tj) U_i
#'
#'
#'
#' pen = ||beta||_1
#'
#' prox(beta) = argmin(y + )
#'
#'
rm(list = ls()) ; graphics.off()
require(SAEM4JLS)



proxi <- function(theta, gamma, alpha, lambda)
{
  coef <- ifelse(theta > gamma*alpha*lambda, theta-gamma*alpha*lambda,
                 ifelse(theta < -gamma*alpha*lambda, theta+gamma*alpha*lambda, 0))
  1/(1+gamma*lambda*(1-alpha)) * coef
}

prox <- function(theta, gamma, alpha, lambda) sapply(theta, proxi, gamma, alpha, lambda)

SPGD <- function(niter, m, theta0, step = 1e-6, grad.fi, f, ...)
{
  gamma <- step
  if(!is.function(step)) gamma <- function(k) step


  args <- list(...)
  if(length(args) == 1 && is.list(args[[1]])) args <- args[[1]]

  theta.tilde <- theta0
  theta.t <- matrix( rep(theta0, m+1), nrow = m+1)
  grad.f.tilde <- sapply(1:n, function(i) do.call(grad.fi, c(list(theta.tilde, i), args)))

  f.value <- rep(NA, niter+1)
  f.value[1] <- do.call(f, c(list(theta.tilde), args))

  k <- 1
  while(k < niter  && mean(abs(grad.f.tilde)) > 1e-3)
  {
    for(t in 1:m+1)
    {
      i <- sample(1:n, 1) #Tirage dans une uniform

      grad.f.hat <- do.call(grad.fi, c(list(theta.t[t-1,], i), args))#approxMCMC()

      d <- grad.f.hat+ mean(grad.f.tilde) - grad.f.tilde[i]
      theta.t[t,] <- prox(theta.t[t-1,] - gamma(k)*d, gamma(k), 1, 0)
    }
    theta.tilde <- apply(theta.t, 2, mean)
    grad.f.tilde <- sapply(1:n, function(i) do.call(grad.fi, c(list(theta.tilde, i), args)))

<<<<<<< HEAD
    # print(mean(grad.f.tilde))
=======
    print(mean(grad.f.tilde))
>>>>>>> 921be6196f3715ca4457f1fda9a740a63d721f55
    f.value[k+1] <- do.call(f, c(list(theta.tilde), args))
    theta.t[1,] <- theta.tilde

    k <- k + 1
  }

  theta.tilde <- matrix(theta.tilde, nrow = 1)
  attr(theta.tilde, 'f.value') <- na.omit(f.value)

  return(theta.tilde)
}


#
#
# t_NIRS <- function(spectre)
# {
#   NIRS <- spectre$NIRS
#   variety <- colnames(NIRS)
#
#   data <- t(NIRS)
#
#   data <- as.data.frame(apply(data,2, as.numeric))
#
#   colnames(data) <- sapply(spectre$lambda, function(x) paste0('x',x))
#
#   row.names(data) <- variety
#
#   return(data)
# }
#
# load('spectres_gamme_BT_Irr_Mean.Rdata')
# #dt <- readRDS('nirs_feuille_irr_mean.rds')
# saveRDS(dt, 'nirs_feuille_irr_mean.rds')
#
# x <- t_NIRS(dt$der1)
#
#
# dt <- spectres %>% lapply(t_NIRS)
# saveRDS(dt$der1, 'nirs_feuille_irr_mean.rds')

x <- readRDS('nirs_feuille_irr_mean.rds')

#
# x %>% t %>% as_tibble %>% mutate(lambda = 1:nrow(.)) %>% melt(id = 'lambda') %>%
#   ggplot(aes(lambda, value, col = variable)) + geom_line() + theme(legend.position = 'null') +
#   geom_vline(xintercept = c(50, 110, 155, 510, 750, 820))
#
# heatmap(as.matrix(x))







<<<<<<< HEAD
param <- list(sigma2 = 0.5,
=======
param <- list(sigma2 = 0.00005,
>>>>>>> 921be6196f3715ca4457f1fda9a740a63d721f55
              rho2 = .5,
              mu = 3)

J <- 10
<<<<<<< HEAD
n <- 100#nrow(x)
p <- 10#ncol(x)

time <- seq(-1,1, len = J)
ttime <- t(time)
S <- sample(1:p, 5) #c(50, 110, 155, 510, 750, 820)
U <- matrix(runif(n*p), nrow = n, ncol = p)#1e4*as.matrix(x)
=======
n <- nrow(x)
p <- ncol(x)

time <- seq(-1,1, len = J)
ttime <- t(time)
S <- c(50, 110, 155, 510, 750, 820) #sample(1:p, 5)
U <- 1e4*as.matrix(x) #matrix(runif(n*p), nrow = n, ncol = p)
>>>>>>> 921be6196f3715ca4457f1fda9a740a63d721f55
tU <- t(U)
beta <- matrix(rep(0, p), nrow = 1) ; beta[S] <- 1

eps <- matrix(rnorm(n*J, sd = sqrt(param$sigma2)), nrow = n)
Z <- rnorm(n, param$mu, sd = sqrt(param$rho2))

Y <- as.vector(beta %*% t(U)) + Z %*% t(time) + eps

data.frame(Y) %>% mutate(id = 1:n) %>%
  melt(id = 'id', value.name = 'Y') %>%
  mutate(variable = rep(time, each = n)) %>% rename(time = variable) %>%
  ggplot(aes(time, Y, col = factor(id))) + geom_line() + theme(legend.position = 'null')




l <- function(beta, Z, ... ) - 1/param$sigma2 * sum( (Y - as.vector(beta %*% t(U)) + Z %*% t(time) )^2 ) /n
grad.li <- function(beta, i, Z, ...)  -1/param$sigma2 * sum( (Y[i,] - sum(beta * U[i,]) - Z[i]*time) )* U[i,] /n


<<<<<<< HEAD
res <- SPGD(500, 10, theta0 = matrix(rep(0, p), nrow = 1),
=======
res <- stochastic.proximal.gradient.descent(500, 10, theta0 = matrix(rep(0, p), nrow = 1),
>>>>>>> 921be6196f3715ca4457f1fda9a740a63d721f55
                                            step = burnin_fct(200, 0.8, 1e-5), grad.li, l, Z)

as.numeric(beta)
plot(attr(res,'f.value'))
as.numeric(res)
sum(beta - round(as.numeric(res)))

l(beta, Z)
l(res, Z)


beta %*% tU
res %*% tU


#==========================================#
#               --- SAEM ---               #
#==========================================#

model <- SAEM_model(
  function(sigma2, ...) - n*J/(2*sigma2),
  function(Z, beta, ...) mean( (Y - as.vector(beta %*% t(U)) - Z %*% t(time) )^2 ),

  # === Variable Latente === #
  latent_vars = list(
    latent_variable('Z', nrow = n, prior = list(mean = 'mu', variance = 'rho2'))
    ),

  # === Paramètre de regression === #
  regression.parameter = list(
    regression_parameter('beta', p, function(...)SPGD(100, 10, theta0 = beta[[1]],
                                                   step = burnin_fct(15, 0.8, 1e-5),
                                                   grad.li, l, ...) )
    )
  )





Phi <- model$Phi$eval
S <- model$S

loglik.Z <- model$loglik.fct$Z
sim <- model$simulation.fct

var <- list(Z = list(matrix(rep(0, n), ncol = 1)),
            beta = list( matrix(rep(0,p), nrow = 1) ))

maxi <- function(S)
{
  list(sigma2 = S%a%1,
       rho2 = S%a%3 - (S%a%2)^2,
       mu = S%a%2 )
}



sd.Z <- .2
simulation_test(sim, Phi, param, 50, var) %>% lapply(function(z)plot(z[[1]], nrow = 2))

res <- SAEM(50, 5, para, Phi, S$eval, var, sim, maxi, verbatim = 2)

plot(res, var = 'special', exclude = 'beta', true.value = maxi(S$eval(Z = Z, beta = beta)) )




as.numeric(beta)
as.numeric(res@Z$beta[[1]]) %>% round

as.numeric(res@Z$beta[[1]])








as.numeric(res@Z$beta[[1]])[which(beta == 1)]
as.numeric(res@Z$beta[[1]])[which(beta == 0)]


sum(beta - round(as.numeric(res@Z$beta[[1]])) )


res@chain$beta %>% as.data.frame %>% select(-id) %>% mutate(iter = 1:nrow(.)) %>%
  melt(id = 'iter') %>%

  ggplot(aes(iter, value, col = variable)) + geom_line() + theme(legend.position = 'null')


res@chain$beta %>% as.data.frame %>% select(-id) %>%
  apply(1, function(beta) l(matrix(beta, nrow = 1), Z)) %>%
  plot





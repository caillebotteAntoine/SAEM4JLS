
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
#' \nabla f_i = -1\sigma2 * \som j1J (Y_{i,j} - \beta^T U_i - Z_i tj) U_i
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



# source("D:/SAEM4JLS/R/SPGD.R", echo=TRUE)


param <- list(sigma2 = 0.005,
              rho2 = .5,
              mu = 3)

J <- 10
n <- 5#nrow(x)
p <- 10#ncol(x)

time <- seq(-1,1, len = J)
ttime <- t(time)
S <- 2#sample(1:p, 5) #c(50, 110, 155, 510, 750, 820)
U <- matrix(runif(n*p), nrow = n, ncol = p)#1e4*as.matrix(x)
U <- matrix(0.5, nrow = n, ncol = p) ; U[,1:S] <- 1


tU <- t(U)
beta <- rep(0,p) ; matrix(rep(0, p), nrow = 1) ; beta[1:S] <- 1

eps <- matrix(rnorm(n*J, sd = sqrt(param$sigma2)), nrow = n)
Z <- rnorm(n, param$mu, sd = sqrt(param$rho2))

Y <- as.vector(beta %*% tU) + Z %*% ttime + eps


data.frame(Y) %>% mutate(id = 1:n) %>%
  melt(id = 'id', value.name = 'Y') %>%
  mutate(variable = rep(time, each = n)) %>% rename(time = variable) %>%
  ggplot(aes(time, Y, col = factor(id))) + geom_line() + theme(legend.position = 'null')

# =============================================================================#



l <- function(beta, Z, param, ... ) 1/(2*param$sigma2) * sum( (Y - as.vector(beta %*% tU) - Z %*% ttime )^2 ) /n
grad.li <- function(beta, i, Z, ...)  -1/param$sigma2 * sum(Y[i,] - sum(beta * U[i,]) - Z[i]*time) * U[i,] /n


x <- seq(-1,4, 0.1)
plot(x, sapply(x, l, Z), col = 'blue')
plot(x, sapply(x, grad.li, 3, Z), col = 'red')
abline(v = beta)
abline(v = res)


l(beta, Z)
l(beta-0.05, Z)



res <- SPGD(100, 5, theta0 = matrix(rep(0, p), nrow = 1),
            step = burnin_fct(200, 0.8, 1e-3), grad.li, n, l, Z)



as.numeric(beta)
plot(attr(res,'f.value'))
as.numeric(res)
sum(beta - round(as.numeric(res)))


grad.li(beta, 1, Z)
grad.li(res, 1, Z)


l(beta, Z)
l(res, Z)



#==========================================#
#               --- SAEM ---               #
#==========================================#

model <- SAEM_model(
  function(sigma2, ...) - n*J/(2*sigma2),
  function(Z, beta, ...) mean( (Y - as.vector(beta %*% tU) - Z %*% ttime )^2 ),
  noise.name = 'sigma2',

  # === Variable Latente === #
  latent_vars = list(
    latent_variable('Z', dim = n, prior = list(mean = 'mu', variance = 'rho2'))
    ),

  # === Paramètre de regression === #
  regression.parameter = list(
    regression_parameter('beta', p, function(...)SPGD(100, 10, theta0 = beta,
                                                   step = burnin_fct(15, 0.8, 1e-5),
                                                   grad.li, l, Z, parameter) )
    )
  )

#==============================================================================#

load.SAEM(model)
var0 <- init.SAEM(model,
                  x0 = list(Z = 0),
                  sd = list(Z = .2))


param0 <- list(sigma2 = 0.1, rho2 = 0.1, mu = 1, beta = rep(0, p))

res <- SAEM(50, 5, param0, var0, Phi, S$eval, max, sim, verbatim = 3)


plot(res)




tmp <- sim(50, var0, Phi, param0, 2)





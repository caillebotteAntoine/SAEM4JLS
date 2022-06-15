

#==============================================================================#
#=================================stat exhaustive, echo = F}
#==============================================================================#
sigma2_a <- sigma2_b <- sigma2_alpha <- 0.1


Phi <- fct_vector(function(sigma2, rho2, mu, omega2, nu2, a, b, alpha, beta) {
  c(- n/(2*sigma2), -N/(2*rho2),               #1, 2
    - G/(2*omega2), G*mu/omega2,               #3, 4
    - G/(2*nu2),                               #5
    a/sigma2_a,         -1/(2*sigma2_a),     #6, 7
    b/sigma2_b        , -1/(2*sigma2_b),     #8, 9
    alpha/sigma2_alpha, -1/(2*sigma2_alpha), #10, 11
    beta ) },#12
  dim = c(1,1,F., F., rep(1,8)) )$eval

zeta <- function(beta, eta, phi, gamma, a,b, alpha)
{
  lbd <- function(t,g) t^{b-1} * exp(alpha*m(t, eta[g], phi[g,]))
  P1 <- 1:nrow(dt_SF) %>% sapply(function(i) integrate(function(t) lbd(t,dt_SF$gen[i]) , 0, dt_SF$obs[i])$value )

  P2 <- 1:nrow(dt_SF) %>% sapply(function(i) exp(beta*dt_SF$U[i] + gamma[dt_SF$gen[i]] )  )

  beta*sum(dt_SF$U) -  b*a^-b * sum(P2*P1)
}
zeta.der <- function(beta, eta, phi, gamma, a, b, alpha)
{
  lbd <- function(t,g) t^{b-1} * exp(alpha*m(t, eta[g], matrix(phi[g,], nrow = 1)))
  P1 <- 1:nrow(dt_SF) %>% sapply(function(i) integrate(function(t) lbd(t,dt_SF$gen[i]) , 0, dt_SF$obs[i])$value )

  P2 <- 1:nrow(dt_SF) %>% sapply(function(i) dt_SF$U[i]*exp(beta*dt_SF$U[i] + gamma[dt_SF$gen[i]] )  )

  sum(dt_SF$U) -  b*a^-b * sum(P2*P1)
}

S <- fct_vector(function(eta, phi, gamma, a, b, alpha) mean((Y - get_obs(m, dt_NLME, eta = eta, phi = phi) )^2 ),
                function(eta, phi, gamma, a, b, alpha) mean(eta^2),           #2
                function(eta, phi, gamma, a, b, alpha) apply(phi^2, 2, mean), #3
                function(eta, phi, gamma, a, b, alpha) apply(phi, 2, mean),   #4
                function(eta, phi, gamma, a, b, alpha) mean(gamma^2),         #5
                function(eta, phi, gamma, a, b, alpha) a,                     #6
                function(eta, phi, gamma, a, b, alpha) a^2,                   #7
                function(eta, phi, gamma, a, b, alpha) b,                     #8
                function(eta, phi, gamma, a, b, alpha) b^2,                   #9
                function(eta, phi, gamma, a, b, alpha) alpha,                 #10
                function(eta, phi, gamma, a, b, alpha) alpha^2,               #11
                function(eta, phi, gamma, a, b, alpha)                        #12
                  param$beta,
                #uniroot(function(beta)zeta.der(beta, eta, phi, gamma, a,b,alpha), lower = -100, upper = 100, tol = 1e-3)$root,
                dim = c(1,1,F., F., rep(1,8)) )

#==============================================================================#
#================================= Metropolis Hastings
#==============================================================================#
loglik.phi <- function(x, eta, Phi){
  id <- c(1,3,4)
  sum( Phi%a%id * S$eval(eta = eta, phi = x, i = id) )
}
loglik.eta <- function(x, phi, Phi){
  id <- c(1,2)
  sum( Phi%a%id * S$eval(eta = x, phi = phi, i = id) )
}

loglik.gamma <- function(x, eta, phi, a, b, alpha, Phi){
  Phi%a%5  * sum(x^2) + ng*sum(x) +
    zeta(Phi%a%12, eta, phi, x, a, b, alpha) #Correction fait à la va vite
}

loglik.a <- function(x, b, Phi) sum( Phi%a%6:7 * S(a = x, b = b, i = 6:7) ) + G*ng*log(b*x^-b)
loglik.b <- function(x, Phi)    sum( Phi%a%8:9 * S(b = x, i = 8:9) ) + (x-1)*sum(log(t))
loglik.alpha <- function(x, Phi)sum( Phi%a%10:11 * S(alpha = x, i = 10:11) )

#==============================================================================#
#=================================SAEM
#==============================================================================#
M <- 1 #nombre de simulation

# ---  Initialisation des paramètres --- #
para <- param %>% sapply(function(x) x* runif(1, 1.2,1.4))
para$rho2 = 0.2 ; para$omega2 <- rep(.1,3)

# --- Initialisation des chaines MC : Z_0 --- #list(attr(dt_NLME,'eta')),#list(attr(dt_NLME,'phi')),#
Z <- list(eta = list(attr(dt_NLME,'eta')),#1:M %>% lapply(function(i) rnorm(G*ng, 0, para$rho2)  %>% matrix(ncol = 1) ),
          phi = 1:M %>% lapply(function(i) matrix(rnorm(F.*G, para$mu, para$omega2), nrow = F.) %>% t ),

          gamma = list(attr(dt_SF,'gamma')),#1:M %>% lapply(function(i) matrix(rnorm(G, 0, para$nu2), ncol = 1) ),

          a = list(matrix(param$a)),
          b = list(matrix(param$b)),
          alpha = list(matrix(param$alpha))
)

#==============================================================================#
#=================================Etape simulation et maximisation du SEAM
#==============================================================================#
sim <- function(niter, k, Phih, eta, phi, gamma, a, b, alpha)
{
  M <- length(phi)

  # eta <- 1:M %>% lapply( function(i)
  #   MH_High_Dim_para_future(niter, eta[[i]], sd = sd.eta(k), loglik.eta, phi[[i]], Phih, cores = ncores ))

  phi <- 1:M %>% lapply( function(i)
    MH_High_Dim_para_future(niter, phi[[i]], sd = sd.phi(k), loglik.phi, eta[[i]], Phih, cores = ncores ))

  # gamma <- 1:M %>% lapply( function(i)
  #   MH_High_Dim_para_future(niter, gamma[[i]], sd = .03, loglik.gamma,
  #                    eta[[i]], phi[[i]], a[[i]], b[[i]], alpha[[i]],
  #                    Phih, cores = 1 ))

  # a <- 1:M %>% lapply( function(i) MH_High_Dim_para_future(niter, a[[i]],     sd = .02, loglik.a,b[[i]],  Phih, cores = 1 ))
  # b <- 1:M %>% lapply( function(i) MH_High_Dim_para_future(niter, b[[i]],     sd = .02, loglik.b,         Phih, cores = 1 ))
  # alpha <- 1:M %>% lapply( function(i) MH_High_Dim_para_future(niter, alpha[[i]], sd = .02, loglik.alpha, Phih, cores = 1 ))

  list(eta = eta, phi = phi, gamma = gamma, a = a, b = b, alpha = alpha)
}

maxi <- function(S)
{
  list(sigma2 = S%a%1,
       rho2 =   S%a%2,
       mu =     S%a%4,
       omega2 = S%a%3 - (S%a%4)^2,
       nu2 =    S%a%5,
       a =      S%a%6,  b =    S%a%8,
       alpha =  S%a%10, beta = S%a%12 )
}









#==============================================================================#
#=================================stat exhaustive, echo = F}
#==============================================================================#

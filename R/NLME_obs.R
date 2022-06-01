

#' Non Linear Mixed Effect observation
#'
#' @param G number of group
#' @param ng number of individuals per group
#' @param t vector containing the times the observation
#' @param param list of parameter( rho2, sigma2, mu, omega2)
#' @param NLME_fct an non linear function (t, phi, eta)
#'
#' @return
#' @export
#'
#' @examples
NLME_obs  <- function(G, ng, t, param, NLME_fct)
{
  ni <- length(t)#nombre observation
  F. <- length(param$omega2)#dimention du vecteur phi

  n <- G*ng*ni #nombre total de data
  N <- G*ng #nombre total d'individu

  #=======================================#

  eps <- rnorm(n, 0, sqrt(param$sigma2))

  if('rho2' %in% names(param)) #Varification de la présence de rho2
    eta <- rnorm(N, 0, sqrt(param$rho2)) %>% matrix(ncol = 1)
  else
    eta <- rep(1,N)

  phi <- 1:G %>% sapply(function(i) rnorm(F., param$mu, sqrt(param$omega2))) %>% t
  if(F. == 1)
    phi <- t(phi)

  attr(phi, 'G') <- G ; attr(phi, 'F.') <- F.

  # Définition des trois premières colonnes : id de l'individu, son group genetic et les temps d'observation
  data <- data.frame(id = rep(1:N, each = ni) %>% factor,
                     gen = rep(1:G, each = ni*ng) %>% factor,
                     t = rep(t, N)) %>%
    #Rajout des observations
    mutate(obs = get_obs(NLME_fct, ., eta = eta, phi = phi)) %>%
    #bruitage des observation
    mutate(obs = obs + eps)

  attr(data,'phi') <- phi
  attr(data,'eta') <- eta
  attr(data,'eps') <- eps

  return(data)
}


get_obs <- function(NLME_fct, data, ...)
{
  args <- list(...)
  if('eta' %in% names(args)) #Varification de la présence de eta
    eta <- args$eta
  else
    eta <- rep(1,length(levels(data$gen)))

  if('phi' %in% names(args)) #Varification de la présence de phi
    phi <- args$phi
  else
    phi <- matrix(rep(1,length(levels(data$gen))), ncol = 1)

  sapply(1:nrow(data), function(i) NLME_fct(data$t[i], eta[data$gen[i]] , phi[data$gen[i],] ) )
}








SF_obs <- function(dt_NLME, param, NLME_fct)
{
  G <- unique(dt_NLME$gen) %>% as.numeric %>% max
  ng <- unique(dt_NLME$id) %>% as.numeric %>% max %>% {./G}

  gamma <- rnorm(G, 0, sqrt(param$nu))

  data <- data.frame(id = 1:(G*ng), gen = rep(1:G, each = ng) ) %>%
    mutate(U = gen %% 2)

  data$obs <- get_obs_sf(NLME_fct, dt_NLME, data, param, gamma)

  attr(data, 'gamma') <- gamma

  return(data)
}




get_obs_sf <- function(NLME_fct, dt_NLME, dt_SF, param, gamma)
{
  survival_fct <- function(alpha, a,b, beta, dt_NLME, dt_SF, gamma, i)
  {
    g <- dt_SF$gen[i]
    eta <- attr(dt_NLME,'eta')[g]
    phi <- attr(dt_NLME,'phi')[g,]
    u <- dt_SF$U[i]


    lbd <- function(t) t^{b-1} * exp(alpha * NLME_fct(t, eta, phi) )

    uni <- runif(1)
    LBD <- function(t) b*a^-b *integrate(lbd, 0, t)$value * exp(beta*u +gamma[g]) + log(1-uni)

    uniroot(LBD, lower = 0, upper = 2*a)$root
  }

  sapply(1:nrow(dt_SF), function(i) survival_fct(param$alpha, param$a, param$b, param$beta, dt_NLME, dt_SF, gamma, i))
}














































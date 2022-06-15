
SF_obs <- function(dt_NLME, param, NLME_fct)
{
  G <- unique(dt_NLME$gen) %>% as.numeric %>% max
  ng <- unique(dt_NLME$id) %>% as.numeric %>% max %>% {./G}

  gamma <- rnorm(G, 0, sqrt(param$nu))  %>% matrix(ncol = 1)

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
    phi <- matrix(attr(dt_NLME,'phi')[g,], nrow = 1)
    u <- dt_SF$U[i]


    lbd <- function(t) t^{b-1} * exp(alpha * NLME_fct(t, eta, phi) )

    uni <- runif(1)
    LBD <- function(t) b*a^-b *integrate(lbd, 0, t)$value * exp(beta*u +gamma[g]) + log(1-uni)

    uniroot(LBD, lower = 0, upper = 2*a)$root
  }

  sapply(1:nrow(dt_SF), function(i) survival_fct(param$alpha, param$a, param$b, param$beta, dt_NLME, dt_SF, gamma, i))
}
















































create_NLME_data <- function(G, ng, t, m, param)
{
  assign('N', G*ng, envir = globalenv())
  assign('n', N*length(t), envir = globalenv())


  assign('time', rep(t, N), envir = globalenv())

  var.true <- list()
  var.true$phi1 <- rnorm(G, param$mu[1], sqrt(param$omega2[1]))
  var.true$phi2 <- rnorm(G, param$mu[2], sqrt(param$omega2[2]))
  var.true$phi3 <- rnorm(G, param$mu[3], sqrt(param$omega2[3]))

  var.true$a <- param$a
  var.true$b <- param$b
  # var.true$eta <- rnorm(N, 0, sqrt(param$rho2))

  get_obs <- function(phi1, phi2, phi3, ...){
    m( time,
       rep(phi1, each = ng*length(t)),
       rep(phi2, each = ng*length(t)),
       rep(phi3, each = ng*length(t))
       # rep(eta, each = length(t))
       )
  }

  assign('get_obs', get_obs, envir = globalenv())

  return(var.true)
}



create_JLS_HD_data <- function(G, ng, t, m, link, param, U = NULL)
{
  var.true <- create_NLME_data(G, ng, t, m, param)

  a <- param$bara
  var.true$a <- a
  b <- param$barb
  var.true$b <- b

  if('baralpha' %in% names(param))
  {
    alpha <- param$baralpha
    var.true$alpha <- alpha
  }else if('alpha' %in% names(param))
  {
    alpha <- param$alpha
  }

  beta <- param$beta
  p <- length(beta)

  survival <- data.frame(id = 1:(G*ng), gen = rep(1:G, each = ng) )

  if(p == 1){
    U <- matrix(sample(c(0,1), G*ng, replace = T), ncol = p)
  }else{

    if(is.null(U) || (ncol(U) < p & nrow(U) != G*ng))
    {
      U <- matrix(runif(G*ng*p, min  = -1, max = 1), ncol = p)
    }else{
      U <- U[,1:p]
    }
  }

  survival_fct <- function(i)
  {
    g <- survival$gen[i]
    phi1 <- var.true$phi1[g]
    phi2 <- var.true$phi2[g]
    phi3 <- var.true$phi3[g]
    u <- U[i,]

    lbd <- function(t) t^{b-1} * exp(sum(beta*u) + alpha * link(t, phi1, phi2, phi3) )

    # LBD <- function(t) b*a^-b * lbd(t) *exp(b*a^-b *integrate(lbd, 0, t)$value )
    #U = F(T) <=> F^-1 (U) = T ou chercher T
    #U = 1-S(T)
    #log(1-U) = log(S(t))= - int(\lambda(t))


    uni <- runif(1)
    uniroot(function(t) b*a^-b *integrate(lbd, 0, t)$value + log(1-uni) , lower = 0, upper = 10*a)$root
  }

  survival$obs = sapply(1:nrow(survival), survival_fct)
  return(list(var.true = var.true, survival = survival, U = U))
}









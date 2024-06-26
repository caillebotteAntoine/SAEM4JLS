
setOldClass('NLME_data')

JLS_data <- setClass(
  Class = 'JLS_data',
  contains = c('NLME_data','data.frame'),
  slots = list(hey = 'numeric',

               linkfct = 'function',

               gamma = 'matrix', survival = 'data.frame')
)

setMethod('initialize', 'JLS_data', function(.Object, ..., G, ng, time, fct, param, linkfct){
  args <- list(...)
  if(length(args) != 0){
    .Object <- callNextMethod()
  }else{
    .Object <- new('JLS_data', new('NLME_data', G = G, ng = ng, time = time, fct = fct, param = param) )

    if(!missing(linkfct)) .Object@linkfct <- linkfct
    else .Object@linkfct <- fct

    .Object@gamma <- rnorm(G, 0, sqrt(param$nu2))  %>% matrix(ncol = 1)

    .Object@survival <- data.frame(id = 1:(G*ng), gen = rep(1:G, each = ng) ) %>%
      mutate(U = gen %% 2)

    # === Génération donnée de survie === #

    alpha <- .Object@parameter$alpha
    a <- .Object@parameter$a
    b<- .Object@parameter$b
    beta <- .Object@parameter$beta

    survival_fct <- function(i)
    {
      g <- .Object@survival$gen[i]
      eta <- .Object@eta[g]
      phi <- matrix(.Object@phi[g,], nrow = 1)
      u <- .Object@survival$U[i]


      lbd <- function(t) t^{b-1} * exp(beta*u + alpha * .Object@linkfct(t, eta, phi) + .Object@gamma[g])

      # LBD <- function(t) b*a^-b * lbd(t) *exp(b*a^-b *integrate(lbd, 0, t)$value )
      #U = F(T) <=> F^-1 (U) = T ou chercher T
      #U = 1-S(T)
      #log(1-U) = log(S(t))= - int(\lambda(t))


      uni <- runif(1)
      uniroot(function(t) b*a^-b *integrate(lbd, 0, t)$value + log(1-uni) , lower = 0, upper = 2*a)$root
    }

    .Object@survival$obs = sapply(1:nrow(.Object@survival), survival_fct)
  }
  return(.Object)
})

  setGeneric('getLatente', function(.Object, ...) standardGeneric('getLatente'))
setMethod('getLatente', 'JLS_data', function(.Object, format = 'none'){
  Z <- list(eta = .Object@eta, phi =  .Object@phi, gamma = .Object@gamma)
  if(format == 'list')
    Z <- Z %>% lapply(function(z)list(z))
  return(Z)
})

plot.JLS_data = function(data, legend.position = 'null', nrow, ncol)
{
  gg <- list(NLME_plot = plot(as(data, 'NLME_data'), legend.position = legend.position) )

  # --- Survival data --- #
  gg$survival_plot <- data@survival %>% ggplot(aes(obs, fill = factor(U))) +
    geom_histogram(col = 'white', position = 'identity', bins = 30)

   gg$survival_plot <- gg$survival_plot + theme(legend.position = legend.position)

  if(length(gg) == 1)
    return(gg[[1]])

  if(!missing(nrow))
    return( grid.arrange(grobs =  gg, nrow = nrow))

  if(!missing(ncol))
    return( grid.arrange(grobs =  gg, ncol = ncol))

  return(gg)
}







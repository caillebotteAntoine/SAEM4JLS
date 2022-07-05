
NLME_data <- setClass(
  Class = "NLME_data",
  contains = "data.frame",
  slots = list(G = 'numeric', ng = 'numeric',
               n = 'numeric', N = 'numeric',
               ni  = 'numeric', F. = 'numeric',

               parameter = 'list',
               fct = 'function',

               eps = 'numeric', eta = 'matrix', phi = 'matrix')
)

setMethod('initialize', 'NLME_data', function(.Object, ..., G, ng, time, fct, param, nonzero.value = T){
  args <- list(...)
  if(length(args) != 0){
    .Object <- callNextMethod()
  }else{
    n <- G*ng*length(time)
    .Object <- new('NLME_data', data.frame(id = factor(rep(0, n)),
                                           gen = factor(rep(0, n)),
                                           time = rep(0, n),
                                           obs = rep(0, n)) )

    #--- Generation of data ---#
    .Object@parameter <- param
    .Object@G <- G
    .Object@ng <- ng

    .Object@ni <- length(time) #nombre observation
    .Object@n <- G*ng*.Object@ni #nombre total de data
    .Object@N <- G*ng #nombre total d'individu

    .Object@F. <- length(param$omega2)#dimention du vecteur phi

    # Initialisation des variables aléatoire
    #--- epsilon ---#
    .Object@eps <- rnorm(.Object@n, 0, sqrt(param$sigma2))
    #--- eta ---#
    if('rho2' %in% names(param)) #Varification de la présence de rho2
      .Object@eta <- matrix(rnorm(.Object@N, 0, sqrt(param$rho2)), ncol = 1)
    else
      .Object@eta <- matrix(rep(1,.Object@N), ncol = 1)
    #--- phi ---#
    .Object@phi <- t( sapply(1:G, function(i) rnorm(.Object@F., param$mu, sqrt(param$omega2))) )
    if(.Object@F. == 1)
      .Object@phi <- t(.Object@phi)


    #=== Creation of the data.frame ===#
    # Définition des trois premières colonnes : id de l'individu, son group genetic et les temps d'observation
    .Object$id <- factor(rep(1:.Object@N, .Object@ni) )
    .Object$gen <- factor(rep(rep(1:G, .Object@ng), .Object@ni) )
    .Object$time <- rep(time, each = .Object@N)
    #Rajout des observations
    .Object@fct <- fct

    .Object$obs <- get_obs(.Object, eta = .Object@eta, phi = .Object@phi) + .Object@eps

    if(nonzero.value)
      .Object$obs[which(.Object$obs < 0)] <- 0

  }
  return(.Object)
})

setMethod('getLatente', 'NLME_data', function(.Object, format = 'none'){
  Z <- list(eta = .Object@eta, phi =  .Object@phi)
  if(format == 'list')
    Z <- Z %>% lapply(function(z)list(z))
  return(Z)
})

get_obs <- function(data, ...)
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

  nrep.phi <- data@ng * data@ni #nrow(data) %/% nrow(phi)
  nrep.eta <- data@ni #nrow(data) %/% length(eta)
  #data$eta <- apply(data@eta, 2, rep, times = nrep.eta) ;data$phi <- apply(data@phi, 2, rep, times = nrep.phi)
  data@fct(data$time, apply(eta, 2, rep, times = nrep.eta), apply(phi, 2, rep, times = nrep.phi))
}

getDim = function(data, varname = c('G','ng','N','n','F.') ) {
  varname %>% sapply(function(var) assign(var, slot(data,var), envir = .GlobalEnv))
}

plot.NLME_data = function(data, legend.position = 'null')
{
  gg <- data %>% ggplot(aes(time, obs, col = gen, group = id)) +
    geom_point() + geom_line()

  gg <- gg + theme(legend.position = legend.position)

  return(gg)
}





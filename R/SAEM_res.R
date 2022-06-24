
SAEM_res <- setClass(
  Class = "SAEM_res",
  contains = 'list',

  slots = list(Z = 'list',
               chain = 'list',
               times_elasped  = 'difftime'),

  prototype = prototype(Z = list(), chain = list(), times_elasped = difftime(NULL, NULL) )

)


setMethod('initialize', 'SAEM_res', function(.Object, ..., param, niter, Z){
  args <- list(...)
  if(length(args) != 0){
    .Object <- callNextMethod()
  }else{
    .Object <- new('SAEM_res', lapply(param, function(x)
                                              { y <- matrix(NA, ncol = length(x), nrow = niter+1)
                                                y[1,] <- x ; return(y)
                                              }) )

    if(!missing(Z))
    {
      .Object@chain <- lapply(names(Z), function(v) numeric())
      names(.Object@chain) <- names(Z)
      .Object <- addchain(.Object, Z)
    }
  }
  return(.Object)
})

setMethod('[', 'SAEM_res', definition = function(x, i) lapply(x, function(p) p[i,]))

setMethod(addchain, signature = c('SAEM_res', 'list'),
          function(object, c){
            varnames <- unique(c(names(object@chain), names(c)))
            object@chain <- lapply(varnames, function(var) rbind(object@chain[[var]],
                                                                 cbind(v = c[[var]][[1]], id = 1:nrow(c[[var]][[1]]))
                                                                 ))
          names(object@chain) <- varnames
          return(object)})

as_data.frame <- function(x){
  data <- na.omit(as.data.frame(x))



}

setMethod(getchain, 'SAEM_res',
          function(object){
            niter <- nrow(na.omit(object[[1]]))

            dt <- object@chain %>% lapply(function(x) x %>% as.data.frame %>%
                                      mutate(iteration = rep(1:niter, each = max(id))) )%>%
                                   lapply(melt,id = c('iteration', 'id')) %>%
                                   lapply(function(x) x %>% mutate(component = gsub('V', 'component ', variable)))

            dt <- names(dt) %>% lapply(function(var) dt[[var]] %>% mutate(variable = factor(var))) %>%
              {do.call(rbind, .)}


            return(dt)
            })





simulation_test <- function(simulation, Phi, parameter, niter, Z, h = 0, verbatim = T)
{
  Phih <- do.call(Phi, parameter)

  res <- do.call(simulation, c(list(niter = niter, h = h, Phih = Phih), Z, list(verbatim = verbatim)))

  return(res)
}

oracle <- function(maximisation, exhaustive, data, ...)
{
  S <- do.call(exhaustive$eval, c(getLatente(data), list(...)))
  maximisation(S)
}





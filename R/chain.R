
chain <- setClass(
  Class = 'chain',
  contains = 'numeric',
  slots = list(acceptation = 'numeric',
               chain = 'matrix',

               sd = 'numeric',
               propto_distrib_fct = 'function',
               pre_build_fct = 'function'
               )
)

#--- Show ---#
setMethod(show, 'chain', function(object) {object@sd <- 0.2 ; show(as(object, 'numeric')) })

setMethod(initialize, 'chain', function(.Object, ..., sd, propto_distrib_fct){
  .Object <- callNextMethod()

  .Object@chain <- matrix(c(as.numeric(.Object), iteration = 0), nrow = 1)
  .Object@acceptation <- 0


  .Object@sd <- sd
  .Object@propto_distrib_fct <- propto_distrib_fct
  return(.Object)
})

adaptive_sd <- function(x, target = 0.6)
{
  rate <- x@acceptation[length(x@acceptation)]/(length(x)*length(x@acceptation))

  ifelse(rate < target, x@sd * 0.99, x@sd * 1.01)
}

get_rate <- function(object, x, ...) object@propto_distrib_fct(x, ...)

#========================#
# === Plot Function ==== #
#========================#
plot.chain <- function(..., name, nrow, ncol, var = c('chain', 'acceptation') )
{
  args <- list(...)
  if(length(args) == 1 && 'list' %in% class(args[[1]])) args <- args[[1]]

  var.name <- names(args)

  if(is.null(var.name)){
    var.name <- lapply(sys.call()[-1], as.character)

    var.name <- var.name[which(names(var.name) == "")]

    names(args) <- var.name
  }

  args <- args %>% keep(function(a) 'chain' %in% class(a))
  var.name <- names(args)

  gg <- list()

  if('chain' %in% var && length(args[[1]]@chain) != 1 )
  {
    dt <- data.frame()
    for(v in var.name)
    {
      dt <- rbind( dt,
                   args[[v]]@chain %>% as.data.frame %>%
                     melt(id = 'iteration') %>%
                     mutate(id = factor(variable)) %>%
                     mutate(variable = factor(v))
      )
    }

    gg$plot_value <- dt %>%
      ggplot(aes(iteration, value, col = variable, group = interaction(variable, id) )) +
      geom_line() + labs(title = 'Metropolis Hastings', x = 'iteration')


    if(!missing(name))
      gg$plot_value <- gg$plot_value + labs(subtitle = paste0('Variable ', name))
  }


  if('acceptation' %in% var && length(args[[1]]@acceptation) != 0 )
  {
    dim <- args %>% sapply(length)
    dt <- args %>% lapply(function(var) var@acceptation) %>% { Reduce(cbind,.) } %>%
      as.matrix

    for(i in 1:base::ncol(dt))
      dt[,i] <- dt[,i]/(dim[i]*1:base::nrow(dt))

    dt <- dt %>% as.data.frame

    names(dt) <- var.name

    gg$plot_acceptation <-
      dt %>% mutate(iteration = 1:base::nrow(.)) %>%
        melt(id = 'iteration') %>%

        ggplot(aes(iteration, value, col = variable)) +
        geom_line() +
        labs(title = "Acceptance rate over time", y = 'Acceptation rate') +
        theme(legend.position = 'null')

    if(!missing(name))
      gg$plot_acceptation <- gg$plot_acceptation + labs(subtitle = paste0('Variable ', name))
  }

  if(length(gg) == 1)
    return(gg[[1]])

  if(!missing(nrow))
    return( grid.arrange(grobs =  gg, nrow = nrow))

  if(!missing(ncol))
    return( grid.arrange(grobs =  gg, ncol = ncol))

  return(gg)
}



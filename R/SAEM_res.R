



SAEM_res <- setClass(
  Class = "SAEM_res",
  contains = 'list',

  slots = list(Z = 'list',
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

    .Object@Z <- Z
  }
  return(.Object)
})


setMethod('[', 'SAEM_res', definition = function(x, i) lapply(x, function(p) p[i,]))



vline <- function(gg, x, col, size = 1) gg + geom_vline(xintercept = x, size = size, col = col)

plot.SAEM_res <- function(res, nrow,ncol, var = c('parameter', 'MCMC', 'acceptation'), exclude, time = T, true.value )
{
  if(missing(exclude)) exclude <- c()

  if('special' %in% var)
  {
    gg1 <- plot(res, true.value = true.value, exclude = exclude,
                var = c('MCMC', 'parameter'), ncol = 2)

    return( grid.arrange(gg1, plot(res, var = 'acceptation', exclude = exclude, time = F), nrow = 2) )
  }

  gg <- list()

  if(time){
    diff_time <- format(as.POSIXct(as.numeric(res@times_elasped),
                                   origin = '1970-01-01', tz = 'UTC'), "%Mmin %Ssec")

    print(paste0('SAEM execution time = ', diff_time))

  }

  dt <-  na.omit(as.data.frame(res))

  if(!missing(true.value) && 'summary' %in% var)
  {
    #Resultat de l'estimations
    est <- rbind( true.value[names(res)] %>% unlist, dt[base::nrow(dt),]) %>%
      t %>% data.frame

    names(est) <- c('Real value', 'Estimated value')

    est <- est %>%
      mutate( Rrmse = abs(`Real value` -`Estimated value`)/abs(`Real value`))

    gg$table_estimation <-
      est %>% t %>% round(digits = 4) %>%
      knitr::kable(caption = "Result of the SAEM-MCMC", format = 'html') %>%
      kableExtra::kable_styling(full_width = F)
  }

  # MCMC des paramètres
  if('parameter' %in% var)
  {
    niter <- base::nrow(dt)
    dt <- dt %>% mutate(i = 1:base::nrow(.)-1) %>% melt(id = 'i')
    if(!missing(true.value))
      dt <- dt %>% mutate(hline = true.value[names(res)] %>% unlist %>% rep(each = niter) )

    gg$plot_parameter <- dt %>%
      ggplot(aes(i, value, col = variable)) + geom_line() +
      labs(title = "Result of the SAEM algorithm",
           x = 'iteration') + facet_grid( vars(variable), scales = 'free') +
      theme(legend.position = 'null')

    if(!missing(true.value))
      gg$plot_parameter <- gg$plot_parameter +
      geom_hline(aes(yintercept = hline, col = variable), linetype='dashed')

    if(!is.null( attr(res, 'stop')) )
    {
      gg$plot_parameter <- gg$plot_parameter %>% vline(attr(res, 'stop'), 'red')
    }
  }


  if('MCMC' %in% var)
  {
    gg$plot_MCMC <- plot(res@Z, var = 'chain') + facet_grid(vars(variable), scale = 'free')
  }

  if('acceptation' %in% var)
  {
    gg$plot_acceptation <- plot(res@Z, var = 'acceptation')
  }

  if(length(gg) == 1)
    return(gg[[1]])

  if(!missing(nrow))
    return( arrangeGrob(grobs =  gg, nrow = nrow))

  if(!missing(ncol))
    return( arrangeGrob(grobs =  gg, ncol = ncol))

  return(gg)
}




















setGeneric('plot.fitted.value', function(res, data, ...) standardGeneric("plot.fitted.value" ) )
setMethod(plot.fitted.value, c('SAEM_res', 'NLME_data'), function(res, data, selected.id = 1:12, burn.in = 20){

  if(length(res@chain) != 0)
  {
    Z <- getchain(res) %>% filter(iteration >= max(iteration) - burn.in ) %>%
      group_by(variable, component, id) %>% summarise(value = mean(value), .groups = 'drop')

    eta <- Z[which(Z$variable == 'eta'),]$value %>% matrix(ncol = 1)
    phi <- Z[which(Z$variable == 'phi'),]$value %>% matrix(ncol = data@F.)
  }else{
    eta <- res@Z$eta[[1]]
    phi <- res@Z$phi[[1]]

  }
  data$fitted_value <- get_obs(data, eta = eta, phi = phi)


  data[which(data$id %in% selected.id),] %>%
    ggplot(aes(time, obs, col = gen, group = id)) +
    geom_point() + geom_line(aes(y = fitted_value)) +

    labs(title = 'Fitted value', y = '') +

    theme(legend.position = 'null') + facet_wrap(vars(id))
})







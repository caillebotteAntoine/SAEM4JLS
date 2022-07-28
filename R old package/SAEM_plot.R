

vline <- function(gg, x, col, size = 1) gg + geom_vline(xintercept = x, size = size, col = col)

#' plot the result of the SAEM funciton
#'
#' @param var what to plot
#' @param res 'SAEM' type response variable
#' @param true.value the true value of the parameter, if it is not missing the plot function will return a knitr array summarizing the estimates with RMSE calculation
#' @param nrow
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
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

  dt <- res %>% as_data.frame

  if(!missing(true.value) && 'summary' %in% var)
  {
    #Resultat de l'estimations
    est <- rbind( true.value %>% unlist, dt[base::nrow(dt),]) %>%
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
      dt <- dt %>% mutate(hline = true.value %>% unlist %>% rep(each = niter) )

    gg$plot_parameter <- dt %>%
        ggplot(aes(i, value, col = variable)) + geom_line() +
        labs(title = "Result of the SAEM algorithm",
             x = 'iteration') + facet_grid( vars(variable), scales = 'free') +
        theme(legend.position = 'null')

    if(!missing(true.value))
      gg$plot_parameter <- gg$plot_parameter +
        geom_hline(aes(yintercept = hline, col = variable), linetype='dashed')

    if(!is.null( attr(res$parameter, 'stop')) )
    {
      gg$plot_parameter <- gg$plot_parameter %>% vline(attr(res$parameter, 'stop'), 'red')
    }
  }


  if('MCMC' %in% var)
  {
    gg$plot_MCMC <- getchain(res) %>% filter(!variable %in% exclude) %>%
      ggplot(aes(iteration, value, group = interaction(id, component, variable), color = component)) +
        geom_line() +
        labs(title = "Latent variables simulated by the SAEM" ) +
        facet_grid( vars(variable, component), scales = 'free') +
        theme(legend.position = 'null')
  }

  if('acceptation' %in% var)
  {
    gg.MH <- names(res@Z) %>% keep(function(v) 'MH_res' %in% class(res@Z[[v]][[1]]) ) %>%
      keep(function(v) !var %in% exclude) %>%
      lapply(function(v)plot(res@Z[[v]][[1]], name = v, var = 'acceptation'))

    gg$plot_acceptation <- arrangeGrob(grobs  = gg.MH)
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






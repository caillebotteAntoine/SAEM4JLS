

vline <- function(gg, x, col, size = 1) gg + geom_vline(xintercept = x, size = size, col = col)

#' plot the result of the SAEM funciton
#'
#' @param var what to plot
#' @param res 'SAEM' type response variable
#' @param true.value the true value of the parameter, if it is not missing the plot function will return a knitr array summarizing the estimates with RMSE calculation
#'
#' @return
#' @export
#'
#' @examples
plot.SAEM_res <- function(res, var = c('parameter', 'MCMC', 'summary'), true.value )
{
  gg <- list()

  diff_time <- format(as.POSIXct(as.numeric(res@times_elasped),
                                 origin = '1970-01-01', tz = 'UTC'), "%Mmin %Ssec")

  print(paste0('SAEM execution time = ', diff_time))

  dt <- res %>% as_data.frame

  if(!missing(true.value) && 'summary' %in% var)
  {
    #Resultat de l'estimations
    est <- rbind( true.value %>% unlist, dt[nrow(dt),]) %>%
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
    niter <- nrow(dt)
    dt <- dt %>% mutate(i = 1:nrow(.)-1) %>% melt(id = 'i')
    if(!missing(true.value))
      dt <- dt %>% mutate(hline = true.value %>% unlist %>% rep(each = niter) )

    gg$plot_parameter <- dt %>%
        ggplot(aes(i, value, col = variable)) + geom_line() +
        labs(title = "Result of the SAEM algorithm",
             x = 'iteration') + facet_grid( vars(variable), scales = 'free')

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
    gg$plot_MCMC <- getchain(res) %>%
      ggplot(aes(iteration, value, group = interaction(id, component, variable), color = component)) +
        geom_line() +
        labs(title = "Latent variables simulated by the SAEM" ) +
      facet_grid( vars(variable, component), scales = 'free')
  }

  if(length(gg) == 1)
    return(gg[[1]])

  return(gg)
}




plot.MH_res_old <- function(x, name, nrow,ncol, var = c('value', 'acceptation') )
{
  gg <- list()
  if('value' %in% var && 'value' %in% names( attributes(x) ) )
  {
    gg$plot_value <-
      attr(x, "value") %>% melt(id = c('iter', 'id')) %>%
        ggplot(aes(iter, value, group = interaction(variable, id), col = variable)) +
        geom_line() +# theme(legend.position = 'null') +
        labs(title = 'Metropolis Hastings', x = 'iteration')

    if(!missing(name))
      gg$plot_value <- gg$plot_value + labs(subtitle = paste0('Variable ', name))


  }

  if('acceptation' %in% var && 'acceptation' %in% names( attributes(x) ) )
  {
    gg$plot_acceptation <-
      attr(x, 'acceptation') %>%
      mutate(iteration = iter/base::nrow(x)) %>% filter(iter != 0) %>%
      ggplot(aes(iteration, naccept/iter)) + geom_line() +
      labs(title = "Acceptance rate over time")

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


#' plot resultat of MH function
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot.MH_res <- function(x, name, nrow,ncol, value = T, acceptation = T)
{
  gg <- list()
  if(value && 'value' %in% names( attributes(x) ) )
  {
    gg$plot_value <-
      attr(x, "value") %>% melt(id = c('iter', 'id')) %>%
        ggplot(aes(iter, value, group = interaction(variable, id), col = variable)) +
        geom_line() +# theme(legend.position = 'null') +
        labs(title = 'Resultat de Metropolis Hastings', x = 'iteration', y = 'valeur de Z')

    if(!missing(name))
      gg$plot_value <- gg$plot_value + labs(subtitle = paste0('Variable ', name))


  }

  if(acceptation && 'acceptation' %in% names( attributes(x) ) )
  {
    gg$plot_acceptation <-
      attr(x, 'acceptation') %>%
      mutate(iteration = iter/base::nrow(x)) %>% filter(iter != 0) %>%
      ggplot(aes(iteration, naccept/iter)) + geom_line() +
      labs(title = "Taux d'acceptation au cours du temps")

    if(!missing(name))
      gg$plot_acceptation <- gg$plot_acceptation + labs(subtitle = paste0('Variable ', name))
  }

  if(!missing(nrow))
    return( grid.arrange(grobs =  gg, nrow = nrow))

  if(!missing(ncol))
    return( grid.arrange(grobs =  gg, ncol = ncol))

  return(gg)
}





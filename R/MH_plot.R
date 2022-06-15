
#' plot resultat of MH function
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

plot.MH_res <- function(x, name, nrow,ncol, var = c('chain', 'acceptation') )
{
  gg <- list()
  dim <- base::nrow(x$value)

  if('chain' %in% var && length(x@chain) != 0 )
  {
    gg$plot_value <- getchain(x) %>%
      melt(id = c('id', 'iteration')) %>%
      ggplot(aes(iteration, value, group = interaction(variable, id), col = variable)) +
      geom_line() + labs(title = 'Metropolis Hastings', x = 'iteration')

    if(!missing(name))
      gg$plot_value <- gg$plot_value + labs(subtitle = paste0('Variable ', name))
  }

  if('acceptation' %in% var && length(x@acceptation) != 0 )
  {
    gg$plot_acceptation <- getacceptation(x) %>%
      ggplot(aes(iteration, rate)) + geom_line() +
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





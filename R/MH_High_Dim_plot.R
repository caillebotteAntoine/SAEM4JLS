
#' plot resultat of MH function
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot.MH_res <- function(x, dim = 1, value = T, acceptation = T)
{
  gg <- list()
  if(value && 'value' %in% names( attributes(x) ) )
  {
    gg$plot_value <-
      attr(x, "value") %>% melt(id = c('iter', 'id')) %>%
        ggplot(aes(iter, value, group = interaction(variable, id), col = variable)) +
        geom_line() +# theme(legend.position = 'null') +
        labs(title = 'Resultat de Metropolis Hastings', x = 'iteration', y = 'valeur de Z')

  }

  if(acceptation && 'acceptation' %in% names( attributes(x) ) )
  {
    gg$plot_acceptation <-
      attr(x, 'acceptation') %>%
      ggplot(aes(iter/dim, naccept/iter)) + geom_line() +
      labs(title = "Taux d'acceptation au cours du temps")

  }
  gg
}





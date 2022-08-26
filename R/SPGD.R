

#' Proximal operator for one component
#'
#' @param theta scalar argument
#' @param gamma step
#' @param alpha choice between ridge and lasso
#' @param lambda tuning parameter of elastic net
#'
proxi <- function(theta, gamma, alpha, lambda)
{
  res <- 1/(1+gamma*lambda*(1-alpha)) # alpha = 1 => res = 1
  gal <- gamma*alpha*lambda # =  0

  if(theta > gal){
    return(res*(theta - gal))
  }else if(theta < - gal){
    return(res*(theta + gal))
  }
  return(0)
}

#' proximal operator for a vector
#'
#' @param theta vector argument
#' @param gamma step
#' @param alpha choice between ridge and lasso
#' @param lambda tuning parameter of elastic net
#'
prox <- function(theta, gamma, alpha, lambda) sapply(theta, proxi, gamma, alpha, lambda)

#' Calculation of the averaged gradient
#'
#' Compute grad(f)(theta) = sum(grad(f_i)(theta))
#' where the support of the sum can be chosen
#'
#' @param grad.fi
#' @param theta
#' @param mini.batch support of the sum
#' @param args additional argument
grad <- function(grad.fi, theta, mini.batch, normalized.grad, args = list())
{
  # Computes for all i in mini.batch of grad(f_i)(theta, args)
  res <- sapply(mini.batch, function(i) do.call(grad.fi, c(list(theta, i), args))) %>%
    # make sure the form is the right one and average over the lines (for all i in mini.batch)
    matrix(nrow = length(theta)) %>% apply(1, mean)

  if(normalized.grad) return(res/sqrt(sum(res^2)))
  return(res)
}



#' Stochastic Proximal Gradient Descent
#'
#' compute a minimization of : f(theta) + pen(theta) where pen is the elastic net
#'
#' @param niter number iteration of the gradient descent
#' @param m number of iteration for the average computation of theta
#' @param theta0 init point
#' @param step of the gradient descent
#' @param grad.fi function that return grad(f_i, ...)
#' @param f function that return f
#' @param ... additional argument for the grad function (and f)
#'
#' @return
#' @export
#'
#' @examples
SPGD <- function(niter, theta0, step = 1e-6, grad.fi, n, f, lambda, alpha,..., mini.batch.size = NULL, normalized.grad = F, verbatim = F)
{
  if(is.null(mini.batch.size)) mini.batch.size <- n
  gamma <- SAEM4JLS::as_function(step)

  args <- list(...)
  if(length(args) == 1 && is.list(args[[1]])) args <- args[[1]]



  theta.tilde <- theta0
  #matrix for the average computation step
  # theta.t <- matrix( rep(theta0, m+1), nrow = m+1)
  # grad.f.tilde <- sapply(1:n, function(i) do.call(grad.fi, c(list(theta.tilde, i), args))) %>%
  #   matrix(nrow = n) %>% apply(2, mean)

  f.value <- c()
  gradf.value <- matrix(NA, ncol = length(theta0), nrow = niter)
  theta.value <- matrix(NA, ncol = length(theta0), nrow = niter)

  # --- main loop --- #
  k <- 1
  while(k <= niter)#  && mean(abs(grad.f.tilde)) > 1e-3)
  {
    mini.batch <- sort(sample(1:n, mini.batch.size, replace = F))
    grad.f.tilde <- grad(grad.fi, theta.tilde, mini.batch, normalized.grad, args)

    theta.tilde <- prox(theta.tilde - gamma(k)*grad.f.tilde, gamma(k),alpha,lambda)

    # print(theta.value)
    # print(theta.tilde)

    if(verbatim)
    {
      f.value[k] <- do.call(f, c(list(theta.tilde), args))
      theta.value[k,] <- theta.tilde
      gradf.value[k,] <- grad.f.tilde
    }

    k <- k + 1
  }

  theta.tilde <- theta.tilde#matrix(theta.tilde, nrow = 1)
  if(verbatim)
  {
    attr(theta.tilde, 'f.value') <- f.value
    attr(theta.tilde, 'gradf.value') <- gradf.value
    attr(theta.tilde, 'theta.value') <- theta.value
    class(theta.tilde) <- c(class(theta.tilde) , 'SPGD_res')
  }
  return(theta.tilde)

}


plot.SPGD_res <- function(res, x, f, grad.fi, i, n, theta.real = NULL, ...)
{
  x0 <- if(is.null(theta.real)) attr(res, 'theta.value')[1,] else theta.real

  args <- list(...)
  dt <- data.frame(f = attr(res,'f.value'), x = attr(res, 'theta.value')[,i] %>% as.numeric)


  x.tmp <- matrix(rep(x0, length(x)), ncol = length(x)) ; x.tmp[i,] <- x


  gg1 <- data.frame(x = x,
                    grad = sapply(1:ncol(x.tmp), function(k) do.call(grad, list(grad.fi, x.tmp[,k], 1:n, args)) ) %>%
                      matrix(ncol = length(x)) %>% { .[i,]} ) %>%

    ggplot(aes(x, grad)) + geom_line(col = 'blue') +
    geom_hline(yintercept = 0, col = 'green') +
    theme(legend.position = 'null') + labs(title = 'grad') +

    geom_vline(xintercept = res[i], col = 'purple')+
    xlim(c(min(x),max(x)))

  gg2 <- data.frame(x = x,
                    f =  sapply(1:ncol(x.tmp), function(k) do.call(f, c(list(x.tmp[,k]), args)) )  )%>%
    ggplot() + geom_line(aes(x, f), col = 'red')  +
    theme(legend.position = 'null') + labs(title = 'function')+

    geom_vline(xintercept = res[i], col = 'purple') +
    geom_point(data = dt, aes(x,f), col = 'purple') +
    xlim(c(min(x),max(x)))


  grid.arrange(gg1, gg2, nrow = 2)


}

# # require(SAEM4JLS)
# f <- function(x) ( 2*(x^2 - x - 1)^4 - x^2 + x )
# grad.fi <- function(x,i) ( 8*(2*x - 1)*(x^2-x-1)^3 -2*x + 1 )
#
# res1 <- SPGD(35, 1, step = 1e-2, grad.fi, 1, f, verbatim = T)
# res2 <- SPGD(35, 0, step = 1e-2, grad.fi, 1, f, verbatim = T)
#
# plot(res1, seq(-1,2, 0.01), f, grad.fi, 1, n = 1)
# plot(res2, seq(-1,2, 0.01), f, grad.fi, 1, n = 1)
#
#
#
#
# library(plotly)
# f <- function(x) ( 2*(x[1]^2 - x[1] - 1)^4 - x[1]^2 + x[1] ) * ( 2*(x[2]^2 - x[2] - 1)^4 - x[2]^2 + x[2] )
# grad.fi <- function(x,i) c( ( 8*(2*x[1] - 1)*(x[1]^2-x[1]-1)^3 -2*x[1] + 1 ) * ( 2*(x[2]^2 - x[2] - 1)^4 - x[2]^2 + x[2] ),
#                             ( 2*(x[1]^2 - x[1] - 1)^4 - x[1]^2 + x[1] ) * ( 8*(2*x[2] - 1)*(x[2]^2-x[2]-1)^3 -2*x[2] + 1 ) )
#
# x <- seq(-1.2,1.3, len = 200)
# y <- seq(-1.2,1.3, len = 200)
#
# dt <- data.frame(x,y)
# z <- outer(dt$x, dt$y, Vectorize(function(x,y)f(c(x,y))))
# plot_ly(dt, x = ~x, y = ~y, z = ~z) %>%
#   add_surface()
#
#
# res1 <- SPGD(100, c(0,0.4), step = 1e-2, grad.fi, 1, f, verbatim = T)
# plot(res1, seq(-1,2, 0.01), f, grad.fi, 1, n = 1)
# plot(res1, seq(-1,2, 0.01), f, grad.fi, 2, n = 1)
#
#
# res2 <- SPGD(100, c(0.7,-0.2), step = 1e-2, grad.fi, 1, f, verbatim = T)
# plot(res2, seq(-1,2, 0.01), f, grad.fi, 1, n = 1)
# plot(res2, seq(-1,2, 0.01), f, grad.fi, 2, n = 1)
#
#







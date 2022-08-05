

#' Proximal operator for one component
#'
#' @param theta scalar argument
#' @param gamma step
#' @param alpha choice between ridge and lasso
#' @param lambda tuning parameter of elastic net
#'
proxi <- function(theta, gamma, alpha, lambda)
{
  res <- 1/(1+gamma*lambda*(1-alpha))
  gal <- gamma*alpha*lambda

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
SPGD <- function(niter, theta0, step = 1e-6, grad.fi, n, f, ..., verbatim = F)
{
  gamma <- SAEM4JLS::as_function(step)

  args <- list(...)
  if(length(args) == 1 && is.list(args[[1]])) args <- args[[1]]



  theta.tilde <- theta0
  #matrix for the average computation step
  # theta.t <- matrix( rep(theta0, m+1), nrow = m+1)
  # grad.f.tilde <- sapply(1:n, function(i) do.call(grad.fi, c(list(theta.tilde, i), args)))

  f.value <- c()
  gradf.value <- c()
  theta.value <- matrix(NA, ncol = length(theta0), nrow = niter)

  # --- main loop --- #
  k <- 1
  while(k <= niter)#  && mean(abs(grad.f.tilde)) > 1e-3)
  {
    grad.f.tilde <- sapply(1:n, function(i) do.call(grad.fi, c(list(theta.tilde, i), args))) %>%
      matrix(nrow = n) %>% apply(2, sum)
    theta.tilde <- prox(theta.tilde - gamma(k)*grad.f.tilde, gamma(k),1,0)

    # print(theta.value)
    # print(theta.tilde)

    if(verbatim)
    {
      gradf.value[k] <- base::max(abs(grad.f.tilde))
      f.value[k] <- do.call(f, c(list(theta.tilde), args))
      theta.value[k,] <- theta.tilde
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


plot.SPGD_res <- function(res, x, f, grad.fi, i = 1, ...)
{
  x0 <- attr(res, 'theta.value')[1,]
  args <- list(...)
  dt <- data.frame(f = attr(res,'f.value'), x = attr(res, 'theta.value')[,i] %>% as.numeric)


  x.tmp <- matrix(rep(x0, length(x)), ncol = length(x)) ; x.tmp[i,] <- x

  gg1 <- data.frame(x = x,
                    grad = sapply(1:ncol(x.tmp), function(k) do.call(grad.fi, c(list(x.tmp[,k], i), args)) ) ) %>%
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
#
#b^2 - 4ac = 49 - 4*13/4 = 6^2
# x = (7+-6)/2 =

# f <- function(x) ( 2*(x^2 - x - 1)^4 - x^2 + x )
# grad.fi <- function(x,i) ( 8*(2*x - 1)*(x^2-x-1)^3 -2*x + 1 )
#
# res1 <- SPGD(35, 1, step = 1e-2, grad.fi, 1, f, verbatim = T)
# res2 <- SPGD(35, 0, step = 1e-2, grad.fi, 1, f, verbatim = T)
#
# plot(res1, seq(-1,2, 0.01), f, grad.fi, 1)
# plot(res2, seq(-1,2, 0.01), f, grad.fi, 1)
#










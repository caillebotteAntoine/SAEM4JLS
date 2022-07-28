
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
SPGD <- function(niter, m, theta0, step = 1e-6, grad.fi, f, ...)
{
  gamma <- as_function(step)

  args <- list(...)
  if(length(args) == 1 && is.list(args[[1]])) args <- args[[1]]

  theta.tilde <- theta0
  n <- length(theta0)
  #matrix for the average computation step
  theta.t <- matrix( rep(theta0, m+1), nrow = m+1)
  grad.f.tilde <- sapply(1:n, function(i) do.call(grad.fi, c(list(theta.tilde, i), args)))

  f.value <- rep(NA, niter+1)
  f.value[1] <- do.call(f, c(list(theta.tilde), args))

  # --- main loop --- #
  k <- 1
  while(k < niter  && mean(abs(grad.f.tilde)) > 1e-3)
  {
    #Average computation step
    for(t in 1:m+1)
    {
      i <- sample(1:n, 1) #Tirage dans une uniform

      grad.f.hat <- do.call(grad.fi, c(list(theta.t[t-1,], i), args))#approxMCMC()

      #SVRG
      d <- grad.f.hat - grad.f.tilde[i] + mean(grad.f.tilde)
      theta.t[t,] <- prox(theta.t[t-1,] - gamma(k)*d, gamma(k), 1, 0)
    }
    theta.tilde <- apply(theta.t, 2, mean)
    grad.f.tilde <- sapply(1:n, function(i) do.call(grad.fi, c(list(theta.tilde, i), args)))

    # print(mean(grad.f.tilde))
    # cat('theta = ', theta.tilde)
    f.value[k+1] <- do.call(f, c(list(theta.tilde), args))
    # print(cat('f = ', f.value[k+1]))
    theta.t[1,] <- theta.tilde

    k <- k + 1
  }

  theta.tilde <- theta.tilde#matrix(theta.tilde, nrow = 1)
  attr(theta.tilde, 'f.value') <- na.omit(f.value)

  return(theta.tilde)

}








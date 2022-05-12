logfct <- function(theta, Z)
{
  p <- do.call(P, theta)
  s <- do.call(S, Z)

  sum(p*s) - do.call(psi, theta)
}

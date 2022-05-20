
rm(list = ls())
require(SAEM4JLS)

n <- 10000# nombre d'observation
x <- c(3,3) #moyenne et variance
obs <- rnorm(n, x[1], sqrt(x[2])) #tirage dans une loi normale

transi = function(x) rnorm(length(x), x, 0.1) #fonction de transition entre x et xnew

loglik <- function(x, obs) sum( -1/2*log(2*pi*x[2]) -1/(2*x[2])* (obs-x[1])^2 ) #vraisemblence

#Metropolis hasting
res <- MH_High_Dim(2000, matrix(c(1,1), nrow = 1), transi, loglik, obs, verbatim = T)

#Affichage résultat
attr(res, 'value') %>% melt(id = c('iter', 'id')) %>%
  ggplot(aes(iter, value, col = variable)) + geom_line() + geom_point(shape = 20)




data.frame( naccept = attr(res, 'naccept'),
            iter = attr(res, 'niter') ) %>% ggplot(aes(iter, naccept/iter)) + geom_line()










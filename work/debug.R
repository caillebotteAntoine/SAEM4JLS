rm(list = ls()); graphics.off()

require(SAEM4JLS)
require(dplyr)
require(tictoc)

m <- function(phi) (phi[1] )/(1+exp((phi[2]-s)/phi[3])) #phi[1]*(s + phi[2])  #phi[1]/(1+exp(phi[2]*s)) #(phi[1] )/(1+exp((phi[2]-s)/phi[3]))

m <- function(phi) phi[1] * t #+ phi[2]
#m <- function(phi) (phi[1] )/(1+exp((phi[2]-t)/phi[3]))

ni <- 10 #nombre observation
ng <- 1 #nombre d'individu par groupe
G <- 40 # nombre groupe
F. <- 1#dimention du vecteur phi

n <- G*ng*ni #nombre total de data
N <- G*ng #nombre total d'individu

#===============================#
sigma2 = 0.005
mu = c(5,4,0.5)#1:F.*2#bc(1,4,0.1)1:F.*2  #
omega2 = c(0.5,0.1,0.01)#c(0.5,0.1,0.01)1:F./2#
#===============================#

eps <- rnorm(n, 0, sqrt(sigma2))
phi <- 1:G %>% sapply(function(i) rnorm(F., mu, sqrt(omega2)))  %>% matrix %>% t #Pour Phi de dim F. = 1

X <- data.frame('gen' = rep(1:G, each = ng) %>% as.factor) #Matrice de design
t <- seq(2,6, length.out = ni) #valeur des temps

# Y_{g,i,j} = m(s_j, phi_g) + \epsilon_{g,i,j}
# Y_{i,j} = m(s_j, phi_{X[i]}) + \epsilon_{i,j}

get.Y <- function(phi) 1:N %>% sapply(function(i) m(phi[,X[i, 'gen']]))
Y <- get.Y(phi) + eps

#================================= Affichage ================================#
data.frame(Y = Y, t = t ) %>% melt(id = 't') %>% mutate(gen = rep(X$gen, each = ni)) %>%
  ggplot(aes(t, value, col = variable, shape = gen)) + geom_point() + geom_line() +#+ theme(legend.position = 'null')
  guides(color = 'none')
#============================================================================#
Phi <- function(sigma2, mu, omega2)
  c(- 1/(2*sigma2), -1/(2*omega2), mu/omega2)

S <- function(phi)
{
  s <- c(sum((Y - get.Y(phi))^2 ),
    apply(phi^2, 1, sum),
    apply(phi  , 1, sum) )

  attr(s, '1') <- 1
  attr(s, '3') <- 1 + 1:nrow(phi)
  attr(s, '4') <- 1 + nrow(phi) + 1:nrow(phi)
  return(s)
}

#=============================================================================#
transi.phi <- function(x) rnorm(length(x), x, 0.01)

loglik.phi <- function(phi, Phi)
{
  stat <- S(phi)
  sum(Phi*stat)
}

phi.MH <- matrix(rnorm(F.*G, 0, 1),ncol = G, nrow = F.)
phi.MH <- MH_High_Dim(200, phi.MH, transi.phi, loglik.phi, Phi(sigma2, mu, omega2), verbatim = T)


#=============================================================================#

# Vérifier à la main la loi et empirique dnas le model linéaire
#utilisé MH grand dim
#envoyer si ça marche pas sinon histo burn in et tout

#attributes(eta0)
v <- attr(phi.MH, 'value') %>% lapply(function(x) as.numeric(x)) %>% as.data.frame %>% t %>% as.data.frame()

v %>% mutate(i = 1:nrow(.)) %>% melt(id = 'i')  %>% mutate(phi = rep(factor(c(1:F.)), each = nrow(v)*G)) %>%
  ggplot(aes(i, value, col = phi, group = variable)) +
  geom_line()




# loi sachant Y
n <- 1500
v <- MH_burnin(phi.MH, n)
v

1:length(v) %>% sapply(function(i) v[[i]][1,]) %>% mean#apply(1, mean)
1:length(v) %>% sapply(function(i) v[[i]][2,]) %>% mean#apply(1, mean)




















# --- Nombre iteration --- #
M <- 1
u <- function(k) 1
#---
para <- list( sigma2 = sample(1:5, size = 1),
              mu = rep(0, F.),
              omega2 = rep(1, F.) )

phigivenY <- 1:M %>% lapply(function(i) rnorm(F.*G, 0,1) %>% matrix(ncol = G, nrow = F.) )
#---
sim <- function(niter, Phih, phigivenY)
{
  M <- length(phigivenY)

  phigivenY <- 1:M %>% lapply( function(i)
    MH_High_Dim(niter, phigivenY[[i]], transi.phi, loglik.phi, Phih ))

  list(phi = phigivenY)
}

maxi <- function(S)
{
  #return(list(sigma2 = sigma2, mu = mu, omega2 = omega2))

  mu <- S[attr(S,'4')]/G # = S[4]
  return(list(sigma2 = S[attr(S,'1')]/n,
              mu = mu,
              omega2 = S[attr(S,'3')]/G - mu^2)
  )
}

niter <- 200
Z <- list(phigivenY)
res <- SAEM(niter, 10, u, para, Phi, S, Z, sim, maxi, verbatim = T)



res$parameter %>% as.data.frame %>%
  mutate(i = 1:nrow(.)) %>% melt(id = 'i') %>%
  ggplot(aes(i, value, col = variable)) + geom_line()




v <- attr(res$Z, 'value')[[1]] %>% lapply(function(x) as.numeric(x)) %>% as.data.frame %>% t %>% as.data.frame()

v %>% mutate(i = 1:nrow(.)) %>% melt(id = 'i')  %>% mutate(phi = rep(factor(c(1:F.)), each = nrow(v)*G)) %>%
  ggplot(aes(i, value, col = phi, group = variable)) +
  geom_line()







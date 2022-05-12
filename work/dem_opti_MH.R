rm(list = ls()); graphics.off()

require(SAEM4JLS)
require(dplyr)
require(tictoc)

m <- function(phi) (phi[1] )/(1+exp((phi[2]-s)/phi[3])) #phi[1]*(s + phi[2])  #phi[1]/(1+exp(phi[2]*s)) #(phi[1] )/(1+exp((phi[2]-s)/phi[3]))

m <- function(phi) phi[1] * t #+ phi[2]
ni <- 10 #nombre observation
ng <- 1 #nombre d'individu par groupe
G <- 40 # nombre groupe
F. <- 1#dimention du vecteur phi

n <- G*ng*ni #nombre total de data
N <- G*ng #nombre total d'individu

#===============================#
sigma2 = 0.5
mu = 1:F.*2  # c(5,4,0.5)#1:F.*2#bc(1,4,0.1)
omega2 = 1:F./2#c(0.5,0.1,0.01)#c(0.5,0.1,0.01)
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











P <- function(sigma2, mu, omega2)
  c(- 1/(2*sigma2), -1/(2*omega2), mu/omega2)
p <- P( sigma2, mu, omega2)

S <- function(phi)
  c(sum((Y - get.Y(phi))^2 ),
    apply(phi^2, 1, sum),
    apply(phi  , 1, sum) )


#=============================================================================#
transi.phi <- function(x) rnorm(length(x), x, 0.01) %>% matrix(ncol = G, nrow = F.)
transi.phi3 <- function(x) rnorm(length(x), x, 0.01)

#taux d'acceptation = min(1, f(xnew)/f(x)) = rho(x, xnew)
# log(\rho) = log(f(xnew)) - log(f(x))

loglik.phi <- function(phi)
{
  -1/(2*sigma2) * sum((Y - get.Y(phi))^2) + sum( -1/(2*omega2) * apply(phi^2, 1, sum) + mu/omega2* apply(phi,1, sum) ) #-G*sum(mu^2/(2*omega2))
  #-1/(2*sigma2) * sum((Y - get.Y(phi))^2)  -1/2*(1:G %>% sapply(function(i) t(phi[,i]-mu) %*% diag(omega2^-1) %*% (phi[,i]-mu) ) %>% sum )
}
loglik.phi2 <- function(phi)
{
  stat <- S(phi)
  sum(p*stat)
}
loglik.phi3 <- function(phi)
{
  stat <- S(t(phi))
  sum(p*stat)
}


tic()
phi.MH <- matrix(rnorm(F.*G, 0, 1),ncol = G, nrow = F.)
phi.MH <- MH(10000, phi.MH, transi.phi, loglik.phi, verbatim = T)
toc()

tic()
phi.MH2 <- matrix(rnorm(F.*G, 0, 1),ncol = G, nrow = F.)
phi.MH2 <- MH(10000, phi.MH2, transi.phi, loglik.phi2, verbatim = T)
toc()

tic()
phi.MH3 <- matrix(rnorm(F.*G, 0, 1),ncol = G, nrow = F.) %>% t
phi.MH3 <- MH_High_Dim(10000, phi.MH3, transi.phi3, loglik.phi3, verbatim = T)
toc()

#=============================================================================#

# Vérifier à la main la loi et empirique dnas le model linéaire
#utilisé MH grand dim
#envoyer si ça marche pas sinon histo burn in et tout

#attributes(eta0)
v <- attr(phi.MH, 'value') %>% lapply(function(x) as.numeric(x)) %>% as.data.frame %>% t %>% as.data.frame()

v %>% mutate(i = 1:nrow(.)) %>% melt(id = 'i')  %>% mutate(phi = rep(factor(c(1:F.)), each = nrow(v)*G)) %>%
  ggplot(aes(i, value, col = phi, group = variable)) +
  geom_line()


v <- attr(phi.MH2, 'value') %>% lapply(function(x) as.numeric(x)) %>% as.data.frame %>% t %>% as.data.frame()

v %>% mutate(i = 1:nrow(.)) %>% melt(id = 'i')  %>% mutate(phi = rep(factor(c(1:F.)), each = nrow(v)*G)) %>%
  ggplot(aes(i, value, col = phi, group = variable)) +
  geom_line()


v <- attr(phi.MH3, 'value') %>% lapply(function(x) as.numeric(x)) %>% as.data.frame %>% t %>% as.data.frame()

v %>% mutate(i = 1:nrow(.)) %>% melt(id = 'i')  %>% mutate(phi = rep(factor(c(1:F.)), each = nrow(v)*G)) %>%
  ggplot(aes(i, value, col = phi, group = variable)) +
  geom_line()


# loi sachant Y
n <- 6000
m1 <- MH_burnin(phi.MH, n) %>% unlist %>% matrix(ncol = G) %>% apply(2, mean)
m2 <- MH_burnin(phi.MH2, n) %>% unlist %>% matrix(ncol = G) %>% apply(2, mean)
m3 <- MH_burnin(phi.MH3, n) %>% unlist %>% matrix(ncol = G) %>% apply(2, mean)

mean(abs(m1 - m2))
mean(abs(m1 - m3))
mean(abs(m2 - m3))









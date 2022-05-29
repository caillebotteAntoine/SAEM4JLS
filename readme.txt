#===================#
# --- préambule --- #
#===================#
Ceci est un package pour faire tourner les codes crée pour le stage WP2.

Il faut donc le compiler et l'installer pour exécuter les scripts : onglet : Build -> Install and Restart (sous Rstudio)

En cas de problème avec l'installation du package, remplacer la ligne (en début de script) : require(SAEM4JLS)
En : 
require(ggplot2)
require(reshape2)
require(dplyr)
require(tidyr)
require(furrr)
require(future)

source('R/include.R')

#=============================#
# --- Description fichier --- #
#=============================#

Les fichiers dans le dossier work sont les fichiers principale (à lancer)

Des exemples d'utilisation des fonctions se trouvent dans le dossier work/vignette
Le sous dossier work/achieved_model rassemble des .rmd présentant des applications de modèle achevé !













# Shell à utiliser pour l'exécution du job
# -S /bin/bash

# Nom du job
# -N BLAST_test

# Nom de la queue
#$ -q short.q

# Export de toutes les variables d'environnement
# -V

# Sortie standard
#$ -o 'cluster.out'

# Sortie d’erreur
#$ -e 'cluster.err'

# Lance la commande depuis le répertoire où est lancé le script
#$ -cwd

# nombre de CPUs
#$ -pe thread 8


#$ -N SAEM_vignette

-b y "Rscript work/clusterCompilationRmd.R"







qsub -cwd -N SAEM_JLS -q long.q -pe thread 16 -o 'cluster.out' -e 'cluster.err' -b y "Rscript work/clusterCompilationRmd.R"
qsub -cwd -N SAEM_JLS -q long.q -pe thread 4 -o 'cluster.out' -e 'cluster.err' -b y "Rscript work/clusterCompilationRmd.R"














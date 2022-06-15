

# Shell à utiliser pour l'exécution du job
# -S /bin/bash

# Lance la commande depuis le répertoire où est lancé le script
#$ -cwd -N saem -q short.q -pe thread 10 -o 'saem.out' -e 'saem.err' -b y "Rscript work/clusterCompilationRmd.R"










#
# qsub -cwd -N SAEM_JLS -q long.q -pe thread 16 -o 'cluster.out' -e 'cluster.err' -b y "Rscript work/clusterCompilationRmd.R"
# qsub -cwd -N SAEM_JLS -q long.q -pe thread 4 -o 'cluster.out' -e 'cluster.err' -b y "Rscript work/clusterCompilationRmd.R"
qsub -cwd -N SAEM_JLS -q long.q -pe thread 4 -o 'saem.out' -e 'saem.err' -b y "Rscript work/clusterCompilationRmd.R"













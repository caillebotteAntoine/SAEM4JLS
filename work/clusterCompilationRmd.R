#Script simple pour compiler sur cluster un rmarkdown

# print(future::availableCores())

# rmarkdown::render('work/vignette/vignette_SAEM.Rmd')
filename <- gsub(' ','_',  gsub('-','_', gsub(':','_', Sys.time()) ))
rmarkdown::render('work/JLS_bis.Rmd', output_file = filename)



# rmarkdown::render('work/achieved_model/Longitudinal_nonlinear_mixed_model_SAEM.Rmd')
# rmarkdown::render('work/vignette/vignette_MH.Rmd')
# rmarkdown::render('work/test_variance.Rmd')

#cd /work_home/acaillebotte
#cd SAEM4JLS


# qsub -cwd -N vignette_MH -q short.q -pe thread 4 -o 'cluster.out' -e 'cluster.err' -b y "Rscript work/clusterCompilationRmd.R"
#qsub -cwd -N vignette_SAEM -q short.q -pe thread 4 -b y "Rscript rmarkdown::render('work/vignette_SAEM.Rmd')"

#qsub -cwd -N vignette_SAEM -q short.q -pe thread 16 -o 'cluster.out' -e 'cluster.err' -b y "Rscript work/clusterCompilationRmd.R"


#qstat
#qdel -u acaillebotte



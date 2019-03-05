cat knit.sh
srun --x11 --partition=intel --mem=8gb --cpus-per-task 1 --ntasks 1 --time 02-0:00:00 --job-name=Knitr time Rscript -e "rmarkdown::render('systemPipeRNAseqIntegrateClusterProfiler.Rmd')"

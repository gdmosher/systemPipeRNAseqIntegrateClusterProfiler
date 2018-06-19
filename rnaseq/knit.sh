cat knit.sh
srun --x11 --partition=intel --mem=4gb --cpus-per-task 1 --ntasks 1 --time 07-0:00:00 --job-name=Knitr time Rscript -e "rmarkdown::render('systemPipeRNAseqIntegrateClusterProfiler.Rmd')"

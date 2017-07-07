#!/bin/bash -l

#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2G
#SBATCH --time=1-00:15:00     # 1 day and 15 minutes
#SBATCH --output=my.stdout
#SBATCH --mail-user=gmosh001@ucr.edu
#SBATCH --mail-type=ALL
#SBATCH --job-name="just_a_test"
#SBATCH -p intel # This is the default partition, you can use any of the following; intel, batch, highmem, gpu


# Print current date
date

# Load samtools
# module load samtools

# Change directory to where you submitted the job from, so that relative paths resolve properly
# cd $SLURM_SUBMIT_DIR

# Concatenate BAMs
R -e "rmarkdown::render('systemPipeRNAseqIntegrateClusterProfiler.Rmd')"

# Print name of node
hostname

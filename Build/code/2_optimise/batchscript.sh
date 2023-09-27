#!/bin/bash
#SBATCH -o /n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/output/sa_run.out
#SBATCH -e /n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/output/sa_run.err
#SBATCH -p shared
#SBATCH -N 1
#SBATCH -n 5
#SBATCH -t 1-23:00
#SBATCH --mem=140G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=tgraff@g.harvard.edu

module load matlab
matlab -nodisplay -nodesktop -r "run /n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/code/2_optimise/find_optimal_network_SERVER.m"

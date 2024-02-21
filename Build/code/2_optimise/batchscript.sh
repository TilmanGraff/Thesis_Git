#!/bin/bash
#SBATCH -o /n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/output/sa_run.out
#SBATCH -e /n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/output/sa_run.err
#SBATCH -p bigmem
#SBATCH -N 1
#SBATCH -n 20
#SBATCH -t 01-23:59
#SBATCH --mem=1800G
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=tgraff@g.harvard.edu

module load matlab
matlab -nodisplay -nodesktop -r "run /n/holystore01/LABS/kreindler_lab/Lab/transjakarta/tilman/spin/code/2_optimise/find_optimal_network_mobile_SERVER.m"

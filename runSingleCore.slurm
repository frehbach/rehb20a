#!/bin/bash
#SBATCH --job-name=bbobBPvsEI
#SBATCH --time=4-0:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1024
#SBATCH --partition=all
#SBATCH --output=slurmOut/%A-%a.out
#SBATCH --mail-type=NONE

echo "$SHELL"
pwd
source ~/.bashrc
for f in /etc/profile.d/*; do source $f; done
echo $PATH
which R
which python
which python3

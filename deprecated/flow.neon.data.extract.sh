#!/bin/bash -l

module load R/4.1.1
Rscript 'flow.siteDf.R'
# Notify with a beep sound when the script is done
printf '\a'
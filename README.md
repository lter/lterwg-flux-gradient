# LTER Synthesis working group -- The Flux Gradient Project: Understanding the methane sink-source capacity of natural ecosystems

## PIs: 

- Sparkle L. Malone, Assistant Professor, Yale University
- Jackie H. Matthes, Senior Scientist, Harvard University


## Project Summary

- https://lternet.edu/working-groups/the-flux-gradient-project/

## Guidelines for sharing scripts

- All code should include sufficient annotation and documentation so that other users can understand and run the scripts 
- Use RStudio projects to manage working directory
- Write your scripts such that other users can run the code without modifications. Keep file paths and other variables that might need to be changed at the beginning of the script, just after attaching the necessary libraries
- Check out the Scientific Computing Team’s best practice tips for [storing file paths](https://nceas.github.io/scicomp.github.io/best_practices.html#file-paths)
- Include an attribution header to your scripts or Rmarkdown documents

Example:

```r
## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: 
##
## Email: 
##
## ---------------------------

library(tidyverse)

```


## Supplementary Resources

NCEAS Scientific Computing Support Team page [link](https://nceas.github.io/scicomp.github.io)

## Description of Current Functions
Function Name: Description
SiteAttributes: Grab site tower measurement heights
Site.DF: Grab site gas concentrations, CO2 H LE fluxes, uStar, uBar, airtemp, airpress, z0, radiSwIn
Flux_Gradient_MBR: Use gas concentrations,  CO2 LE fluxes and tower measurement heights to estimate fluxes using modified bowen ratio
Flux_Gradient_AE: Use gas concentrations, CO2 H LE fluxes, uStar, uBar, airtemp, z0, radiSwIn, and tower measurement heights using aerodynamic model and wind profile

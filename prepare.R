### load packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(doBy, 
               ggplot2,
               cowplot,
               metafor,
               broom,
               dplyr,
               tidyverse, 
               plantecophys,
               gdata) 


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)

#### Create output folder
if(!dir.exists(paste0(getwd(), "/output"))) {
    dir.create(paste0(getwd(), "/output"), showWarnings = FALSE)
}


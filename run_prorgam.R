#### Script to analyze field-based CO2 enrichment dataset 

####
#### Author: Mingkai Jiang
####
##########################################################################
#### Step 1: basic set-up
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")

##########################################################################
#### make a global map to show the site distribution
make_global_map()

#### process the dataset to reformat and filter out poor quality entries
myDF <- data_processing_and_formatting()

#### fit g1 values
fit_g1_values_and_plot(myDF)

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


#### break data by vpd groups and summarize response ratios, then plot
#### return a summaryDF that calculates mean response ratios at different
#### VPD bins for different dataset
sDF <- break_data_by_VPD_bins_and_summarize_response_ratios(myDF)

#### us rma.mv function to generate meta-analysis results
fit_multivariate_model_and_plot(sDF)




#### end. 
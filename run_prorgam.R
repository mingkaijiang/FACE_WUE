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

#### plot site-specific climatic conditions
plot_site_specific_climate(myDF)

#### perform generalized additive model analysis to understand the 
#### relationship between VPD and WUE
perform_generalized_additive_model_for_each_dataset(myDF)

### notes: the above relationships indicate that, VPD bins should be
### study-specific. 
### Also, in additon to the likely influence if VPD, 
### some datasets have treatment differences in Tair and PAR.
### So in the model, we may need to consider these effects as well. 
### Below we will explore several ways of binning the VPD data:
### 1. no binning
### 2. a general bnning scheme of user-defined interval
### 3. a study-specific binning scheme
### We may need to remove outliers to further increase confidence in the analysis...

#### 1. no binning, assuming site-specific VPD is the site's prevailing VPD range



### perform leave one out analysis
perform_leave_one_out_analysis(sDF)


### us rma.mv function to generate meta-analysis results
fit_multivariate_model_and_plot(sDF)

#### break data by vpd groups and summarize response ratios, then plot
#### return a summaryDF that calculates mean response ratios at different
#### VPD bins for different dataset
sDF <- break_data_by_VPD_bins_and_summarize_response_ratios(myDF)





### 


#### make forest plot, based on site-specific prevailing VPD



#### To do:
#### 1. Effect of VPD bins - different datasets cover different VPD range, so it may not be appropriate to
####                         extrapolate CO2 response ratios beyond the site-specific observed VPD ranges.
#### 2. Do we need to split the data by season? Currently all seasons are lumped together within each dataset.
#### 3. Forest plot - how do we incorporate the VPD effect and make prediction?
#### 4. Heterogeneity test - to see whether CO2 response ratios differ significantly among groups (type or PFT).




#### end. 
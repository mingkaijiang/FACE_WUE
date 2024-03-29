### Script to analyze WUE database
### date created: 18-oct-2020


setwd("C:/Users/axg042/Desktop/Anna PhD/WSU project/1. Stomatal responses at Forest FACE/2. Datasets/R/stomatalresponses_analysis_NEW/output")


### load packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(doBy, 
               ggplot2,
               cowplot,
               metafor,
               broom,
               dplyr,
               tidyverse, 
               plantecophys)  


### read in data
myDF<- read.csv("WUEdatabase_fixed.csv")

### calculate WUE
myDF$WUE <- with(myDF, Photo/Cond)

### Warren_ORNl dataset has no ci values.# Need to assign one so that it is included n the analysis.
myDF$Ci[is.na(myDF$Ci)] <- "200"
myDF$Ci <- as.numeric(myDF$Ci) # Change Ci back to numeric


### revise season information
myDF$Season <- as.factor(myDF$Season)

## Filter to just have the summer season
myDF <- filter(myDF, Season == "summer")    ##If I just wanted to look  at summer responses.
myDF<- drop.levels(myDF)

### G1 values
myDF$fitgroup<-as.factor(myDF$fitgroup)
myDF$Dataset<-as.factor(myDF$Dataset)
myDF<-as.data.frame(myDF)

# Rename locations for plot
myDF$Location[myDF$Location =="Glencorse near Edinburgh"] <- "Glencorse"
myDF$Location[myDF$Location =="Duke Forest Chapel Hill"] <- "Duke FACE"
myDF$Location[myDF$Location =="POPFACE Italy"] <- "POPFACE"
myDF$Location[myDF$Location =="EucFACE Richmond"] <- "EucFACE"


graph_all <- function (myDF) {
  # checking model fits
  list <- split(myDF,myDF$Dataset)
  fit <- lapply(list,fitBB,gsmodel="BBOpti",
                varnames=list(VPD="VPD",ALEAF="Photo",GS="Cond",Ca="CO2S"))
  g1pars <- sapply(fit,function(x)x$coef[[2]])
  g1pars <- stack(g1pars)
  g1pars
  do.call(rbind, lapply(fit, coef))
  with(myDF,plot(Photo/sqrt(VPD)/CO2S,Cond,col=Dataset, group=Dataset)) #fitgroup
  #legend(x = 0.00, y = 2.0, legend = levels(myDF$Dataset),col = c(1:10), pch=1)
}

graph_all(myDF)




### Data structure:
### Experiment >> CO2 treatment >> Tree replicate >> leaf replicate >> Ci concentration
### Leaf gas exchange was measured for different CO2 manipulation experiments,
### We are interested to see the CO2 response ratio, normalized to the CO2 treatment concentration ratio,
### But at the same time, we have different SLA, Ci, tree and leaf identity associated with each data entry.
### So we need to provide mean and se for a certain level. 
### There is no useful information in tree and leaf identity, because we can't take the ratio of tree A / tree B in different CO2 treatments;
### So we may need to average values across experiment. 
### But there are many SLA, Ci within the same experiment, hence we may not see a strong CO2 response ratio.
### Let's check first. 

### summary dataset
sDF1 <- summaryBy(Photo+Cond+WUE+SLA+VPD+CO2S+Ci+PARin~Species+Season+Treatment+Pathway+Type+Plantform+Leafspan+Tregion+Wregion+Growthcond+Location+Country+Dataset+PFT+latitude+longitude+altitude,
                  FUN=c(mean, sd), data=myDF, keep.names=T, na.rm=T)

myDF$Photo.n <- ifelse(is.na(myDF$Photo), 0, 1)
myDF$Cond.n <- ifelse(is.na(myDF$Cond), 0, 1)
myDF$WUE.n <- ifelse(is.na(myDF$WUE), 0, 1)
myDF$SLA.n <- ifelse(is.na(myDF$SLA), 0, 1)

sDF2 <- summaryBy(Photo.n+Cond.n+WUE.n+SLA.n~Species+Season+Location+Dataset,
                  FUN=sum, data=myDF, keep.names=T, na.rm=T)


### merge the two
sDF <- merge(sDF1, sDF2, by=c("Species", "Season", "Location", "Dataset"))

### now separate by CO2 treatment
sDF1 <- sDF[sDF$Treatment == "Ambient CO2",]
sDF2 <- sDF[sDF$Treatment == "Elevated CO2",]

### remove two extra entries in aCO2 df
sDF1 <- sDF1[!is.na(sDF1$Pathway),]
sDF1 <- sDF1[!is.na(sDF1$Photo.sd),]

### change tree label in sDF2
sDF2$Plantform[sDF2$Species=="Phillyrea angustifolia"] <- "tree"


### merge the two

sDF <- merge(sDF1, sDF2, by=c("Species", "Season", "Location",
                              "Pathway", "Type", "Plantform", "Leafspan",
                              "Tregion", "Wregion", "Growthcond",
                              "Country", "Dataset", "PFT", "latitude", "longitude",
                              "altitude"))

### re-label all columns
colnames(sDF) <- c("Species", "Season", "Location",
                   "Pathway", "Type", "Plantform",
                   "Leafspan", "Tregion", "Wregion", "Growthcond",
                   "Country", "Dataset","PFT", "latitude",
                   "longitude", "altitude", 
                   "TreatmentA", 
                   "Photo.mean.aCO2", "Cond.mean.aCO2",
                   "WUE.mean.aCO2", "SLA.mean.aCO2", "VPD.mean.aCO2",
                   "CO2.mean.aCO2", "Ci.mean.aCO2",
                   "PARin.mean.aCO2", 
                   "Photo.sd.aCO2", "Cond.sd.aCO2",
                   "WUE.sd.aCO2", "SLA.sd.aCO2","VPD.sd.aCO2",
                   "CO2.sd.aCO2", "Ci.sd.aCO2",
                   "PARin.sd.aCO2", 
                   "Photo.n.aCO2", "Cond.n.aCO2",
                   "WUE.n.aCO2", "SLA.n.aCO2", 
                   "TreatmentE", 
                   "Photo.mean.eCO2", "Cond.mean.eCO2",
                   "WUE.mean.eCO2", "SLA.mean.eCO2","VPD.mean.eCO2",
                   "CO2.mean.eCO2", "Ci.mean.eCO2",
                   "PARin.mean.eCO2", 
                   "Photo.sd.eCO2", "Cond.sd.eCO2",
                   "WUE.sd.eCO2", "SLA.sd.eCO2","VPD.sd.eCO2",
                   "CO2.sd.eCO2", "Ci.sd.eCO2",
                   "PARin.sd.eCO2", 
                   "Photo.n.eCO2", "Cond.n.eCO2",
                   "WUE.n.eCO2", "SLA.n.eCO2")


### calculate response ratio and variance
sDF$CO2_resp <- with(sDF, CO2.mean.eCO2/CO2.mean.aCO2) 

sDF$Photo_resp <- with(sDF, log(Photo.mean.eCO2/Photo.mean.aCO2)/log(CO2_resp))
sDF$Cond_resp <- with(sDF, log(Cond.mean.eCO2/Cond.mean.aCO2)/log(CO2_resp))
sDF$WUE_resp <- with(sDF, log(WUE.mean.eCO2/WUE.mean.aCO2)/log(CO2_resp))

sDF$Photo_var <- with(sDF, 
                      (Photo.sd.eCO2 * Photo.sd.eCO2 / (Photo.n.eCO2 * Photo.mean.eCO2 * Photo.mean.eCO2) +
                          Photo.sd.aCO2 * Photo.sd.aCO2 / (Photo.n.aCO2 * Photo.mean.aCO2 * Photo.mean.aCO2))/log(CO2_resp))

sDF$Cond_var <- with(sDF, 
                     (Cond.sd.eCO2 * Cond.sd.eCO2 / (Cond.n.eCO2 * Cond.mean.eCO2 * Cond.mean.eCO2) +
                         Cond.sd.aCO2 * Cond.sd.aCO2 / (Cond.n.aCO2 * Cond.mean.aCO2 * Cond.mean.aCO2))/log(CO2_resp))


sDF$WUE_var <- with(sDF, 
                    (WUE.sd.eCO2 * WUE.sd.eCO2 / (WUE.n.eCO2 * WUE.mean.eCO2 * WUE.mean.eCO2) +
                        WUE.sd.aCO2 * WUE.sd.aCO2 / (WUE.n.aCO2 * WUE.mean.aCO2 * WUE.mean.aCO2))/log(CO2_resp))


### Previous response ratos of 1 = wrongly assigned CO2 treatments. 
## This occured for a) Betula papyrifera,Rhinelander; b) Picea sitchensis, Glencorse, 
                  #c) Populus tremuloides, Rhinelander and the 4 Leuzinger_SCC data points.
## Update: All fixed





# For Forest plots: ALL SEASONS


###################################################################################################################

# iWUE
test_WUE_DF <- sDF[complete.cases(sDF$WUE_resp),]
res_WUE <- rma(WUE_resp, WUE_var, mods = ~sDF$VPD.mean.eCO2, data = test_WUE_DF)

# Cond

test_cond_DF <- sDF[complete.cases(sDF$Cond_resp),]
res_cond <- rma(Cond_resp, Cond_var, mods = ~sDF$VPD.mean.eCO2, data = test_cond_DF)

#Photo
test_photo_DF <- sDF[complete.cases(sDF$Photo_resp),]
res_photo <- rma(Photo_resp, Photo_var, mods = ~sDF$VPD.mean.eCO2, data = test_photo_DF)





#################################################################################--------------------------------
## SUMMER ONLY


#iWUE

tiff('iWUE_forest_summer.tiff', units="in", width=9, height=6, res=500)
forest(res_WUE, 
       ylim=c(-1.5, 30), 
       xlim=c(-10, 5),rows=c(25:13,10:6,3:1),
       slab=paste(sDF$Location,sDF$Species, sep=", "),
       refline=0,
       order=order(sDF$PFT, sDF$Location),
       cex = 0.6,
       mlab="", psize=1, 
       header="iWUE response to eCO2")
forest(res_WUE,
       text(-7, c(26,11,4), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.6))
dev.off()


#Cond
tiff('Cond_forest_summer.tiff', units="in", width=9, height=6, res=500)
forest(res_cond, 
       ylim=c(-1.5, 30), 
       xlim=c(-10, 5),rows=c(25:13,10:6,3:1),
       slab=paste(sDF$Location, sDF$Species, sep=", "),
       refline=0,
       order=order(sDF$PFT,sDF$Location),
       cex = 0.6,
       mlab="", psize=1, 
       header="Stomatal conductance response to eCO2")
forest(res_cond,
       text(-7, c(26,11,4), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.6))
dev.off()



#Photo
tiff('Photo_forest_summer.tiff', units="in", width=9, height=6, res=500)
forest(res_photo, 
       ylim=c(-1.5, 30), 
       xlim=c(-10, 5),rows=c(25:13,10:6,3:1),
       slab=paste(sDF$Location, sDF$Species, sDF$Season, sep=", "),
       ilab = cbind(as.character(sDF$Season)),
       ilab.xpos = c(-60), 
       refline=0,
       order=order(sDF$PFT,sDF$Location),
       cex = 0.6,
       mlab="", psize=1, 
       header="Photosynthesis response to eCO2")
forest(res_photo,
       text(-7, c(26,11,4), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.6))
dev.off()














---------
## As '.tiff.'

#iWUE
tiff('iWUE_forest_2503.tiff', units="in", width=9, height=6, res=500)
forest(res_WUE, 
       ylim=c(-1.5, 35), 
       xlim=c(-10, 5),rows=c(31:10,7:1),
       slab=paste(sDF$Location, sDF$Species, sDF$Season, sep=", "),
       ilab = cbind(as.character(sDF$Season)),
       ilab.xpos = c(-60), 
       refline=0,
       order=order(sDF$PFT),
       cex = 0.6,
       mlab="", psize=1, 
       header="iWUE response to eCO2")
forest(res_WUE,
       text(-8, c(32,8), c( "Angiosperm", "Gymnosperm"), pos=2, font=4, cex=0.6))
dev.off()



#Cond

tiff('Cond_forest_2503.tiff', units="in", width=9, height=6, res=500)
forest(res_cond, 
       ylim=c(-1.5, 35), 
       xlim=c(-10, 5),rows=c(31:10,7:1),
       slab=paste(sDF$Location, sDF$Species, sDF$Season, sep=", "),
       ilab = cbind(as.character(sDF$Season)),
       ilab.xpos = c(-60), 
       refline=0,
       order=order(sDF$PFT),
       cex = 0.6,
       mlab="", psize=1, 
       header="Stomatal conductance response to eCO2")
forest(res_cond,
       text(-8, c(32,8), c("Angiosperm", "Gymnosperm"), pos=2, font=4, cex=0.6))
dev.off()



#Photo

tiff('Photo_forest_2503.tiff', units="in", width=9, height=6, res=500)
forest(res_photo, 
       ylim=c(-1.5, 35), 
       xlim=c(-10, 5),rows=c(31:10,7:1),
       slab=paste(sDF$Location, sDF$Species, sDF$Season, sep=", "),
       ilab = cbind(as.character(sDF$Season)),
       ilab.xpos = c(-60), 
       refline=0,
       order=order(sDF$PFT),
       cex = 0.6,
       mlab="", psize=1, 
       header="Photosynthesis response to eCO2")
forest(res_photo,
       text(-8, c(32,8), c("Angiosperm", "Gymnosperm"), pos=2, font=4, cex=0.6))
dev.off()






































###########################################################################################################################
###########################################################################################################################
# As PDF

# For Forest plots: (as PDF)

##WUE
### multivariate linear (mixed-effects) model with study as a random variable
test_WUE_DF <- sDF[complete.cases(sDF$WUE_resp),]
res_WUE <- rma(WUE_resp, WUE_var, data = test_WUE_DF)

### forest plot
## Create output folder
if(!dir.exists("output")) {
  dir.create("output", showWarnings = FALSE)
}

pdf(paste0("output/test_0903_WUE.pdf"),
    height=16, width=9)
forest(res_WUE, slab=paste(sDF$Location, sDF$Species, sDF$Season, sep=", "),
       ilab = cbind(as.character(sDF$Season)),
       ilab.xpos = c(-60), 
       refline=0,
       cex = 0.6,
       mlab="", psize=1, 
       header="Location, Species, Season")
dev.off()



##Cond
### multivariate linear (mixed-effects) model with study as a random variable
test_cond_DF <- sDF[complete.cases(sDF$Cond_resp),]
res_cond <- rma(Cond_resp, Cond_var, data = test_cond_DF)

### forest plot
## Create output folder
if(!dir.exists("output")) {
  dir.create("output", showWarnings = FALSE)
}

pdf(paste0("output/test_Cond_0903.pdf"),
    height=16, width=9)
forest(res_cond, slab=paste(sDF$Location, sDF$Species,sDF$Season, sep=", "),
       ilab = cbind(as.character(sDF$Season)),
       ilab.xpos = c(-60), 
       refline=0,
       cex = 0.6,
       mlab="", psize=1, 
       header="Location, Species, Season")
dev.off()





##Photo
### multivariate linear (mixed-effects) model with study as a random variable
test_photo_DF <- sDF[complete.cases(sDF$Photo_resp),]
res_photo <- rma(Photo_resp, Photo_var, data = test_photo_DF)

### forest plot
## Create output folder
if(!dir.exists("output")) {
  dir.create("output", showWarnings = FALSE)
}

pdf(paste0("output/test_Photo_0903.pdf"),
    height=16, width=9)
forest(res_photo, slab=paste(sDF$Location, sDF$Species, sDF$Season, sep=", "),
       ilab = cbind(as.character(sDF$Season)),
       ilab.xpos = c(-60), 
       refline=0,
       cex = 0.6,
       mlab="", psize=1, 
       header="Location, Species, Season")
dev.off()



####################################################################################################################

# Plot the 'mean response of A' versus 'the mean resposne of gsw'.
install.packages("viridis")  # Install
library("viridis")  

sDF$Location<- as.factor(sDF$Location)
sDF$Species<- as.factor(sDF$Species)



graph<- ggplot(sDF, aes(x=Photo_resp, y = Cond_resp, fill=Location, shape=PFT))+
  theme_bw()+
  geom_point(size=2)+
  scale_shape_manual(values=c(21,24, 22))+
  scale_fill_viridis(option = "D", discrete = TRUE)+
  guides(fill=guide_legend(override.aes=list(shape=21)))+
  scale_x_continuous(expand = c(0, 0),limits=c(-0.5,2.5), breaks=seq(-0.5,2.5,0.5))+
  scale_y_continuous(expand = c(0, 0),limits=c(-1.5,1.5), breaks=seq(-1.5,1.5,0.5))+ 
  geom_abline(slope=1, intercept=-1); graph


tiff('Mean Photo vs mean Cond_scatterplot.tiff', units="in", width=8, height=6, res=500)
graph
dev.off()


####################################################################################################################

## Supplemental figure to show the data distributon with CO2 treatment.

myDF$Treatment<- as.factor(myDF$Treatment)


graph<- ggplot(myDF, aes(x=Photo/sqrt(VPD)/CO2S, y = Cond, 
       shape = Treatment, fill = Treatment))+
  theme_bw()+
  geom_point(colour="black")+
  facet_wrap(~ Dataset, nrow = 3)+
  scale_shape_manual(values=c(21,24)) +
  scale_fill_manual(values=c("blue","red")); graph


tiff('G1 Scatterplots_Datasets.tiff', units="in", width=14, height=10, res=500)
graph
dev.off()










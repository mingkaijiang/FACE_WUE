### Script to analyze WUE database
### date created: 18-oct-2020

#### clear wk space
rm(list=ls(all=TRUE))

#### set directory
#setwd("C:/Users/axg042/Desktop/Anna PhD/WSU project/1. Stomatal responses at Forest FACE/2. Datasets/R/stomatalresponses_analysis_NEW/output")


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

#### Create output folder
if(!dir.exists(paste0(getwd(), "/output"))) {
  dir.create(paste0(getwd(), "/output"), showWarnings = FALSE)
}


### read in data
myDF<- read.csv(paste0(getwd(), "/data/WUEdatabase_fixed.csv"))

### calculate WUE
myDF$WUE <- with(myDF, Photo/Cond)

### Warren_ORNl dataset has no ci values.# Need to assign one so that it is included n the analysis.
myDF$Ci[is.na(myDF$Ci)] <- "200"
myDF$Ci <- as.numeric(myDF$Ci) # Change Ci back to numeric

### change tree label in sDF2
myDF$Plantform[myDF$Species=="Phillyrea angustifolia"] <- "tree"

### make CO2 treatment consistent
myDF$Treatment[myDF$Treatment%in%c("Outside Control", "Control")] <- "Ambient CO2"

myDF$Treatment[myDF$Treatment=="OTC CO2" & myDF$GrowthCa%in%c("Ambient CO2", "Ambient")] <- "Ambient CO2"
myDF$Treatment[myDF$Treatment=="OTC CO2" & myDF$GrowthCa%in%c("Elevated CO2", "Elevated")] <- "Elevated CO2"

### only include CO2 treatment
myDF <- myDF[myDF$Treatment %in%c("Ambient CO2", "Elevated CO2"),]


### revise season information
myDF$Season <- as.factor(myDF$Season)

## Filter to just have the summer season
#myDF <- filter(myDF, Season == "summer")    ##If I just wanted to look  at summer responses.
myDF<- drop.levels(myDF)

### Bader_ACI_curves_enhancement_ratios dataset has no VPD values, give it a random value
#myDF$VPD[myDF$Dataset=="Bader_ACI_curves_enhancement_ratios"] <- 1.0
myDF <- myDF[myDF$Dataset!="Bader_ACI_curves_enhancement_ratios",]
myDF <- myDF[!is.na(myDF$VPD), ]

### G1 values
myDF$fitgroup<-as.factor(myDF$fitgroup)
myDF$Dataset<-as.factor(myDF$Dataset)
myDF<-as.data.frame(myDF)

# Rename locations for plot
myDF$Location[myDF$Location =="Glencorse near Edinburgh"] <- "Glencorse"
myDF$Location[myDF$Location =="Duke Forest Chapel Hill"] <- "Duke FACE"
myDF$Location[myDF$Location =="POPFACE Italy"] <- "POPFACE"
myDF$Location[myDF$Location =="EucFACE Richmond"] <- "EucFACE"

### there are two missing values in conductance, ignore them
myDF <- myDF[complete.cases(myDF$Cond),]


#graph_all <- function (myDF) {
#  # checking model fits
#  list <- split(myDF,myDF$Dataset)
#  fit <- lapply(list,fitBB,gsmodel="BBOpti",
#                varnames=list(VPD="VPD",ALEAF="Photo",GS="Cond",Ca="CO2S"))
#  g1pars <- sapply(fit,function(x)x$coef[[2]])
#  g1pars <- stack(g1pars)
#  g1pars
#  do.call(rbind, lapply(fit, coef))
#  with(myDF,plot(Photo/sqrt(VPD)/CO2S,Cond,col=Dataset))#, group=Dataset)) #fitgroup
#  #legend(x = 0.00, y = 2.0, legend = levels(myDF$Dataset),col = c(1:10), pch=1)
#}
#
#pdf(paste0(getwd(), "/output/photo_vs_cond.pdf"))
#graph_all(myDF)
#dev.off()

### Let's check VPD distribution in different dataset
p1 <- ggplot(myDF, aes(x=VPD))+
  geom_histogram(aes(color=Treatment, fill=Treatment), 
                 alpha=0.5, binwidth=0.2)+
  facet_grid(Dataset ~ ., scales="free")
  
pdf(paste0(getwd(), "/output/VPD_distribution_by_dataset.pdf"), width=4, height=10)
plot(p1)
dev.off()


### To continue the meta-analysis, we will need to decide how to treat different VPD in different dataset and CO2 treatment
### Two options:
###             1. To bin VPD into categorical bins and then calculate CO2 response ratio within each bin
###             2. To filter out outliers (i.e. outside 95th percentile), and then calcuklate 
### Let's try categorize the dataset into different VPD bins first and see how it goes.

breaks <- c(seq(0, 6.8, 0.4))

#tags <- c("[0-0.2)","[0.2-0.4)", "[0.4-0.6)", "[0.6-0.8)", "[0.8-1.0)", 
#          "[1.0-1.2)","[1.2-1.4)", "[1.4-1.6)","[1.6-1.8)", "[1.8-2.0)",
#          "[2.0-2.2)","[2.2-2.4)", "[2.4-2.6)","[2.6-2.8)", "[2.8-3.0)",
#          "[3.0-3.2)","[3.2-3.4)", "[3.4-3.6)","[3.6-3.8)", "[3.8-4.0)",
#          "[4.0-4.2)","[4.2-4.4)", "[4.4-4.6)","[4.6-4.8)", "[4.8-5.0)",
#          "[5.0-5.2)","[5.2-5.4)", "[5.4-5.6)","[5.6-5.8)", "[5.8-6.0)",
#          "[6.0-6.2)","[6.2-6.4)", "[6.4-6.6)")


#tags <- c("[0-0.4)","[0.4-0.8)", "[0.8-1.2)", "[1.2-1.6)", "[1.6-2.0)", 
#          "[2.0-2.4)","[2.4-2.8)", "[2.8-3.2)","[3.2-3.6)", "[3.6-4.0)",
#          "[4.0-4.4)","[4.4-4.8)", "[4.8-5.2)","[5.2-5.6)", "[5.6-6.0)",
#          "[6.0-6.4)","[6.4-6.8)")

tags <- c(seq(0.2, 6.6, 0.4))

myDF$VPD_group <- cut(myDF$VPD, 
                       breaks=breaks, 
                       include.lowest=TRUE, 
                       right=FALSE, 
                       labels=tags)

myDF$VPD_group <- as.numeric(as.character(myDF$VPD_group))

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
sDF1 <- summaryBy(Photo+Cond+WUE+SLA+VPD+CO2S+Ci+PARin~Species+Season+Treatment+Pathway+Type+Plantform+Leafspan+Tregion+Wregion+Growthcond+Location+Country+Dataset+PFT+latitude+longitude+altitude+VPD_group,
                  FUN=c(mean, sd), data=myDF, keep.names=T, na.rm=T)

myDF$Photo.n <- ifelse(is.na(myDF$Photo), 0, 1)
myDF$Cond.n <- ifelse(is.na(myDF$Cond), 0, 1)
myDF$WUE.n <- ifelse(is.na(myDF$WUE), 0, 1)
myDF$SLA.n <- ifelse(is.na(myDF$SLA), 0, 1)

sDF2 <- summaryBy(Photo.n+Cond.n+WUE.n+SLA.n~Species+Season+Location+Dataset+Treatment+VPD_group,
                  FUN=sum, data=myDF, keep.names=T, na.rm=T)


### merge the two
sDF <- merge(sDF1, sDF2, by=c("Species", "Season", "Location", "Dataset", 
                              "Treatment", "VPD_group"))

### now separate by CO2 treatment
sDF1 <- sDF[sDF$Treatment == "Ambient CO2",]
sDF2 <- sDF[sDF$Treatment == "Elevated CO2",]


### check number of samples in aCO2 and eCO2 dataset
#table(sDF1$Photo.n)
#table(sDF2$Photo.n)
#
#test1 <- subset(sDF1, Photo.n==1)
#test2 <- subset(sDF2, Photo.n==1)
#
#unique(test1$VPD_group)
#unique(test2$VPD_group)


### merge the two
sDF <- merge(sDF1, sDF2, by=c("Species", "Season", "Location",
                              "Pathway", "Type", "Plantform", "Leafspan",
                              "Tregion", "Wregion", "Growthcond",
                              "Country", "Dataset", "PFT", "latitude", "longitude",
                              "altitude", "VPD_group"))

### re-label all columns
colnames(sDF) <- c("Species", "Season", "Location",
                   "Pathway", "Type", "Plantform",
                   "Leafspan", "Tregion", "Wregion", "Growthcond",
                   "Country", "Dataset","PFT", "latitude",
                   "longitude", "altitude", "VPD_group",
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
### many data entries has eCO2/aCO2 < 1, that means eCO2 concentration is smaller than
### aCO2 concentration. Labelling error?

sDF <- subset(sDF, CO2_resp > 1)


### calculate response ratios
sDF$Photo_resp <- with(sDF, log(Photo.mean.eCO2/Photo.mean.aCO2)/log(CO2_resp))
sDF$Cond_resp <- with(sDF, log(Cond.mean.eCO2/Cond.mean.aCO2)/log(CO2_resp))
sDF$WUE_resp <- with(sDF, log(WUE.mean.eCO2/WUE.mean.aCO2)/log(CO2_resp))


### calculate variance
sDF$Photo_var <- with(sDF, 
                      (Photo.sd.eCO2 * Photo.sd.eCO2 / (Photo.n.eCO2 * Photo.mean.eCO2 * Photo.mean.eCO2) +
                          Photo.sd.aCO2 * Photo.sd.aCO2 / (Photo.n.aCO2 * Photo.mean.aCO2 * Photo.mean.aCO2))/log(CO2_resp))

sDF$Cond_var <- with(sDF, 
                     (Cond.sd.eCO2 * Cond.sd.eCO2 / (Cond.n.eCO2 * Cond.mean.eCO2 * Cond.mean.eCO2) +
                         Cond.sd.aCO2 * Cond.sd.aCO2 / (Cond.n.aCO2 * Cond.mean.aCO2 * Cond.mean.aCO2))/log(CO2_resp))


sDF$WUE_var <- with(sDF, 
                    (WUE.sd.eCO2 * WUE.sd.eCO2 / (WUE.n.eCO2 * WUE.mean.eCO2 * WUE.mean.eCO2) +
                        WUE.sd.aCO2 * WUE.sd.aCO2 / (WUE.n.aCO2 * WUE.mean.aCO2 * WUE.mean.aCO2))/log(CO2_resp))


### add continuous factor for VPD values
sDF$VPDmean <- round((sDF$VPD.mean.aCO2+sDF$VPD.mean.eCO2)/2, 1)

### replace all inf numbers to NAs
is.na(sDF) <- do.call(cbind,lapply(sDF, is.infinite))


### Previous response ratos of 1 = wrongly assigned CO2 treatments. 
## This occured for a) Betula papyrifera,Rhinelander; b) Picea sitchensis, Glencorse, 
                  #c) Populus tremuloides, Rhinelander and the 4 Leuzinger_SCC data points.
## Update: All fixed





# For Forest plots: ALL SEASONS


###################################################################################################################

### iWUE
wueDF <- sDF[complete.cases(sDF$WUE_resp),]

### separate into different vegetation type
wueDF.ang <- subset(wueDF, Type=="angiosperm")
wueDF.gym <- subset(wueDF, Type=="gymnosperm")


### multi-variate linear mixed effect model
res_WUE1 <- rma.mv(WUE_resp, WUE_var, mods = ~VPD_group, random = ~1|Dataset, data = wueDF.ang)
res_WUE2 <- rma.mv(WUE_resp, WUE_var, mods = ~VPD_group, random = ~1|Dataset, data = wueDF.gym)

### predict effect size, at different VPD values
predDF1 <- predict(res_WUE1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
predDF2 <- predict(res_WUE2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)

predDF1$lab <- "Angiosperm"
predDF2$lab <- "Gymnosperm"

plotDF1 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2))

### make a plot
p1 <- ggplot(plotDF1) +
  geom_col(aes(X.VPD_group, pred, fill=lab, group=lab),
           position=position_dodge(), col="black")+
  geom_errorbar(aes(x=X.VPD_group, ymin=pred-se, ymax=pred+se, group=lab), 
                position=position_dodge(), width = 0.4)+
  theme_linedraw() +
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="right",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("WUE CO2 response")

#plot(p1)



### Conductance
condDF <- sDF[complete.cases(sDF$Cond_resp),]

### separate into different vegetation type
condDF.ang <- subset(condDF, Type=="angiosperm")
condDF.gym <- subset(condDF, Type=="gymnosperm")


### multi-variate linear mixed effect model
res_cond1 <- rma.mv(Cond_resp, Cond_var, mods = ~VPD_group, random = ~1|Dataset, data = condDF.ang)
res_cond2 <- rma.mv(Cond_resp, Cond_var, mods = ~VPD_group, random = ~1|Dataset, data = condDF.gym)

### predict effect size, at different VPD values
predDF1 <- predict(res_cond1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
predDF2 <- predict(res_cond2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)

predDF1$lab <- "Angiosperm"
predDF2$lab <- "Gymnosperm"

plotDF2 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2))

### make a plot
p2 <- ggplot(plotDF2) +
  geom_col(aes(X.VPD_group, pred, fill=lab, group=lab),
           position=position_dodge(), col="black")+
  geom_errorbar(aes(x=X.VPD_group, ymin=pred-se, ymax=pred+se, group=lab), 
                position=position_dodge(), width = 0.4)+
  theme_linedraw() +
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="right",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("Conductance CO2 response")

#plot(p2)



#Photo
photoDF <- sDF[complete.cases(sDF$Photo_resp),]

### separate into different vegetation type
photoDF.ang <- subset(photoDF, Type=="angiosperm")
photoDF.gym <- subset(photoDF, Type=="gymnosperm")


### multi-variate linear mixed effect model
res_photo1 <- rma.mv(Photo_resp, Photo_var, mods = ~VPD_group, random = ~1|Dataset, data = photoDF.ang)
res_photo2 <- rma.mv(Photo_resp, Photo_var, mods = ~VPD_group, random = ~1|Dataset, data = photoDF.gym)

### predict effect size, at different VPD values
predDF1 <- predict(res_photo1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
predDF2 <- predict(res_photo2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)

predDF1$lab <- "Angiosperm"
predDF2$lab <- "Gymnosperm"

plotDF3 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2))

### make a plot
p3 <- ggplot(plotDF3) +
  geom_col(aes(X.VPD_group, pred, fill=lab, group=lab),
           position=position_dodge(), col="black")+
  geom_errorbar(aes(x=X.VPD_group, ymin=pred-se, ymax=pred+se, group=lab), 
                position=position_dodge(), width = 0.4)+
  theme_linedraw() +
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="right",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("Photosynthesis CO2 response")

#plot(p3)


### plot
combined_plot <- plot_grid(p1, p2, p3, 
                           ncol=1, align="vh", axis = "l")

save_plot(paste0(getwd(), "/output/predicted_response.pdf"),
          combined_plot, base_width = 6, base_height=10)








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










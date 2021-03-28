### Script to analyze WUE database
### date created: 18-oct-2020

### read in data
myDF <- read.csv("data/WUEdatabase_checked.csv")

### calculate WUE
myDF$WUE <- with(myDF, Photo/Cond)

### remove outliers
myDF <- subset(myDF, Ci >= 0)
myDF <- myDF[!is.na(myDF$Cond),]

### remove non-CO2 treatment
myDF <- myDF[myDF$Treatment%in%c("Ambient CO2", "Elevated CO2"),]

### revise season information
myDF$Season <- as.character(myDF$Season)
myDF$Season[is.na(myDF$Season)] <- "unclear"

#with(myDF, plot(WUE~Ci, col=Treatment))

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
sDF1 <- summaryBy(Photo+Cond+WUE+SLA+CO2S+Ci+PARin~Species+Sp.Abb+Season+Treatment+Pathway+Type+Plantform+Leafspan+Tregion+Wregion+Growthcond+Location+Loc.Abb+Country+PFT+latitude+longitude+altitude,
                  FUN=c(mean, sd), data=myDF, keep.names=T, na.rm=T)

myDF$Photo.n <- ifelse(is.na(myDF$Photo), 0, 1)
myDF$Cond.n <- ifelse(is.na(myDF$Cond), 0, 1)
myDF$WUE.n <- ifelse(is.na(myDF$WUE), 0, 1)
myDF$SLA.n <- ifelse(is.na(myDF$SLA), 0, 1)

sDF2 <- summaryBy(Photo.n+Cond.n+WUE.n+SLA.n~Species+Season+Location,
                  FUN=sum, data=myDF, keep.names=T, na.rm=T)


### merge the two
sDF <- merge(sDF1, sDF2, by=c("Species", "Season", "Location"))

### now separate by CO2 treatment
sDF1 <- sDF[sDF$Treatment == "Ambient CO2",]
sDF2 <- sDF[sDF$Treatment == "Elevated CO2",]

### remove two extra entries in aCO2 df
sDF1 <- sDF1[!is.na(sDF1$Pathway),]
sDF1 <- sDF1[!is.na(sDF1$Photo.sd),]

### change tree label in sDF2
sDF2$Plantform[sDF2$Species=="Phillyrea angustifolia"] <- "tree"

### merge the two
sDF <- merge(sDF1, sDF2, by=c("Species", "Season", "Location", "Sp.Abb",
                              "Pathway", "Type", "Plantform", "Leafspan",
                              "Tregion", "Wregion", "Growthcond", "Loc.Abb",
                              "Country", "PFT", "latitude", "longitude",
                              "altitude"))

### re-label all columns
colnames(sDF) <- c("Species", "Season", "Location",
                   "Sp.Abb", "Pathway", "Type", "Plantform",
                   "Leafspan", "Tregion", "Wregion", "Growthcond",
                   "Loc.Abb", "Country", "PFT", "latitude",
                   "longitude", "altitude", 
                   "TreatmentA", 
                   "Photo.mean.aCO2", "Cond.mean.aCO2",
                   "WUE.mean.aCO2", "SLA.mean.aCO2",
                   "CO2.mean.aCO2", "Ci.mean.aCO2",
                   "PARin.mean.aCO2", 
                   "Photo.sd.aCO2", "Cond.sd.aCO2",
                   "WUE.sd.aCO2", "SLA.sd.aCO2",
                   "CO2.sd.aCO2", "Ci.sd.aCO2",
                   "PARin.sd.aCO2", 
                   "Photo.n.aCO2", "Cond.n.aCO2",
                   "WUE.n.aCO2", "SLA.n.aCO2", 
                   "TreatmentE", 
                   "Photo.mean.eCO2", "Cond.mean.eCO2",
                   "WUE.mean.eCO2", "SLA.mean.eCO2",
                   "CO2.mean.eCO2", "Ci.mean.eCO2",
                   "PARin.mean.eCO2", 
                   "Photo.sd.eCO2", "Cond.sd.eCO2",
                   "WUE.sd.eCO2", "SLA.sd.eCO2",
                   "CO2.sd.eCO2", "Ci.sd.eCO2",
                   "PARin.sd.eCO2", 
                   "Photo.n.eCO2", "Cond.n.eCO2",
                   "WUE.n.eCO2", "SLA.n.eCO2")


### calculate response ratio and variance
sDF$CO2_resp <- with(sDF, CO2.mean.eCO2/CO2.mean.aCO2)

### there are some strange CO2 response ratios, i.e. = 1, remove these points
sDF <- sDF[sDF$CO2_resp != 1,]

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



### multivariate linear (mixed-effects) model with study as a random variable
testDF <- sDF[complete.cases(sDF$WUE_resp),]
res <- rma(WUE_resp, WUE_var, data = testDF)

### forest plot
## Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

pdf(paste0("output/test.pdf"),
    height=16, width=9)
forest(res, slab = sDF$Loc.Abb,
       ilab = cbind(as.character(sDF$Sp.Abb),
                    as.character(sDF$Season)),
       ilab.xpos = c(-80, -60), 
       cex = 0.6)
dev.off()



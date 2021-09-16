## Simple forest plot

# without considering VPD effect.
#- No VPD bins.
#- No mod = VPDmean
# We do need mod=PFT, 1 column to describe VPD range and have 1 graph with species and 1 without.


simple_forest <- function(inDF) {
  
  ### summary dataset
  inDF <- summaryBy(Photo+Cond+WUE+SLA+VPD+CO2S+Ci+PARin+Tair+Tleaf~
                      Dataset+Species+Treatment+Type+Leafspan+PFT+Tregion+Wregion+Growthcond+TreeNum,
                    FUN=c(mean), data=inDF, keep.names=T, na.rm=T)
  
  
  inDF$Photo.n <- ifelse(is.na(inDF$Photo), 0, 1)
  inDF$Cond.n <- ifelse(is.na(inDF$Cond), 0, 1)
  inDF$WUE.n <- ifelse(is.na(inDF$WUE), 0, 1)
  
  inDF$Season <- as.character(inDF$Season)
  inDF$Dataset <- as.character(inDF$Dataset)
  
  
  ### count number of data entries
  tmpDF <- summaryBy(Photo.n+Cond.n+WUE.n~Dataset+Species+Treatment,
                     FUN=sum, data=inDF, keep.names=T, na.rm=T)
  
  ### summary dataset
  sDF <- summaryBy(Photo+Cond+WUE+SLA+VPD+CO2S+Ci+PARin+Tair+Tleaf~Dataset+Species+Age+Treatment+Type+Leafspan+PFT+Tregion+Wregion+Growthcond,
                   FUN=c(mean, sd), data=inDF, keep.names=T, na.rm=T)
  
  ### merge the two
  sDF <- merge(sDF, tmpDF, by=c("Dataset", "Species", "Treatment"))
  
  ### now separate by CO2 treatment
  sDF1 <- sDF[sDF$Treatment == "Ambient CO2",]
  sDF2 <- sDF[sDF$Treatment == "Elevated CO2",]
  
  ### merge the two
  sDF <- merge(sDF1, sDF2, by=c("Dataset", "Type","Species","Growthcond",
                                "Leafspan", "Tregion","Wregion", "Growthcond", "PFT"))
  
  
  
  ### re-label all columns
  colnames(sDF) <- c("Dataset","Type", "Species","Growthcond",
                     "Leafspan", "Tregion", "Wregion",  "PFT",
                     "TreatmentA", 
                     "Photo.mean.aCO2", "Cond.mean.aCO2",
                     "WUE.mean.aCO2", "SLA.mean.aCO2", "VPD.mean.aCO2",
                     "CO2.mean.aCO2", "Ci.mean.aCO2",
                     "PARin.mean.aCO2", "Tair.mean.aCO2", "Tleaf.mean.aCO2", 
                     "Photo.sd.aCO2", "Cond.sd.aCO2",
                     "WUE.sd.aCO2", "SLA.sd.aCO2","VPD.sd.aCO2",
                     "CO2.sd.aCO2", "Ci.sd.aCO2",
                     "PARin.sd.aCO2", "Tair.sd.aCO2", "Tleaf.sd.aCO2", 
                     "Photo.n.aCO2", "Cond.n.aCO2", "WUE.n.aCO2", 
                     "TreatmentE", 
                     "Photo.mean.eCO2", "Cond.mean.eCO2",
                     "WUE.mean.eCO2", "SLA.mean.eCO2","VPD.mean.eCO2",
                     "CO2.mean.eCO2", "Ci.mean.eCO2",
                     "PARin.mean.eCO2", "Tair.mean.eCO2", "Tleaf.mean.eCO2", 
                     "Photo.sd.eCO2", "Cond.sd.eCO2",
                     "WUE.sd.eCO2", "SLA.sd.eCO2","VPD.sd.eCO2",
                     "CO2.sd.eCO2", "Ci.sd.eCO2",
                     "PARin.sd.eCO2", "Tair.sd.eCO2", "Tleaf.sd.eCO2", 
                     "Photo.n.eCO2", "Cond.n.eCO2","WUE.n.eCO2")
  
  
  
  ### calculate response ratio and variance
  sDF$CO2_resp <- with(sDF, CO2.mean.eCO2/CO2.mean.aCO2) 
  
  
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
  
  
    ### replace all inf numbers to NAs
  is.na(sDF) <- do.call(cbind,lapply(sDF, is.infinite))
  
  #SAVE DATA
  sDF <- read.csv("data/simple forest.csv",header=T)
  sDF$X <- NULL
  
  
  #### Make simplified forest plot
  wueDF <- sDF[complete.cases(sDF$WUE_resp),]
  wueDF <- sDF[order(wueDF$PFT, wueDF$Species, wueDF$Dataset),]
  wueDF.angE <- subset(wueDF, PFT=="evergreen angiosperm")
  wueDF.angD <- subset(wueDF, PFT=="deciduous angiosperm")
  wueDF.gym <- subset(wueDF, PFT=="evergreen gymnosperm")
  l1 <- length(wueDF$Dataset)
  ns1 <- length(unique(wueDF$Dataset))
  
  condDF <- sDF[complete.cases(sDF$Cond_resp),]
  condDF <- condDF[order(condDF$PFT, condDF$Species, condDF$Dataset),]
  condDF.angE <- subset(condDF, PFT=="evergreen angiosperm")
  condDF.angD <- subset(condDF, PFT=="deciduous angiosperm")
  condDF.gym <- subset(condDF, PFT=="evergreen gymnosperm")
  l2 <- length(condDF$Dataset)
  ns2 <- length(unique(condDF$Dataset))
  
  photoDF <- sDF[complete.cases(sDF$Photo_resp),]
  photoDF <- photoDF[order(photoDF$PFT, photoDF$Species, photoDF$Dataset),]
  photoDF.angE <- subset(photoDF, PFT=="evergreen angiosperm")
  photoDF.angD <- subset(photoDF, PFT=="deciduous angiosperm")
  photoDF.gym <- subset(photoDF, PFT=="evergreen gymnosperm")
  l3 <- length(photoDF$Dataset)
  ns3 <- length(unique(photoDF$Dataset))
  

  
  
  
  #---
  res_WUE_EBF_S <- rma.mv(WUE_resp, WUE_var,  random = ~1|Dataset, data = wueDF.angE) 
  res_WUE_DBF_S <- rma.mv(WUE_resp, WUE_var,  random = ~1|Dataset, data = wueDF.angD) 
  res_WUE_ENF_S <- rma.mv(WUE_resp, WUE_var,  random = ~1|Dataset, data = wueDF.gym) 
  
  pdf(paste0(getwd(), "/output/forest_simple_WUE_EBF2.pdf"),width=4, height=2.4)
  forest(res_WUE_EBF_S, slab=paste(wueDF.angE$Dataset, wueDF.angE$Species, sep=", "),
         xlim=c(-2, 2.5),
         ylim=c(-1, 6),
         rows=c(3:1),
         at=c(0,0.5,1,1.5,2),
         refline=1,
         mlab="", psize=1, 
         annotate=FALSE,
         cex=0.5,
         order=order(wueDF.angE$PFT,wueDF.angE$Dataset,wueDF.angE$Species),
         header="EBF")
  text(1.5, 5, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  pdf(paste0(getwd(), "/output/forest_simple_WUE_DBF2.pdf"),width=4, height=3.5)
  forest(res_WUE_DBF_S, slab=paste(wueDF.angD$Dataset, wueDF.angD$Species, sep=", "),
         xlim=c(-2, 2.5),
         ylim=c(-1, 14),
         rows=c(12:1),
         at=c(0,0.5,1,1.5,2),
         refline=1,
         mlab="", psize=1, 
         annotate=FALSE,
         cex=0.5,
         order=order(wueDF.angD$PFT,wueDF.angD$Dataset,wueDF.angD$Species),
         header="DBF")
  #text(2, 13, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  pdf(paste0(getwd(), "/output/forest_simple_WUE_ENF2.pdf"),width=4, height=2.5)
  forest(res_WUE_ENF_S, slab=paste(wueDF.gym$Dataset, wueDF.gym$Species, sep=", "),
         xlim=c(-2, 2.5),
         ylim=c(-1, 7),
         rows=c(5:1),
         at=c(0,0.5,1,1.5,2),
         refline=1,
         mlab="", psize=1, 
         annotate=FALSE,
         cex=0.5,
         order=order(wueDF.gym$PFT,wueDF.gym$Dataset,wueDF.gym$Species),
         header="ENF")
  #text(2, 6, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  
  # Stomatal conductance
  
  res_Cond_EBF_S <- rma.mv(Cond_resp, Cond_var,  random = ~1|Dataset, data = condDF.angE) 
  res_Cond_DBF_S <- rma.mv(Cond_resp, Cond_var,  random = ~1|Dataset, data = condDF.angD) 
  res_Cond_ENF_S <- rma.mv(Cond_resp, Cond_var,  random = ~1|Dataset, data = condDF.gym)
  
  
  pdf(paste0(getwd(), "/output/forest_simple_Cond_EBF2.pdf"),width=4, height=2.4)
  forest(res_Cond_EBF_S, slab=paste(condDF.angE$Dataset, condDF.angE$Species, sep=", "),
         xlim=c(-4, 2),
         ylim=c(-1, 6),
         rows=c(3:1),
         at=c(-1.0, -0.5, 0.0, 0.5, 1.0),
         refline=0,
         mlab="", psize=1, 
         annotate=FALSE,
         cex=0.5,
         order=order(condDF.angE$PFT,condDF.angE$Dataset,condDF.angE$Species),
         header="EBF")
  text(0.5, 5, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  pdf(paste0(getwd(), "/output/forest_simple_Cond_DBF2.pdf"),width=4, height=3.5)
  forest(res_Cond_DBF_S, slab=paste(condDF.angD$Dataset, condDF.angD$Species, sep=", "),
         xlim=c(-4, 2),
         ylim=c(-1, 14),
         rows=c(12:1),
         at=c(-1.0, -0.5, 0.0, 0.5, 1.0),
         refline=0,
         mlab="", psize=1, 
         annotate=FALSE,
         cex=0.5,
         order=order(condDF.angD$PFT,condDF.angD$Dataset,condDF.angD$Species),
         header="DBF")
  #text(1, 13, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  pdf(paste0(getwd(), "/output/forest_simple_Cond_ENF2.pdf"),width=4, height=2.5)
  forest(res_Cond_ENF_S, slab=paste(condDF.gym$Dataset, condDF.gym$Species, sep=", "),
         xlim=c(-4, 2.5),
         ylim=c(-1, 7),
         rows=c(5:1),
         at=c(-1.0, -0.5, 0.0, 0.5, 1.0),
         refline=0,
         mlab="", psize=1,
         annotate=FALSE,
         cex=0.5,
         order=order(condDF.gym$PFT,condDF.gym$Dataset,condDF.gym$Species),
         header="ENF")
  #text(1, 6, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  

  
  
  # Photo
  res_Photo_EBF_S <- rma.mv(Photo_resp, Photo_var,  random = ~1|Dataset, data = photoDF.angE) 
  res_Photo_DBF_S <- rma.mv(Photo_resp, Photo_var,  random = ~1|Dataset, data = photoDF.angD)
  res_Photo_ENF_S <- rma.mv(Photo_resp, Photo_var,  random = ~1|Dataset, data = photoDF.gym) 
  
  
  
  pdf(paste0(getwd(), "/output/forest_simple_Photo_EBF2.pdf"),width=4, height=2.4)
  forest(res_Photo_EBF_S, slab=paste(photoDF.angE$Dataset, photoDF.angE$Species, sep=", "),
         xlim=c(-2.5, 2.5),
         ylim=c(-1, 6),
         rows=c(3:1),
         at=c(0,0.5,1,1.5),
         refline=0,
         mlab="", psize=1, 
         annotate=FALSE,
         cex=0.5,
         order=order(photoDF.angE$PFT,photoDF.angE$Dataset,photoDF.angE$Species),
         header="EBF")
  text(1, 5, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  pdf(paste0(getwd(), "/output/forest_simple_Photo_DBF2.pdf"),width=4, height=3.5)
  forest(res_Photo_DBF_S, slab=paste(photoDF.angD$Dataset, photoDF.angD$Species, sep=", "),
         xlim=c(-2.5, 2.5),
         ylim=c(-1, 14),
         rows=c(12:1),
         at=c(0,0.5,1,1.5),
         refline=0,
         mlab="", psize=1,
         annotate=FALSE,
         cex=0.5,
         order=order(photoDF.angD$PFT,photoDF.angD$Dataset,photoDF.angD$Species),
         header="DBF")
  #text(1, 13, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  pdf(paste0(getwd(), "/output/forest_simple_Photo_ENF2.pdf"),width=4, height=2.5)
  forest(res_Photo_ENF_S, slab=paste(photoDF.gym$Dataset, photoDF.gym$Species, sep=", "),
         xlim=c(-2.5, 3),
         ylim=c(-1, 7),
         rows=c(5:1),
         at=c(0,0.5,1,1.5),
         refline=0,
         mlab="", psize=1,
         annotate=FALSE,
         cex=0.5,
         order=order(photoDF.gym$PFT,photoDF.gym$Dataset,photoDF.gym$Species),
         header="ENF")
  #text(1, 6, "Relative Response [95% CI]", pos = 2, cex = 0.5)
  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #--------------------------------
  # Simple plot - one data entry per species per dataset, no VPD effect.
  
  pdf(paste0(getwd(), "/output/forest_simple_modPFT_Treenum.pdf"),width=10, height=10)
 
  ## WUE --------
  res_WUE <- rma.mv(WUE_resp, WUE_var,  random = ~1|Dataset, data = wueDF) #mods = ~PFT,
  forest(res_WUE, slab=paste(wueDF$Dataset, wueDF$Species, sep=", "),
         xlim=c(-6, 6),
         ylim=c(0, 24),
         rows=c(22:11,9:7,5:1),
         at=c(0,0.5,1,1.5,2),
         refline=1,
         mlab="", psize=1, 
         cex=0.6,
         order=order(wueDF$PFT,wueDF$Dataset,wueDF$Species),
         header="Water-use efficiency response to eCO2")
  text(2, 23, "Relative Response [95% CI]", pos = 2, cex = 0.7)
  text(-3.5, c(21,9,5), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.7)
  
  
  
  res_Cond <- rma.mv(Cond_resp, Cond_var, random = ~1|Dataset, data = condDF) #mods = ~PFT,
  
  forest(res_Cond, slab=paste(condDF$Dataset, condDF$Species, sep=", "), 
         xlim=c(-6, 6),
         ylim=c(0, 24),
         rows=c(24:11,9:7,5:1),
         at=c(-1.5,-1,0,-0.5,0,0.5,1),
         refline=0,
         mlab="", psize=1, 
         cex=0.6,
         order=order(condDF$PFT,condDF$Dataset,condDF$Species),
         header="Stomatal condutance response to eCO2")
  text(2, 23, "Relative Response [95% CI]", pos = 2, cex = 0.7)
  text(-3.5, c(21,9,5), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.7)
  
  
  ## Photo --------
  res_Photo <- rma.mv(Photo_resp, Photo_var, random = ~1|Dataset, data = photoDF) #mods = ~PFT,
  
  forest(res_Photo, slab=paste(photoDF$Dataset, photoDF$Species, sep=", "),
         xlim=c(-6, 6),
         ylim=c(0, 24),
         rows=c(24:11,9:7,5:1),
         at=c(0,0.5,1,1.5,2,2.5),
         refline=0,
         mlab="", psize=1, 
         cex=0.6,
         order=order(photoDF$PFT,photoDF$Dataset,photoDF$Species),
         header="Photosynthesis response to eCO2")
  text(2, 23, "Relative Response [95% CI]", pos = 2, cex = 0.7)
  text(-3.5, c(21,9,5), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.7)
  dev.off()
  
  
  
  print(res_WUE)
  print(res_Cond)
  print(res_Photo)
}  








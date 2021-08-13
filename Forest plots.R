forest_plots <- function(inDF, vpd.brks) {
  
  inDF <- myDF
  vpd.brks=c(seq(0, 7.0, 0.5))
  ### subset
  inDF <- subset(inDF, VPD<=max(vpd.brks))
  
  
  ### pass in the breaks
  breaks <- vpd.brks
  
  ### seq
  seq.val <- max(breaks)/(length(breaks)-1)
  
  
  tags <- c(seq(min(breaks)+(seq.val/2), max(breaks)-(seq.val/2), 
                by=seq.val))
  
  inDF$VPD_group <- cut(inDF$VPD, 
                        breaks=breaks, 
                        include.lowest=TRUE, 
                        right=FALSE, 
                        labels=tags)
  
  inDF$VPD_group <- as.numeric(as.character(inDF$VPD_group))
  
  ### summary dataset
  inDF <- summaryBy(Photo+Cond+WUE+SLA+VPD+CO2S+Ci+PARin+Tair+Tleaf~
                     Dataset+Species+Treatment+Type+Leafspan+PFT+Tregion+Wregion+Growthcond+VPD_group+TreeNum,
                   FUN=c(mean), data=inDF, keep.names=T, na.rm=T)
  
  
  inDF$Photo.n <- ifelse(is.na(inDF$Photo), 0, 1)
  inDF$Cond.n <- ifelse(is.na(inDF$Cond), 0, 1)
  inDF$WUE.n <- ifelse(is.na(inDF$WUE), 0, 1)
  
  
  ### count number of data entries
  tmpDF <- summaryBy(Photo.n+Cond.n+WUE.n~Dataset+Species+Treatment+VPD_group,
                     FUN=sum, data=inDF, keep.names=T, na.rm=T)
  
  ### summary dataset
  sDF <- summaryBy(Photo+Cond+WUE+SLA+VPD+CO2S+Ci+PARin+Tair+Tleaf~
                     Dataset+Species+Treatment+Type+Leafspan+PFT+Tregion+Wregion+Growthcond+VPD_group,
                   FUN=c(mean, sd), data=inDF, keep.names=T, na.rm=T)
  
  ### merge the two
  sDF <- merge(sDF, tmpDF, by=c("Dataset", "Species",
                                "Treatment", "VPD_group"))
  
  ### now separate by CO2 treatment
  sDF1 <- sDF[sDF$Treatment == "Ambient CO2",]  
  sDF2 <- sDF[sDF$Treatment == "Elevated CO2",]
  
  
  ### merge the two
  sDF <- merge(sDF1, sDF2, by=c("Dataset", "Species", "VPD_group", "Growthcond", "PFT"))
  
  sDF<- sDF[,c("Dataset", "Species", "VPD_group", "Growthcond", "PFT","VPD.mean.x","CO2S.mean.x", 
               "Photo.mean.x","Cond.mean.x", "WUE.mean.x",
               "Photo.sd.x","Cond.sd.x","WUE.sd.x",
               "Photo.n.x", "Cond.n.x", "WUE.n.x",
               "VPD.mean.y","CO2S.mean.y",
               "Photo.mean.y","Cond.mean.y", "WUE.mean.y",
               "Photo.sd.y","Cond.sd.y","WUE.sd.y",
               "Photo.n.y", "Cond.n.y", "WUE.n.y")]
  
  ### re-label all columns
  colnames(sDF) <- c("Dataset", "Species", "VPD_group", "Growthcond", "PFT","VPD.mean.aCO2","CO2.mean.aCO2",
                       "Photo.mean.aCO2","Cond.mean.aCO2", "WUE.mean.aCO2",
                       "Photo.sd.aCO2","Cond.sd.aCO2","WUE.sd.aCO2",
                       "Photo.n.aCO2", "Cond.n.aCO2", "WUE.n.aCO2",
                       "VPD.mean.eCO2","CO2.mean.eCO2",
                       "Photo.mean.eCO2","Cond.mean.eCO2", "WUE.mean.eCO2",
                       "Photo.sd.eCO2","Cond.sd.eCO2","WUE.sd.eCO2",
                       "Photo.n.eCO2", "Cond.n.eCO2", "WUE.n.eCO2")
  
  
  
  ### calculate response ratio and variance
  sDF$CO2_resp <- with(sDF, CO2.mean.eCO2/CO2.mean.aCO2) 
  
  
  ### calculate response ratios # LOGGED (for forest plot and CO2 response graphs)
  sDF$Photo_resp <- with(sDF, log(Photo.mean.eCO2/Photo.mean.aCO2)/log(CO2_resp))
  sDF$Cond_resp <- with(sDF, log(Cond.mean.eCO2/Cond.mean.aCO2)/log(CO2_resp))
  sDF$WUE_resp <- with(sDF, log(WUE.mean.eCO2/WUE.mean.aCO2)/log(CO2_resp))
  
  ### calculate response ratios # NOT LOGGED (not needed anymore)
  #sDF$Photo_resp <- with(sDF, (Photo.mean.eCO2/Photo.mean.aCO2)/(CO2_resp))
  #sDF$Cond_resp <- with(sDF, (Cond.mean.eCO2/Cond.mean.aCO2)/(CO2_resp))
  #sDF$WUE_resp <- with(sDF, (WUE.mean.eCO2/WUE.mean.aCO2)/(CO2_resp))
  
  
  
  
  ### calculate variance   # LOGGED (for forest plot)
  sDF$Photo_var <- with(sDF, 
                        (Photo.sd.eCO2 * Photo.sd.eCO2 / (Photo.n.eCO2 * Photo.mean.eCO2 * Photo.mean.eCO2) +
                           Photo.sd.aCO2 * Photo.sd.aCO2 / (Photo.n.aCO2 * Photo.mean.aCO2 * Photo.mean.aCO2))/log(CO2_resp))
  sDF$Cond_var <- with(sDF, 
                       (Cond.sd.eCO2 * Cond.sd.eCO2 / (Cond.n.eCO2 * Cond.mean.eCO2 * Cond.mean.eCO2) +
                          Cond.sd.aCO2 * Cond.sd.aCO2 / (Cond.n.aCO2 * Cond.mean.aCO2 * Cond.mean.aCO2))/log(CO2_resp))
  sDF$WUE_var <- with(sDF, 
                      (WUE.sd.eCO2 * WUE.sd.eCO2 / (WUE.n.eCO2 * WUE.mean.eCO2 * WUE.mean.eCO2) +
                         WUE.sd.aCO2 * WUE.sd.aCO2 / (WUE.n.aCO2 * WUE.mean.aCO2 * WUE.mean.aCO2))/log(CO2_resp))
  
  
  #--- # NOT LOGGED (not needed anymore)
  #sDF$Photo_var <- with(sDF, 
                      (Photo.sd.eCO2 * Photo.sd.eCO2 / (Photo.n.eCO2 * Photo.mean.eCO2 * Photo.mean.eCO2) +
                         Photo.sd.aCO2 * Photo.sd.aCO2 / (Photo.n.aCO2 * Photo.mean.aCO2 * Photo.mean.aCO2))/(CO2_resp))
  #sDF$Cond_var <- with(sDF, 
                     (Cond.sd.eCO2 * Cond.sd.eCO2 / (Cond.n.eCO2 * Cond.mean.eCO2 * Cond.mean.eCO2) +
                        Cond.sd.aCO2 * Cond.sd.aCO2 / (Cond.n.aCO2 * Cond.mean.aCO2 * Cond.mean.aCO2))/(CO2_resp))
  #sDF$WUE_var <- with(sDF, 
                    (WUE.sd.eCO2 * WUE.sd.eCO2 / (WUE.n.eCO2 * WUE.mean.eCO2 * WUE.mean.eCO2) +
                       WUE.sd.aCO2 * WUE.sd.aCO2 / (WUE.n.aCO2 * WUE.mean.aCO2 * WUE.mean.aCO2))/(CO2_resp))


  #If variance is NA, need to replace it with the whole site average of varience.
  mean(sDF$Photo_var,na.rm=TRUE)
  sDF$Photo_var[is.na(sDF$Photo_var)] <- 0.1187654
  mean(sDF$Cond_var,na.rm=TRUE)
  sDF$Cond_var[is.na(sDF$Cond_var)] <- 0.2008364
  mean(sDF$WUE_var,na.rm=TRUE)
  sDF$WUE_var[is.na(sDF$WUE_var)] <- 0.08878483

  
  ### add continuous factor for VPD values
  sDF$VPDmean <- round((sDF$VPD.mean.aCO2+sDF$VPD.mean.eCO2)/2, 1)
  
  ### replace all inf numbers to NAs
  is.na(sDF) <- do.call(cbind,lapply(sDF, is.infinite))
  
  
  
  
  
####### Using the SUBSET VPD range #####----------------------  
  
  ### get a subset dataset based on VPD range
  angE.vpd <- range(sDF$VPD_group[sDF$PFT=="evergreen angiosperm"])
  angD.vpd <- range(sDF$VPD_group[sDF$PFT=="deciduous angiosperm"])
  gym.vpd <- range(sDF$VPD_group[sDF$PFT=="evergreen gymnosperm"])
  vpd.range <- c(max(angE.vpd[1],angD.vpd[1], gym.vpd[1]), min(angE.vpd[2],angD.vpd[2], gym.vpd[2])) #range = 0.75 to 2.75
  
  subDF <- subset(sDF, VPD_group <= max(vpd.range) & VPD_group >= min(vpd.range))
  drop.levels(subDF)
  
  
  # summarise the VPD group
  subDF <- summaryBy(Photo_resp+Cond_resp+WUE_resp+CO2_resp+ VPDmean+Photo_var+Cond_var+WUE_var~
                       Dataset+Species+Treatment+PFT+ VPD_group,
                   FUN=c(mean, sd), data=subDF, keep.names=T, na.rm=T)
  
  ### re-label columns
  colnames(subDF) <- c("Dataset","Species", "PFT", "VPD_group",
                       "Photo_resp","Cond_resp", "WUE_resp", "CO2_resp","VPDmean",
                       "Photo_var","Cond_var", "WUE_var",
                       "Photo_resp.sd","Cond_resp.sd", "WUE_resp.sd", "CO2_resp.sd","VPDmean.sd")
  
  
  
  #### Now make forest plot based on the vpd range dataset
  SwueDF <- subDF[complete.cases(subDF$WUE_resp),]
  SwueDF <- SwueDF[order(SwueDF$PFT, SwueDF$VPD_group, SwueDF$Species, SwueDF$Dataset),]
  SwueDF.EBF <- subset(SwueDF, PFT=="EBF")
  SwueDF.DBF <- subset(SwueDF, PFT=="DBF")
  SwueDF.ENF <- subset(SwueDF, PFT=="ENF")
  Sl1 <- length(SwueDF$Dataset)
  Sns1 <- length(unique(SwueDF$Dataset))
  
  
  ScondDF <- subDF[complete.cases(subDF$Cond_resp),]
  ScondDF <- ScondDF[order(ScondDF$PFT, ScondDF$VPD_group, ScondDF$Species, ScondDF$Dataset),]
  ScondDF.EBF <- subset(ScondDF, PFT=="evergreen angiosperm")
  ScondDF.DBF <- subset(ScondDF, PFT=="deciduous angiosperm")
  ScondDF.ENF <- subset(ScondDF, PFT=="evergreen gymnosperm")
  Sl2 <- length(ScondDF$Dataset)
  Sns2 <- length(unique(ScondDF$Dataset))
  #ScondDF.EBF<-drop.levels(ScondDF.EBF)
  
  SphotoDF <- subDF[complete.cases(subDF$Photo_resp),]
  SphotoDF <- SphotoDF[order(SphotoDF$PFT, SphotoDF$VPD_group, SphotoDF$Species, SphotoDF$Dataset),]
  SphotoDF.EBF <- subset(SphotoDF, PFT=="evergreen angiosperm")
  SphotoDF.DBF <- subset(SphotoDF, PFT=="deciduous angiosperm")
  SphotoDF.ENF <- subset(SphotoDF, PFT=="evergreen gymnosperm")
  Sl3 <- length(SphotoDF$Dataset)
  Sns3 <- length(unique(SphotoDF$Dataset))
  
  
  
  ######################################################----------------------------------------
  
  ## Full forest plot - all VPD_groups ## Logged responses.## Supplementary
  
  ### make forest plots
  #pdf(paste0(getwd(), "/output/forest_extended_TreeNum.pdf"),
      width=12, height=10)

  ## WUE --------
  res_WUE <- rma.mv(WUE_resp, WUE_var, mods = ~PFT*VPDmean, random = ~1|Dataset, data = SwueDF)
  forest(res_WUE, slab=paste(SwueDF$Dataset, SwueDF$Species, sep=", "),
         xlim=c(-10, 10),
         ylim=c(0, 92),
         rows=c(82:37,34:20,17:1), #78
         ilab=cbind(SwueDF$VPDmean, SwueDF$VPD_group),
         ilab.xpos=c(-4.0,-2.5), 
         at=c(-0.5, 0.0,0.5,1.0,1.5,2.0,2.5),
         refline=1,
         mlab="", psize=1, 
         cex=0.6,
         order=order(SwueDF$PFT,SwueDF$Dataset,SwueDF$Species),
         header="Water-use efficiency response to eCO2")
  
  text(c(-4.0,-1.5), 88, c("Mean VPD", "VPD_group"),cex=0.7)
  text(3.5, 91, "Relative Response [95% CI]", pos = 2, cex = 0.7)
  text(-6, c(83,35,18), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.6)
  
  
  ## Cond --------
  res_Cond <- rma.mv(Cond_resp, Cond_var, mods = ~PFT*VPDmean, random = ~1|Dataset, data = ScondDF)
  
  forest(res_Cond, slab=paste(ScondDF$Dataset, ScondDF$Species, sep=", "), 
         xlim=c(-10, 10),
         ylim=c(0, 92),
         rows=c(82:37,34:20,17:1), #81
         ilab=cbind(ScondDF$VPDmean, ScondDF$VPD_group),
         ilab.xpos=c(-4.0,-2.5), 
         at=c(-2.0,-1.5, -1.0, -0.5, 0.0, 0.5, 1.0,1.5),
         mlab="", psize=1, 
         cex=0.6,
         refline=0,
         order=order(ScondDF$PFT,ScondDF$Dataset,ScondDF$Species),
         header="Stomatal condutance response to eCO2")
  
  text(c(-4.0,-1.5), 88, c("Mean VPD", "VPD_group"),cex=0.7)
  text(3.5, 91, "Relative Response [95% CI]", pos = 2, cex = 0.7)
  text(-6.5, c(83,35,18), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.6)
  
  
  
  
  ## Photo --------
  res_Photo <- rma.mv(Photo_resp, Photo_var, mods = ~PFT*VPDmean, random = ~1|Dataset, data = SphotoDF)
  
  forest(res_Photo, slab=paste(SphotoDF$Dataset, SphotoDF$Species, sep=", "),
         xlim=c(-10, 10),
         ylim=c(0, 92),
         rows=c(82:37,34:20,17:1), #81
         ilab=cbind(SphotoDF$VPDmean, SphotoDF$VPD_group),
         ilab.xpos=c(-4.0,-2.5), 
         at=c(-0.5, 0.0, 0.5, 1.0,1.5,2.0,2.5),
         mlab="", psize=1, 
         cex=0.6,
         refline=0,
         order=order(SphotoDF$PFT,SphotoDF$Dataset,SphotoDF$Species),
         header="Photosynthesis response to eCO2")
  text(c(-4.0,-1.5), 88, c("Mean VPD", "VPD_group"),cex=0.7)
  text(3.5, 91, "Relative Response [95% CI]", pos = 2, cex = 0.7)
  text(-6.5, c(83,35,18), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.6)
  dev.off()
  
  
print(res_WUE)
print(res_Cond)
print(res_Photo)










######################################################----------------------------------------

#--------------------------------
# Redoing the plot with just one entry per dataset and species
# Single entry plot means that VPD groups and VPD mean cannot be taken into account.
# MJ suggested that this plot would be in the supplementary.
# Go to script called 'simple forest'


#----------------------------------------------


######################################## CO2 RESPONSE GRAPH ###############################################

## Now plot the CO2 response graph. # First with just one regression line.


### 

# WUE
res_WUE <- rma.mv(WUE_resp, WUE_var, mods = ~PFT*VPDmean, random = ~1|Dataset, data = SwueDF)
preds<-predict(res_WUE, addx=TRUE,interval = 'confidence')
test <- as.data.frame(preds)

p6 <- ggplot() +
  stat_smooth(data=test,aes(x=X.VPDmean,y=pred),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="black")+
  stat_smooth(data=test,aes(x=X.VPDmean,y=ci.lb),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,
              se=FALSE,size=1,linetype=3,colour="black")+
  stat_smooth(data=test,aes(x=X.VPDmean,y=ci.ub),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,
              se=FALSE,size=1,linetype=3,colour="black")+
  geom_point(data=SwueDF,aes(x=VPDmean,y=WUE_resp,fill=Dataset, group=Dataset, shape=PFT, size=1/WUE_var),
             position="jitter")+
  geom_abline(slope=0, intercept=1, lty=4, color = "grey")+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="none",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("WUE response ratio (log_Resp / log_CO2)")+
  scale_shape_manual(name="PFT",
                     values=c("evergreen angiosperm"=22,"deciduous angiosperm"=24,"evergreen gymnosperm"=21))+
  #scale_y_continuous(limits = c(NA, 3),breaks=c(0.5, 1, 2,3))+
  guides(size=FALSE, shape=FALSE, fill = guide_legend(override.aes=list(shape=21, size=3)));p6



# Cond
res_Cond <- rma.mv(Cond_resp, Cond_var, mods = ~PFT*VPDmean, random = ~1|Dataset, data = ScondDF)
preds2<-predict(res_Cond, addx=TRUE,interval = 'confidence')
test2 <- as.data.frame(preds2)



p7 <- ggplot(test2,aes(x=X.VPDmean,y=pred)) +
  stat_smooth(method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="black")+
  stat_smooth(data=test2,aes(x=X.VPDmean,y=ci.lb),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,
              se=FALSE,size=1,linetype=3,colour="black")+
  stat_smooth(data=test2,aes(x=X.VPDmean,y=ci.ub),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,
              se=FALSE,size=1,linetype=3,colour="black")+
  geom_point(data=ScondDF,aes(x=VPDmean,y=Cond_resp,fill=Dataset, group=Dataset, shape=PFT, size=1/Cond_var),
             position="jitter")+
  geom_abline(slope=0, intercept=0, lty=4, color = "grey")+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="none",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("Gsw response ratio (log_Resp / log_CO2)")+
  scale_shape_manual(name="PFT",
                     values=c("evergreen angiosperm"=22,"deciduous angiosperm"=24,"evergreen gymnosperm"=21));p7
  #scale_y_continuous(limits = c(NA, 3),breaks=c(0.3,0.5, 1, 2,3));p7
  
  
  

# Photo
res_Photo <- rma.mv(Photo_resp, Photo_var, mods = ~PFT*VPDmean, random = ~1|Dataset, data = SphotoDF)

preds3<-predict(res_Photo, addx=TRUE,interval = 'confidence')
test3 <- as.data.frame(preds3)


p8 <- ggplot(test3,aes(x=X.VPDmean,y=pred)) +
  stat_smooth(method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="black")+
  stat_smooth(data=test3,aes(x=X.VPDmean,y=ci.lb),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,
              se=FALSE,size=1,linetype=3,colour="black")+
  stat_smooth(data=test3,aes(x=X.VPDmean,y=ci.ub),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,
              se=FALSE,size=1,linetype=3,colour="black")+
  geom_point(data=SphotoDF,aes(x=VPDmean,y=Photo_resp,fill=Dataset, group=Dataset, shape=PFT, size=1/Photo_var),
             position="jitter")+
  geom_abline(slope=0, intercept=0, lty=4, color = "grey")+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="none",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("Photo response ratio (log_Resp / log_CO2)")+
  scale_shape_manual(name="PFT",
                     values=c("evergreen angiosperm"=22,"deciduous angiosperm"=24,"evergreen gymnosperm"=21));p8
  #scale_y_continuous(trans = "log", limits = c(NA, 3),breaks=c(0.5, 1, 2,3));p8


legend_shared <- get_legend(p6 + theme(legend.box = 'horizontal', legend.justification=c(1,1), 
                                       legend.position="bottom", legend.title = element_blank(),
                                       legend.text = element_text(size = 11), legend.key = element_blank(), 
                                       legend.background = element_blank(),legend.spacing.x = unit(0.25, "cm"),
                                       legend.key.height = unit(0.55, "cm"),legend.key.width = unit(0.2, "cm")))

combined_plot <- plot_grid(p6,p7,p8,
                           labels="AUTO",
                           ncol=1, align="vh", axis = "l",
                           label_x=0.8, label_y=0.95,
                           label_size = 18)


pdf(paste0(getwd(), "/output/CO2resp_gam_TreeNum.pdf"),
    width=7, height=15)
plot_grid(combined_plot, legend_shared, ncol=1, rel_heights=c(1,0.15))
dev.off()  





### Splitting the data into the three PFT in order to plot the three PFT regressions
#WUE
res_WUE_EBF <- rma.mv(WUE_resp, WUE_var, mods = ~VPDmean, random = ~1|Dataset, data = SwueDF.EBF)
res_WUE_DBF <- rma.mv(WUE_resp, WUE_var, mods = ~VPDmean, random = ~1|Dataset, data = SwueDF.DBF)
res_WUE_ENF <- rma.mv(WUE_resp, WUE_var, mods = ~VPDmean, random = ~1|Dataset, data = SwueDF.ENF)

preds_WUE_EBF<-predict(res_WUE_EBF, addx=TRUE,interval = 'confidence')
preds_WUE_DBF<-predict(res_WUE_DBF, addx=TRUE,interval = 'confidence')
preds_WUE_ENF<-predict(res_WUE_ENF, addx=TRUE,interval = 'confidence')

test_WUE_EBF <- as.data.frame(preds_WUE_EBF)
test_WUE_DBF <- as.data.frame(preds_WUE_DBF)
test_WUE_ENF <- as.data.frame(preds_WUE_ENF)

#Cond
res_Cond_EBF <- rma.mv(Cond_resp, Cond_var, mods = ~VPDmean, random = ~1|Dataset, data = ScondDF.EBF)
res_Cond_DBF <- rma.mv(Cond_resp, Cond_var, mods = ~VPDmean, random = ~1|Dataset, data = ScondDF.DBF)
res_Cond_ENF <- rma.mv(Cond_resp, Cond_var, mods = ~VPDmean, random = ~1|Dataset, data = ScondDF.ENF)

preds_Cond_EBF<-predict(res_Cond_EBF, addx=TRUE,interval = 'confidence')
preds_Cond_DBF<-predict(res_Cond_DBF, addx=TRUE,interval = 'confidence')
preds_Cond_ENF<-predict(res_Cond_ENF, addx=TRUE,interval = 'confidence')

test_Cond_EBF <- as.data.frame(preds_Cond_EBF)
test_Cond_DBF <- as.data.frame(preds_Cond_DBF)
test_Cond_ENF <- as.data.frame(preds_Cond_ENF)

#Photo
res_Photo_EBF <- rma.mv(Photo_resp, Photo_var, mods = ~VPDmean, random = ~1|Dataset, data = SphotoDF.EBF)
res_Photo_DBF <- rma.mv(Photo_resp, Photo_var, mods = ~VPDmean, random = ~1|Dataset, data = SphotoDF.DBF)
res_Photo_ENF <- rma.mv(Photo_resp, Photo_var, mods = ~VPDmean, random = ~1|Dataset, data = SphotoDF.ENF)

preds_Photo_EBF<-predict(res_Photo_EBF, addx=TRUE,interval = 'confidence')
preds_Photo_DBF<-predict(res_Photo_DBF, addx=TRUE,interval = 'confidence')
preds_Photo_ENF<-predict(res_Photo_ENF, addx=TRUE,interval = 'confidence')

test_Photo_EBF <- as.data.frame(preds_Photo_EBF)
test_Photo_DBF <- as.data.frame(preds_Photo_DBF)
test_Photo_ENF <- as.data.frame(preds_Photo_ENF)

# Plot 
p6_PFT <- ggplot() +
  stat_smooth(data=test_WUE_EBF,aes(x=X.VPDmean,y=pred, colour="evergreen angiosperm", fill="evergreen angiosperm"),method="lm",formula=y~x,fullrange=T,size=1)+
  geom_ribbon(data=test_WUE_EBF, 
              aes(X.VPDmean, ymin=ci.lb,
                  ymax=ci.ub,
              fill=alpha("grey", 0.3)))+
  stat_smooth(data=test_WUE_EBF,aes(x=X.VPDmean,y=ci.lb,colour="evergreen angiosperm"),method="loess",size=1,linetype=3)+
  stat_smooth(data=test_WUE_EBF,aes(x=X.VPDmean,y=ci.ub,colour="evergreen angiosperm"),method="loess", size=1,linetype=3)+
  stat_smooth(data=test_WUE_DBF,aes(x=X.VPDmean,y=pred,colour="deciduous angiosperm"),method="lm",formula=y~x,fullrange=T,size=1)+
  stat_smooth(data=test_WUE_DBF,aes(x=X.VPDmean,y=ci.lb,colour="deciduous angiosperm"),method="loess",size=1,linetype=3)+
  stat_smooth(data=test_WUE_DBF,aes(x=X.VPDmean,y=ci.ub,colour="deciduous angiosperm"),method="loess",size=1,linetype=3)+
  stat_smooth(data=test_WUE_ENF,aes(x=X.VPDmean,y=pred,colour="evergreen gymnosperm"),method="lm",formula=y~x,fullrange=T,size=1)+
  stat_smooth(data=test_WUE_ENF,aes(x=X.VPDmean,y=ci.lb,colour="evergreen gymnosperm"),method="loess", size=1,linetype=3)+
  stat_smooth(data=test_WUE_ENF,aes(x=X.VPDmean,y=ci.ub,colour="evergreen gymnosperm"),method="loess",size=1,linetype=3)+
  geom_point(data=SwueDF,aes(x=VPDmean,y=WUE_resp,fill=PFT, shape=PFT, size=1/WUE_var))+
  geom_abline(slope=0, intercept=1, lty=4, colour = "grey")+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="none",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("WUE response ratio (log_Resp / log_CO2)")+
  scale_colour_manual(name="PFT",
                     values=c("evergreen angiosperm"="cornflowerblue","deciduous angiosperm"="forestgreen","evergreen gymnosperm"="tomato"))+
  scale_shape_manual(values=c(21,22,23,24,25))+
  guides(colour=guide_legend(direction='vertical'),fill=FALSE,size=FALSE);p6_PFT
    #guides(colour=guide_legend(direction='vertical'),shape=TRUE, size=FALSE, fill = guide_legend(override.aes=list(shape=21, size=3)));p6_PFT
  


p7_PFT <- ggplot() +
  stat_smooth(data=test_Cond_EBF,aes(x=X.VPDmean,y=pred,colour="evergreen angiosperm"),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1)+
  stat_smooth(data=test_Cond_EBF,aes(x=X.VPDmean,y=ci.lb,colour="evergreen angiosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Cond_EBF,aes(x=X.VPDmean,y=ci.ub,colour="evergreen angiosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Cond_DBF,aes(x=X.VPDmean,y=pred,colour="deciduous angiosperm"),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1)+
  stat_smooth(data=test_Cond_DBF,aes(x=X.VPDmean,y=ci.lb,colour="deciduous angiosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Cond_DBF,aes(x=X.VPDmean,y=ci.ub,colour="deciduous angiosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Cond_ENF,aes(x=X.VPDmean,y=pred,colour="evergreen gymnosperm"),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1)+
  stat_smooth(data=test_Cond_ENF,aes(x=X.VPDmean,y=ci.lb,colour="evergreen gymnosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Cond_ENF,aes(x=X.VPDmean,y=ci.ub,colour="evergreen gymnosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  geom_point(data=ScondDF,aes(x=VPDmean,y=Cond_resp,fill=Dataset, group=Dataset, shape=Dataset, size=1/Cond_var))+
  geom_abline(slope=0, intercept=0, lty=4, color = "grey")+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="none",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("Gsw response ratio (log_Resp / log_CO2)")+
  scale_shape_manual(values=c(1,2,3,5,6,7,8,10,18,12,13,14,15,16,17))+
  #scale_shape_manual(name="PFT",
                     #values=c("evergreen angiosperm"=22,"deciduous angiosperm"=24,"evergreen gymnosperm"=21))+
  scale_colour_manual(name="PFT",
                      values=c("evergreen angiosperm"="cornflowerblue","deciduous angiosperm"="forestgreen","evergreen gymnosperm"="tomato"));p7_PFT




p8_PFT <- ggplot() +
  stat_smooth(data=test_Photo_EBF,aes(x=X.VPDmean,y=pred,colour="evergreen angiosperm"),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1)+
  stat_smooth(data=test_Photo_EBF,aes(x=X.VPDmean,y=ci.lb,colour="evergreen angiosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Photo_EBF,aes(x=X.VPDmean,y=ci.ub,colour="evergreen angiosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Photo_DBF,aes(x=X.VPDmean,y=pred,colour="deciduous angiosperm"),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1)+
  stat_smooth(data=test_Photo_DBF,aes(x=X.VPDmean,y=ci.lb,colour="deciduous angiosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Photo_DBF,aes(x=X.VPDmean,y=ci.ub,colour="deciduous angiosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Photo_ENF,aes(x=X.VPDmean,y=pred,colour="evergreen gymnosperm"),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1)+
  stat_smooth(data=test_Photo_ENF,aes(x=X.VPDmean,y=ci.lb,colour="evergreen gymnosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  stat_smooth(data=test_Photo_ENF,aes(x=X.VPDmean,y=ci.ub,colour="evergreen gymnosperm"),method="loess",se=FALSE,size=1,linetype=3)+
  geom_point(data=SphotoDF,aes(x=VPDmean,y=Photo_resp,fill=Dataset, group=Dataset, shape=Dataset, size=1/Photo_var))+
  geom_abline(slope=0, intercept=0, lty=4, color = "grey")+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="none",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("Photo response ratio (log_Resp / log_CO2)")+
  scale_shape_manual(values=c(1,2,3,5,6,7,8,10,18,12,13,14,15,16,17))+
  #scale_shape_manual(name="PFT",
                     #values=c("evergreen angiosperm"=22,"deciduous angiosperm"=24,"evergreen gymnosperm"=21))+
  scale_colour_manual(name="PFT",
                      values=c("evergreen angiosperm"="cornflowerblue","deciduous angiosperm"="forestgreen","evergreen gymnosperm"="tomato"));p8_PFT
  #scale_y_continuous(trans = "log", limits = c(NA, 3),breaks=c(0.5, 1, 2,3));p8_PFT



legend_shared_PFT <- get_legend(p6_PFT + theme(legend.box = 'horizontal', legend.justification=c(1,1), 
                                       legend.position="bottom", legend.title = element_blank(),
                                       legend.text = element_text(size = 10), legend.key = element_blank(), 
                                       legend.background = element_blank(),legend.spacing.x = unit(0.1, "cm"),
                                       legend.key.height = unit(0.55, "cm"),legend.key.width = unit(0.2, "cm")))

combined_plot_PFT <- plot_grid(p6_PFT,p7_PFT,p8_PFT,
                           labels="AUTO",
                           ncol=1, align="vh", axis = "l",
                           label_x=0.8, label_y=0.95,
                           label_size = 18)


pdf(paste0(getwd(), "/output/CO2resp_PFT_Shape=Dataset.pdf"),
    width=7, height=15)
plot_grid(combined_plot_PFT, legend_shared_PFT, ncol=1, rel_heights=c(1,0.15))
dev.off()  








}  


















# Old code that might be useful






##Older version of regression and CI's

p7_PFT <- ggplot() +
  stat_smooth(data=test_Cond_EBF,aes(x=X.VPDmean,y=pred),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="blue")+
  stat_smooth(data=test_Cond_EBF,aes(x=X.VPDmean,y=ci.lb),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,se=FALSE,size=1,linetype=3,colour="blue")+
  stat_smooth(data=test_Cond_EBF,aes(x=X.VPDmean,y=ci.ub),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,se=FALSE,size=1,linetype=3,colour="blue")+
  stat_smooth(data=test_Cond_DBF,aes(x=X.VPDmean,y=pred),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="green")+
  stat_smooth(data=test_Cond_DBF,aes(x=X.VPDmean,y=ci.lb),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,se=FALSE,size=1,linetype=3,colour="green")+
  stat_smooth(data=test_Cond_DBF,aes(x=X.VPDmean,y=ci.ub),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,se=FALSE,size=1,linetype=3,colour="green")+
  stat_smooth(data=test_Cond_ENF,aes(x=X.VPDmean,y=pred),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="red")+
  stat_smooth(data=test_Cond_ENF,aes(x=X.VPDmean,y=ci.lb),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,se=FALSE,size=1,linetype=3,colour="red")+
  stat_smooth(data=test_Cond_ENF,aes(x=X.VPDmean,y=ci.ub),method="gam",formula=y~s(x,bs="cs"),
              fullrange=T,se=FALSE,size=1,linetype=3,colour="red")+
  geom_point(data=ScondDF,aes(x=VPDmean,y=Cond_resp,fill=Dataset, group=Dataset, shape=PFT, size=1/Cond_var),
             position="jitter")+
  geom_abline(slope=0, intercept=0, lty=4, color = "grey")+
  theme_linedraw()+
  theme(panel.grid.minor=element_blank(),
        axis.title.x = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=12),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        panel.grid.major=element_blank(),
        legend.position="none",
        legend.text.align=0)+
  xlab("VPD (kPa)")+
  ylab("Stomatal conductance response (log_Resp / log_CO2)")+
  scale_shape_manual(name="PFT",
                     values=c("evergreen angiosperm"=22,"deciduous angiosperm"=24,"evergreen gymnosperm"=21))+
  scale_y_continuous(trans = "log", limits = c(NA, 3),breaks=c(0.3,0.5, 1, 2,3));p7_PFT'



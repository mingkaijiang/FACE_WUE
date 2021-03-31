








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










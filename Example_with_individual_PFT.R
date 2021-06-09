#Example_with_individual_PFT

#### Now make forest plot based on the entire dataset
wueDF <- sDF[complete.cases(sDF$WUE_resp),]
wueDF <- wueDF[order(wueDF$Type, wueDF$VPD_group, wueDF$Dataset),]
wueDF.ang <- subset(wueDF, Type=="angiosperm")
wueDF.gym <- subset(wueDF, Type=="gymnosperm")
l1 <- length(wueDF$Dataset)
ns1 <- length(unique(wueDF$Dataset))

condDF <- sDF[complete.cases(sDF$Cond_resp),]
condDF <- wueDF[order(condDF$Type, condDF$VPD_group, condDF$Dataset),]
condDF.ang <- subset(condDF, Type=="angiosperm")
condDF.gym <- subset(condDF, Type=="gymnosperm")
l2 <- length(condDF$Dataset)
ns2 <- length(unique(condDF$Dataset))

photoDF <- sDF[complete.cases(sDF$Photo_resp),]
photoDF <- wueDF[order(photoDF$Type, photoDF$VPD_group, photoDF$Dataset),]
photoDF.ang <- subset(photoDF, Type=="angiosperm")
photoDF.gym <- subset(photoDF, Type=="gymnosperm")
l3 <- length(photoDF$Dataset)
ns3 <- length(unique(photoDF$Dataset))


#WUE
res_WUE_ang <- rma.mv(WUE_resp, WUE_var, mods = ~VPDmean, random = ~1|Dataset, data = wueDF.ang)
res_WUE_gym <- rma.mv(WUE_resp, WUE_var, mods = ~VPDmean, random = ~1|Dataset, data = wueDF.gym)

preds_WUE_ang<-predict(res_WUE_ang, addx=TRUE,interval = 'confidence')
preds_WUE_gym<-predict(res_WUE_gym, addx=TRUE,interval = 'confidence')

test_WUE_ang <- as.data.frame(preds_WUE_ang)
test_WUE_gym <- as.data.frame(preds_WUE_gym)


p6_PFT <- ggplot() +
    stat_smooth(data=test_WUE_ang,aes(x=X.VPDmean,y=pred),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="blue")+
    stat_smooth(data=test_WUE_ang,aes(x=X.VPDmean,y=ci.lb),method="gam",formula=y~s(x,bs="cs"),
                fullrange=T,se=FALSE,size=1,linetype=3,colour="blue")+
    stat_smooth(data=test_WUE_ang,aes(x=X.VPDmean,y=ci.ub),method="gam",formula=y~s(x,bs="cs"),
                fullrange=T,se=FALSE,size=1,linetype=3,colour="blue")+
    stat_smooth(data=test_WUE_gym,aes(x=X.VPDmean,y=pred),method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="green")+
    stat_smooth(data=test_WUE_gym,aes(x=X.VPDmean,y=ci.lb),method="loess",#formula=y~s(x,bs="cs"),
                #fullrange=T,
                se=FALSE,size=1,linetype=3,colour="green")+
    stat_smooth(data=test_WUE_gym,aes(x=X.VPDmean,y=ci.ub),method="loess",#formula=y~s(x,bs="cs"),
                #fullrange=T,
                se=FALSE,size=1,linetype=3,colour="green")+
    geom_point(data=wueDF,aes(x=VPDmean,y=WUE_resp,fill=Dataset, group=Dataset, shape=Type, size=1/WUE_var),
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
    ylab("WUE response")+
    scale_shape_manual(name="PFT",
                       values=c("angiosperm"=22,"gymnosperm"=21))+
    scale_y_continuous(trans = "log", limits = c(NA, 3),breaks=c(0.5, 1, 2,3))+
    guides(size=FALSE, shape=FALSE, fill = guide_legend(override.aes=list(shape=21, size=3)));p6_PFT


plot(p6_PFT)



analysis_2_standardized_VPD_bins <- function(inDF, vpd.brks) {
    
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
    
    
    inDF$Photo.n <- ifelse(is.na(inDF$Photo), 0, 1)
    inDF$Cond.n <- ifelse(is.na(inDF$Cond), 0, 1)
    inDF$WUE.n <- ifelse(is.na(inDF$WUE), 0, 1)
    
    inDF$Season <- as.character(inDF$Season)
    inDF$Dataset <- as.character(inDF$Dataset)
    
    
    ### count number of data entries
    tmpDF <- summaryBy(Photo.n+Cond.n+WUE.n~Dataset+Treatment+VPD_group,
                       FUN=sum, data=inDF, keep.names=T, na.rm=T)
    
    ### summary dataset
    sDF <- summaryBy(Photo+Cond+WUE+SLA+VPD+CO2S+Ci+PARin+Tair+Tleaf~Dataset+Treatment+Type+Leafspan+Tregion+Wregion+Growthcond+VPD_group,
                     FUN=c(mean, sd), data=inDF, keep.names=T, na.rm=T)
    
    ### merge the two
    sDF <- merge(sDF, tmpDF, by=c("Dataset", 
                                  "Treatment", "VPD_group"))
    
    ### now separate by CO2 treatment
    sDF1 <- sDF[sDF$Treatment == "Ambient CO2",]
    sDF2 <- sDF[sDF$Treatment == "Elevated CO2",]
    
    ### merge the two
    sDF <- merge(sDF1, sDF2, by=c("Dataset", "VPD_group","Type",
                                  "Leafspan", "Tregion","Wregion", "Growthcond"))
    
    
    
    ### re-label all columns
    colnames(sDF) <- c("Dataset","VPD_group", "Type", 
                       "Leafspan", "Tregion", "Wregion", "Growthcond",
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
    
    
    ### add continuous factor for VPD values
    sDF$VPDmean <- round((sDF$VPD.mean.aCO2+sDF$VPD.mean.eCO2)/2, 1)
    
    ### replace all inf numbers to NAs
    is.na(sDF) <- do.call(cbind,lapply(sDF, is.infinite))
    
  
    ### make VPD bins and means check
    p1 <- ggplot(sDF) +
        geom_point(aes(VPD_group, VPDmean, fill=Dataset), pch=21)+
        geom_abline(slope=1,intercept=0)+
        geom_smooth(method = "lm", aes(VPD_group, VPDmean, color=Dataset, group=Dataset),
                    se=F)+
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
        xlab("VPD category (kPa)")+
        ylab("VPD mean (kPa)")
    
    p2 <- ggplot(sDF) +
        geom_point(aes(VPD_group, Photo_resp, fill=Dataset, group=Dataset), pch=21)+
        geom_smooth(method = "lm", aes(VPD_group, Photo_resp, color=Dataset, group=Dataset),
                    se=F)+
        geom_abline(slope=0, intercept=3, lty=2)+
        geom_abline(slope=0, intercept=1, lty=1)+
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
    
    
    p3 <- ggplot(sDF) +
        geom_point(aes(VPD_group, Photo_resp, fill=Dataset), pch=21)+
        geom_errorbar(aes(VPD_group, ymin=Photo_resp-Photo_var, ymax=Photo_resp+Photo_var))+
        geom_smooth(method = "lm", aes(VPD_group, Photo_resp, col=Dataset),
                    se=F)+
        geom_abline(intercept=1, slope=0)+
        facet_wrap(facet="Dataset", scales="free")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0,
              strip.text.y.left = element_text(angle = 0))+
        xlab("VPD (kPa)")+
        ylab("Photosynthesis CO2 response")+
        ylim(c(-1,3))
    
    
    
    p4 <- ggplot(sDF) +
        geom_point(aes(VPD_group, Cond_resp, fill=Dataset), pch=21)+
        geom_errorbar(aes(VPD_group, ymin=Cond_resp-Cond_var, ymax=Cond_resp+Cond_var))+
        geom_smooth(method = "lm", aes(VPD_group, Cond_resp, col=Dataset),
                    se=F)+
        geom_abline(intercept=0, slope=0)+
        facet_wrap(facet="Dataset", scales="free")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0,
              strip.text.y.left = element_text(angle = 0))+
        xlab("VPD (kPa)")+
        ylab("Conductance CO2 response")+
        ylim(c(-1,2))
    
    
    
    p5 <- ggplot(sDF) +
        geom_point(aes(VPD_group, WUE_resp, fill=Dataset), pch=21)+
        geom_errorbar(aes(VPD_group, ymin=WUE_resp-WUE_var, ymax=WUE_resp+WUE_var))+
        geom_smooth(method = "lm", aes(VPD_group, WUE_resp, col=Dataset),
                    se=F)+
        geom_abline(intercept=1, slope=0)+
        facet_wrap(facet="Dataset", scales="free")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0,
              strip.text.y.left = element_text(angle = 0))+
        xlab("VPD (kPa)")+
        ylab("WUE CO2 response")+
        ylim(c(-1,2))
    
    
    pdf(paste0(getwd(), "/output/CO2_responses_at_generalized_VPD_bins.pdf"), width=12, height=12)
    plot(p3)
    plot(p4)
    plot(p5)
    dev.off()

    
    #### Now make forest plot
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
    
    
    ### make forest plots
    pdf(paste0(getwd(), "/output/forest_plot_generalized_VPD_bins_WUE_analysis.pdf"),
        width=12, height=10)
    
    ### make the simplest forest plot model
    ## WUE
    res_WUE <- rma.uni(WUE_resp, WUE_var, data = wueDF)
    
    forest(res_WUE, slab=wueDF$Dataset, 
           at=c(-1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
           ilab=cbind(wueDF$Type, wueDF$VPDmean),
           ilab.xpos=c(-3.0,-1.5), 
           cex=0.6)
    text(c(-3.0,-1.5), l1+3, c("Type", "Mean VPD"),
         cex=0.7)
    text(4, l1+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    
    
    ### add dataset as a random factor
    ## WUE
    res_WUE <- rma.mv(WUE_resp, WUE_var, random = ~1|Dataset, data = wueDF)
    
    forest(res_WUE, slab=wueDF$Dataset, 
           at=c(-1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
           ilab=cbind(wueDF$Type, wueDF$VPDmean),
           ilab.xpos=c(-3.0,-1.5), 
           cex=0.6)
    text(c(-3.0,-1.5), l1+3, c("Type", "Mean VPD"),
         cex=0.7)
    text(4, l1+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    
    
    ### add moderator: vegetation type
    res_WUE <- rma.mv(WUE_resp, WUE_var, mods = ~Type, random = ~1|Dataset, data = wueDF)
    
    forest(res_WUE, slab=wueDF$Dataset, 
           at=c(-1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
           ilab=cbind(wueDF$Type, wueDF$VPDmean),
           ilab.xpos=c(-3.0,-1.5), 
           cex=0.6)
    text(c(-3.0,-1.5), l1+3, c("Type", "Mean VPD"),
         cex=0.7)
    text(4, l1+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    
    
    res_WUE <- rma.mv(WUE_resp, WUE_var, mods = ~Type*VPDmean, random = ~1|Dataset, data = wueDF)
    
    forest(res_WUE, slab=wueDF$Dataset, 
           at=c(-1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
           ilab=cbind(wueDF$Type, wueDF$VPDmean),
           ilab.xpos=c(-3.0,-1.5), 
           cex=0.6)
    text(c(-3.0,-1.5), l1+3, c("Type", "Mean VPD"),
         cex=0.7)
    text(4, l1+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    
    dev.off()
    
    print(res_WUE)
    
    ### end
}

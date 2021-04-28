analysis_1_no_VPD_bins <- function(inDF) {
    
    
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
    
    inDF$Photo.n <- ifelse(is.na(inDF$Photo), 0, 1)
    inDF$Cond.n <- ifelse(is.na(inDF$Cond), 0, 1)
    inDF$WUE.n <- ifelse(is.na(inDF$WUE), 0, 1)
    
    inDF$Season <- as.character(inDF$Season)
    inDF$Dataset <- as.character(inDF$Dataset)
    
    tmpDF <- summaryBy(Photo.n+Cond.n+WUE.n~Species+Season+Dataset+PFT+Treatment,
                       FUN=sum, data=inDF, keep.names=T, na.rm=T)
    
    ### summary dataset
    sDF <- summaryBy(Photo+Cond+WUE+SLA+VPD+CO2S+Ci+PARin+Tair+Tleaf~Species+Season+Dataset+PFT+Treatment+Type+Leafspan+Tregion+Wregion+Growthcond,
                      FUN=c(mean, sd), data=inDF, keep.names=T, na.rm=T)
    
    ### merge the two
    sDF <- merge(sDF, tmpDF, by=c("Species", "Season", "Dataset", "PFT",
                                 "Treatment"))
    #sDF$Photo.n <- tmpDF$Photo.n
    #sDF$Cond.n <- tmpDF$Cond.n
    #sDF$WUE.n <- tmpDF$WUE.n
    
    
    
    
    ### now separate by CO2 treatment
    sDF1 <- sDF[sDF$Treatment == "Ambient CO2",]
    sDF2 <- sDF[sDF$Treatment == "Elevated CO2",]
    
    
    ### merge the two
    sDF <- merge(sDF1, sDF2, by=c("Species", "Season", "Type", "Leafspan",
                                  "Tregion", "Wregion", "Growthcond",
                                  "Dataset", "PFT"))
    
    ### re-label all columns
    colnames(sDF) <- c("Species", "Season", "Type", 
                       "Leafspan", "Tregion", "Wregion", "Growthcond",
                       "Dataset","PFT", 
                       "TreatmentA", 
                       "Photo.mean.aCO2", "Cond.mean.aCO2",
                       "WUE.mean.aCO2", "SLA.mean.aCO2", "VPD.mean.aCO2",
                       "CO2.mean.aCO2", "Ci.mean.aCO2",
                       "PARin.mean.aCO2", "Tair.mean.aCO2", "Tleaf.mean.aCO2",
                       "Photo.sd.aCO2", "Cond.sd.aCO2",
                       "WUE.sd.aCO2", "SLA.sd.aCO2","VPD.sd.aCO2",
                       "CO2.sd.aCO2", "Ci.sd.aCO2",
                       "PARin.sd.aCO2", "Tair.sd.aCO2", "Tleaf.sd.aCO2",
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
    
    
    
    
    
    
    ### make a plot of CO2 response ratio at different VPD bins
   # p2 <- ggplot(sDF) +
   #     geom_point(aes(VPD_group, Photo_resp, fill=Dataset, group=Dataset), pch=21)+
   #     geom_smooth(method = "lm", aes(VPD_group, Photo_resp, color=Dataset, group=Dataset),
   #                 se=F)+
   #     theme_linedraw() +
   #     theme(panel.grid.minor=element_blank(),
   #           axis.title.x = element_text(size=12), 
   #           axis.text.x = element_text(size=12),
   #           axis.text.y=element_text(size=12),
   #           axis.title.y=element_text(size=12),
   #           legend.text=element_text(size=10),
   #           legend.title=element_text(size=12),
   #           panel.grid.major=element_blank(),
   #           legend.position="right",
   #           legend.text.align=0)+
   #     xlab("VPD (kPa)")+
   #     ylab("Photosynthesis CO2 response")
    
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
        ylab("Photosynthesis CO2 response")
    
    
    
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
        ylab("Conductance CO2 response")
    
    
    
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
        ylab("WUE CO2 response")
    
    
    
    pdf(paste0(getwd(), "/output/CO2_responses_at_VPD_bins.pdf"), width=12, height=12)
    plot(p3)
    plot(p4)
    plot(p5)
    dev.off()
    
    
    
    ### return the summarized DF
    return(sDF)
    
    ### end
}

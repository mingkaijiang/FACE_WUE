break_data_by_VPD_bins_and_summarize_response_ratios <- function(inDF) {
    
    ### To continue the meta-analysis, we will need to decide how to treat different VPD in different dataset and CO2 treatment
    ### Two options:
    ###             1. To bin VPD into categorical bins and then calculate CO2 response ratio within each bin
    ###             2. To filter out outliers (i.e. outside 95th percentile), and then calcuklate 
    ### Let's try categorize the dataset into different VPD bins first and see how it goes.
    
    breaks <- c(seq(0, 6.8, 0.2))
    
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
    
    tags <- c(seq(0.1, 6.7, 0.2))
    
    inDF$VPD_group <- cut(inDF$VPD, 
                          breaks=breaks, 
                          include.lowest=TRUE, 
                          right=FALSE, 
                          labels=tags)
    
    inDF$VPD_group <- as.numeric(as.character(inDF$VPD_group))
    
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
                      FUN=c(mean, sd), data=inDF, keep.names=T, na.rm=T)
    
    inDF$Photo.n <- ifelse(is.na(inDF$Photo), 0, 1)
    inDF$Cond.n <- ifelse(is.na(inDF$Cond), 0, 1)
    inDF$WUE.n <- ifelse(is.na(inDF$WUE), 0, 1)
    inDF$SLA.n <- ifelse(is.na(inDF$SLA), 0, 1)
    
    sDF2 <- summaryBy(Photo.n+Cond.n+WUE.n+SLA.n~Species+Season+Location+Dataset+Treatment+VPD_group,
                      FUN=sum, data=inDF, keep.names=T, na.rm=T)
    
    
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
    
    
    
    ### plot density plot of VPD values for each dataset
    p1 <- ggplot(inDF) +
        geom_histogram(aes(VPD, fill=Treatment), 
                       alpha=0.5, position="identity", col="black")+
        facet_grid(PFT~Dataset, scales="free")+
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
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                     "Elevated CO2"="red3"))
        
    pdf(paste0(getwd(), "/output/VPD_distribution_by_dataset.pdf"), width=24, height=10)
    plot(p1)
    dev.off()
    
    
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

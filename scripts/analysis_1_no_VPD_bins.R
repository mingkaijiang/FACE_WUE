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
    
    
    # Plot the 'mean response of A' versus 'the mean resposne of gsw'.
    p1 <- ggplot(sDF)+
        theme_bw()+
        geom_abline(slope=1, intercept=-1)+
        geom_smooth(method="lm", aes(Photo_resp, Cond_resp))+
        geom_point(aes(x=Photo_resp, y = Cond_resp, fill=Dataset, shape=PFT), size=2)+
        scale_shape_manual(values=c(21, 24, 22))+
        scale_fill_viridis(option = "D", discrete = TRUE)+
        guides(fill=guide_legend(override.aes=list(shape=21)))
    #scale_x_continuous(expand = c(0, 0),limits=c(-1.5,2.5), breaks=seq(-0.5,2.5,0.5))+
    #scale_y_continuous(expand = c(0, 0),limits=c(-1.5,2.5), breaks=seq(-1.5,1.5,0.5))+ 
    
    
    pdf(paste0(getwd(), "/output/photo_vs_cond_scatterplot.pdf"))
    plot(p1)
    dev.off()
    
    
    
    ### make plots
    p1 <- ggplot(sDF,aes(VPDmean, Photo_resp)) +
        geom_point(aes(VPDmean, Photo_resp, fill=Dataset), pch=21, size=4)+
        geom_errorbar(aes(VPDmean, ymin=Photo_resp-Photo_var, ymax=Photo_resp+Photo_var))+
        geom_smooth(method = "gam", formula = y ~s(x, bs = "cs"), 
                    se=T)+
        geom_abline(intercept=1, slope=0)+
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
        ylim(c(-2,2.2))
    

    p2 <- ggplot(sDF,aes(VPDmean, Cond_resp)) +
        geom_point(aes(VPDmean, Cond_resp, fill=Dataset), pch=21, size=4)+
        geom_errorbar(aes(VPDmean, ymin=Cond_resp-Cond_var, ymax=Cond_resp+Cond_var))+
        geom_smooth(method = "gam", formula = y ~s(x, bs = "cs"), 
                    se=T)+
        geom_abline(intercept=0, slope=0)+
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
        ylim(c(-2,2.2))
    
    
    p3 <- ggplot(sDF,aes(VPDmean, WUE_resp)) +
        geom_point(aes(VPDmean, WUE_resp, fill=Dataset), pch=21, size=4)+
        geom_errorbar(aes(VPDmean, ymin=WUE_resp-WUE_var, ymax=WUE_resp+WUE_var))+
        geom_smooth(method = "gam", formula = y ~s(x, bs = "cs"), 
                    se=T)+
        geom_abline(intercept=1, slope=0)+
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
        ylim(c(-2,2.2))
    
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'horizontal',
                                           legend.box.just = 'left'))
    
    combined_plot <- plot_grid(p1, p2, p3, 
                                labels="AUTO",
                                ncol=3, align="vh", axis = "l",
                                label_x=0.8, label_y=0.95,
                                label_size = 18)
    

    pdf(paste0(getwd(), "/output/CO2_responses_no_VPD_bins.pdf"),
        width=12, height=10)
    plot_grid(combined_plot, legend_shared, ncol=1, rel_heights=c(1,0.4))
    dev.off()  
    
    
    ######################### forest plot ###############################
    
    ### Now make forest plot
    wueDF <- sDF[complete.cases(sDF$WUE_resp),]
    wueDF <- wueDF[order(wueDF$Type, wueDF$PFT),]
    wueDF.ang <- subset(wueDF, Type=="angiosperm")
    wueDF.gym <- subset(wueDF, Type=="gymnosperm")
    l1 <- length(wueDF$Dataset)
    ns1 <- length(unique(wueDF$Dataset))
    
    condDF <- sDF[complete.cases(sDF$Cond_resp),]
    condDF <- wueDF[order(condDF$Type, condDF$PFT),]
    condDF.ang <- subset(condDF, Type=="angiosperm")
    condDF.gym <- subset(condDF, Type=="gymnosperm")
    l2 <- length(condDF$Dataset)
    ns2 <- length(unique(condDF$Dataset))
    
    photoDF <- sDF[complete.cases(sDF$Photo_resp),]
    photoDF <- wueDF[order(photoDF$Type, photoDF$PFT),]
    photoDF.ang <- subset(photoDF, Type=="angiosperm")
    photoDF.gym <- subset(photoDF, Type=="gymnosperm")
    l3 <- length(photoDF$Dataset)
    ns3 <- length(unique(photoDF$Dataset))
    
    
    ### make forest plots
    pdf(paste0(getwd(), "/output/forest_plot_no_VPD_bins_WUE_analysis.pdf"),
        width=12, height=10)
    
    ### make the simplest forest plot model
    ## WUE
    res_WUE <- rma.uni(WUE_resp, WUE_var, data = wueDF)

    forest(res_WUE, slab=wueDF$Dataset, 
           at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
           ilab=cbind(wueDF$PFT, wueDF$Season, wueDF$Species, wueDF$VPDmean),
           ilab.xpos=c(-1.3,-1.0,-0.5, 3.0), cex=0.6)
    text(c(-1.8,-1.3,-1.0,-0.5, 3.0), l1+3, c("Dataset", 
                                              "PFT",
                                              "Season",
                                              "Species",
                                              "Mean VPD"),
         cex=0.7)
    text(4, l1+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)

    
    ### add dataset as a random factor
    ## WUE
    res_WUE <- rma.mv(WUE_resp, WUE_var, random = ~1|Dataset, data = wueDF)
    
    forest(res_WUE, slab=wueDF$Dataset, 
           at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
           ilab=cbind(wueDF$PFT, wueDF$Season, wueDF$Species, wueDF$VPDmean),
           ilab.xpos=c(-1.3,-1.0,-0.5, 3.0), cex=0.6)
    text(c(-1.8,-1.3,-1.0,-0.5, 3.0), l1+3, c("Dataset", 
                                              "PFT",
                                              "Season",
                                              "Species",
                                              "Mean VPD"),
         cex=0.7)
    text(4, l1+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)

    
    ### add moderator: vegetation type
    res_WUE <- rma.mv(WUE_resp, WUE_var, mods = ~Type, random = ~1|Dataset, data = wueDF)

    forest(res_WUE, slab=wueDF$Dataset, 
           at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
           ilab=cbind(wueDF$PFT, wueDF$Season, wueDF$Species, wueDF$VPDmean),
           ilab.xpos=c(-1.3,-1.0,-0.5, 3.0), cex=0.6)
    text(c(-1.8,-1.3,-1.0,-0.5, 3.0), l1+3, c("Dataset", 
                                              "PFT",
                                              "Season",
                                              "Species",
                                              "Mean VPD"),
         cex=0.7)
    text(4, l1+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    
    
    res_WUE <- rma.mv(WUE_resp, WUE_var, mods = ~Type*VPDmean, random = ~1|Dataset, data = wueDF)
    
    forest(res_WUE, slab=wueDF$Dataset, 
           at=c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5),
           ilab=cbind(wueDF$PFT, wueDF$Season, wueDF$Species, wueDF$VPDmean),
           ilab.xpos=c(-1.3,-1.0,-0.5, 3.0), cex=0.6)
    text(c(-1.8,-1.3,-1.0,-0.5, 3.0), l1+3, c("Dataset", 
                                              "PFT",
                                              "Season",
                                              "Species",
                                              "Mean VPD"),
         cex=0.7)
    text(4, l1+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    
    dev.off()
    
    
    ### end
}

perform_generalized_additive_model_for_each_dataset <- function(inDF) {
    
    ### new library for the generalized additive model
    library(mgcv)
    
    ### create storage directory
    dir.create("output/individual_dataset")
    
    ### create storage DF to store the statistical results
    #statDF <- data.frame("Dataset"=c(unique(inDF$Dataset)), 
    #                     "mod1")
    
    
    datasets <- unique(inDF$Dataset)
    
    for (i in datasets) {
        
        if (i %in% c("Freeman_GribSkov_BB", "Leuzinger_SCC", "Warren_ORNL")) {
            next
        } else {
            ### subset individual dataset
            test <- inDF[inDF$Dataset==i,]
            
            
            ### make a blind plot
            p1 <- ggplot(test) +
                geom_point(aes(x=VPD, y=WUE, fill=Treatment),
                           col="black", pch=21)+
                geom_smooth(aes(x=VPD, y=WUE, group=Treatment, col=Treatment))+
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
                ylab("WUE (%)")+
                scale_fill_manual(values=c("Ambient CO2"="blue2",
                                           "Elevated CO2"="red3"))+
                scale_color_manual(values=c("Ambient CO2"="blue2",
                                            "Elevated CO2"="red3"))
            
            
            ### fit VPD only
            mod_lm <- gam(WUE ~ s(VPD, bs="cr"), data=test)
            summary(mod_lm)
            AIC(mod_lm)
            
            mod_lm2 <- gam(WUE ~ s(VPD)+Treatment, data=test)
            summary(mod_lm2)
            AIC(mod_lm2)
            
            
            mod_lm3 <- gam(WUE ~ s(VPD)+s(CO2S), data=test)
            summary(mod_lm3)
            AIC(mod_lm3)
            
            
            ### output
            pdf(paste0(getwd(), "/output/individual_dataset/", i, "_GAM_results.pdf"), width=12, height=12)
            
            ### raw data
            plot(p1)
            
            ### fitted model with VPD only
            plot(mod_lm)
            
            ### fitted model with VPD and Treatment (factor)
            plot(mod_lm2)
            
            ### fitted model with VPD and CO2 (continuous)
            plot(mod_lm3)
            
            dev.off()
        }
        
        
        
    }
    
    
}
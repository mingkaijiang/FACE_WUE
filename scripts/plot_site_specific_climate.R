plot_site_specific_climate <- function(inDF) {
    
    ### plot density plot of VPD values for each dataset
    p1 <- ggplot(inDF) +
        geom_density(aes(VPD, fill=Treatment), 
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
                                   "Elevated CO2"="red3")); p1
    
    
    ### CO2S
    p2 <- ggplot(inDF) +
        geom_density(aes(CO2S, fill=Treatment), 
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
        xlab("CO2 (ppm)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))
    
    
    ### PAR
    p3 <- ggplot(inDF) +
        geom_density(aes(PARin, fill=Treatment), 
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
        xlab("PAR (umol m-2 s-1)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))
    
    
    ### Tair
    p4 <- ggplot(inDF) +
        geom_density(aes(Tair, fill=Treatment), 
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
        xlab("Tair (degree C)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))
    
    
    ### RH
    p5 <- ggplot(inDF) +
        geom_density(aes(RH, fill=Treatment), 
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
        xlab("RH (%)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))
    
    
    ### Tleaf
    p6 <- ggplot(inDF) +
        geom_density(aes(Tleaf, fill=Treatment), 
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
        xlab("Tleaf (degree C)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))
    
    
    
    pdf(paste0(getwd(), "/output/climate_distribution_by_dataset.pdf"), width=30, height=14)
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    plot(p5)
    plot(p6)
    dev.off()
    
    
    ### calculate site prevailing VPD, ambient CO2, and CO2 ratios
    sumDF <- summaryBy(VPD+CO2S+PARin+Tair+RH+Tleaf~Dataset+PFT+Treatment, FUN=c(mean, sd),
                       data=inDF, keep.names=T, na.rm=T)
    
    
    ### plot density plot of VPD values for each dataset
    p1 <- ggplot(sumDF) +
        geom_point(aes(x=Dataset, y=VPD.mean, fill=Treatment, pch=PFT), 
                     stat="identity", position=position_dodge(width=0.2), col="black", size=4)+
        geom_errorbar(aes(x=Dataset, ymin=VPD.mean-VPD.sd, ymax=VPD.mean+VPD.sd, col=Treatment),
                      stat="identity", position=position_dodge(width=0.2))+
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
        ylab("VPD (kPa)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))+
        scale_shape_manual(name="PFT",
                           values=c("ENF" = 21, "DBF" = 22, "EBF" = 23))+
        scale_color_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))+
        coord_flip()
        
    
    ### plot density plot of CO2S values for each dataset
    p2 <- ggplot(sumDF) +
        geom_point(aes(x=Dataset, y=CO2S.mean, fill=Treatment, pch=PFT), 
                   stat="identity", position=position_dodge(width=0.2), col="black", size=4)+
        geom_errorbar(aes(x=Dataset, ymin=CO2S.mean-CO2S.sd, ymax=CO2S.mean+CO2S.sd, col=Treatment),
                      stat="identity", position=position_dodge(width=0.2))+
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
        ylab("CO2 (ppm)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))+
        scale_shape_manual(name="PFT",
                           values=c("ENF" = 21, "DBF" = 22, "EBF" = 23))+
        scale_color_manual(values=c("Ambient CO2"="blue2",
                                    "Elevated CO2"="red3"))+
        coord_flip()
    
    
    ### plot density plot of PAR values for each dataset
    p3 <- ggplot(sumDF) +
        geom_point(aes(x=Dataset, y=PARin.mean, fill=Treatment, pch=PFT), 
                   stat="identity", position=position_dodge(width=0.2), col="black", size=4)+
        geom_errorbar(aes(x=Dataset, ymin=PARin.mean-PARin.sd, ymax=PARin.mean+PARin.sd, col=Treatment),
                      stat="identity", position=position_dodge(width=0.2))+
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
        ylab("PAR (umol m-2 s-1)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))+
        scale_shape_manual(name="PFT",
                           values=c("ENF" = 21, "DBF" = 22, "EBF" = 23))+
        scale_color_manual(values=c("Ambient CO2"="blue2",
                                    "Elevated CO2"="red3"))+
        coord_flip()
    
    
    ### plot density plot of PAR values for each dataset
    p4 <- ggplot(sumDF) +
        geom_point(aes(x=Dataset, y=Tair.mean, fill=Treatment, pch=PFT), 
                   stat="identity", position=position_dodge(width=0.2), col="black", size=4)+
        geom_errorbar(aes(x=Dataset, ymin=Tair.mean-Tair.sd, ymax=Tair.mean+Tair.sd, col=Treatment),
                      stat="identity", position=position_dodge(width=0.2))+
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
        ylab("Tair (degree C)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))+
        scale_shape_manual(name="PFT",
                           values=c("ENF" = 21, "DBF" = 22, "EBF" = 23))+
        scale_color_manual(values=c("Ambient CO2"="blue2",
                                    "Elevated CO2"="red3"))+
        coord_flip()
    
    
    
    ### plot density plot of PAR values for each dataset
    p5 <- ggplot(sumDF) +
        geom_point(aes(x=Dataset, y=RH.mean, fill=Treatment, pch=PFT), 
                   stat="identity", position=position_dodge(width=0.2), col="black", size=4)+
        geom_errorbar(aes(x=Dataset, ymin=RH.mean-RH.sd, ymax=RH.mean+RH.sd, col=Treatment),
                      stat="identity", position=position_dodge(width=0.2))+
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
        ylab("RH (%)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))+
        scale_shape_manual(name="PFT",
                           values=c("ENF" = 21, "DBF" = 22, "EBF" = 23))+
        scale_color_manual(values=c("Ambient CO2"="blue2",
                                    "Elevated CO2"="red3"))+
        coord_flip()
    
    
    
    ### plot density plot of Tleaf values for each dataset
    p6 <- ggplot(sumDF) +
        geom_point(aes(x=Dataset, y=Tleaf.mean, fill=Treatment, pch=PFT), 
                   stat="identity", position=position_dodge(width=0.2), col="black", size=4)+
        geom_errorbar(aes(x=Dataset, ymin=Tleaf.mean-Tleaf.sd, ymax=Tleaf.mean+Tleaf.sd, col=Treatment),
                      stat="identity", position=position_dodge(width=0.2))+
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
        ylab("Tleaf (degree C)")+
        scale_fill_manual(values=c("Ambient CO2"="blue2",
                                   "Elevated CO2"="red3"))+
        scale_shape_manual(name="PFT",
                           values=c("ENF" = 21, "DBF" = 22, "EBF" = 23))+
        scale_color_manual(values=c("Ambient CO2"="blue2",
                                    "Elevated CO2"="red3"))+
        coord_flip()
    
    
    pdf(paste0(getwd(), "/output/climate_summary_by_dataset.pdf"), width=10, height=10)
    plot(p1)
    plot(p2)
    plot(p3)
    plot(p4)
    plot(p5)
    plot(p6)
    dev.off()
    

    
    ### end
}

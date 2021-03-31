fit_g1_values_and_plot <- function(inDF) {
    
      ### split by dataset
      list <- split(inDF,inDF$Dataset)
      
      ### fit g1 values
      fit <- lapply(list,fitBB,gsmodel="BBOpti",
                    varnames=list(VPD="VPD",ALEAF="Photo",GS="Cond",Ca="CO2S"))
      g1pars <- sapply(fit,function(x)x$coef[[2]])
      g1pars <- stack(g1pars)
      g1DF <- do.call(rbind, lapply(fit, coef))
      
      g1DF <- cbind("Dataset" = rownames(g1DF), as.data.frame(g1DF))
      
      ### make plot
      p1 <- ggplot(inDF) +
          geom_point(aes(Photo/sqrt(VPD)/CO2S,Cond, fill=Dataset, group=Dataset), pch=21)+
          geom_smooth(aes(Photo/sqrt(VPD)/CO2S,Cond, color=Dataset, group=Dataset), se=F)+
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
          #xlab("VPD (kPa)")+
          ylab("Conductance")
      
      pdf(paste0(getwd(), "/output/fit_g1_plot.pdf"))
      plot(p1)
      dev.off()
      

      ### plot g1 by dataset
      p2 <- ggplot(g1DF) +
          geom_bar(aes(Dataset, g1, fill=Dataset), stat='identity')+
          coord_flip()+
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
                legend.text.align=0)
      
      pdf(paste0(getwd(), "/output/g1_by_dataset.pdf"))
      plot(p2)
      dev.off()
      
      
}
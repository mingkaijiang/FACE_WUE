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
      
      
      
      ## Supplemental figure to show the data distributon with CO2 treatment.
      p2<- ggplot(inDF, aes(x=Photo/sqrt(VPD)/CO2S, y = Cond, 
                               shape = Treatment, fill = Treatment))+
        theme_bw()+
        geom_point(colour="black")+
        facet_wrap(~ Dataset, nrow = 3)+
        scale_shape_manual(values=c(21,24)) +
        scale_fill_manual(values=c("blue","red"));p2
      
      
      pdf(paste0(getwd(), "/output/g1_scatterplots_by_dataset.pdf"), width=14, height=10)
      plot(p2)
      dev.off()
      
      #tiff('G1 Scatterplots_Datasets.tiff', units="in", width=14, height=10, res=500)
      #plot(p2)
      #dev.off()
      
      

      ### plot g1 by dataset
      p3 <- ggplot(g1DF) +
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
                legend.text.align=0);p3
      
      pdf(paste0(getwd(), "/output/g1_by_dataset.pdf"))
      plot(p3)
      dev.off()
      
      
      
      ### plot g1 by dataset, CO2 treatment and order by large to small
      p3 <- ggplot(g1DF) +
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
              legend.text.align=0);p3
      
      
      
      ### split by CO2 treatment, dataset
      list <- split(inDF,interaction(inDF$Dataset, inDF$Treatment))
      
      ### fit g1 values
      fit <- lapply(list,fitBB,gsmodel="BBOpti",
                    varnames=list(VPD="VPD",ALEAF="Photo",GS="Cond",Ca="CO2S"))
      g1pars <- sapply(fit,function(x)x$coef[[2]])
      g1confint1 <- sapply(fit,function(x)confint(x$fit)[1])
      g1confint2 <- sapply(fit,function(x)confint(x$fit)[2])
      
      g1pars <- stack(g1pars)
      g1confint1 <- stack(g1confint1)
      g1confint2 <- stack(g1confint2)
      
      g1DF <- cbind(as.data.frame(g1pars), as.data.frame(g1confint1), as.data.frame(g1confint2))
      colnames(g1DF) <- c("g1.mean", "Dataset", "g1.low", "X", "g1.up", "Y")
      g1DF <- g1DF[,c("Dataset", "g1.mean", "g1.low", "g1.up")]
      
      g1DF$Dataset <- as.character(g1DF$Dataset)
      
      g1DF$Treatment <- c("Ambient", "Elevated")
      
      g1DF$Dataset <- gsub(".Ambient CO2", "", g1DF$Dataset)
      g1DF$Dataset <- gsub(".Elevated CO2", "", g1DF$Dataset)
      
      
      dodge <- position_dodge2(width = 0.5)
      p3 <- g1DF %>%
        mutate(name = fct_reorder(Dataset, g1)) %>%
        ggplot(aes(x=interaction(Species,name), g1, 
                   group=interaction(Species,name), 
                   shape= PFT, fill=Treatment)) +
        theme_bw()+
        geom_errorbar(aes(ymin = g1.low, ymax = g1.up), position=dodge, width=0.5, size=0.2)+ 
        geom_point(size=2, position=dodge)  +
        scale_y_continuous(name="g1",expand = c(0, 0),limits=c(0,8), breaks=seq(0,8,2)) +
        scale_x_discrete(name="Dataset") +
        scale_shape_manual(values=c(21,22,23)) +
        scale_fill_manual(values=c("blue","red")) +
        facet_grid(PFT~. , scales="free", space = "free")+
        theme(legend.position="none") +
        coord_flip()+
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank());p3
      
      
      pdf(paste0(getwd(), "/output/g1_by_dataset_treatment.pdf"))
      plot(p3)
      dev.off()
      
      
}
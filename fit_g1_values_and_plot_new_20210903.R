getr2 <- function(x){
  
  lmfit <- lm(x$data$Cond ~ fitted(x$fit))
  summary(lmfit)$r.squared
}


fit_g1_values_and_plot <- function(inDF) {
    
  myDF<- read.csv("data/WUEdatabase_fixed.csv")
  inDF <- myDF
    
  
  ### split by dataset
      inDF$fits <- paste0(inDF$Dataset, "-", inDF$Species, "-", inDF$Treatment)

      
      list <- split(inDF, inDF$fits)
     
      ### fit g1 values
      fit <- lapply(list,fitBB,gsmodel="BBOpti",
                    varnames=list(VPD="VPD",ALEAF="Photo",GS="Cond",Ca="CO2S")) 
      lapply(fit,coef)
      g1pars <- sapply(fit,function(x)x$coef[[2]])
      g1cilows <- lapply(fit,function(x)confint(x$fit)[1])
      g1cihighs <- lapply(fit,function(x)confint(x$fit)[2])
      ret <- data.frame(stack(g1pars),stack(g1cilows),stack(g1cihighs))
      g1pars <- ret[,c(2,1,3,5)]
      names(g1pars) <- c("fitgroup","g1","lowCI","highCI")

      out <- strsplit(as.character(g1pars$fitgroup),'-') 
      out2<- do.call(rbind, out)
      out3<- data.frame(g1pars$g1, do.call(rbind, out))
      out3 <- renameCol(out3, c("g1pars.g1","X1","X2","X3"),
                        c("g1","Dataset","Species","Treatment"))
      g1DF<- merge(g1pars, out3, by="g1")
      g1DF<- subset(g1DF, select = -c(fitgroup))
      
      
      r2 <- sapply(fit,getr2)  
      r2<-as.data.frame(r2)
      write.csv(r2, "r2 values.csv")    # Manually seperate columns (renamed as 'g1_r2 values')
      }
      
      
      # Merge g1 and r2 datasets
      g1_r2<- read.csv("g1_r2 values.csv")
      g1DF<- as.data.frame(g1DF)
      g1DFb<- merge(g1_r2, g1DF, by=c("Dataset","Species", "Treatment"))
      g1DFb<-as.data.frame(g1DFb)
      write.csv(g1DFb, "g1 with r2.csv")
      
      

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
          ylab("Conductance");p1
      
      pdf("output/fit_g1_plot_dataset.pdf")
      plot(p1)
      dev.off()
      
      

      
      
      
      ### make plot
      p1 <- ggplot(inDF) +
        geom_point(aes(Photo/sqrt(VPD)/CO2S,Cond, fill=Treatment, group=Treatment), pch=21)+
        geom_smooth(aes(Photo/sqrt(VPD)/CO2S,Cond, color=Treatment, group=Treatment), se=F)+
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
        ylab("Conductance");p1
      
      pdf("output/fit_g1_plot_all.pdf")
      plot(p1)
      dev.off()
      
      #rsq <- function (x, y) cor(x, y) ^ 2
      
      
    
      
      
      ## Supplemental figure to show the data distribution with CO2 treatment.
      p2<- ggplot(inDF, aes(x=Photo/sqrt(VPD)/CO2S, y = Cond, 
                               shape = Treatment, fill = Treatment))+
        theme_bw()+
        geom_point(colour="black")+
        facet_wrap(~ Dataset, nrow = 5)+
        scale_shape_manual(values=c(21,24)) +
        scale_fill_manual(values=c("blue","red"))+
        scale_y_continuous(name="Stomatal Conductance",expand = c(0, 0),limits=c(0,1.5), breaks=seq(0,1.5,0.5)) +
        scale_x_continuous(expand = c(0, 0),limits=c(0,0.1), breaks=seq(0,0.1,0.05)) +
        theme(legend.box = 'horizontal', legend.justification=c(1,1), 
              legend.position=c(1,1), legend.title = element_blank(),
              legend.text = element_text(size = 11), legend.key = element_blank(), 
              legend.background = element_blank(),legend.spacing.x = unit(0.25, "cm"),
              legend.key.height = unit(0.55, "cm"),legend.key.width = unit(0.2, "cm"));p2
      
      pdf("output/g1_scatterplots_by_dataset.pdf", width=10, height=10)
      plot(p2)
      dev.off()
      
      

     # Add in PFT for facet
            
      ENF <- g1DF$Dataset %in% c("Flakaliden", "Flakaliden_2","B_Glencorse", "Duke FACE", "B_SCC")
      EBF <- g1DF$Dataset %in% c("EucFACE", "Richmond_WTC1", "Richmond_WTC2")
      DBF <- g1DF$Dataset %in% c("BIFOR", "Rhinelander", "R_Glencorse", "Gribskov", "ORNL", "POPFACE","L_SCC")
      
      g1DF$PFT[ENF] <- "Evergreen Gymnosperm"
      g1DF$PFT[EBF] <- "Evergreen Angiosperm"
      g1DF$PFT[DBF] <- "Deciduous Angiosperm"
      g1DF$PFT<- as.factor(g1DF$PFT)
      
      g1DF$Dataset<-as.factor(g1DF$Dataset)
      g1DF$Species<-as.factor(g1DF$Species)
      g1DF$Treatment<-as.factor(g1DF$Treatment)
      

      # Assign an age
      mature <- g1DF$Dataset %in% c("EucFACE", "BIFOR","L_SCC", "B_SCC")
      young <- g1DF$Dataset %in% c("Duke FACE", "ORNL", "B_Glencorse","Flakaliden", "Flakaliden_2","Gribskov","R_Glencorse")
      sapling <- g1DF$Dataset %in% c("Richmond_WTC1", "Richmond_WTC2", "Rhinelander", "POPFACE")
      
      g1DF$Age[mature] <- "Old"
      g1DF$Age[young] <- "Mature"
      g1DF$Age[sapling] <- "Sapling"
      
      g1DF$Age = factor(g1DF$Age, levels=c('Old','Mature','Sapling'))
      
     
      
      ### plot g1_Dataset, Species by PFT
    dodge <- position_dodge2(width = 0.5)
    p3 <- g1DF %>%
      unite(DS, c("Dataset", "Species")) %>%
      mutate(name = fct_reorder(DS,g1)) %>%
      ggplot(aes(x=name, g1, 
                   group=interaction(DS,Treatment), 
                   shape= PFT, fill=Treatment)) +
      theme_bw()+
      geom_errorbar(aes(ymin = lowCI, ymax = highCI), position=dodge, width=0.5, size=0.2)+ 
      geom_point(size=2, position=dodge)  +
      scale_y_continuous(name="g1",expand = c(0, 0),limits=c(0,10), breaks=seq(0,10,2)) +
      scale_x_discrete(name="Dataset") +
      scale_shape_manual(values=c(21,22,23)) +
      scale_fill_manual(values=c("blue","red")) +
      facet_grid(PFT~. , scales="free", space = "free")+
      theme(legend.position="none") +
      coord_flip()+
      #stat_compare_means(aes(group=interaction(DS,Treatment)),label = "p.signif", label.y = c(1, 1, 1))+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank());p3
    
    
  
    
    
    pdf("output/g1_DatasetSp_PFT_TreeNum.pdf")
    plot(p3)
    dev.off()
    
    
   
    ## Testing for significance between the CO2 treatments. -------------
    
    ## A hacky way to get the ggsignif correct would be to create three separate plots and combine. 
    # Not got this to work yet. All datasets and CO2 treatments show 'not significant' but I dont belive that to be true.
    
    g1DFc<- g1DF %>% unite(DS, c("Dataset", "Species"))
    g1DFc$DS<-as.factor(g1DFc$DS)
    drop.levels(g1DFc)
    
    g1ENF<- filter(g1DFc, PFT=="Evergreen Gymnosperm")
    g1DBF<- filter(g1DFc, PFT=="Deciduous Angiosperm")
    g1EBF<- filter(g1DFc, PFT=="Evergreen Angiosperm")
    drop.levels(g1ENF)
    drop.levels(g1DBF)
    drop.levels(g1EBF)
    
    
    pDBF<- g1DBF %>%
      mutate(name = fct_reorder(DS,g1)) %>%
      ggplot(aes(x=name, g1, 
                 group=interaction(DS,Treatment), 
                 shape= PFT, fill=Treatment)) +
      theme_bw()+
      geom_errorbar(aes(ymin = lowCI, ymax = highCI), position=dodge, width=0.5, size=0.2)+ 
      geom_point(size=2, position=dodge)  +
      scale_y_continuous(name="g1",expand = c(0, 0),limits=c(0,10), breaks=seq(0,10,2)) +
      scale_x_discrete(name="Dataset") +
      scale_shape_manual(values=c(21,22,23)) +
      scale_fill_manual(values=c("blue","red")) +
      theme(legend.position="none") +
      coord_flip()+
      #stat_compare_means(aes(group=Treatment),label = "p.format", label.y = c(1))+
      theme(plot.title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(color="black", size=10),
            axis.title.y=element_text(color="black", size=12),
            axis.title.x=element_blank(),
            legend.position="none") +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank());pDBF
    
    pEBF<- g1EBF %>%
      mutate(name = fct_reorder(DS,g1)) %>%
      ggplot(aes(x=name, g1, 
                 group=interaction(DS,Treatment), 
                 shape= PFT, fill=Treatment)) +
      theme_bw()+
      geom_errorbar(aes(ymin = lowCI, ymax = highCI), position=dodge, width=0.5, size=0.2)+ 
      geom_point(size=2, position=dodge)  +
      scale_y_continuous(name="g1",expand = c(0, 0),limits=c(0,10), breaks=seq(0,10,2)) +
      scale_x_discrete(name="Dataset") +
      scale_shape_manual(values=c(21,22,23)) +
      scale_fill_manual(values=c("blue","red")) +
      theme(legend.position="none") +
      coord_flip()+
      #stat_compare_means(aes(group=Treatment),label = "p.format", label.y = c(1), size=2)+
      theme(plot.title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(color="black", size=10),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            legend.position="none") +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank());pEBF
    
    pENF<- g1ENF %>%
      mutate(name = fct_reorder(DS,g1)) %>%
      ggplot(aes(x=name, g1, 
                 group=interaction(DS,Treatment), 
                 shape= PFT, fill=Treatment)) +
      theme_bw()+
      geom_errorbar(aes(ymin = lowCI, ymax = highCI), position=dodge, width=0.5, size=0.2)+ 
      geom_point(size=2, position=dodge)  +
      scale_y_continuous(name="g1",expand = c(0, 0),limits=c(0,10), breaks=seq(0,10,2)) +
      scale_x_discrete(name="Dataset") +
      scale_shape_manual(values=c(21,22,23)) +
      scale_fill_manual(values=c("blue","red")) +
      theme(legend.position="none") +
      coord_flip()+
      theme(plot.title = element_blank(),
            axis.text.x = element_text(color="black", size=10),
            axis.text.y = element_text(color="black", size=10),
            axis.title.y=element_blank(),
            axis.title.x=element_text(color="black",size=12),
            legend.position="none") +
    #stat_compare_means(aes(group=Treatment),label = "p.format", label.y = c(1),size=2)+
     theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank());pENF
    
    
    
    p1<- pENF + stat_compare_means(aes(group=interaction(DS,Treatment)),
                                   method = "anova",
                                       label = "p.format", 
                                       label.y = c(1),
                                       size=4); p1
    
                                   
                                   
                                   
                                   
  library(patchwork)
    
    p3<- pEBF/ pDBF / pENF + plot_layout(widths = c(1,1,1), heights = c(1.8,5.5,2.5))
    
    # END --------- 
    # --------------------------------------------------------------------------------------------------
    
 
    
    
    
    pdf("output/g1_DatasetSp_PFT_TreeNum_seperate.pdf")
    plot(p3)
    dev.off()
    
    ### plot g1 by Dataset, Species by Age
    
    dodge <- position_dodge2(width = 0.5)
    p4 <- g1DF %>%
      unite(DS, c("Dataset", "Species")) %>%
      mutate(name = fct_reorder(DS,g1)) %>%
      ggplot(aes(x=name, g1, 
                 group=interaction(DS,Treatment), 
                 shape= PFT, fill=Treatment)) +
      theme_bw()+
      geom_errorbar(aes(ymin = lowCI, ymax = highCI), position=dodge, width=0.5, size=0.2)+ 
      geom_point(size=2, position=dodge)  +
      scale_y_continuous(name="g1",expand = c(0, 0),limits=c(0,10), breaks=seq(0,10,2)) +
      scale_x_discrete(name="Dataset") +
      scale_shape_manual(values=c(21,22,23)) +
      scale_fill_manual(values=c("blue","red")) +
      facet_grid(Age~. , scales="free", space = "free")+
      theme(legend.position="none") +
      coord_flip()+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank());p4
    
    pdf("output/g1_DatasetSp_Age.pdf")
    plot(p4)
    dev.off()
    
    
  
    ### plot g1 by Dataset, Species by Watered/ not watered ### NOT DONE YET
    
    Watered <- g1DF$Dataset %in% c("POPFACE","Richmond_WTC1", "Richmond_WTC2")
    NotWatered <- g1DF$Dataset %in% c("BIFOR", "Rhinelander", "R_Glencorse", "Gribskov", "ORNL", "L_SCC","EucFACE", "Flakaliden", "Flakaliden_2","B_Glencorse", "Duke FACE", "B_SCC")

    
    g1DF$Water[Watered] <- "Watered"
    g1DF$Water[NotWatered] <- "NotWatered"
    g1DF$Water<- as.factor(g1DF$Water)

    
    
    dodge <- position_dodge2(width = 0.5)
    p5 <- g1DF %>%
      unite(DS, c("Dataset", "Species")) %>%
      mutate(name = fct_reorder(DS,g1)) %>%
      ggplot(aes(x=name, g1, 
                 group=interaction(DS,Treatment), 
                 shape= PFT, fill=Treatment)) +
      theme_bw()+
      geom_errorbar(aes(ymin = lowCI, ymax = highCI), position=dodge, width=0.5, size=0.2)+ 
      geom_point(size=2, position=dodge)  +
      scale_y_continuous(name="g1",expand = c(0, 0),limits=c(0,10), breaks=seq(0,10,2)) +
      scale_x_discrete(name="Dataset") +
      scale_shape_manual(values=c(21,22,23)) +
      scale_fill_manual(values=c("blue","red")) +
      facet_grid(Water~. , scales="free", space = "free")+
      theme(legend.position="none") +
      coord_flip()+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank());p5
    
    pdf("output/g1_DatasetSp_Water.pdf")
    plot(p5)
    dev.off()
    
    
    
    
    
    
      
      
}


  



# r2 values for each graph 
#rsq <- function (x, y) cor(x, y) ^ 2

my.formula <- y ~ x


p1 <- ggplot(inDF, aes(Photo/sqrt(VPD)/CO2S,Cond, group=Dataset)) +
  geom_point(aes(fill=Dataset, group=Dataset), pch=21)+
  geom_smooth(aes(color=Dataset), formula = my.formula, se=F)+
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
  ylab("Stomatal Conductance");p1

pdf("output/fit_g1_plot_dataset_R2.pdf")
plot(p1)
dev.off()



















##------------------------------------  ## Start here
## Forest plot for g1 values


g1_forest <- function(g1DF) {
  

  ### now separate by CO2 treatment
  g1DF1 <- g1DF[g1DF$Treatment == "Ambient CO2",]
  g1DF2 <- g1DF[g1DF$Treatment == "Elevated CO2",]
  
  ### merge the two
  g1DF <- merge(g1DF1, g1DF2, by=c("Dataset", "Species","PFT","Age"))
  
  
  
  ### re-label all columns
  colnames(g1DF) <- c("Dataset","Species","PFT","Age",
                     "g1_aCO2","lowCI_aCO2", "highCI_aCO2", "Treatment_aCO2", "Water_aCO2",
                     "g1_eCO2","lowCI_eCO2", "highCI_eCO2", "Treatment_eCO2","Water_eCO2")
  
  
  
  
  ### obtain sample size for each CO2 treatment of each dataset
  inDF$count_variable <- 1.0
  tmpDF <- summaryBy(count_variable~Dataset+Species+Treatment, FUN=sum,
                     data=inDF, keep.names=T, na.rm=T)
  
  outDF <- merge(g1DF, tmpDF, by.x=c("Dataset", "Species", "Treatment_aCO2"),
                 by.y=c("Dataset", "Species", "Treatment"))
  
  names(outDF)[names(outDF)=="count_variable"] <- "g1_aCO2_n"
  
  outDF <- merge(outDF, tmpDF, by.x=c("Dataset", "Species", "Treatment_eCO2"),
                 by.y=c("Dataset", "Species", "Treatment"))
  
  names(outDF)[names(outDF)=="count_variable"] <- "g1_eCO2_n"
  
  g1DF <- outDF
  
 
  ### calculate response ratios   
  g1DF$g1_resp <- with(g1DF, g1_eCO2/g1_aCO2) 
    
  ### convert from CI to standard deviation
  g1DF$g1_aCO2_sd <- sqrt(g1DF$g1_aCO2_n) * (g1DF$highCI_aCO2 - g1DF$lowCI_aCO2) / 3.92
  g1DF$g1_eCO2_sd <- sqrt(g1DF$g1_eCO2_n) * (g1DF$highCI_eCO2 - g1DF$lowCI_eCO2) / 3.92


  ### calculate variance
  g1DF$g1_var <- with(g1DF, g1_resp*sqrt(((g1_aCO2_sd/g1_aCO2)^2 + (g1_eCO2_sd/g1_eCO2)^2)/2))
  

  
  #### Make simplified forest plot
  g1DF <- g1DF [complete.cases(g1DF$g1_resp),]
  g1DF  <- g1DF [order(g1DF$PFT, g1DF$Species, g1DF$Dataset),]
  l1 <- length(g1DF$Dataset)
  ns1 <- length(unique(g1DF$Dataset))
  
  
  #--------------------------------
  # Simple plot - one data entry per species per dataset, no VPD effect.
  
  pdf(paste0(getwd(), "/output/forest_g1.pdf"),width=10, height=10)
  
  ## WUE --------
  res_g1 <- rma.mv(g1_resp, g1_var,  random = ~1|Dataset, data = g1DF)
  forest(res_g1, slab=paste(g1DF$Dataset, g1DF$Species, sep=", "),
         xlim=c(-10, 10),
         ylim=c(0, 26),
         rows=c(22:11,9:7,5:1),
         at=c(-1,-0.5,0,0.5,1,2,3),
         refline=1,
         mlab="", psize=1, 
         cex=0.6,
         order=order(g1DF$PFT,g1DF$Dataset,g1DF$Species),
         header="g1 response to eCO2")
  text(2, 25, "Relative Response [95% CI]", pos = 2, cex = 0.7)
  text(-3.5, c(23,10,6), c("Decidious Broadleaf Forest", "Evergreen Broadleaf Forest", "Evergreen Needle Forest"), pos=2, font=4, cex=0.7)
  dev.off()
  
  
  print(res_g1)
}







































### LOOKING AT L_SCC data 



LCC<- filter(inDF, Dataset == "L_SCC")

p1 <- ggplot(LCC, aes(Photo/sqrt(VPD)/CO2S,Cond, group=Species)) +
  geom_point(aes(fill=Species, group=Species, shape=Treatment))+
  geom_smooth(aes(color=Species), formula = my.formula, se=F)+
  theme_linedraw() +
  scale_shape_manual(values=c(21,23)) +
  stat_poly_eq(formula = my.formula, 
               aes(color=Species,label = paste(..rr.label..)), 
               parse = TRUE, size= 4, vstep = 0.02) +
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
  ylab("Stomatal Conductance");p1

pdf("output/fit_g1_Just L_CC_R2.pdf")
plot(p1)
dev.off()

LCC_A<- filter(inDF, Dataset == "L_SCC" & Species =="Acer campestre")
LCC_F<- filter(inDF, Dataset == "L_SCC" & Species =="Fagus sylvatica")
LCC_P<- filter(inDF, Dataset == "L_SCC" & Species =="Prunus avium")
LCC_Q<- filter(inDF, Dataset == "L_SCC" & Species =="Quercus petraea")
LCC_T<- filter(inDF, Dataset == "L_SCC" & Species =="Tilia platyphyllos")


p <- ggplot(LCC_A, aes(Photo/sqrt(VPD)/CO2S,Cond, group=Treatment)) +
  geom_point(aes(fill=Treatment,  shape=Treatment))+
  theme_linedraw() +
  scale_shape_manual(values=c(21,23)) +
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
  ylab("Stomatal Conductance");p




LCC$fits <- paste0(LCC$Species, "-", LCC$Treatment)

list <- split(LCC, LCC$fits)

### fit g1 values

fit <- lapply(list,fitBB,gsmodel="BBOpti",
              varnames=list(VPD="VPD",ALEAF="Photo",GS="Cond",Ca="CO2S")) 
lapply(fit,coef)
g1pars <- sapply(fit,function(x)x$coef[[2]])
g1cilows <- lapply(fit,function(x)confint(x$fit)[1])
g1cihighs <- lapply(fit,function(x)confint(x$fit)[2])
ret <- data.frame(stack(g1pars),stack(g1cilows),stack(g1cihighs))
g1pars <- ret[,c(2,1,3,5)]
names(g1pars) <- c("fitgroup","g1","lowCI","highCI")






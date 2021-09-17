make_combined_forest_plot <- function() {
    
    #### read input
    sDF <- read.csv("data/simple forest.csv",header=T)
    sDF$X <- NULL
    
    #### prepare the WUE, Cond and Photo dataset for rma analysis
    wueDF <- sDF[complete.cases(sDF$WUE_resp),]
    wueDF <- sDF[order(wueDF$PFT, wueDF$Species, wueDF$Dataset),]
    wueDF <- wueDF[complete.cases(wueDF$WUE_var),]
    wueDF.angE <- subset(wueDF, PFT=="evergreen angiosperm")
    wueDF.angD <- subset(wueDF, PFT=="deciduous angiosperm")
    wueDF.gym <- subset(wueDF, PFT=="evergreen gymnosperm")

    condDF <- sDF[complete.cases(sDF$Cond_resp),]
    condDF <- condDF[order(condDF$PFT, condDF$Species, condDF$Dataset),]
    condDF <- condDF[complete.cases(condDF$Cond_var),]
    condDF.angE <- subset(condDF, PFT=="evergreen angiosperm")
    condDF.angD <- subset(condDF, PFT=="deciduous angiosperm")
    condDF.gym <- subset(condDF, PFT=="evergreen gymnosperm")

    
    photoDF <- sDF[complete.cases(sDF$Photo_resp),]
    photoDF <- photoDF[order(photoDF$PFT, photoDF$Species, photoDF$Dataset),]
    photoDF <- photoDF[complete.cases(photoDF$Photo_var),]
    photoDF.angE <- subset(photoDF, PFT=="evergreen angiosperm")
    photoDF.angD <- subset(photoDF, PFT=="deciduous angiosperm")
    photoDF.gym <- subset(photoDF, PFT=="evergreen gymnosperm")

    
    #### rma analysis
    res_WUE_EBF_S <- rma.mv(WUE_resp, WUE_var,  random = ~1|Dataset, data = wueDF.angE) 
    res_WUE_DBF_S <- rma.mv(WUE_resp, WUE_var,  random = ~1|Dataset, data = wueDF.angD) 
    res_WUE_ENF_S <- rma.mv(WUE_resp, WUE_var,  random = ~1|Dataset, data = wueDF.gym) 
    
    res_Cond_EBF_S <- rma.mv(Cond_resp, Cond_var,  random = ~1|Dataset, data = condDF.angE) 
    res_Cond_DBF_S <- rma.mv(Cond_resp, Cond_var,  random = ~1|Dataset, data = condDF.angD) 
    res_Cond_ENF_S <- rma.mv(Cond_resp, Cond_var,  random = ~1|Dataset, data = condDF.gym)
    
    res_Photo_EBF_S <- rma.mv(Photo_resp, Photo_var,  random = ~1|Dataset, data = photoDF.angE) 
    res_Photo_DBF_S <- rma.mv(Photo_resp, Photo_var,  random = ~1|Dataset, data = photoDF.angD)
    res_Photo_ENF_S <- rma.mv(Photo_resp, Photo_var,  random = ~1|Dataset, data = photoDF.gym) 
    
    
    ### merge the rma result together
    rmaDF <- data.frame("Dataset" = rep(c("EBF overall", "DBF overall", "ENF overall"), 
                                        length=3),
                        "Variable" = rep(c("WUE", "Cond", "Photo"), each = 3),
                        "PFT" = rep(c("EBF", "DBF", "ENF"), length=3),
                        "Mean" = NA,
                        "SE" = NA)
    
    ### assign values
    ## WUE
    rmaDF$Mean[rmaDF$Variable=="WUE"&rmaDF$PFT=="EBF"] <- res_WUE_EBF_S$b
    rmaDF$SE[rmaDF$Variable=="WUE"&rmaDF$PFT=="EBF"] <- res_WUE_EBF_S$se
    
    rmaDF$Mean[rmaDF$Variable=="WUE"&rmaDF$PFT=="DBF"] <- res_WUE_DBF_S$b
    rmaDF$SE[rmaDF$Variable=="WUE"&rmaDF$PFT=="DBF"] <- res_WUE_DBF_S$se
    
    rmaDF$Mean[rmaDF$Variable=="WUE"&rmaDF$PFT=="ENF"] <- res_WUE_ENF_S$b
    rmaDF$SE[rmaDF$Variable=="WUE"&rmaDF$PFT=="ENF"] <- res_WUE_ENF_S$se
    
    ## Cond
    rmaDF$Mean[rmaDF$Variable=="Cond"&rmaDF$PFT=="EBF"] <- res_Cond_EBF_S$b
    rmaDF$SE[rmaDF$Variable=="Cond"&rmaDF$PFT=="EBF"] <- res_Cond_EBF_S$se
    
    rmaDF$Mean[rmaDF$Variable=="Cond"&rmaDF$PFT=="DBF"] <- res_Cond_DBF_S$b
    rmaDF$SE[rmaDF$Variable=="Cond"&rmaDF$PFT=="DBF"] <- res_Cond_DBF_S$se
    
    rmaDF$Mean[rmaDF$Variable=="Cond"&rmaDF$PFT=="ENF"] <- res_Cond_ENF_S$b
    rmaDF$SE[rmaDF$Variable=="Cond"&rmaDF$PFT=="ENF"] <- res_Cond_ENF_S$se
    
    ## Photo
    rmaDF$Mean[rmaDF$Variable=="Photo"&rmaDF$PFT=="EBF"] <- res_Photo_EBF_S$b
    rmaDF$SE[rmaDF$Variable=="Photo"&rmaDF$PFT=="EBF"] <- res_Photo_EBF_S$se
    
    rmaDF$Mean[rmaDF$Variable=="Photo"&rmaDF$PFT=="DBF"] <- res_Photo_DBF_S$b
    rmaDF$SE[rmaDF$Variable=="Photo"&rmaDF$PFT=="DBF"] <- res_Photo_DBF_S$se
    
    rmaDF$Mean[rmaDF$Variable=="Photo"&rmaDF$PFT=="ENF"] <- res_Photo_ENF_S$b
    rmaDF$SE[rmaDF$Variable=="Photo"&rmaDF$PFT=="ENF"] <- res_Photo_ENF_S$se
    
    rmaDF$alpha <- "Overall"
    
    
    ### prepare individual dataset entries - WUE
    tmpDF1 <- wueDF.angE[,c("Dataset", "Species", "PFT", "WUE_resp", "WUE_var")]
    tmpDF2 <- wueDF.angD[,c("Dataset", "Species", "PFT", "WUE_resp", "WUE_var")]
    tmpDF3 <- wueDF.gym[,c("Dataset", "Species", "PFT", "WUE_resp", "WUE_var")]
    
    ### fill with predicted values - WUE
    tmpDF1$WUE_resp <- res_WUE_EBF_S$yi
    tmpDF1$WUE_var <- res_WUE_EBF_S$vi
    
    tmpDF2$WUE_resp <- res_WUE_DBF_S$yi
    tmpDF2$WUE_var <- res_WUE_DBF_S$vi
    
    tmpDF3$WUE_resp <- res_WUE_ENF_S$yi
    tmpDF3$WUE_var <- res_WUE_ENF_S$vi
    
    myDF1 <- rbind(tmpDF1, rbind(tmpDF2, tmpDF3))
    

    ### prepare individual dataset entries - Cond
    tmpDF1 <- condDF.angE[,c("Dataset", "Species", "PFT", "Cond_resp", "Cond_var")]
    tmpDF2 <- condDF.angD[,c("Dataset", "Species", "PFT", "Cond_resp", "Cond_var")]
    tmpDF3 <- condDF.gym[,c("Dataset", "Species", "PFT", "Cond_resp", "Cond_var")]
    
    ### fill with predicted values - WCond
    tmpDF1$Cond_resp <- res_Cond_EBF_S$yi
    tmpDF1$Cond_var <- res_Cond_EBF_S$vi
    
    tmpDF2$Cond_resp <- res_Cond_DBF_S$yi
    tmpDF2$Cond_var <- res_Cond_DBF_S$vi
    
    tmpDF3$Cond_resp <- res_Cond_ENF_S$yi
    tmpDF3$Cond_var <- res_Cond_ENF_S$vi
    
    myDF2 <- rbind(tmpDF1, rbind(tmpDF2, tmpDF3))
    
    
    ### prepare individual dataset entries - Photo
    tmpDF1 <- photoDF.angE[,c("Dataset", "Species", "PFT", "Photo_resp", "Photo_var")]
    tmpDF2 <- photoDF.angD[,c("Dataset", "Species", "PFT", "Photo_resp", "Photo_var")]
    tmpDF3 <- photoDF.gym[,c("Dataset", "Species", "PFT", "Photo_resp", "Photo_var")]
    
    ### fill with predicted values - Photo
    tmpDF1$Photo_resp <- res_Photo_EBF_S$yi
    tmpDF1$Photo_var <- res_Photo_EBF_S$vi
    
    tmpDF2$Photo_resp <- res_Photo_DBF_S$yi
    tmpDF2$Photo_var <- res_Photo_DBF_S$vi
    
    tmpDF3$Photo_resp <- res_Photo_ENF_S$yi
    tmpDF3$Photo_var <- res_Photo_ENF_S$vi
    
    myDF3 <- rbind(tmpDF1, rbind(tmpDF2, tmpDF3))
    
    ### assing variable
    colnames(myDF1) <- colnames(myDF2) <- colnames(myDF3) <- c("Dataset", "Species", "PFT", "Mean", "SE")
    myDF1$Variable <- "WUE"
    myDF2$Variable <- "Cond"
    myDF3$Variable <- "Photo"
    
    ### merge
    indDF <- rbind(myDF1, rbind(myDF2, myDF3))
    
    ### prepare PFT name
    indDF$PFT <- gsub("evergreen angiosperm", "EBF", indDF$PFT)
    indDF$PFT <- gsub("deciduous angiosperm", "DBF", indDF$PFT)
    indDF$PFT <- gsub("evergreen gymnosperm", "ENF", indDF$PFT)
    
    ### prepare dataset name
    indDF$Dataset <- paste0(indDF$Dataset, ", ", indDF$Species)
    
    ### reorder
    indDF$Species <- NULL
    indDF <- indDF[,c("Dataset", "Variable", "PFT", "Mean", "SE")]
    
    indDF$alpha <- "Individual"
    
    ### merge with the rmaDF
    plotDF <- rbind(rmaDF, indDF)
    
    ### assign plot label
    plotDF$Lab <- plotDF$Dataset
        
    ### arrange order 
    plotDF <- plotDF[order(plotDF$Variable, plotDF$PFT, plotDF$Dataset),]

    ### plot order
    plotDF$Dataset <- gsub("Rhnlndr, P.tremuloides", 
                           "02_Rhnlndr, P.tremuloides",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Rhnlndr, B.papyrifera", 
                           "03_Rhnlndr, B.papyrifera",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("R_Glncrs, B.pendula", 
                           "04_R_Glncrs, B.pendula",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("POPFACE, P.nigra", 
                           "05_POPFACE, P.nigra",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("POPFACE, P.euramericana", 
                           "06_POPFACE, P.euramericana",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("POPFACE, P.alba", 
                           "07_POPFACE, P.alba",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("ORNL, L.styraciflua", 
                           "08_ORNL, L.styraciflua",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("L_SCC, Q.petraea", 
                           "09_L_SCC, Q.petraea",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("L_SCC, F.sylvatica", 
                           "10_L_SCC, F.sylvatica",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Grbskv, F.sylvatica", 
                           "11_Grbskv, F.sylvatica",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("BIFOR, Q.robur", 
                           "12_BIFOR, Q.robur",
                           plotDF$Dataset)
    
    plotDF$Dataset[plotDF$PFT=="DBF"&plotDF$Dataset=="DBF overall"] <- "01_DEB_Overall"
    
    
    plotDF$Dataset <- gsub("Rchmnd_2, E.globulus", 
                           "02_Rchmnd_2, E.globulus",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Rchmnd_1, E.saligna", 
                           "03_Rchmnd_1, E.saligna",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("EucFACE, E.tereticornis", 
                           "04_EucFACE, E.tereticornis",
                           plotDF$Dataset)
    
    plotDF$Dataset[plotDF$PFT=="EBF"&plotDF$Dataset=="EBF overall"] <- "01_EBF_Overall"
    
    
    plotDF$Dataset <- gsub("Flkldn, P.abies", 
                           "02_Flkldn, P.abies",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Flkldn_2, P.abies", 
                           "03_Flkldn_2, P.abies",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Duke, P.taeda", 
                           "04_Duke, P.taeda",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("B_SCC, P.abies", 
                           "05_B_SCC, P.abies",
                           plotDF$Dataset)
    
    plotDF$Dataset[plotDF$PFT=="ENF"&plotDF$Dataset=="ENF overall"] <- "01_ENF_Overall"
    
    
    ### re-order
    plotDF <- plotDF[order(plotDF$Variable, plotDF$PFT, plotDF$Dataset),]
    
    
    ### make multi-panel plot
    #MakeExp <- function(x,y){
    #    exp <- vector(length = 0, mode = "expression")
    #    for (i in seq_along(x)) {
    #        if (i %in% y) exp[[i]] <- bquote(bold(.(x[i])))
    #        else exp[[i]] <- x[i]
    #    }
    #    return(exp)
    #}
    
    ## WUE, EBF
    plotDF1 <- plotDF[plotDF$Variable=="WUE"&plotDF$PFT=="EBF",]
    
    p1 <- ggplot(plotDF1)+ 
        geom_vline(xintercept = 1.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="purple", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="purple",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(hjust=0.5,
                                        size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(0,2))+
        #scale_y_discrete(labels=c(plotDF1$Lab))+
        #scale_y_discrete(labels=MakeExp(plotDF1$Lab, c("EBF overall")))+
        scale_y_discrete(labels=c(expression(bold("EBF overall")),
                                  plotDF1$Lab[2:4]))+
        scale_alpha_manual(name="",
                             limits=c("Individual", "Overall"),
                             values=c(0.5, 1))+
        #ggtitle("(a)")+
        guides(fill = guide_legend(title.position = "top"))+
        ggtitle("Water-Use Efficiency"); p1
    
    
    ## WUE, DBF
    plotDF2 <- plotDF[plotDF$Variable=="WUE"&plotDF$PFT=="DBF",]
    
    p2 <- ggplot(plotDF2)+ 
        geom_vline(xintercept = 1.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="green", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="green",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(0.,2))+
        #scale_y_discrete(labels=c(plotDF2$Lab))+
        #scale_y_discrete(labels=MakeExp(plotDF2$Lab, "EDF overall"))+
        scale_y_discrete(labels=c(expression(bold("EDF overall")),
                                  plotDF2$Lab[2:12]))+
        scale_alpha_manual(name="",
                           limits=c("Individual", "Overall"),
                           values=c(0.5, 1))+
        #ggtitle("(b)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    ## WUE, ENF
    plotDF3 <- plotDF[plotDF$Variable=="WUE"&plotDF$PFT=="ENF",]
    
    p3 <- ggplot(plotDF3)+ 
        geom_vline(xintercept = 1.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="blue3", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="blue3",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(name = "Relative Response (95% CI)",
                               limits=c(0.,2))+
        #scale_y_discrete(labels=c(plotDF3$Lab))+
        #scale_y_discrete(labels=MakeExp(plotDF3$Lab, "ENF overall"))+
        scale_y_discrete(labels=c(expression(bold("ENF overall")),
                                  plotDF3$Lab[2:5]))+
        scale_alpha_manual(name="",
                           limits=c("Individual", "Overall"),
                           values=c(0.5, 1))+
        #ggtitle("(c)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    
    
    ## Photo, EBF
    plotDF4 <- plotDF[plotDF$Variable=="Photo"&plotDF$PFT=="EBF",]
    
    p4 <- ggplot(plotDF4)+ 
        geom_vline(xintercept = 1.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="purple", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="purple",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(hjust=0.5,
                                        size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(0,2.5))+
        scale_y_discrete(labels=c(plotDF4$Lab))+
        scale_alpha_manual(name="",
                           limits=c("Individual", "Overall"),
                           values=c(0.5, 1))+
        #ggtitle("(d)")+
        guides(fill = guide_legend(title.position = "top"))+
        ggtitle("Photosynthesis")
    
    
    ## Photo, DBF
    plotDF5 <- plotDF[plotDF$Variable=="Photo"&plotDF$PFT=="DBF",]
    
    p5 <- ggplot(plotDF5)+ 
        geom_vline(xintercept = 1.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="green", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="green",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(0.,2.5))+
        scale_y_discrete(labels=c(plotDF5$Lab))+
        scale_alpha_manual(name="",
                           limits=c("Individual", "Overall"),
                           values=c(0.5, 1))+
        #ggtitle("(e)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    ## photo, ENF
    plotDF6 <- plotDF[plotDF$Variable=="Photo"&plotDF$PFT=="ENF",]
    
    p6 <- ggplot(plotDF6)+ 
        geom_vline(xintercept = 1.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="blue3", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="blue3",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(name = "Relative Response (95% CI)",
                           limits=c(0.,2.5))+
        scale_y_discrete(labels=c(plotDF6$Lab))+
        scale_alpha_manual(name="",
                           limits=c("Individual", "Overall"),
                           values=c(0.5, 1))+
        #ggtitle("(f)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    
    ## Cond, EBF
    plotDF7 <- plotDF[plotDF$Variable=="Cond"&plotDF$PFT=="EBF",]
    
    p7 <- ggplot(plotDF7)+ 
        geom_vline(xintercept = 0.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="purple", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="purple",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(hjust=0.5,
                                        size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-1,1.5))+
        scale_y_discrete(labels=c(plotDF7$Lab))+
        scale_alpha_manual(name="",
                           limits=c("Individual", "Overall"),
                           values=c(0.5, 1))+
        #ggtitle("(g)")+
        guides(fill = guide_legend(title.position = "top"))+
        ggtitle("Conductance")
    
    
    ## Cond, DBF
    plotDF8 <- plotDF[plotDF$Variable=="Cond"&plotDF$PFT=="DBF",]
    
    p8 <- ggplot(plotDF8)+ 
        geom_vline(xintercept = 0.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="green", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="green",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-1,1.5))+
        scale_y_discrete(labels=c(plotDF8$Lab))+
        scale_alpha_manual(name="",
                           limits=c("Individual", "Overall"),
                           values=c(0.5, 1))+
        #ggtitle("(h)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    ## Cond, ENF
    plotDF9 <- plotDF[plotDF$Variable=="Cond"&plotDF$PFT=="ENF",]
    
    p9 <- ggplot(plotDF9)+ 
        geom_vline(xintercept = 0.0, lty=2)+
        geom_hline(yintercept = 1.5, lty=1)+
        geom_errorbarh(aes(y=Dataset, xmin=Mean-SE, xmax=Mean+SE), 
                       col="blue3", height=0.3) + 
        geom_point(aes(y=Dataset, x=Mean, alpha=alpha), 
                   fill="blue3",
                   size=4, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 12, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(name = "Relative Response (95% CI)",
                           limits=c(-1,1.5))+
        scale_y_discrete(labels=c(plotDF9$Lab))+
        scale_alpha_manual(name="",
                           limits=c("Individual", "Overall"),
                           values=c(0.5, 1))+
        #ggtitle("(i)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    plot_1 <- plot_grid(p1, p2, p3,
                        labels=c("(a)", "(b)", "(c)"),
                        rel_heights=c(0.6,1.2,0.7),
                        ncol=1, 
                        align="vh", 
                        axis = "l",
                        label_x=0.4, 
                        label_y=c(0.85, 0.9, 0.85),
                        label_size = 18)
    
    plot_2 <- plot_grid(p4, p5, p6,
                        labels=c("(d)", "(e)", "(f)"),
                        rel_heights=c(0.6,1.2,0.7),
                        ncol=1, 
                        align="vh", 
                        axis = "l",
                        label_x=0.1, 
                        label_y=c(0.85, 0.9, 0.85),
                        label_size = 18)
    
    plot_3 <- plot_grid(p7, p8, p9,
                        labels=c("(g)", "(h)", "(i)"),
                        rel_heights=c(0.6,1.2,0.7),
                        ncol=1, 
                        align="vh", 
                        axis = "l",
                        label_x=0.1, 
                        label_y=c(0.85, 0.9, 0.85),
                        label_size = 18)
    
    pdf("output/check.pdf", 
        width=14, height=8)
    plot_grid(plot_1, plot_2, plot_3,
              rel_widths=c(1.5,1,1),
              labels="", 
              ncol=3, 
              align="hv", 
              axis = "l")    
    dev.off()
    
}

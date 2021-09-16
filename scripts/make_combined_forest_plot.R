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
    rmaDF <- data.frame("Dataset" = "Overall",
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
    
    ### merge with the rmaDF
    plotDF <- rbind(rmaDF, indDF)
    
    ### assign plot label
    plotDF$Lab <- plotDF$Dataset
        
    ### arrange order 
    plotDF <- plotDF[order(plotDF$Variable, plotDF$PFT, plotDF$Dataset),]

    ### plot order
    plotDF$Dataset <- gsub("Rhnlndr, P.tremuloides", 
                           "01_Rhnlndr, P.tremuloides",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Rhnlndr, B.papyrifera", 
                           "02_Rhnlndr, B.papyrifera",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("R_Glncrs, B.pendula", 
                           "03_R_Glncrs, B.pendula",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("POPFACE, P.nigra", 
                           "04_POPFACE, P.nigra",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("POPFACE, P.euramericana", 
                           "05_POPFACE, P.euramericana",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("POPFACE, P.alba", 
                           "06_POPFACE, P.alba",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("ORNL, L.styraciflua", 
                           "07_ORNL, L.styraciflua",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("L_SCC, Q.petraea", 
                           "08_L_SCC, Q.petraea",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("L_SCC, F.sylvatica", 
                           "09_L_SCC, F.sylvatica",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Grbskv, F.sylvatica", 
                           "10_Grbskv, F.sylvatica",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("BIFOR, Q.robur", 
                           "11_BIFOR, Q.robur",
                           plotDF$Dataset)
    
    plotDF$Dataset[plotDF$PFT=="DBF"&plotDF$Dataset=="Overall"] <- "12_Overall"
    
    
    plotDF$Dataset <- gsub("Rchmnd_2, E.globulus", 
                           "01_Rchmnd_2, E.globulus",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Rchmnd_1, E.saligna", 
                           "01_Rchmnd_1, E.saligna",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("EucFACE, E.tereticornis", 
                           "03_EucFACE, E.tereticornis",
                           plotDF$Dataset)
    
    plotDF$Dataset[plotDF$PFT=="EBF"&plotDF$Dataset=="Overall"] <- "04_Overall"
    
    
    plotDF$Dataset <- gsub("Flkldn, P.abies", 
                           "01_Flkldn, P.abies",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Flkldn_2, P.abies", 
                           "02_Flkldn_2, P.abies",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("Duke, P.taeda", 
                           "03_Duke, P.taeda",
                           plotDF$Dataset)
    
    plotDF$Dataset <- gsub("B_SCC, P.abies", 
                           "04_B_SCC, P.abies",
                           plotDF$Dataset)
    
    plotDF$Dataset[plotDF$PFT=="ENF"&plotDF$Dataset=="Overall"] <- "05_Overall"
    
    
    ### re-order
    plotDF <- plotDF[order(plotDF$Variable, plotDF$PFT, plotDF$Dataset),]
    
    
    ### make multi-panel plot
    
    
}

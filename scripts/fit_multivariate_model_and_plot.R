fit_multivariate_model_and_plot <- function(inDF) {
    
    
    ###################################################################################################################
    
    ### iWUE
    wueDF <- sDF[complete.cases(sDF$WUE_resp),]
    
    ### separate into different vegetation type
    wueDF.ang <- subset(wueDF, Type=="angiosperm")
    wueDF.gym <- subset(wueDF, Type=="gymnosperm")
    
    
    ### multi-variate linear mixed effect model
    res_WUE1 <- rma.mv(WUE_resp, WUE_var, mods = ~VPD_group, random = ~1|Dataset, data = wueDF.ang)
    res_WUE2 <- rma.mv(WUE_resp, WUE_var, mods = ~VPD_group, random = ~1|Dataset, data = wueDF.gym)
    
    ### predict effect size, at different VPD values
    predDF1 <- predict(res_WUE1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF2 <- predict(res_WUE2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    
    predDF1$lab <- "Angiosperm"
    predDF2$lab <- "Gymnosperm"
    
    plotDF1 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2))
    
    ### make a plot
    p1 <- ggplot(plotDF1) +
        geom_col(aes(X.VPD_group, pred, fill=lab, group=lab),
                 position=position_dodge(), col="black")+
        geom_errorbar(aes(x=X.VPD_group, ymin=pred-se, ymax=pred+se, group=lab), 
                      position=position_dodge(), width = 0.4)+
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
        ylab("WUE CO2 response")
    
    #plot(p1)
    
    
    
    ### Conductance
    condDF <- sDF[complete.cases(sDF$Cond_resp),]
    
    ### separate into different vegetation type
    condDF.ang <- subset(condDF, Type=="angiosperm")
    condDF.gym <- subset(condDF, Type=="gymnosperm")
    
    
    ### multi-variate linear mixed effect model
    res_cond1 <- rma.mv(Cond_resp, Cond_var, mods = ~VPD_group, random = ~1|Dataset, data = condDF.ang)
    res_cond2 <- rma.mv(Cond_resp, Cond_var, mods = ~VPD_group, random = ~1|Dataset, data = condDF.gym)
    
    ### predict effect size, at different VPD values
    predDF1 <- predict(res_cond1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF2 <- predict(res_cond2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    
    predDF1$lab <- "Angiosperm"
    predDF2$lab <- "Gymnosperm"
    
    plotDF2 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2))
    
    ### make a plot
    p2 <- ggplot(plotDF2) +
        geom_col(aes(X.VPD_group, pred, fill=lab, group=lab),
                 position=position_dodge(), col="black")+
        geom_errorbar(aes(x=X.VPD_group, ymin=pred-se, ymax=pred+se, group=lab), 
                      position=position_dodge(), width = 0.4)+
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
        ylab("Conductance CO2 response")
    
    #plot(p2)
    
    
    
    #Photo
    photoDF <- sDF[complete.cases(sDF$Photo_resp),]
    
    ### separate into different vegetation type
    photoDF.ang <- subset(photoDF, Type=="angiosperm")
    photoDF.gym <- subset(photoDF, Type=="gymnosperm")
    
    
    ### multi-variate linear mixed effect model
    res_photo1 <- rma.mv(Photo_resp, Photo_var, mods = ~VPD_group, random = ~1|Dataset, data = photoDF.ang)
    res_photo2 <- rma.mv(Photo_resp, Photo_var, mods = ~VPD_group, random = ~1|Dataset, data = photoDF.gym)
    
    ### predict effect size, at different VPD values
    predDF1 <- predict(res_photo1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF2 <- predict(res_photo2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    
    predDF1$lab <- "Angiosperm"
    predDF2$lab <- "Gymnosperm"
    
    plotDF3 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2))
    
    ### make a plot
    p3 <- ggplot(plotDF3) +
        geom_col(aes(X.VPD_group, pred, fill=lab, group=lab),
                 position=position_dodge(), col="black")+
        geom_errorbar(aes(x=X.VPD_group, ymin=pred-se, ymax=pred+se, group=lab), 
                      position=position_dodge(), width = 0.4)+
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
        ylab("Photosynthesis CO2 response")
    
    #plot(p3)
    
    
    ### plot
    combined_plot <- plot_grid(p1, p2, p3, 
                               ncol=1, align="vh", axis = "l")
    
    save_plot(paste0(getwd(), "/output/predicted_response.pdf"),
              combined_plot, base_width = 6, base_height=10)
    
    
}
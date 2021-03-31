fit_multivariate_model_and_plot <- function(inDF) {
    
    wueDF <- inDF[complete.cases(inDF$WUE_resp),]
    condDF <- inDF[complete.cases(inDF$Cond_resp),]
    photoDF <- inDF[complete.cases(inDF$Photo_resp),]
    
    
    #########################Splitt by vegetation type###############################
    
    ### iWUE
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
    
    save_plot(paste0(getwd(), "/output/predicted_CO2_response_at_VPD_bins_by_vegetation_type.pdf"),
              combined_plot, base_width = 6, base_height=10)
    
    
    
    
    
    
    ######################### Splitt by PFT ##############################
    
    ### WUE
    ### separate into different vegetation type
    wueDF.dbf <- subset(wueDF, PFT=="DBF")
    wueDF.ebf <- subset(wueDF, PFT=="EBF")
    wueDF.enf <- subset(wueDF, PFT=="ENF")
    
    
    ### multi-variate linear mixed effect model
    res_WUE1 <- rma.mv(WUE_resp, WUE_var, mods = ~VPD_group, random = ~1|Dataset, data = wueDF.dbf)
    res_WUE2 <- rma.mv(WUE_resp, WUE_var, mods = ~VPD_group, random = ~1|Dataset, data = wueDF.ebf)
    res_WUE3 <- rma.mv(WUE_resp, WUE_var, mods = ~VPD_group, random = ~1|Dataset, data = wueDF.enf)
    
    ### predict effect size, at different VPD values
    predDF1 <- predict(res_WUE1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF2 <- predict(res_WUE2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF3 <- predict(res_WUE3, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    
    predDF1$lab <- "DBF"
    predDF2$lab <- "EBF"
    predDF3$lab <- "ENF"
    
    plotDF1 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2),
                     as.data.frame(predDF3))
    
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
    
    
    
    ### conductance
    ### separate into different vegetation type
    condDF.dbf <- subset(condDF, PFT=="DBF")
    condDF.ebf <- subset(condDF, PFT=="EBF")
    condDF.enf <- subset(condDF, PFT=="ENF")
    
    
    ### multi-variate linear mixed effect model
    res_cond1 <- rma.mv(Cond_resp, Cond_var, mods = ~VPD_group, random = ~1|Dataset, data = condDF.dbf)
    res_cond2 <- rma.mv(Cond_resp, Cond_var, mods = ~VPD_group, random = ~1|Dataset, data = condDF.ebf)
    res_cond3 <- rma.mv(Cond_resp, Cond_var, mods = ~VPD_group, random = ~1|Dataset, data = condDF.enf)
    
    ### predict effect size, at different VPD values
    predDF1 <- predict(res_cond1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF2 <- predict(res_cond2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF3 <- predict(res_cond3, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    
    predDF1$lab <- "DBF"
    predDF2$lab <- "EBF"
    predDF3$lab <- "ENF"
    
    plotDF2 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2),
                     as.data.frame(predDF3))
    
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
    
    
    
    ### photosynthesis
    ### separate into different vegetation type
    photoDF.dbf <- subset(photoDF, PFT=="DBF")
    photoDF.ebf <- subset(photoDF, PFT=="EBF")
    photoDF.enf <- subset(photoDF, PFT=="ENF")
    
    
    ### multi-variate linear mixed effect model
    res_photo1 <- rma.mv(Photo_resp, Photo_var, mods = ~VPD_group, random = ~1|Dataset, data = photoDF.dbf)
    res_photo2 <- rma.mv(Photo_resp, Photo_var, mods = ~VPD_group, random = ~1|Dataset, data = photoDF.ebf)
    res_photo3 <- rma.mv(Photo_resp, Photo_var, mods = ~VPD_group, random = ~1|Dataset, data = photoDF.enf)
    
    ### predict effect size, at different VPD values
    predDF1 <- predict(res_photo1, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF2 <- predict(res_photo2, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    predDF3 <- predict(res_photo3, newmods = c(0.5, 1.0, 1.5, 2.0, 2.5), addx=T)
    
    predDF1$lab <- "DBF"
    predDF2$lab <- "EBF"
    predDF3$lab <- "ENF"
    
    plotDF3 <- rbind(as.data.frame(predDF1), as.data.frame(predDF2),
                     as.data.frame(predDF3))
    
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
    
    
    ### plot
    combined_plot <- plot_grid(p1, p2, p3, 
                               ncol=1, align="vh", axis = "l")
    
    save_plot(paste0(getwd(), "/output/predicted_CO2_response_at_VPD_bins_by_PFT.pdf"),
              combined_plot, base_width = 6, base_height=10)
    
    
    
    
    
    # Plot the 'mean response of A' versus 'the mean resposne of gsw'.
    p1 <- ggplot(inDF)+
        theme_bw()+
        geom_abline(slope=1, intercept=-1)+
        geom_smooth(method="lm", aes(Photo_resp, Cond_resp))+
        geom_point(aes(x=Photo_resp, y = Cond_resp, fill=Location, shape=PFT), size=2)+
        scale_shape_manual(values=c(21, 24, 22))+
        scale_fill_viridis(option = "D", discrete = TRUE)+
        guides(fill=guide_legend(override.aes=list(shape=21)))
        #scale_x_continuous(expand = c(0, 0),limits=c(-1.5,2.5), breaks=seq(-0.5,2.5,0.5))+
        #scale_y_continuous(expand = c(0, 0),limits=c(-1.5,2.5), breaks=seq(-1.5,1.5,0.5))+ 

    
    pdf(paste0(getwd(), "/output/photo_vs_cond_scatterplot.pdf"))
    plot(p1)
    dev.off()
    
    
    #tiff('Mean Photo vs mean Cond_scatterplot.tiff', units="in", width=8, height=6, res=500)
    #plot(p1)
    #dev.off()
    
    
    ### end

}
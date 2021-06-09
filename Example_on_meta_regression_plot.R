
res_WUE <- rma.mv(WUE_resp, WUE_var, mods = ~VPDmean, random = ~1|Dataset, data = wueDF)

preds<-predict(res_WUE, addx=TRUE,interval = 'confidence')

test <- as.data.frame(preds)


g0 <- ggplot(test,aes(x=X.VPDmean,y=pred))+ 
    stat_smooth(method="lm",formula=y~x,fullrange=T,se=FALSE,size=1,colour="black")+
    stat_smooth(data=test,aes(x=X.VPDmean,y=ci.lb),method="gam",formula=y~s(x,bs="cs"),
                fullrange=T,
                se=FALSE,size=1,linetype=3,colour="black")+
    stat_smooth(data=test,aes(x=X.VPDmean,y=ci.ub),method="gam",formula=y~s(x,bs="cs"),
                fullrange=T,
                se=FALSE,size=1,linetype=3,colour="black")+
    geom_point(data=wueDF,aes(x=VPDmean,y=WUE_resp,fill=Dataset, group=Dataset, pch=Type, size=1/WUE_var),
               position="jitter")+
    geom_abline(slope=0, intercept=1, lty=4, color = "grey")+
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
          legend.text.align=0)+
    xlab("VPD (kPa)")+
    ylab("WUE CO2 response (log_Resp / log_CO2)")+
    scale_shape_manual(name="Type",
                       values=c("angiosperm"=21,
                                "gymnosperm"=22))+
    ylim(-0.5, 3)


plot(g0)

pdf(paste0(getwd(), "/output/WUE_response_with_confidence_interval.pdf"),
    width=6, height=6)
plot(g0)
dev.off()


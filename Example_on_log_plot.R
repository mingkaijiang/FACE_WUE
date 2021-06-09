#Example_on_log_plot

sDF$Photo_log_resp <- with(sDF, log((Photo.mean.eCO2/Photo.mean.aCO2)/CO2_resp))
sDF$Photo_resp <- with(sDF, (Photo.mean.eCO2/Photo.mean.aCO2)/CO2_resp)

sDF$Photo_var <- with(sDF, 
                    (Photo.sd.eCO2 * Photo.sd.eCO2 / (Photo.n.eCO2 * Photo.mean.eCO2 * Photo.mean.eCO2) +
                         Photo.sd.aCO2 * Photo.sd.aCO2 / (Photo.n.aCO2 * Photo.mean.aCO2 * Photo.mean.aCO2))/CO2_resp)

sDF$Photo_log_var <- with(sDF, 
                        (Photo.sd.eCO2 * Photo.sd.eCO2 / (Photo.n.eCO2 * Photo.mean.eCO2 * Photo.mean.eCO2) +
                             Photo.sd.aCO2 * Photo.sd.aCO2 / (Photo.n.aCO2 * Photo.mean.aCO2 * Photo.mean.aCO2))/log(CO2_resp))

### add continuous factor for VPD values
sDF$VPDmean <- round((sDF$VPD.mean.aCO2+sDF$VPD.mean.eCO2)/2, 1)

### replace all inf numbers to NAs
is.na(sDF) <- do.call(cbind,lapply(sDF, is.infinite))


### make some plots
p3 <- ggplot(sDF) +
    geom_point(aes(VPD_group, Photo_resp, fill=Dataset, group=Dataset, pch=Type, size=1/Photo_var))+
    #geom_smooth(method = "lm", aes(VPD_group, Photo_resp, color=Type),
    #            se=F)+
    geom_abline(slope=0, intercept=0, lty=4)+
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
    ylab("Photo CO2 response")+
    scale_shape_manual(name="Type",
                       values=c("angiosperm"=21,
                                "gymnosperm"=22))+
    scale_y_continuous(trans = "log", breaks=c(0, 0.5, 1, 2, 3, 4))

pdf(paste0(getwd(), "/output/Photo_response_with_log_scale.pdf"),
    width=6, height=6)
plot(p3)
dev.off()


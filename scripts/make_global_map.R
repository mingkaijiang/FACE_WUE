make_global_map <- function() {
    
    ### read in data
    myDF <- read.csv("data/WUEdatabase_fixed.csv")

    plotDF <- summaryBy(latitude+longitude+altitude~Location+PFT+Type+Tregion+Wregion, 
                        FUN=mean, data=myDF, na.rm=T, keep.names=T)
    
    
    ### Flakaliden latitude and longitude incorrect, swap
    plotDF$longitude[plotDF$Location=="Flakaliden"] <- 19.27
    plotDF$latitude[plotDF$Location=="Flakaliden"] <- 64.07
    
    ### BIFOR FACE incorrect
    plotDF$longitude[plotDF$Location=="BIFOR FACE"] <- -2.3
    plotDF$latitude[plotDF$Location=="BIFOR FACE"] <- 52.8
    
    
    ### Richmond
    plotDF$latitude[plotDF$Location=="Richmond"] <- -33.62
    
    ### read in a global MAT and MAP map
    globDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")
    
    xlim.range <- range(globDF$lon)
    ylim.range <- range(globDF$lat)
    
    globDF$prec_cat <- ifelse(globDF$prec_annual_sum <= 100, 1, 
                              ifelse(globDF$prec_annual_sum > 100 & globDF$prec_annual_sum <= 500, 2,
                                     ifelse(globDF$prec_annual_sum > 500 & globDF$prec_annual_sum <= 2000, 3, 
                                            ifelse(globDF$prec_annual_sum > 2000 & globDF$prec_annual_sum <= 4000, 4, 5))))
    
    

    ### plotting
    p1 <- ggplot() + 
        geom_tile(data=globDF, aes(y=lat, x=lon, fill=as.character(prec_cat))) +
        coord_quickmap(xlim=xlim.range, ylim=ylim.range)+
        borders(database="world", colour="black", lty=1.0)+
        geom_point(data=plotDF, aes(y=latitude, x=longitude, color=PFT, pch = Type), size=2)+
        scale_fill_manual(name="Rainfall (mm/yr)", 
                          values=alpha(c("indianred4", "indianred1","thistle1", "skyblue", "blue"),0.2),
                          label=c("0-100", "100-500", "500-2000", "2000-4000", ">4000"))+
        #scale_color_manual(name="Wet factor", 
        #                   values=c("indianred4", "indianred3", "indianred1","thistle1", 
        #                            "slateblue1","purple1",  "purple4"),
        #                   label=c("< -2", "-2 to -1", "-1 to -0.1", "-0.1 to 0.1", 
        #                           "0.1 to 1", "1 to 2", "> 2"))+
        theme(legend.position="right",
              panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank())+
        guides(fill=guide_legend(nrow=5), color=guide_legend(nrow=3))+
        xlab("Longitude")+
        ylab("Latitude")
    
    
    ### output PDF
    pdf("output/global_map.pdf", width=8, height=6)
    plot(p1)
    dev.off()
    
    
    
    ### end   
}
test_script_on_g1_comparison <- function() {
    
    myDF<- read.csv("data/WUEdatabase_fixed.csv")
    inDF <- myDF
    
    
    ### split by dataset
    inDF$fits <- paste0(inDF$Dataset, "-", inDF$Species, "-", inDF$Treatment)
    
    inDF <- inDF[-3086,] 
    
    list <- split(inDF, inDF$fits)
    
    ## ignore the 14th data
    
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
    
    ### add PFT
    ENF <- g1DF$Dataset %in% c("Flakaliden", "Flakaliden_2","Barton_Glencorse", "Ellsworth_Duke", "Bader_SCC")
    EBF <- g1DF$Dataset %in% c("Gimeno_EucFACE", "Ellsworth_Richmond_WTC1", "Ellsworth_WTC2")
    DBF <- g1DF$Dataset %in% c("Gardner_BIFOR", "Uddling_Rhinelander", "Rey_Glencorse", "Freeman_Gribskov_BB", "Warren_ORNL", "Bernacchi_POPFACE","Leuzinger_SCC")
    
    g1DF$PFT[ENF] <- "Evergreen Gymnosperm"
    g1DF$PFT[EBF] <- "Evergreen Angiosperm"
    g1DF$PFT[DBF] <- "Deciduous Angiosperm"
    #g1DF$PFT<- as.factor(g1DF$PFT)
    
    #g1DF$Dataset<-as.factor(g1DF$Dataset)
    #g1DF$Species<-as.factor(g1DF$Species)
    #g1DF$Treatment<-as.factor(g1DF$Treatment)
    
    
    ### convert CI range to variance of standard deviation
    g1DF$var <- with(g1DF, (highCI-lowCI)/3.92)
    
    ### test significance
    res_g1 <- rma.mv(g1, var, mods = ~Treatment*PFT,
                      random = ~1|Dataset, data = g1DF)
    
    
    
}
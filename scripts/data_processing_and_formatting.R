data_processing_and_formatting <- function() {
    
    ### read in data
    myDF<- read.csv(paste0(getwd(), "/data/WUEdatabase_fixed.csv"))
    
    ### calculate WUE
    myDF$WUE <- with(myDF, Photo/Cond)
    
    ### Warren_ORNl dataset has no ci values.# Need to assign one so that it is included n the analysis.
    myDF$Ci[is.na(myDF$Ci)] <- "200"
    myDF$Ci <- as.numeric(myDF$Ci) # Change Ci back to numeric
    
    ### change tree label in sDF2
    myDF$Plantform[myDF$Species=="Phillyrea angustifolia"] <- "tree"
    
    ### make CO2 treatment consistent
    myDF$Treatment[myDF$Treatment%in%c("Outside Control", "Control")] <- "Ambient CO2"
    
    myDF$Treatment[myDF$Treatment=="OTC CO2" & myDF$GrowthCa%in%c("Ambient CO2", "Ambient")] <- "Ambient CO2"
    myDF$Treatment[myDF$Treatment=="OTC CO2" & myDF$GrowthCa%in%c("Elevated CO2", "Elevated")] <- "Elevated CO2"
    
    ### only include CO2 treatment
    myDF <- myDF[myDF$Treatment %in%c("Ambient CO2", "Elevated CO2"),]
    
    
    ### revise season information
    myDF$Season <- as.factor(myDF$Season)
    
    ## Filter to just have the summer season
    myDF<- drop.levels(myDF)
    
    ### remove VPD is missing
    myDF <- myDF[!is.na(myDF$VPD), ]
    
    ### groupping
    myDF$fitgroup<-as.factor(myDF$fitgroup)
    myDF$Dataset<-as.factor(myDF$Dataset)
    myDF<-as.data.frame(myDF)
    
    # Rename locations for plot
    myDF$Location[myDF$Location =="Glencorse near Edinburgh"] <- "Glencorse"
    myDF$Location[myDF$Location =="Duke Forest Chapel Hill"] <- "Duke FACE"
    myDF$Location[myDF$Location =="POPFACE Italy"] <- "POPFACE"
    myDF$Location[myDF$Location =="EucFACE Richmond"] <- "EucFACE"
    
    ### there are two missing values in conductance, ignore them
    myDF <- myDF[complete.cases(myDF$Cond),]
    
    ### return the dataset
    return(myDF)
}
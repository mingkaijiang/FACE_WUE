## Flakaliden

source("/scripts")

#Read data
library(readr)
df <- read_csv("data/WUEdatabase_fixed_new.csv")

# subset flaklinden2
df <- subset(df, Dataset=="Flakaliden_2")

#List of issues to fix. 

# Filter out reciprocal CO2: i.e.the aCO2 at elevated treatment.
df<- df %>% filter(!((Treatment == "Elevated CO2" & CO2S <500) | (Treatment == "Elevated CO2"& CO2S >700))) 
df<- df %>% filter(!((Treatment == "Ambient CO2" & CO2S >400))) # four outliers 


df$fitgroup <- as.factor(df$fitgroup)
df <- as.data.frame(df) 

#Calculate iWUE
df$iWUE<- df$Photo/df$Cond


##Write new data file with adjustments.
write.csv(df,"Fixed_Flakaliden_2.csv")



## Analysis from here------------------------------
##  Read in Flakaliden_fixed.

df<- read_csv("data_fixed/Fixed_Flakaliden_2.csv")

df$fitgroup <- as.factor(df$fitgroup)
df <- drop.levels(df)
df <- as.data.frame(df)



### G1's

graph_Flakaliden_2 <- function (df) {
  # checking model fits
  list <- split(df,df$fitgroup)
  fit <- lapply(list,fitBB,gsmodel="BBOpti",
                varnames=list(VPD="VPD",ALEAF="Photo",GS="Cond",Ca="CO2S"))
  g1pars <- sapply(fit,function(x)x$coef[[2]])
  g1cilows <- lapply(fit,function(x)confint(x$fit)[1])
  g1cihighs <- lapply(fit,function(x)confint(x$fit)[2])
  ret <- data.frame(stack(g1pars),stack(g1cilows),stack(g1cihighs))
  g1pars <- ret[,c(2,1,3,5)]
  names(g1pars) <- c("fitgroup","par","lowCI","highCI")
  return(g1pars)
  
  do.call(rbind, lapply(fit, coef))
  with(df,plot(Photo/sqrt(VPD)/CO2S,Cond,col=fitgroup)) #fitgroup
  legend("topleft",legend=as.factor(unique(df$fitgroup)),
         col=as.factor(unique(df$fitgroup)),pch=1)
}

graph_Flakaliden_2(df)


Flakaliden_2<- graph_Flakaliden_2(df)
Flakaliden_2_g1 <- as.data.frame(Flakaliden_2)
Flakaliden_2_g1$Dataset <- "Flakaliden"

C<- count(df, 'fitgroup')  
g1_Flakaliden_2<-merge(Flakaliden_2_g1, C, by= 'fitgroup') 

write.csv(g1_Flakaliden_2,"/data/g1_Flakaliden_2.csv")











#Data plotted
### G1's


graph_FL2 <- function (df) {
  # checking model fits
  list <- split(df,df$fitgroup)
  fit <- lapply(list,fitBB,gsmodel="BBOpti",
                varnames=list(VPD="VPD",ALEAF="Photo",GS="Cond",Ca="CO2S"))
  g1pars <- sapply(fit,function(x)x$coef[[2]])
  g1pars <- stack(g1pars)
  g1pars
  do.call(rbind, lapply(fit, coef))
  with(df,plot(Photo/sqrt(VPD)/CO2S,Cond,col=fitgroup)) #fitgroup
  legend(x = 0.00, y = 2.00, legend = levels(df$fitgroup),col = c(1:6), pch=1)
}

graph_FL2(df)

## Need a way to get g1 values into a table?


#Graphs

g0<- ggplot(df) +
  theme_bw()+
  geom_point(aes(x=Treatment, y = CO2S, shape = Treatment, fill = Treatment))+
  scale_fill_manual(values=c("blue","red")) +
  scale_colour_manual(values=c("blue","red")) +
  scale_shape_manual(values=c(21,24)) +
  labs(title= df$Dataset)+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank());g0


#VPD vs Temp
# No temperature data (Tchamber is in original)

#Temperature
# No temperature data 

g3<- ggplot(df) +
  theme_bw()+
  geom_boxplot(aes(x=Species, y = Photo, fill = Treatment), width=0.2)+
  #scale_x_continuous(expand = c(0, 0),limits=c(0,1), breaks=seq(0,1,0.5))+
  scale_y_continuous(expand = c(0, 0),limits=c(0,40), breaks=seq(0,40,10))+
  scale_fill_manual(values=c("blue","red")) +
  labs(title= df$Dataset)+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank());g3


g4<- ggplot(df) +
  theme_bw()+
  geom_boxplot(aes(x=Species, y = Cond, fill = Treatment), width=0.2)+
  #scale_x_continuous(expand = c(0, 0),limits=c(0,1), breaks=seq(0,1,0.5))+
  #scale_y_continuous(expand = c(0, 0),limits=c(0,3), breaks=seq(0,3,1))+
  scale_fill_manual(values=c("blue","red")) +
  labs(title= df$Dataset)+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank());g4



g5<- ggplot(df) +
  theme_bw()+
  geom_boxplot(aes(x=Species, y = iWUE, fill = Treatment), width=0.2)+
  #scale_x_continuous(expand = c(0, 0),limits=c(0,1), breaks=seq(0,1,0.5))+
  #scale_y_continuous(expand = c(0, 0),limits=c(0,3), breaks=seq(0,3,1))+
  scale_fill_manual(values=c("blue","red")) +
  labs(title= df$Dataset)+
  theme(legend.box = 'horizontal', legend.justification=c(1,1), 
        legend.position=c(1,1), legend.title = element_blank(),
        legend.text = element_text(size = 11), legend.key = element_blank(), 
        legend.background = element_blank(),legend.spacing.x = unit(0.25, "cm"),
        legend.key.height = unit(0.55, "cm"),legend.key.width = unit(0.2, "cm")) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank());g5


require(EnvStats)
g6 <- df %>%
  mutate(bin=cut_width(VPD, width=0.5, boundary=0)) %>%
  ggplot(aes(x=bin, y=Cond) ) +
  geom_boxplot(fill="#69b3a2") +
  stat_n_text(size= 4,y.pos = 0.85) +
  theme_bw() +
  labs(title= df$Dataset, x ="VPD")+ 
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank()); g6



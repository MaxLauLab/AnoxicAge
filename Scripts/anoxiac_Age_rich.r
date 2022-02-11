
# Graph look
papermet_nofrills <- function(base_size = 14, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size=14,colour="black"),
      axis.text.y = element_text(size=14,colour="black", margin=margin(0,5,0,0), vjust = 0.5),
      axis.ticks =  element_line(colour = "black"), 
      axis.title.x= element_text(size=21,colour="black",vjust=-2),
      axis.title.y= element_text(size=21,angle=90,colour="black",vjust = 6),
      panel.background = element_rect(fill="white",size = 1.5, colour = "black"), 
      panel.border =element_blank(),  
      panel.grid.major = element_blank(), 
      #panel.grid.minor = element_line(colour = "lightgrey"),
      panel.grid.minor = element_blank(), 
      plot.background = element_rect(fill="white",color=NA), 
      plot.title =element_text(size=16,colour="black",margin = margin(0, 0, 10, 0)),
      plot.subtitle =element_text(size=12,colour="black",margin = margin(10, 0, 10, 0)),
      plot.margin = unit(c(1,  1, 1, 2), "lines"),
      legend.background=element_rect(fill='white',color=NA),
      legend.title=element_text(size=14,colour="black"),
      legend.text=element_text(size=14,colour="black"),
      legend.key = element_rect( fill = 'lightgrey',color='white'),
      legend.key.size = unit(c(1.2, 1.2), "lines"),
      axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1),
      #factettes
      strip.background = element_rect(colour = 'lightgrey',size = 1,fill = 'lightgrey'), #for boxes of facettes
      panel.spacing = unit(0.6 , "lines"),   #panel margins
      strip.text.x = element_text(size = 12,colour = 'black'),
      strip.text.y = element_text(size = 12,colour = 'black',angle = 90)
    )
}

#packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)


#information
#thresh.ox tox <- c(0.3)#mg/L entsprich 9.375 uM O2

#imporat data
rich_import <- read.csv("./Output/YSI.aa.2021.csv", fileEncoding = "UTF-8") 
rich_import = t(rich_import)

#calculate anoxic age
dptt <- seq(1:(ncol(rich_import)-1)) #only data from 30 m, add top rows later, only days after onset of stratification?
startdate_doy <- 0 #day of onset of stratification
lengthofdata <- nrow(rich_import)
aa.coerced <- data.frame(diy=0,anox.age=0,lake=0,depth.x=0)

#
for (m in 2:length(dptt)) {
#as.numeric turns T/F in 1/0  
depth_series <- data.frame(o2.threshcrossed=as.numeric(rich_import[,m]),diy=1:lengthofdata )

for(i in 2:nrow(depth_series)) { 
  depth_series[i,1] <- (depth_series[i-1,1]+depth_series[i,1])*depth_series[i,1]}

aa.coerced <- rbind(
  aa.coerced,data.frame(
    diy=seq(1:max(depth_series[,2]))+startdate_doy,
    anox.age=depth_series[,1],
    lake="Arendsee2021",
    depth.x=dptt[m]+29))
}
aa.coerced<-aa.coerced%>%filter(lake!=0)

#data format for plot: diy, anox.age, lake, depth.x


#REARRANGE DATA
grids<-7 #usual grid size is 1 (1 day and 0.1m), multiply to make grid broader
grids2<-5 #

holom<- data.frame(diy=c(80,343), #DIYs of circulation events
                   ys=-35,                       # position of "full circulation" text
                   mixi="full circulation", 
                   ymins=c(-47.5), #can be different values if different lakes are displayed
                   lake=c("Arendsee2021")) #some adjustements would be necessary if you wish to change that, or add anther year
monthss<- data.frame(diy=c(seq(from=15,to=365,by=30)),
                     mons=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct", "Nov", "Dec"),
                     lake="Arendsee2021") #Displayed only in the last row, so "lake" here would be only the lowermost panel
#PLOT

p2 <-ggplot(data=aa.coerced)+
  geom_tile(aes(x=diy,y=-depth.x,fill=anox.age))+
  geom_rect(data=aa.coerced%>%filter(anox.age==0),
            aes(xmin=round(diy/grids)*grids - grids/2, 
                xmax=round(diy/grids)*grids + grids/2, 
                ymin=-round(depth.x/(grids2/10))*(grids2/10) - (grids2/10), 
                ymax=-round(depth.x/(grids2/10))*(grids2/10) + (grids2/10)),
            size=0.05, fill=NA, colour="lightgrey",alpha=0.25)+
  geom_rect(data=aa.coerced%>%filter(anox.age>0),
            aes(xmin=round(diy/grids)*grids - grids/2, 
                xmax=round(diy/grids)*grids + grids/2, 
                ymin=-round(depth.x/(grids2/10))*(grids2/10) - (grids2/10), 
                ymax=-round(depth.x/(grids2/10))*(grids2/10) + (grids2/10)),
            size=0.05, fill=NA, colour="black")+
  scale_fill_gradient(low = "white", high = "red",name="Anoxic since (days)",na.value = 'white')+
  papermet_nofrills()+
  scale_y_continuous(name="Depth (m)",limits=c(-49,-28),breaks=c(-30,-35,-40,-45),labels=c(30,35,40,45))+
  scale_x_continuous(name="Day of Year")+
  theme(legend.position="bottom")+
  guides(fill = guide_colourbar(barheight = 0.5,barwidth  = 10))+
  facet_grid(lake~.#,
             #labeller=labeller(lake=c("cc"="Lake Croche","cw"="Lake Cromwell"))
             )+
  geom_text(data=monthss,aes(x=diy,y=-49,label=mons))+
  geom_rect(data=holom,aes(xmin=diy,xmax=diy+10,ymin=ymins,ymax=-30.5),fill="lightgrey")+ #change bar for "circulation" here
  geom_text(data=holom,aes(x=diy+3.5,y=ys,label=mixi),angle=90) # position of "full circulation" text

p2

ggsave("./Output/AnoxicAgeGrid_2021.jpg", p2, device = "jpeg")

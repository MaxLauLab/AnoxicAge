#Klimaprojekt
#Script Data gathering
#Parameter: bathymetry
#version original March 26 2020
#Update[No] [Date] : *Updated section
#Update[No] [Date] : *Updated section
#Update[No] [Date] : *Updated section

#working directory
setwd("C:/Users/lau/Documents/WORK/IGB_POST/Klimaprojekt")


#load packages
#
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(lubridate)

#load logger metadata
lakelist <- read_excel("Daten/loggermeta.xlsx",
                         sheet="lake_meta")


#bathymetry data class, in column bathy_file_avail
#1. area/depth curve: "a.d",2. average depth av.d, 3. unknown
eachfile<-data.frame()
eachfile1 <- eachfile
eachfile2 <- eachfile


for (i in 1:nrow(lakelist)) { #check out every lake one by one
  
if (lakelist[i,11]=="a.d"){

#this case: bathymetry file availible with 1m increments (or higher) and volume per layer over area

#rules: area@0m=full area, depth in 1 m increments, volume is from depth above to depth@increment
bat.file <- read_excel(paste("Daten/bats/",lakelist[i,2],"_bats_r.xlsx",sep=""),
                           sheet="bats") %>% select(depth,volume.share,area.m2) %>%
  group_by(depth_1m=ceiling(depth)) %>% summarize(vol.inc=sum(volume.share,na.rm=TRUE),area_1m=mean(area.m2)) %>%
  data.frame() %>% mutate(lake=as.character(lakelist[i,2])) 

lake_cumu <- (bat.file%>%summarize(sum(vol.inc)))[1,1]
#[1] "depth_1m" "vol.inc"  "area_1m"  "lake"     "cumvol"  
bat.file.assambled <- bat.file %>% mutate(cumvol=lake_cumu)
eachfile1 <- bind_rows(bat.file.assambled,eachfile1)
}
  if (lakelist[i,11]=="av.d"){
    #Az = A0(1-Z:Zmax)^q Imboden 1983, Az is planar area at depth 
    #hakanson 2005: (Vd= 3 · Dmv/Dmax); Awb = (A · ((Dmax – Dwb)/(Dmax + Dwb · EXP (3 – Vd^  1.5)))^(0.5/vd)
    
    Vd<-(lakelist[i,12]*3/lakelist[i,7] %>% data.frame())[1,1]
    bat.file <- data.frame(depth_1m=seq(from=0,to=as.numeric(lakelist[i,7])) )%>% 
      mutate(a_factor=((as.numeric(lakelist[i,7])-depth_1m)/(as.numeric(lakelist[i,7])+depth_1m*exp(3-Vd^1.5)))^(0.5/Vd)) %>%
      mutate(area_1m=as.numeric(lakelist[i,10])*a_factor) %>% 
      mutate(vol.inc=(area_1m+ifelse(!is.na(lead(area_1m)),lead(area_1m),0))/2*1) %>%
      select(depth_1m,vol.inc,area_1m) %>%
      mutate(lake=as.character(lakelist[i,2])) 
    
    lake_cumu <- (bat.file%>%summarize(sum(vol.inc,na.rm=TRUE)))[1,1]
    
    #[1] "depth_1m" "vol.inc"  "area_1m"  "lake"     "cumvol"  
    bat.file.assambled <- bat.file %>% mutate(cumvol=lake_cumu)
    eachfile2 <- bind_rows(bat.file.assambled,eachfile2)
    
  }
  
}
bat.file.assambled <- bind_rows(eachfile1,eachfile2) %>%unique()



#final data 

bathymetries <- bat.file.assambled %>% group_by(lake) %>% arrange(-depth_1m) %>%
  mutate(vol_1m=(cumsum(vol.inc))) %>%
  arrange(depth_1m) %>% mutate(area.inc=area_1m-lead(area_1m,default=0)) %>%
  mutate(inc=paste(as.character((depth_1m)),'-',as.character(lead(depth_1m))) ) %>% data.frame() %>%
  mutate(alpha=(area.inc/vol.inc) ) #(sediment surfacearea  to  water  volume  ratio  [m2 sediment / m3 water]  atwater depthZ)

ggplot(bathymetries)+
  geom_step(aes(y=vol_1m,x=-depth_1m),size=1.5)+
  facet_wrap(lake~.,scales="free")+
  coord_flip()

#compare gg
#gg.pure <- bat.file
ggplot()+
  geom_step(data=bathymetries %>% filter(lake=="gg"),color="red",aes(x=alpha,y=-depth_1m),size=1.5)+
  geom_step(data=bathymetries %>% filter(lake=="gg_test"),aes(x=alpha,y=-depth_1m),size=1.5)


#alpha correct?:
bathymetries %>% filter(alpha>10)
ggplot(bathymetries %>% filter(alpha!=Inf)%>% filter(alpha>0 & alpha<10))+
  geom_point(aes(y=alpha,x=depth_1m,color=lake),size=3)+
  theme_klima()

#
bathymetries %>% select(cumvol,lake) %>% unique()

#export all bats
write.csv(bathymetries,"Daten/database/bats.csv",row.names = FALSE)

  
##################################################################################
############################Processing Rinko profiles#############################
##################################################################################
library(dplyr)

setwd(dir = "D:/Postdoc/Allemagne/Github/AnoxicAge/Data/2021/Rinko")

All.files = list.files("./Raw")
All.files = All.files[grep(".Csv", All.files)]

Lakes_name = c("Ar","Ar","Ar","Ar","Ar","Ar",
               "Sc",
               "FakeGg", "Gg", "FukuNO", "FukuSW",
               "Hs", "Hs", "St")

pdf("./Profiles_2021.pdf")


for(i in 1:length(All.files)){

data = read.csv(paste0("./Raw/",All.files[i]), skip=43)
colnames(data) = c("Depth_m", "T_C", "Salinity", "Cond_uScm", "EC25_uScm",
                   "Density_kgm3", "SigmaT", "Chl_Fluo", "Chla_ugL",
                   "Turbidity_FTU", "DO_pc", "DO_mgL", "Volt", "X")
data = data[,-c(7,8,13,14)] #Remove SigmaT, Chlf-Fluo, Batterie, X

#Look at the profiles
# par(mfrow=c(1,2))
# plot(data$Depth_m ~ data$DO_mgL,
#      ylim = rev(range(data$Depth_m)))              
# plot(data$Depth_m ~ data$T_C,
#      ylim = rev(range(data$Depth_m)))              
#Makes sense


#File #1 is Arendsee
#File #2 is Arendsee again, and lookes better. Save over the first profile
#File #3 is empty
#File #4 looks like Arendsee again, but is much weirder. Profile was done in the mesocosm?
#File #5 looks like Arendsee again, in a bag again?
#File #6 is empty
#File #7 is Scharmutzelsee (or whatever)
#File #8 was supposed to be Grossglinike but is the worst profile of all time
#File #9 is Gg and looks fine
#File #10 is FukuNO and looks fine
#File #11 is FukuSW and looks fine
#File #12 is Hausse and looks fine
#File #13 is Hs again and is a bit less clean
#File #14 is Stechlin and looks ok

#Remove data point that looks in the air (Temperature spike)
if(i == 7 | i ==11) {
 data = data[-1,]
#Correct the depth
 data$Depth_m = data$Depth_m - 0.1
}

#Store Lake name
if(i == 2 | i == 7 | i == 9 | i == 10 | i ==11 | i ==12|i==14) {
  data$Lake = Lakes_name[i]
  par(mfrow=c(1,2))
  plot(data$Depth_m ~ data$DO_mgL,
       ylim = rev(range(data$Depth_m)),
       ylab = "Depth (m)",
       xlab = "DO (mg/L)",
       main = paste(Lakes_name[i]))              
  plot(data$Depth_m ~ data$T_C,
       ylim = rev(range(data$Depth_m)),
       ylab = "Depth (m)",
       xlab = "T (°C)",
       main = paste(Lakes_name[i]))  
#Transform to Long format
data_long = pivot_longer(data,cols = 1:10, names_to = "Parameter", values_to = "Value")
if(i==2) data_long_all = data_long
if(i==2) data_all = data

#Create 1 big long file (saved at the very end)
data_long_all = rbind(data_long_all,data_long)

#Create 1 big wide file (saved at the end)
data_all = rbind(data_all,data)


write.csv(data, paste0("./Processed/",Lakes_name[i],"_wide.csv"), fileEncoding = "UTF-8", row.names = F)
write.csv(data_long, paste0("./Processed/",Lakes_name[i],"_long.csv"), fileEncoding = "UTF-8", row.names = F)
if(i == 14) write.csv(data_all, paste0("./Processed/All_lakes_wide.csv"), fileEncoding = "UTF-8", row.names = F)
if(i == 14) write.csv(data_long_all, paste0("./Processed/All_lakes_long.csv"), fileEncoding = "UTF-8", row.names = F)
}
}
dev.off()

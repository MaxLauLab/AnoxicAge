
###########################################################################################
####################################Calculate anoxix age###################################
###########################################################################################  
setwd("D:/Postdoc/Allemagne/Github/AnoxicAge")
Bats <- read.csv("./Data/Raw/bats.csv")
source("./Scripts/O2_DecayRate.R")
#This step could be made automatic with all the objects produced by the other script
Coef.mat = data.frame(Living.inter = c(0.052309, 0.043523, 0.043853,
                                       0.015032, 0.041168, 0.033804, 0.051304, 0.03033122,
                                       0.04437718, 0.02365026, 0.04775718,0.07117915,
                                       0.046327, 0.031319, 0.038923,0.050912),
                      Living.slope = c(0.72095, 1.189771, 0.708357,
                                       1.581965, 0.70248, 1.053361, 1.449346, 2.09699842,
                                       0.53443228, 1.50538693, 0.62727197, 1.02305984,
                                       0.343572, 0.656356, 0.609426, 1.175155),
                      Log.inter = c(0.22027, 0.31388, 0.209519,
                                    0.201798, 0.200273, 0.189948, 0.222753, 0.2692234,
                                    0.18372782, 0.33931524, 0.18262637, 0.2933936,
                                    0.142668, 0.174929, 0.174651, 0.310416),
                      Log.slope = c(0.0943, 0.15067, 0.093187,
                                    0.091595, 0.089668, 0.080629, 0.084127, 0.11615,
                                    0.07984926, 0.174804, 0.07512701, 0.1240715,
                                    0.056136, 0.08113, 0.077021, 0.146945),
                      Years = c(2017, 2018, 2019,
                                2017, 2018, 2019, 2020, 2020,
                                2017, 2018, 2019, 2020,
                                2017, 2018, 2019, 2020),
                      Method = c("Logger", "Logger", "logger",
                                 "Low res","Low res","Low res","Low res","Low res rinko",
                                 "High res","High res","High res","High res",
                                 "Low res best","Low res best","Low res best","Low res best"))
Coef.mat <- Coef.mat %>% arrange(Years)

#Select alpha from 20m to 47m deep
ar.alpha <- filter(Bats, lake =="ar") %>% arrange(depth_1m) %>% slice(31:48) %>% select(alpha)
ar.depth = c(30:47)

#Create decay rate function
O2.decay <- function(Matrix, Model, Year, Method, ar.alpha){
  if(Model == "Livingstone"){
    inter = Matrix[,1]
    slope = Matrix[,2]
  } else {
    inter = Matrix[,3]
    slope = Matrix[,4]
  }
  
  inter.coef = inter[which(Matrix$Years == Year & Matrix$Method == Method)]
  slope.coef = slope[which(Matrix$Years == Year & Matrix$Method == Method)]
  
  if(Model == "Livingstone")  decay.rate = inter.coef + ar.alpha * slope.coef
  if(Model == "Log") decay.rate = inter.coef + log10(ar.alpha) * slope.coef
  return(decay.rate)
}

#Select the matching Chemistry depths and time
ar.chem <- filter(Chemistry, lake =="ar") %>% filter(depth == 30 | depth == 40 | depth==45 | depth==46 | depth==47 | depth==48)

#In the first visit of 2020, the maximum sampled depth is 48m in the chemistry dataset, but the YSI
#profile stoped at 47m. I ASSUME here that the 48m is in fact 47m
ar.chem.1 <- filter(ar.chem, date == "2020-05-26") 
ar.chem.2 <- filter(ar.chem, date == "2020-08-25")


#Calculate mean CO2 and CH4 concentrations
ar.chem.1.co2 = group_by(ar.chem.1, depth) %>% summarize(CO2 = mean(conc.CO2.corrected))
ar.chem.1.ch4 = group_by(ar.chem.1, depth) %>% summarize(CH4 = mean(conc.CH4.insitu))
ar.chem.2.co2 = group_by(ar.chem.2, depth) %>% summarize(CO2 = mean(conc.CO2.corrected))
ar.chem.2.ch4 = group_by(ar.chem.2, depth) %>% summarize(CH4 = mean(conc.CH4.insitu))

#Remove duplicated lines for other variables
ar.chem.1.single = ar.chem.1[,-c(42:50)]
ar.chem.1.single = unique(ar.chem.1.single)
ar.chem.2.single = ar.chem.2[,-c(42:50)]
ar.chem.2.single = unique(ar.chem.2.single)

#Depth used in 2020
aa.depth = c(30,40,45,46,47)

##################################High temporal resolution, log model####################
O2.consumption.rate <- O2.decay(Coef.mat, "Log", 2020, "High res", ar.alpha)

nday = 150 #Numbers of days to simulate
dbeg = 135 #First day of simulated stratification
ar.2017.YSI.Log = as.data.frame(matrix(nrow=nrow(O2.consumption.rate), ncol = nday))
ar.2017.YSI.Log[,1] = 12.3
for(i in 2:ncol(ar.2017.YSI.Log)){
  ar.2017.YSI.Log[,i] = 12.3 - O2.consumption.rate[,1]*(i-1)
}
ar.2017.YSI.Log[ar.2017.YSI.Log<0] = 0


O2.threshold = 2
ar.2017.YSI.Log.bin <- ifelse(ar.2017.YSI.Log <= O2.threshold, 1, 0)
ar.2017.YSI.Log.bin.cumul <- ar.2017.YSI.Log.bin
for(j in 2:ncol(ar.2017.YSI.Log.bin.cumul)){
  temp = ar.2017.YSI.Log.bin.cumul[,j-1]+ar.2017.YSI.Log.bin.cumul[,j]
  temp = ifelse(temp > ar.2017.YSI.Log.bin.cumul[,j-1], temp, 0)
  ar.2017.YSI.Log.bin.cumul[,j] = temp
}
# ar.2017.YSI.Log.bin.cumul = rbind(matrix(nrow=29,ncol=nday),ar.2017.YSI.Log.bin.cumul)
# ar.2017.YSI.Log.bin.cumul[is.na(ar.2017.YSI.Log.bin.cumul)] = 0
ar.2017.YSI.Log.bin.cumul = cbind(c(30:47), ar.2017.YSI.Log.bin.cumul)
colnames(ar.2017.YSI.Log.bin.cumul)[1] = "Depth"

colnames(ar.2017.YSI.Log.bin.cumul)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

ar.2017.YSI.Log.bin.cumul = as.data.frame(ar.2017.YSI.Log.bin.cumul)

ar.2017.YSI.Log.bin.cumul.long = pivot_longer(ar.2017.YSI.Log.bin.cumul,
                                              cols = 2:ncol(ar.2017.YSI.Log.bin.cumul),
                                              names_to = "DOY", values_to = "HypoxAge")
ar.2017.YSI.Log.bin.cumul.long = as.data.frame(ar.2017.YSI.Log.bin.cumul.long)

ggplot(ar.2017.YSI.Log.bin.cumul.long)+
  geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
  scale_y_reverse()+
  scale_fill_gradient(low="white", high="red")+
  ggtitle("2018")


#Align the modeled oxygen profile with the real data to associate anoxic age with 
#reduced compounds concentration using the lowest sum of squared difference

i=4
Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[i]))
#Remove NA at bottom of profiles with niminum value of the profile
for(j in 1:ncol(Ar.YSI))
{
  if(is.na(Ar.YSI[,j])) Ar.YSI[is.na(Ar.YSI[,j]),j] = min(Ar.YSI[,j], na.rm=T)
}

#Transform to long format
Ar.long = pivot_longer(Ar.YSI, cols = 2:ncol(Ar.YSI), names_to = "Date", values_to = "DO_mgL")
#Split Date into Year, months and day
Ar.long$Date = substring(Ar.long$Date, 2,20) %>% strptime("%Y.%m.%d.%H.%M.%S")

Ar.long$DOY = as.numeric(strftime(Ar.long$Date, format = "%j"))

Ar.long = Ar.long %>% mutate(Year = substring(Date, 1, 4),
                             mm = substring(Date, 6, 7),
                             dd = substring(Date, 9, 10))

#Keep only values below 30 meters to match other methods
Ar.long.deep = Ar.long[Ar.long$Depth_m>=30,]

#Order the dataframe
Ar.long.deep = Ar.long.deep[order(Ar.long.deep$Depth_m ,Ar.long.deep$DOY),]


#Select the date matching chemistry observations
#In 2020, Arendsee was sampled on 2020-05-26 and 2020-08-25	
Ar.first.profile = Ar.long.deep[,-2] %>% filter(mm == "05", dd == "26") %>% 
  group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))

Ar.second.profile = Ar.long.deep[,-2] %>% filter(mm == "08", dd == "25") %>% 
  group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))

Match.temp.1 = ar.2017.YSI.Log
for(i in 1:ncol(ar.2017.YSI.Log)){
  Match.temp.1[,i] = ar.2017.YSI.Log[,i] - Ar.first.profile[,2]
}

Match.temp.1.abs = (Match.temp.1)^2
Match.day.1 = which(colSums(Match.temp.1.abs) == min(colSums(Match.temp.1.abs)))

Match.temp.2 = ar.2017.YSI.Log
for(i in 1:ncol(ar.2017.YSI.Log)){
  Match.temp.2[,i] = ar.2017.YSI.Log[,i] - Ar.second.profile[,2]
}

Match.temp.2.abs = (Match.temp.2)^2
Match.day.2.modelled = which(colSums(Match.temp.2.abs) == min(colSums(Match.temp.2.abs)))
min(colSums(Match.temp.2.abs))

#The lines in comments before are to match modeled data with the second dates
#Here, we use the first profile as the starting date and use the real date of the second profile
#which was 91 days after. Graphically, it doesn't make much difference
#only a translation toward higher anoxic age values
Match.day.2 = Match.day.1+91

#Select the two matching modeled anoxic age profiles
aa.first = ar.2017.YSI.Log.bin.cumul[,Match.day.1+1]
aa.second = ar.2017.YSI.Log.bin.cumul[,Match.day.2+1]



###############################High temporal resolution, Livingstone model#########################
O2.living.highres <- O2.decay(Coef.mat, "Livingstone", 2020, "High res", ar.alpha)

ar.2017.YSI.Living = as.data.frame(matrix(nrow=nrow(O2.living.highres), ncol = nday))
ar.2017.YSI.Living[,1] = 12.3
for(i in 2:ncol(ar.2017.YSI.Living)){
  ar.2017.YSI.Living[,i] = 12.3 - O2.living.highres[,1]*(i-1)
}
ar.2017.YSI.Living[ar.2017.YSI.Living<0] = 0


ar.2017.YSI.Living.bin <- ifelse(ar.2017.YSI.Living <= O2.threshold, 1, 0)
ar.2017.YSI.Living.bin.cumul <- ar.2017.YSI.Living.bin
for(j in 2:ncol(ar.2017.YSI.Living.bin.cumul)){
  temp = ar.2017.YSI.Living.bin.cumul[,j-1]+ar.2017.YSI.Living.bin.cumul[,j]
  temp = ifelse(temp > ar.2017.YSI.Living.bin.cumul[,j-1], temp, 0)
  ar.2017.YSI.Living.bin.cumul[,j] = temp
}
#ar.2017.YSI.Living.bin.cumul = rbind(matrix(nrow=29,ncol=nday),ar.2017.YSI.Living.bin.cumul)
#ar.2017.YSI.Living.bin.cumul[is.na(ar.2017.YSI.Living.bin.cumul)] = 0
ar.2017.YSI.Living.bin.cumul = cbind(c(30:47), ar.2017.YSI.Living.bin.cumul)
colnames(ar.2017.YSI.Living.bin.cumul)[1] = "Depth"

colnames(ar.2017.YSI.Living.bin.cumul)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

ar.2017.YSI.Living.bin.cumul = as.data.frame(ar.2017.YSI.Living.bin.cumul)

ar.2017.YSI.Living.bin.cumul.long = pivot_longer(ar.2017.YSI.Living.bin.cumul,
                                                 cols = 2:ncol(ar.2017.YSI.Living.bin.cumul),
                                                 names_to = "DOY", values_to = "HypoxAge")
ar.2017.YSI.Living.bin.cumul.long = as.data.frame(ar.2017.YSI.Living.bin.cumul.long)

ggplot(ar.2017.YSI.Living.bin.cumul.long)+
  geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
  scale_y_reverse()+
  scale_fill_gradient(low="white", high="red")+
  ggtitle("2018")


#Select the date matching chemistry observations
#In 2020, Arendsee was sampled on 2020-05-26 and 2020-08-25	
Ar.first.profile = Ar.long.deep[,-2] %>% filter(mm == "05", dd == "26") %>% 
  group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))

Ar.second.profile = Ar.long.deep[,-2] %>% filter(mm == "08", dd == "25") %>% 
  group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))

Match.temp.1.Living = ar.2017.YSI.Living
for(i in 1:ncol(ar.2017.YSI.Living)){
  Match.temp.1.Living[,i] = ar.2017.YSI.Living[,i] - Ar.first.profile[,2]
}

Match.temp.1.abs.Living = (Match.temp.1.Living)^2
Match.day.1.Living = which(colSums(Match.temp.1.abs.Living) == min(colSums(Match.temp.1.abs.Living)))
min(colSums(Match.temp.1.abs.Living))


Match.temp.2.Living = ar.2017.YSI.Living
for(i in 1:ncol(ar.2017.YSI.Living)){
  Match.temp.2.Living[,i] = ar.2017.YSI.Living[,i] - Ar.second.profile[,2]
}

Match.temp.2.abs.Living = (Match.temp.2.Living)^2
Match.day.2.Living.modelled = which(colSums(Match.temp.2.abs.Living) == min(colSums(Match.temp.2.abs.Living)))
min(colSums(Match.temp.2.abs.Living))

#The lines in comments before are to match modeled data with the second dates
#Here, we use the first profile as the starting date and use the real date of the second profile
#which was 91 days after. Graphically, it doesn't make much difference
#only a translation toward higher anoxic age values
Match.day.2.Living = Match.day.1.Living+91

#Select the two matching modeled anoxic age profiles
aa.first.Living = ar.2017.YSI.Living.bin.cumul[,Match.day.1.Living+1]
aa.second.Living = ar.2017.YSI.Living.bin.cumul[,Match.day.2.Living+1]




####################################Low temporal resolution, rinko, Log-linear################################

O2.log.LowResRinko <- O2.decay(Coef.mat, "Log", 2020, "Low res rinko", ar.alpha)
ar.low.Log = as.data.frame(matrix(nrow=nrow(O2.log.LowResRinko), ncol = nday))
ar.low.Log[,1] = 12.3
for(i in 2:ncol(ar.low.Log)){
  ar.low.Log[,i] = 12.3 - O2.log.LowResRinko[,1]*(i-1)
}
ar.low.Log[ar.low.Log<0] = 0


ar.low.Log.bin <- ifelse(ar.low.Log <= O2.threshold, 1, 0)
ar.low.Log.bin.cumul <- ar.low.Log.bin
for(j in 2:ncol(ar.low.Log.bin.cumul)){
  temp = ar.low.Log.bin.cumul[,j-1]+ar.low.Log.bin.cumul[,j]
  temp = ifelse(temp > ar.low.Log.bin.cumul[,j-1], temp, 0)
  ar.low.Log.bin.cumul[,j] = temp
}
# ar.low.Log.bin.cumul = rbind(matrix(nrow=29,ncol=nday),ar.low.Log.bin.cumul)
# ar.low.Log.bin.cumul[is.na(ar.low.Log.bin.cumul)] = 0
ar.low.Log.bin.cumul = cbind(c(30:47), ar.low.Log.bin.cumul)
colnames(ar.low.Log.bin.cumul)[1] = "Depth"

colnames(ar.low.Log.bin.cumul)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

ar.low.Log.bin.cumul = as.data.frame(ar.low.Log.bin.cumul)

ar.low.Log.bin.cumul.long = pivot_longer(ar.low.Log.bin.cumul,
                                         cols = 2:ncol(ar.low.Log.bin.cumul),
                                         names_to = "DOY", values_to = "HypoxAge")
ar.low.Log.bin.cumul.long = as.data.frame(ar.low.Log.bin.cumul.long)

ggplot(ar.low.Log.bin.cumul.long)+
  geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
  scale_y_reverse()+
  scale_fill_gradient(low="white", high="red")+
  ggtitle("2018")


#Select the date matching chemistry observations
#In 2020, Arendsee was sampled on 2020-05-26 and 2020-08-25	
Ar.first.profile.low = filter(Ar.long.deep.lowres.a, DOY==124)
Ar.second.profile.low = filter(Ar.long.deep.lowres.a, DOY==179)

Match.temp.1.low = ar.low.Log
for(i in 1:ncol(ar.low.Log)){
  Match.temp.1.low[,i] = ar.low.Log[,i] - Ar.first.profile.low[,3]
}

Match.temp.1.low.abs = (Match.temp.1.low)^2
Match.day.1.low = which(colSums(Match.temp.1.low.abs) == min(colSums(Match.temp.1.low.abs)))

Match.temp.2.low = ar.low.Log
for(i in 1:ncol(ar.low.Log)){
  Match.temp.2.low[,i] = ar.low.Log[,i] - Ar.second.profile.low[,3]
}
Match.temp.2.low.abs = (Match.temp.2.low)^2
Match.day.2.low.modelled = which(colSums(Match.temp.2.low.abs) == min(colSums(Match.temp.2.low.abs)))
min(colSums(Match.temp.2.low.abs))

#The lines in comments before are to match modeled data with the second dates
#Here, we use the first profile as the starting date and use the real date of the second profile
#which was 91 days after. Graphically, it doesn't make much difference
#only a translation toward higher anoxic age values
Match.day.2.low = Match.day.1.low+91

#Select the two matching modeled anoxic age profiles
aa.first.low = ar.low.Log.bin.cumul[,Match.day.1.low+1]
aa.second.low = ar.low.Log.bin.cumul[,Match.day.2.low+1]



################################Low temporal resolution (Rinko) livingstone########################

O2.living.LowResRinko <- O2.decay(Coef.mat, "Livingstone", 2020, "Low res rinko", ar.alpha)

ar.low.Living = as.data.frame(matrix(nrow=nrow(O2.living.LowResRinko), ncol = nday))
ar.low.Living[,1] = 12.3
for(i in 2:ncol(ar.low.Living)){
  ar.low.Living[,i] = 12.3 - O2.living.LowResRinko[,1]*(i-1)
}
ar.low.Living[ar.low.Living<0] = 0

ar.low.Living.bin <- ifelse(ar.low.Living <= O2.threshold, 1, 0)
ar.low.Living.bin.cumul <- ar.low.Living.bin
for(j in 2:ncol(ar.low.Living.bin.cumul)){
  temp = ar.low.Living.bin.cumul[,j-1]+ar.low.Living.bin.cumul[,j]
  temp = ifelse(temp > ar.low.Living.bin.cumul[,j-1], temp, 0)
  ar.low.Living.bin.cumul[,j] = temp
}
# ar.low.Living.bin.cumul = rbind(matrix(nrow=29,ncol=nday),ar.low.Living.bin.cumul)
# ar.low.Living.bin.cumul[is.na(ar.low.Living.bin.cumul)] = 0
ar.low.Living.bin.cumul = cbind(c(30:47), ar.low.Living.bin.cumul)
colnames(ar.low.Living.bin.cumul)[1] = "Depth"

colnames(ar.low.Living.bin.cumul)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

ar.low.Living.bin.cumul = as.data.frame(ar.low.Living.bin.cumul)

ar.low.Living.bin.cumul.long = pivot_longer(ar.low.Living.bin.cumul,
                                            cols = 2:ncol(ar.low.Living.bin.cumul),
                                            names_to = "DOY", values_to = "HypoxAge")
ar.low.Living.bin.cumul.long = as.data.frame(ar.low.Living.bin.cumul.long)

ggplot(ar.low.Living.bin.cumul.long)+
  geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
  scale_y_reverse()+
  scale_fill_gradient(low="white", high="red")+
  ggtitle("2018")

#Select the date matching chemistry observations
#In 2020, Arendsee was sampled on 2020-05-26 and 2020-08-25	
Ar.first.profile.low = filter(Ar.long.deep.lowres.a, DOY==124)
Ar.second.profile.low = filter(Ar.long.deep.lowres.a, DOY==179)

Match.temp.1.low.living = ar.low.Living
for(i in 1:ncol(ar.low.Living)){
  Match.temp.1.low.living[,i] = ar.low.Living[,i] - Ar.first.profile.low[,3]
}

Match.temp.1.low.abs.living = (Match.temp.1.low.living)^2
Match.day.1.low.living = which(colSums(Match.temp.1.low.abs.living) == min(colSums(Match.temp.1.low.abs.living)))

Match.temp.2.low.living = ar.low.Living
for(i in 1:ncol(ar.low.Living)){
  Match.temp.2.low.living[,i] = ar.low.Living[,i] - Ar.second.profile.low[,3]
}
Match.temp.2.low.abs.living = (Match.temp.2.low.living)^2
Match.day.2.low.living.modelled = which(colSums(Match.temp.2.low.abs.living) == min(colSums(Match.temp.2.low.abs.living)))
min(colSums(Match.temp.2.low.abs.living))

#The lines in comments before are to match modeled data with the second dates
#Here, we use the first profile as the starting date and use the real date of the second profile
#which was 91 days after. Graphically, it doesn't make much difference
#only a translation toward higher anoxic age values
Match.day.2.low.living = Match.day.1.low.living+91

#Select the two matching modeled anoxic age profiles
aa.first.low.living = ar.low.Living.bin.cumul[,Match.day.1.low.living+1]
aa.second.low.living = ar.low.Living.bin.cumul[,Match.day.2.low.living+1]




####################################Low temporal resolution, YSI, Log-linear################################

O2.log.LowResYSI <- O2.decay(Coef.mat, "Log", 2020, "Low res", ar.alpha)
ar.low.Log = as.data.frame(matrix(nrow=nrow(O2.log.LowResYSI), ncol = nday))
ar.low.Log[,1] = 12.3
for(i in 2:ncol(ar.low.Log)){
  ar.low.Log[,i] = 12.3 - O2.log.LowResYSI[,1]*(i-1)
}
ar.low.Log[ar.low.Log<0] = 0


ar.low.Log.bin <- ifelse(ar.low.Log <= O2.threshold, 1, 0)
ar.low.Log.bin.cumul <- ar.low.Log.bin
for(j in 2:ncol(ar.low.Log.bin.cumul)){
  temp = ar.low.Log.bin.cumul[,j-1]+ar.low.Log.bin.cumul[,j]
  temp = ifelse(temp > ar.low.Log.bin.cumul[,j-1], temp, 0)
  ar.low.Log.bin.cumul[,j] = temp
}
# ar.low.Log.bin.cumul = rbind(matrix(nrow=29,ncol=nday),ar.low.Log.bin.cumul)
# ar.low.Log.bin.cumul[is.na(ar.low.Log.bin.cumul)] = 0
ar.low.Log.bin.cumul = cbind(c(30:47), ar.low.Log.bin.cumul)
colnames(ar.low.Log.bin.cumul)[1] = "Depth"

colnames(ar.low.Log.bin.cumul)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

ar.low.Log.bin.cumul = as.data.frame(ar.low.Log.bin.cumul)

# ar.low.Log.bin.cumul.long = pivot_longer(ar.low.Log.bin.cumul,
#                                               cols = 2:ncol(ar.low.Log.bin.cumul),
#                                               names_to = "DOY", values_to = "HypoxAge")
# ar.low.Log.bin.cumul.long = as.data.frame(ar.low.Log.bin.cumul.long)
# 
# ggplot(ar.low.Log.bin.cumul.long)+
#   geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
#   scale_y_reverse()+
#    scale_fill_gradient(low="white", high="red")+
#   ggtitle("2018")


#Select the date matching chemistry observations
#In 2020, Arendsee was sampled on 2020-05-26 and 2020-08-25
#Same object as High resolution YSI

Match.temp.1.lowYSI = ar.low.Log
for(i in 1:ncol(ar.low.Log)){
  Match.temp.1.lowYSI[,i] = ar.low.Log[,i] - Ar.first.profile[,2]
}

Match.temp.1.low.absYSI = (Match.temp.1.lowYSI)^2
Match.day.1.lowYSI = which(colSums(Match.temp.1.low.absYSI) == min(colSums(Match.temp.1.low.absYSI)))

Match.temp.2.lowYSI = ar.low.Log
for(i in 1:ncol(ar.low.Log)){
  Match.temp.2.lowYSI[,i] = ar.low.Log[,i] - Ar.second.profile[,2]
}
Match.temp.2.low.absYSI = (Match.temp.2.lowYSI)^2
Match.day.2.lowYSI.modelled = which(colSums(Match.temp.2.low.absYSI) == min(colSums(Match.temp.2.low.absYSI)))
min(colSums(Match.temp.2.low.absYSI))

#The lines in comments before are to match modeled data with the second dates
#Here, we use the first profile as the starting date and use the real date of the second profile
#which was 91 days after. Graphically, it doesn't make much difference
#only a translation toward higher anoxic age values
Match.day.2.lowYSI = Match.day.1.lowYSI+91

#Select the two matching modeled anoxic age profiles
aa.first.lowYSI = ar.low.Log.bin.cumul[,Match.day.1.lowYSI+1]
aa.second.lowYSI = ar.low.Log.bin.cumul[,Match.day.2.lowYSI+1]


####################################Low temporal resolution, YSI, Livingstone################################

O2.Living.LowResYSI <- O2.decay(Coef.mat, "Livingstone", 2020, "Low res", ar.alpha)
ar.low.Living = as.data.frame(matrix(nrow=nrow(O2.Living.LowResYSI), ncol = nday))
ar.low.Living[,1] = 12.3
for(i in 2:ncol(ar.low.Living)){
  ar.low.Living[,i] = 12.3 - O2.Living.LowResYSI[,1]*(i-1)
}
ar.low.Living[ar.low.Living<0] = 0


ar.low.Living.bin <- ifelse(ar.low.Living <= O2.threshold, 1, 0)
ar.low.Living.bin.cumul <- ar.low.Living.bin
for(j in 2:ncol(ar.low.Living.bin.cumul)){
  temp = ar.low.Living.bin.cumul[,j-1]+ar.low.Living.bin.cumul[,j]
  temp = ifelse(temp > ar.low.Living.bin.cumul[,j-1], temp, 0)
  ar.low.Living.bin.cumul[,j] = temp
}
# ar.low.Living.bin.cumul = rbind(matrix(nrow=29,ncol=nday),ar.low.Living.bin.cumul)
# ar.low.Living.bin.cumul[is.na(ar.low.Living.bin.cumul)] = 0
ar.low.Living.bin.cumul = cbind(c(30:47), ar.low.Living.bin.cumul)
colnames(ar.low.Living.bin.cumul)[1] = "Depth"

colnames(ar.low.Living.bin.cumul)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

ar.low.Living.bin.cumul = as.data.frame(ar.low.Living.bin.cumul)

# ar.low.Living.bin.cumul.long = pivot_longer(ar.low.Living.bin.cumul,
#                                               cols = 2:ncol(ar.low.Living.bin.cumul),
#                                               names_to = "DOY", values_to = "HypoxAge")
# ar.low.Living.bin.cumul.long = as.data.frame(ar.low.Living.bin.cumul.long)
# 
# ggplot(ar.low.Living.bin.cumul.long)+
#   geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
#   scale_y_reverse()+
#    scale_fill_gradient(low="white", high="red")+
#   ggtitle("2018")


#Select the date matching chemistry observations
#In 2020, Arendsee was sampled on 2020-05-26 and 2020-08-25
#Same object as High resolution YSI

Match.temp.1.lowYSI.living = ar.low.Living
for(i in 1:ncol(ar.low.Living)){
  Match.temp.1.lowYSI.living[,i] = ar.low.Living[,i] - Ar.first.profile[,2]
}

# Match.temp.1.low.absYSI.living = abs(Match.temp.1.lowYSI.living)
Match.temp.1.low.absYSI.living = (Match.temp.1.lowYSI.living)^2
Match.day.1.lowYSI.living = which(colSums(Match.temp.1.low.absYSI.living) == min(colSums(Match.temp.1.low.absYSI.living)))

Match.temp.2.lowYSI.living = ar.low.Living
for(i in 1:ncol(ar.low.Living)){
  Match.temp.2.lowYSI.living[,i] = ar.low.Living[,i] - Ar.second.profile[,2]
}
# Match.temp.2.low.absYSI.living = abs(Match.temp.2.lowYSI.living)
Match.temp.2.low.absYSI.living = (Match.temp.2.lowYSI.living)^2
Match.day.2.lowYSI.living.modelled = which(colSums(Match.temp.2.low.absYSI.living) == min(colSums(Match.temp.2.low.absYSI.living)))
min(colSums(Match.temp.2.low.absYSI.living))

#The lines in comments before are to match modeled data with the second dates
#Here, we use the first profile as the starting date and use the real date of the second profile
#which was 91 days after. Graphically, it doesn't make much difference
#only a translation toward higher anoxic age values
Match.day.2.lowYSI.living = Match.day.1.lowYSI.living+91

#Select the two matching modeled anoxic age profiles
aa.first.lowYSI.living = ar.low.Living.bin.cumul[,Match.day.1.lowYSI.living+1]
aa.second.lowYSI.living = ar.low.Living.bin.cumul[,Match.day.2.lowYSI.living+1]




####################################Low temporal resolution, best case, log################################

O2.Log.LowResbest <- O2.decay(Coef.mat, "Log", 2020, "Low res best", ar.alpha)
ar.low.best = as.data.frame(matrix(nrow=nrow(O2.Log.LowResbest), ncol = nday))
ar.low.best[,1] = 12.3
for(i in 2:ncol(ar.low.best)){
  ar.low.best[,i] = 12.3 - O2.Log.LowResbest[,1]*(i-1)
}
ar.low.best[ar.low.best<0] = 0


ar.low.best.bin <- ifelse(ar.low.best <= O2.threshold, 1, 0)
ar.low.best.bin.cumul <- ar.low.best.bin
for(j in 2:ncol(ar.low.best.bin.cumul)){
  temp = ar.low.best.bin.cumul[,j-1]+ar.low.best.bin.cumul[,j]
  temp = ifelse(temp > ar.low.best.bin.cumul[,j-1], temp, 0)
  ar.low.best.bin.cumul[,j] = temp
}
# ar.low.best.bin.cumul = rbind(matrix(nrow=29,ncol=nday),ar.low.best.bin.cumul)
# ar.low.best.bin.cumul[is.na(ar.low.best.bin.cumul)] = 0
ar.low.best.bin.cumul = cbind(c(30:47), ar.low.best.bin.cumul)
colnames(ar.low.best.bin.cumul)[1] = "Depth"

colnames(ar.low.best.bin.cumul)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

ar.low.best.bin.cumul = as.data.frame(ar.low.best.bin.cumul)

ar.low.best.bin.cumul.long = pivot_longer(ar.low.best.bin.cumul,
                                          cols = 2:ncol(ar.low.best.bin.cumul),
                                          names_to = "DOY", values_to = "HypoxAge")
ar.low.best.bin.cumul.long = as.data.frame(ar.low.best.bin.cumul.long)

# ggplot(ar.low.best.bin.cumul.long)+
#   geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
#   scale_y_reverse()+
#    scale_fill_gradient(low="white", high="red")+
#   ggtitle("2018")


#Select the date matching chemistry observations
#In 2020, Arendsee was sampled on 2020-05-26 (day 147) and 2020-08-25 (day 238)
#Same object as High resolution YSI

Match.temp.1.lowbest = ar.low.best
for(i in 1:ncol(ar.low.best)){
  Match.temp.1.lowbest[,i] = ar.low.best[,i] - Ar.first.profile[,2]
}

# Match.temp.1.low.absYSI.living = abs(Match.temp.1.lowYSI.living)
Match.temp.1.lowbest.abs = (Match.temp.1.lowbest)^2
Match.day.1.lowbest = which(colSums(Match.temp.1.lowbest.abs) == min(colSums(Match.temp.1.lowbest.abs)))

Match.temp.2.lowbest = ar.low.best
for(i in 1:ncol(ar.low.best)){
  Match.temp.2.lowbest[,i] = ar.low.best[,i] - Ar.second.profile[,2]
}

Match.temp.2.lowbest.abs = (Match.temp.2.lowbest)^2
Match.day.2.lowbest.modelled = which(colSums(Match.temp.2.lowbest.abs) == min(colSums(Match.temp.2.lowbest.abs)))
min(colSums(Match.temp.2.lowbest.abs))

#The lines in comments before are to match modeled data with the second dates
#Here, we use the first profile as the starting date and use the real date of the second profile
#which was 91 days after. Graphically, it doesn't make much difference
#only a translation toward higher anoxic age values
Match.day.2.lowbest = Match.day.1.lowbest+91

#Select the two matching modeled anoxic age profiles
aa.first.lowbest = ar.low.best.bin.cumul[,Match.day.1.lowbest+1]
aa.second.lowbest = ar.low.best.bin.cumul[,Match.day.2.lowbest+1]



####################################Low temporal resolution, best case, log################################

O2.Living.LowResbest <- O2.decay(Coef.mat, "Livingstone", 2020, "Low res best", ar.alpha)
ar.low.best.living = as.data.frame(matrix(nrow=nrow(O2.Living.LowResbest), ncol = nday))
ar.low.best.living[,1] = 12.3
for(i in 2:ncol(ar.low.best.living)){
  ar.low.best.living[,i] = 12.3 - O2.Living.LowResbest[,1]*(i-1)
}
ar.low.best.living[ar.low.best.living<0] = 0


ar.low.best.bin.living <- ifelse(ar.low.best.living <= O2.threshold, 1, 0)
ar.low.best.bin.living.cumul <- ar.low.best.bin.living
for(j in 2:ncol(ar.low.best.bin.living.cumul)){
  temp = ar.low.best.bin.living.cumul[,j-1]+ar.low.best.bin.living.cumul[,j]
  temp = ifelse(temp > ar.low.best.bin.living.cumul[,j-1], temp, 0)
  ar.low.best.bin.living.cumul[,j] = temp
}
# ar.low.best.bin.cumul = rbind(matrix(nrow=29,ncol=nday),ar.low.best.bin.cumul)
# ar.low.best.bin.cumul[is.na(ar.low.best.bin.cumul)] = 0
ar.low.best.bin.living.cumul = cbind(c(30:47), ar.low.best.bin.living.cumul)
colnames(ar.low.best.bin.living.cumul)[1] = "Depth"

colnames(ar.low.best.bin.living.cumul)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

ar.low.best.bin.living.cumul = as.data.frame(ar.low.best.bin.living.cumul)

ar.low.best.bin.living.cumul.long = pivot_longer(ar.low.best.bin.living.cumul,
                                                 cols = 2:ncol(ar.low.best.bin.living.cumul),
                                                 names_to = "DOY", values_to = "HypoxAge")
ar.low.best.bin.cumul.living.long = as.data.frame(ar.low.best.bin.living.cumul.long)

# ggplot(ar.low.best.bin.living.cumul.long)+
#   geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
#   scale_y_reverse()+
#    scale_fill_gradient(low="white", high="red")+
#   ggtitle("2018")


#Select the date matching chemistry observations
#In 2020, Arendsee was sampled on 2020-05-26 (day 147) and 2020-08-25 (day 238)
#Same object as High resolution YSI

Match.temp.1.lowbest.living = ar.low.best.living
for(i in 1:ncol(ar.low.best.living)){
  Match.temp.1.lowbest.living[,i] = ar.low.best.living[,i] - Ar.first.profile[,2]
}

# Match.temp.1.low.absYSI.living = abs(Match.temp.1.lowYSI.living)
Match.temp.1.lowbest.abs.living = (Match.temp.1.lowbest.living)^2
Match.day.1.lowbest.living = which(colSums(Match.temp.1.lowbest.abs.living) == min(colSums(Match.temp.1.lowbest.abs.living)))

Match.temp.2.lowbest.living = ar.low.best.living
for(i in 1:ncol(ar.low.best.living)){
  Match.temp.2.lowbest.living[,i] = ar.low.best.living[,i] - Ar.second.profile[,2]
}

Match.temp.2.lowbest.abs.living = (Match.temp.2.lowbest.living)^2
Match.day.2.lowbest.living.modelled = which(colSums(Match.temp.2.lowbest.abs.living) == min(colSums(Match.temp.2.lowbest.abs.living)))
min(colSums(Match.temp.2.lowbest.abs.living))

#The lines in comments before are to match modeled data with the second dates
#Here, we use the first profile as the starting date and use the real date of the second profile
#which was 91 days after. Graphically, it doesn't make much difference
#only a translation toward higher anoxic age values
Match.day.2.lowbest.living = Match.day.1.lowbest.living+91

#Select the two matching modeled anoxic age profiles
aa.first.lowbest.living = ar.low.best.bin.living.cumul[,Match.day.1.lowbest.living+1]
aa.second.lowbest.living = ar.low.best.bin.living.cumul[,Match.day.2.lowbest.living+1]





aa.matrix.1 = cbind(aa.depth, 
                    aa.highres.log=aa.first[c(1,11,16,17,18)], 
                    aa.highres.living = aa.first.Living[c(1,11,16,17,18)], 
                    aa.lowres.rinko.log = aa.first.low[c(1,11,16,17,18)], 
                    aa.lowres.rinko.living = aa.first.low.living[c(1,11,16,17,18)], 
                    aa.lowres.ysi.log = aa.first.lowYSI[c(1,11,16,17,18)], 
                    aa.lowres.ysi.living = aa.first.lowYSI.living[c(1,11,16,17,18)], 
                    aa.lowres.best.log = aa.first.lowbest[c(1,11,16,17,18)], 
                    aa.lowres.best.living = aa.first.lowbest.living[c(1,11,16,17,18)], 
                    CO2=ar.chem.1.co2[,2], CH4=ar.chem.1.ch4[,2],
                    SRP=ar.chem.1.single$SRP, NH4=ar.chem.1.single$NH4_fia, DOC=ar.chem.1.single$DOC,
                    SO4=ar.chem.1.single$SO4, Fe=ar.chem.1.single$Fe_d, S = ar.chem.1.single$S_d,
                    P = ar.chem.2.single$P_d, Ca = ar.chem.1.single$Ca_d)
aa.matrix.2 = cbind(aa.depth, 
                    aa.highres.log=aa.second[c(1,11,16,17,18)], 
                    aa.highres.living = aa.second.Living[c(1,11,16,17,18)], 
                    aa.lowres.rinko.log = aa.second.low[c(1,11,16,17,18)],  
                    aa.lowres.rinko.living = aa.second.low.living[c(1,11,16,17,18)],  
                    aa.lowres.ysi.log = aa.second.lowYSI[c(1,11,16,17,18)], 
                    aa.lowres.ysi.living = aa.second.lowYSI.living[c(1,11,16,17,18)],  
                    aa.lowres.best.log = aa.second.lowbest[c(1,11,16,17,18)],  
                    aa.lowres.best.living = aa.second.lowbest.living[c(1,11,16,17,18)],
                    CO2=ar.chem.2.co2[,2], CH4=ar.chem.2.ch4[,2],
                    SRP=ar.chem.2.single$SRP, NH4=ar.chem.2.single$NH4_fia, DOC=ar.chem.2.single$DOC,
                    SO4=ar.chem.2.single$SO4, Fe=ar.chem.2.single$Fe_d, S = ar.chem.2.single$S_d,
                    P = ar.chem.2.single$P_d, Ca = ar.chem.2.single$Ca_d)

aa.matrix = rbind(aa.matrix.1, aa.matrix.2)
aa.delta.matrix = aa.matrix.2
aa.delta.matrix[,c(10:17)] = aa.matrix.2[,c(10:17)] - aa.matrix.1[,c(10:17)]

#Plot some relationships and hope for the best
png("./Output/AnoxicAge-Relationships.png", res=450, units="in", height = 4.5, width = 8)
par(mfrow=c(2,3))
par(mar=c(4,5,1,1)+.1)
plot(CO2~aa.highres.log, data=aa.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", col = "black",
     ylab = expression(CO[2]~(mu*mol~L^-1)),
     cex.lab = 1.3, cex.axis=1.3)

plot(CH4~aa.highres.log, data=aa.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", col = "black",
     ylab = expression(CH[4]~(mu*mol~L^-1)),
     cex.lab = 1.3, cex.axis=1.3)
plot(SRP~aa.highres.log, data=aa.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", col = "black",
     cex.lab = 1.3, cex.axis=1.3)
plot(NH4~aa.highres.log, data=aa.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", col = "black",
     cex.lab = 1.3, cex.axis=1.3)
plot(S~aa.highres.log, data=aa.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", col = "black",
     cex.lab = 1.3, cex.axis=1.3)
plot(Fe~aa.highres.log, data=aa.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", col = "black",
     cex.lab = 1.3, cex.axis=1.3)
dev.off()
#Put in comments as graphically, they are practically the same
# points(CO2~aa.highres.living, data=aa.matrix, pch = 15, las = 1,
#        xlim = c(0,90), xlab = "Anoxic age (d)", col = "#73BAE6")
# points(CO2~aa.lowres.rinko.log, data=aa.matrix, pch = 16, las = 1,
#      xlim = c(0,90), xlab = "Anoxic age (d)", col = "#C7144C")
# points(CO2~aa.lowres.rinko.living, data=aa.matrix, pch = 15, las = 1,
#        xlim = c(0,90), xlab = "Anoxic age (d)", col = "#C7144C")
# points(CO2~aa.lowres.ysi.log, data=aa.matrix, pch = 16, las = 1,
#      xlim = c(0,90), xlab = "Anoxic age (d)", col = "#33a02c")
# points(CO2~aa.lowres.ysi.living, data=aa.matrix, pch = 15, las = 1,
#        xlim = c(0,90), xlab = "Anoxic age (d)", col = "#33a02c")
# points(CO2~aa.lowres.best.log, data=aa.matrix, pch = 16, las = 1,
#      xlim = c(0,90), xlab = "Anoxic age (d)", col = "#FFD700")
# points(CO2~aa.lowres.best.living, data=aa.matrix, pch = 15, las = 1,
#        xlim = c(0,90), xlab = "Anoxic age (d)", col = "#FFD700")


par(mfrow=c(2,3))
plot(CO2~aa.highres.log, data=aa.delta.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", ylab = "Delta CO2", col = "black")
plot(CH4~aa.highres.log, data=aa.delta.matrix, 
     xlab = "Anoxic age (d)", ylab = "Delta CH4", pch = 16, las = 1)
plot(SRP~aa.highres.log, data=aa.delta.matrix,
     xlab = "Anoxic age (d)", ylab = "Delta SRP", pch = 16, las = 1)
plot(NH4~aa.highres.log, data=aa.delta.matrix,
     xlab = "Anoxic age (d)", ylab = "Delta NH4", pch = 16, las = 1)
plot(S~aa.highres.log, data=aa.delta.matrix, 
     xlab = "Anoxic age (d)", ylab = "Delta S", pch = 16, las = 1)
plot(Fe~aa.highres.log, data=aa.delta.matrix,
     xlab = "Anoxic age (d)", ylab = "Delta Fe", pch = 16, las = 1)


png("./Delta-Relationship.png")
par(mfrow=c(2,3))
plot(CO2~aa.highres.log, data=aa.delta.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", ylab = "Delta CO2", col = "black")
plot(CH4~aa.highres.log, data=aa.delta.matrix, 
     xlab = "Anoxic age (d)", ylab = "Delta CH4", pch = 16, las = 1)
plot(P~aa.highres.log, data=aa.delta.matrix,
     xlab = "Anoxic age (d)", ylab = "Delta P", pch = 16, las = 1)
plot(Ca~aa.highres.log, data=aa.delta.matrix,
     xlab = "Anoxic age (d)", ylab = "Delta Ca", pch = 16, las = 1)
plot(S~aa.highres.log, data=aa.delta.matrix, 
     xlab = "Anoxic age (d)", ylab = "Delta S", pch = 16, las = 1)
plot(Fe~aa.highres.log, data=aa.delta.matrix,
     xlab = "Anoxic age (d)", ylab = "Delta Fe", pch = 16, las = 1)
dev.off()



#####
#Get the coefficients for Jv and Ja for each years
Ar.Jv.2017 = Ar.Living.2017$coefficients[1,1]
Ar.Ja.2017 = Ar.Living.2017$coefficients[2,1]

Ar.Jv.2018 = Ar.Living.2018$coefficients[1,1]
Ar.Ja.2018 = Ar.Living.2018$coefficients[2,1]

Ar.Jv.2019 = Ar.Living.2019$coefficients[1,1]
Ar.Ja.2019 = Ar.Living.2019$coefficients[2,1]

#Recreate profile below 20 meters
#Select alpha
Ar.deep.alpha <- filter(Bats, lake =="ar") %>% slice(c(20:47)) %>% select(alpha)
#Calculate the daily sink
Ar.DODailySink.2017 <- Ar.Jv.2017 + Ar.Ja.2017*Ar.deep.alpha
Ar.DODailySink.2018 <- Ar.Jv.2018 + Ar.Ja.2018*Ar.deep.alpha
Ar.DODailySink.2019 <- Ar.Jv.2019 + Ar.Ja.2019*Ar.deep.alpha

#Model after 80 days
Ar.mod.2017.80d = 12-Ar.DODailySink.2017*80
Ar.mod.2018.80d = 12-Ar.DODailySink.2018*80
Ar.mod.2019.80d = 12-Ar.DODailySink.2019*80

#Add values for top of lake
Ar.mod.2017.80d = c(rep(12,19),Ar.mod.2017.80d[,1])
Ar.mod.2018.80d = c(rep(12,19),Ar.mod.2018.80d[,1])
Ar.mod.2019.80d = c(rep(12,19),Ar.mod.2019.80d[,1])

#Change negative values to 0
Ar.mod.2017.80d[Ar.mod.2017.80d<0] = 0
Ar.mod.2018.80d[Ar.mod.2018.80d<0] = 0
Ar.mod.2019.80d[Ar.mod.2019.80d<0] = 0


#Model after 120 days
Ar.mod.2017.120d = 12-Ar.DODailySink.2017*120
Ar.mod.2018.120d = 12-Ar.DODailySink.2018*120
Ar.mod.2019.120d = 12-Ar.DODailySink.2019*120

#Add values for top of lake
Ar.mod.2017.120d = c(rep(9,19),Ar.mod.2017.120d[,1])
Ar.mod.2018.120d = c(rep(9,19),Ar.mod.2018.120d[,1])
Ar.mod.2019.120d = c(rep(9,19),Ar.mod.2019.120d[,1])

#Change negative values to 0
Ar.mod.2017.120d[Ar.mod.2017.120d<0] = 0
Ar.mod.2018.120d[Ar.mod.2018.120d<0] = 0
Ar.mod.2019.120d[Ar.mod.2019.120d<0] = 0

plot(seq(1,47,1)~ Ar.mod.2017.80d, ylim=c(47,1), xlim = c(0,14), pch = 16, col = "blue")
points(seq(1,47,1)~ Ar.mod.2017.120d, pch = 16, col = "red")




#Test to calculate anoxic age based only on profiles
output=list()

for(k in 1:length(YSI.yrs)){
  #Read the file
  Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[k]))
  
  #Remove NA at bottom of profiles with niminum value of the profile
  for(j in 1:ncol(Ar.YSI))
  {
    if(is.na(Ar.YSI[,j])) Ar.YSI[is.na(Ar.YSI[,j]),j] = min(Ar.YSI[,j], na.rm=T)
  }
  
  #Transform to long format
  Ar.long = pivot_longer(Ar.YSI, cols = 2:ncol(Ar.YSI), names_to = "Date", values_to = "DO_mgL")
  #Split Date into Year, months and day
  Ar.long$Date = substring(Ar.long$Date, 2,20) %>% strptime("%Y.%m.%d.%H.%M.%S")
  
  Ar.long$DOY = as.numeric(strftime(Ar.long$Date, format = "%j"))
  
  Ar.long = Ar.long %>% mutate(Year = substring(Date, 1, 4),
                               mm = substring(Date, 6, 7),
                               dd = substring(Date, 9, 10))
  
  #Keep only values below 30 meters to match other methods
  Ar.long.deep = Ar.long[Ar.long$Depth_m>=30,]
  
  #Order the dataframe
  Ar.long.deep = Ar.long.deep[order(Ar.long.deep$Depth_m ,Ar.long.deep$DOY),]
  
  temp.output = matrix(nrow = length(unique(Ar.long.deep$Depth_m)), ncol = length(unique(Ar.long.deep$DOY)))
  colnames(temp.output) = unique(Ar.long.deep$DOY)
  rownames(temp.output) = unique(Ar.long.deep$Depth_m)
  
  for(i in 1:length(unique(Ar.long.deep$Depth_m))){
    temp = vector(length = dim(Ar.long.deep[Ar.long.deep$Depth_m == unique(Ar.long.deep$Depth_m)[i],"DOY"])[1])
    temp = Ar.long.deep[Ar.long.deep$Depth_m == unique(Ar.long.deep$Depth_m)[i],] %>% group_by(DOY) %>% summarise(DO = mean(DO_mgL, na.rm=T))
    temp.output[i,] = t(as.vector(temp[,2]))
  }
  output[[k]] = temp.output
}


#Change to 1 or 0 if hypoxic or not
YSIprofil.bin = list()
YSIprofil.bin.cumul = list()
for(i in 1:4)
{
  data.temp = output[[i]]
  YSIprofil.bin[[i]] <- ifelse(data.temp <= O2.threshold, 1, 0)
  
  YSIprofil.bin.cumul.temp = YSIprofil.bin[[i]] 
  
  
  for(j in 2:ncol(YSIprofil.bin.cumul.temp)){
    temp = YSIprofil.bin.cumul.temp[,j-1]+YSIprofil.bin.cumul.temp[,j]
    temp = ifelse(temp > YSIprofil.bin.cumul.temp[,j-1], temp, 0)
    YSIprofil.bin.cumul.temp[,j] = temp
  }
  YSIprofil.bin.cumul[[i]] = YSIprofil.bin.cumul.temp
}

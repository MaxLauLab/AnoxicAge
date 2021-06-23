library(RColorBrewer)
library(plot.matrix)
library(dplyr)
library(segmented)
Interpolation <- function(V1, V2, n){
  output = matrix(nrow = length(V1), ncol=n)
  for(i in 1:n){
    output[,i] = V1 + (V2-V1)/n * i
  }
  return(output)
}
setwd("D:/Postdoc/Allemagne/Github/AnoxicAge")
Rinko <- read.csv("./Data/Raw/rinkos.csv", row.names=1)

# Ar = as.data.frame(matrix(nrow = 481, ncol = 4))
# strftime(Rinko$date.r[1], format = "%j") #147
# strftime(Rinko$date.r[1411], format = "%j") #238
# colnames(Ar) = c("Depth", "Temp", "DO_147", "DO_238")
# 
# Ar$Depth = Rinko[which(Rinko$month == 5 & Rinko$lake == "ar"), "depth"]
# Ar$Temp = Rinko[which(Rinko$month == 5 & Rinko$lake == "ar"), "T_C"]
# Ar$DO_147 = Rinko[which(Rinko$month == 5 & Rinko$lake == "ar"), "DO_mgL"]
# Ar$DO_238 = c(Rinko[which(Rinko$month == 8 & Rinko$lake == "ar"), "DO_mgL"],0,0,0,0) #Complete the series with anoxia, same as above depths
# 
# Ar[Ar<0] = 0
# 
# #Create the 90 days in between the two profiles date (238-147-1)
# Inter.mat <- matrix(nrow = nrow(Ar), ncol = 90)
# Interpolation <- function(V1, V2, n){
#   output = matrix(nrow = length(V1), ncol=n)
#   for(i in 1:n){
#     output[,i] = V1 + (V2-V1)/n * i
#   }
#   return(output)
# }
# Inter.mat = Interpolation(Ar$DO_147, Ar$DO_238, 90)
# 
# Ar.DO <- cbind(Ar$DO_147, Inter.mat, Ar$DO_238)
# Ar.anox <- ifelse(Ar.DO <= 2, 1, 0)
# Ar.anox.a <- Ar.anox
# for(j in 2:ncol(Ar.anox.a)){
#   temp = Ar.anox.a[,j-1]+Ar.anox.a[,j]
#   temp = ifelse(temp > Ar.anox.a[,j-1], temp, 0)
#   Ar.anox.a[,j] = temp
# }
# 
# library(RColorBrewer)
# mypalette = c("white", brewer.pal(9, "YlOrBr"))
# if(log(max(Ar.anox.a),base = 1.5) > length(mypalette)) mypalette = c(mypalette, rep("#662506",round(log(max(Ar.anox.a),base=1.5))-length(mypalette)) )
# 
# plot(log(Ar.anox.a, base = 1.5),
#      col = mypalette,
#      border= NA,
#      key = NULL,
#      main = "Hypoxic days in Arendsee",
#      axis.col = NULL,
#      axis.row = NULL,
#      ylab = "Depth (m)",
#      xlab = "Time (day of year)")
# axis(side = 2, tick = T,at = c(1,240,481), labels = c(48.5, 24.5, 0.5), las=1)
# axis(side = 1, tick = T,at = c(1,46,92), labels = c(147, 193, 238),line = 1)
# 
# 
# #Lake Sc
# 
# 
# 
# #Create the 90 days in between the two profiles date (246-149-1)
# 
# 
# 
# Sc.DO <- cbind(Sc$DO_ini, Inter.mat, Sc$DO_fin)
# Sc.anox <- ifelse(Sc.DO <= 2, 1, 0)
# Sc.anox.a <- Sc.anox
# for(j in 2:ncol(Sc.anox.a)){
#   temp = Sc.anox.a[,j-1]+Sc.anox.a[,j]
#   temp = ifelse(temp > Sc.anox.a[,j-1], temp, 0)
#   Sc.anox.a[,j] = temp
# }
# 
# 
# mypalette = c("white", brewer.pal(9, "YlOrBr"))
# if(log(max(Sc.anox.a),base = 1.5) > length(mypalette)) mypalette = c(mypalette, rep("#662506",round(log(max(Sc.anox.a),base=1.5))-length(mypalette)) )
# 
# plot(log(Sc.anox.a, base = 1.5),
#      col = mypalette,
#      border= NA,
#      key = NULL,
#      main = "Hypoxic days in Sc",
#      axis.col = NULL,
#      axis.row = NULL,
#      ylab = "Depth (m)",
#      xlab = "Time (day of year)")
# axis(side = 2, tick = T,at = c(1,240,481), labels = c(48.5, 24.5, 0.5), las=1)
# axis(side = 1, tick = T,at = c(1,46,92), labels = c(147, 193, 238),line = 1)

#List of lakes
List.lakes = unique(Rinko$lake)
InterpolatedMatrix = list()
output = list()
output.anox.a = list()
DoY = list()
#Create the interpolation for all lakes
for(i in 1:length(List.lakes)){
  #Find first and last day of sampling
  T.ini = as.numeric(strftime(Rinko$date.r[min(which(Rinko$lake == List.lakes[i] & Rinko$month == min(Rinko[Rinko$lake == List.lakes[i],"month"])))], format = "%j")) #149
  T.end = as.numeric(strftime(Rinko$date.r[min(which(Rinko$lake == List.lakes[i] & Rinko$month == max(Rinko[Rinko$lake == List.lakes[i],"month"])))], format = "%j")) #149
  #Find the month with the largest amount of data
  Month.ini = min(Rinko[Rinko$lake == List.lakes[i],"month"])
  Month.end = max(Rinko[Rinko$lake == List.lakes[i],"month"])
  
  Length.ini = length(Rinko[which(Rinko$month == Month.ini & Rinko$lake == List.lakes[i]), "depth"])
  Length.end = length(Rinko[which(Rinko$month == Month.end & Rinko$lake == List.lakes[i]), "depth"])
  
  ShortestMonth = ifelse(Length.ini <= Length.end, Month.ini, Month.end)
  LongestMonth = ifelse(Length.ini > Length.end, Month.ini, Month.end)
  
  #Keep the date to relate to other dataframes
  First.profile = Rinko[Rinko$lake == List.lakes[i],"date.r"][1]
  Last.profile = tail(Rinko[Rinko$lake == List.lakes[i],"date.r"], n=1)
  
  #Difference between the 2 sampling day
  DiffLength = max(Length.end,Length.ini) - min(Length.end,Length.ini)
  DoY[[i]] = list(c(T.ini, T.end, T.end-T.ini, max(Length.end,Length.ini)), First.profile, Last.profile, List.lakes[i])

  #Create the InterpolatedMatrix df
  InterpolatedMatrix[[i]] = as.data.frame(matrix(nrow = max(Length.ini, Length.end), ncol = 5))
  colnames(InterpolatedMatrix[[i]]) = c("Depth", "Temp", "DO_ini", "DO_end", "Jz")
  
  InterpolatedMatrix[[i]]$Depth = Rinko[which(Rinko$month == LongestMonth & Rinko$lake == List.lakes[i]), "depth"]
  InterpolatedMatrix[[i]]$Temp = Rinko[which(Rinko$month == LongestMonth & Rinko$lake == List.lakes[i]), "T_C"]
  DO_ini = Rinko[which(Rinko$month == Month.ini & Rinko$lake == List.lakes[i]), "DO_mgL"]
  DO_end = Rinko[which(Rinko$month == Month.end & Rinko$lake == List.lakes[i]), "DO_mgL"]
  #Complete de DO measurements with minimum value (bottom of lake)
  if(length(DO_ini)<length(DO_end)) DO_ini = c(DO_ini,rep(min(DO_ini),DiffLength))
  if(length(DO_ini)>length(DO_end)) DO_end = c(DO_end,rep(min(DO_end),DiffLength))
  InterpolatedMatrix[[i]]$DO_ini = DO_ini
  InterpolatedMatrix[[i]]$DO_end = DO_end
  InterpolatedMatrix[[i]]$Jz = (InterpolatedMatrix[[i]]$DO_ini - InterpolatedMatrix[[i]]$DO_end)/(T.end-T.ini)
  
  
  
  #Create interpolation matrix
  Inter.mat <- matrix(nrow = nrow(InterpolatedMatrix[[i]]), ncol = T.end - T.ini - 1)
  Inter.mat = Interpolation(InterpolatedMatrix[[i]]$DO_ini, InterpolatedMatrix[[i]]$DO_end, T.end - T.ini - 1)
  
  output[[i]] <- cbind(InterpolatedMatrix[[i]]$DO_ini, Inter.mat, InterpolatedMatrix[[i]]$DO_end)
    #Transform the interpolation in days of hypoxia/anoxia
  output.anox <- ifelse(output[[i]] <= 2, 1, 0)
  output.anox.a[[i]] <- output.anox
  for(j in 2:ncol(output.anox.a[[i]])){
    temp = output.anox.a[[i]][,j-1]+output.anox.a[[i]][,j]
    temp = ifelse(temp > output.anox.a[[i]][,j-1], temp, 0)
    output.anox.a[[i]][,j] = temp
  }
  
}
names(InterpolatedMatrix) <- names(DoY) <- List.lakes

#Concatenate data at 1m interval
Output.Jz = list()
for(i in 1:length(InterpolatedMatrix)){
  Temp = InterpolatedMatrix[[i]]
  Temp = Temp[Temp$DO_ini>4,]
  Jz.temp = as.data.frame(matrix(nrow= ceiling(max(Temp$Depth)), ncol=3))
  colnames(Jz.temp) = c("depth_1m", "Jz", "lake")
  Jz.temp$lake = names(InterpolatedMatrix)[i]
  for(j in 0:(ceiling(max(Temp$Depth))-1)){
    Jz.temp$depth_1m[j+1] = j
    Jz.temp$Jz[j+1] = mean(Temp$Jz[Temp$Depth>=j & Temp$Depth < (j+1)])
  }
  Output.Jz[[i]] = Jz.temp
}
Output.Jz = do.call(rbind, Output.Jz)
Output.Jz$ID = paste(Output.Jz$lake, Output.Jz$depth_1m, sep = "-")

Chemistry <- read.csv("./Data/Raw/chemie_base.csv")
Bats <- read.csv("./Data/Raw/bats.csv")


#Calculate Jz for ar, hs and sc using the loggers
#####
#
Full_data <- read.csv("../full_database.csv")
logger.meta <- read.csv("./Data/loggermeta.csv")
ar.depths = c(30,35,40,45,47)
ar.year = c(2017,2018,2019)
Ar.full <- filter(.data = Full_data, lake == "ar")

pdf("./Ar.loggers.Temp.pdf", width=8, height=5)
par(mfrow=c(1,3))
for(i in 1:length(ar.depths)){
  Ar.Temp <- filter(Ar.full, depth == ar.depths[i]) %>% 
    filter(parameter == "T_C") %>% 
    mutate(year = substring(CET, 1, 4)) %>% 
    mutate(mm_dd = substring(CET,6,10)) %>%
    mutate(mm = substring(mm_dd,1,2))
  
  for(j in 1:length(ar.year)){
    Ar.Temp.yr <- filter(Ar.Temp, year == ar.year[j])
    Ar.Temp.yr <- Ar.Temp.yr %>% mutate(Dec.Day = seq(1,length(year), 1)/24)
    Ar.Temp.yr$Season <- ifelse(as.numeric(Ar.Temp.yr$mm) <= 5, "Spring", ifelse(as.numeric(Ar.Temp.yr$mm) >= 9, "Autumn", "Summer"))
    
    plot(Ar.Temp.yr[,"value"]~ Ar.Temp.yr[,"Dec.Day"],
         xlab = "Time",
         ylab = "Temperature (°C)",
         main = paste0(ar.year[j]," ", ar.depths[i], "m"))
    
    legend("topright", legend = Ar.Temp.yr$mm_dd[1])
    for(k in 2:length(unique(Ar.Temp.yr$mm))){
      abline(v = Ar.Temp.yr[which(Ar.Temp.yr$mm == unique(Ar.Temp.yr$mm)[k])[1],"Dec.Day"], lty=2)
      }
    
    }
}
dev.off()


pdf("./Ar.loggers.DO.pdf", width=8, height=5)
par(mfrow=c(1,3))
for(i in 1:length(ar.depths)){
  Ar.DO <- filter(Ar.full, depth == ar.depths[i]) %>% 
    filter(parameter == "DO_mgL") %>% 
    mutate(year = substring(CET, 1, 4)) %>% 
    mutate(mm_dd = substring(CET,6,10)) %>%
    mutate(mm = substring(mm_dd,1,2))
  
  for(j in 1:length(ar.year)){
    Ar.DO.yr <- filter(Ar.DO, year == ar.year[j]) %>% mutate(Dec.Day = seq(1,length(year), 1)/24)
    Ar.DO.yr$Season <- ifelse(as.numeric(Ar.DO.yr$mm) <= 5, "Spring", ifelse(as.numeric(Ar.DO.yr$mm) >= 9, "Autumn", "Summer"))
    
    plot(Ar.DO.yr[,"value"]~ Ar.DO.yr[,"Dec.Day"],
         xlab = "Time",
         ylab = "DO (mg/L)",
         main = paste0(ar.year[j]," ", ar.depths[i], "m"))

    legend("topright", legend = Ar.DO.yr$mm_dd[1])
    for(k in 2:length(unique(Ar.DO.yr$mm))){
      abline(v = Ar.DO.yr[which(Ar.DO.yr$mm == unique(Ar.DO.yr$mm)[k])[1],"Dec.Day"], lty=2)
    }
  }
}
dev.off()


Jz.mat = as.data.frame(matrix(nrow=length(ar.depths), ncol = length(ar.year)))
colnames(Jz.mat) = ar.year
rownames(Jz.mat) = ar.depths

Ar.full <- filter(.data = Full_data, lake == "ar")
ar.from <- matrix(c(2000,1919,2000,
                    2000,707,2000,
                    2000,2000,2000,
                    2000,1000,2000,
                    2000,2000,2000), nrow=5, ncol=3, byrow = T)
ar.to <- matrix(c(7500,7500,7844,
                  7200,6000,7000,
                  6000,6000,5800,
                  5000,3500,5200,
                  4500,4500,5000), nrow=5, ncol=3, byrow = T)
Ar.seg.dates = matrix(nrow=15,ncol=4)
colnames(Ar.seg.dates) = c("First date", "First DO", "Second date", "Second DO")
rownames(Ar.seg.dates) = as.vector(outer(ar.year, ar.depths, paste, sep=" "))
rownames(Ar.seg.dates) = paste0(rownames(Ar.seg.dates), "m")

Jz.seg.list = list()
Jz.seg.coef = list()
counter=1
pdf("./Ar.loggers.pdf", width=8, height=5)
par(mfrow=c(1,3))
for(i in 1:length(ar.depths)){
  Ar.DO <- filter(Ar.full, depth == ar.depths[i]) %>% 
    filter(parameter == "DO_mgL") %>% 
    mutate(year = substring(CET, 1, 4)) %>% 
    mutate(mm_dd = substring(CET,6,10)) %>%
    mutate(mm = substring(mm_dd,1,2))
  
  for(j in 1:length(ar.year)){
    Ar.DO.yr <- filter(Ar.DO, year == ar.year[j])
    Ar.DO.yr <- Ar.DO.yr[c(ar.from[i,j]:ar.to[i,j]),] %>% mutate(Dec.Day = seq(1,length(year), 1)/24)
    Ar.DO.yr$Season <- ifelse(as.numeric(Ar.DO.yr$mm) <= 5, "Spring", ifelse(as.numeric(Ar.DO.yr$mm) >= 9, "Autumn", "Summer"))
    
    plot(Ar.DO.yr[,"value"]~ Ar.DO.yr[,"Dec.Day"],
         xlab = "Time",
         ylab = "DO (mg/L)",
         main = paste0(ar.year[j]," ", ar.depths[i], "m"))
    
    
    #Try piecewise regressions
    #Piecewise regression (find the inflection point)
    #Linear model
    Dec.Day = Ar.DO.yr$Dec.Day
    value = Ar.DO.yr$value
    lin.mod = lm(value ~ Dec.Day)
    
    #Piecewise model
    if(i <=3) psi = c(30,100) #else psi = c(30)
    segmented.mod <- segmented(lin.mod, psi=psi)
    
    plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "blue", lwd=2)
    #Compare both regressions with AIC
    Delta.AIC = AIC(lin.mod) - AIC(segmented.mod) #3.1
    print(rownames(Ar.seg.dates)[counter])
    print(Delta.AIC)
    
    Jz.mat[i,j] = summary(lin.mod)$coefficients[2,1]*-1 #Multiply by -24 to have a positive consumption rate per day
    Jz.seg.list[[counter]] = segmented.mod
    Ar.seg.dates[counter,1] = Ar.DO.yr[summary(Jz.seg.list[[counter]])$psi[1,2]*24, "mm_dd"]
    Ar.seg.dates[counter,2] = Ar.DO.yr[summary(Jz.seg.list[[counter]])$psi[1,2]*24, "value"]
    Ar.seg.dates[counter,3] = Ar.DO.yr[summary(Jz.seg.list[[counter]])$psi[2,2]*24, "mm_dd"]
    Ar.seg.dates[counter,4] = Ar.DO.yr[summary(Jz.seg.list[[counter]])$psi[2,2]*24, "value"]
    Jz.seg.coef[[counter]] = segmented.mod$coefficients
    legend("topright", legend = c(Ar.seg.dates[counter,1],Ar.seg.dates[counter,3]))
    counter = counter+1
  }
}
dev.off()


Ar.Jz.mat = matrix(unlist(Jz.seg.coef),nrow=6)
rownames(Ar.Jz.mat) = c("Intercept", "First slope", "Second slope", "Third slope", "Useless", "No_Use")
colnames(Ar.Jz.mat) = rownames(Ar.seg.dates)

Ar.Jz = vector(length=15)
Jz.index = c(0,1,0,0,1,0,0,0,0,0,1,0,1,0,0)
for(i in 1:length(Jz.index))
{
  if(Jz.index[i]) Ar.Jz[i] = Ar.Jz.mat[2,i] else Ar.Jz[i] = Ar.Jz.mat[2,i] + Ar.Jz.mat[3,i]
}

Ar.Jz.2017 = Ar.Jz[c(1,4,7,10,13)]*-1
Ar.Jz.2018 = Ar.Jz[c(1,4,7,10,13)+1]*-1
Ar.Jz.2019 = Ar.Jz[c(1,4,7,10,13)+2]*-1
Ar.Jz.Global = (Ar.Jz.2017+ Ar.Jz.2018+Ar.Jz.2019)/3
Jz.mat[,4] = filter(Bats, lake =="ar") %>% slice(c(30,35,40,45,47)) %>% select(alpha)

Ar.Living.2017 = summary(lm(Ar.Jz.2017 ~ Jz.mat[,4]))
Ar.Living.2018 = summary(lm(Ar.Jz.2018 ~ Jz.mat[,4]))
Ar.Living.2019 = summary(lm(Ar.Jz.2019 ~ Jz.mat[,4]))
Ar.Living.global = summary(lm(Ar.Jz.Global ~ Jz.mat[,4]))

pdf("./Arendsee.Jz-Alpha.pdf")
par(mfrow=c(1,1))
plot(Jz.mat[,1] ~ Jz.mat[, 4], las =1,
     xlab = "alpha",
     ylab = "Jz",
     ylim = c(0.04, 0.2),
     main = "Arendsee", col ="blue",pch=16)
abline(lm(Jz.mat[,1] ~ Jz.mat[, 4]), col ="blue")
points(Jz.mat[, 2] ~ Jz.mat[, 4], col ="black",pch=16)
abline(lm(Jz.mat[,2] ~ Jz.mat[, 4]), col ="black")
points(Jz.mat[, 3] ~ Jz.mat[, 4], col = "red",pch=16)
abline(lm(Jz.mat[,3] ~ Jz.mat[, 4]), col ="red")
legend("topleft", legend = c("2017","2018","2019"), text.col = c("blue", "black","red"))
dev.off()

pdf("./Arendsee.Jz-Alpha.SegLm.pdf")
par(mfrow=c(1,1))
plot(Ar.Jz.2017 ~ Jz.mat[, 4], las =1,
     xlab = "alpha",
     ylab = "Jz",
     ylim = c(0.04, 0.3),
     main = "Arendsee", col ="blue",pch=16)
abline(lm(Ar.Jz.2017 ~ Jz.mat[, 4]), col ="blue")
points(Ar.Jz.2018 ~ Jz.mat[, 4], col ="chartreuse3",pch=16)
abline(lm(Ar.Jz.2018 ~ Jz.mat[, 4]), col ="chartreuse3")
points(Ar.Jz.2019 ~ Jz.mat[, 4], col = "red",pch=16)
abline(lm(Ar.Jz.2019 ~ Jz.mat[, 4]), col ="red")
points(Ar.Jz.Global ~ Jz.mat[,4], col="black", pch = 16)
abline(lm(Ar.Jz.Global ~ Jz.mat[, 4]), col ="black")
legend("topleft", legend = c("2017","2018","2019", "global"),
       text.col = c("blue", "chartreuse3","red", "black"))
dev.off()

#2017 Jv = 0.024135 Ja = 1.170962
#2018 Jv = 0.02572 Ja = 1.60033
#2019 Jv = 0.046920 Ja = 0.542862

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

gg.full <- filter(.data = Full_data, lake == "gg")
gg.DO.full <- filter(.data = gg.full, depth == 8) %>% filter(parameter == "DO_mgL")
gg.Temp.full <- filter(.data = gg.full, depth == 8) %>% filter(parameter == "T_C")

#Extract both profiles from Rinko file
gg.depth.1 <- Rinko[which(Rinko$month == 6 & Rinko$lake == "gg"), "depth"]
gg.depth.2 <- Rinko[which(Rinko$month == 8 & Rinko$lake == "gg"), "depth"]
gg.temp.1 <- Rinko[which(Rinko$month == 6 & Rinko$lake == "gg"), "T_C"]
gg.temp.2 <- Rinko[which(Rinko$month == 8 & Rinko$lake == "gg"), "T_C"]
gg.DO.1 <- Rinko[which(Rinko$month == 6 & Rinko$lake == "gg"), "DO_mgL"]
gg.DO.2 <- Rinko[which(Rinko$month == 8 & Rinko$lake == "gg"), "DO_mgL"]


pdf("./gg.loggers.pdf", width=7)
par(mfrow=c(3,2))
gg.DO.8 <- mutate(gg.DO.full, year = substring(CET, 1, 4)) %>% mutate(Hour = seq(1,length(year), 1)) %>% filter(serial == 246451)
gg.Temp.5 <- mutate(gg.Temp.full, year = substring(CET, 1, 4)) %>% mutate(Hour = seq(1,length(year), 1)) %>% filter(serial == 246451)

plot(gg.DO.8[,"value"]~ gg.DO.8[,"Hour"],
     xlab = "Time", 
     ylab = "DO (mg/L)", 
     main = paste(gg.DO.8[27500,"CET"], "to", gg.DO.8[33000,"CET"],", 8m"))
abline(h=2)
gg.lm = lm(gg.DO.8[c(27500:33000),"value"]~gg.DO.8[c(27500:33000),"Hour"])
abline(gg.lm, col = "blue", lwd = 2)
gg.Jz.8m = summary(gg.lm)$coefficients[2,1]*-24 #Multiply by -24 to have a positive consumption rate per day

plot(gg.Temp.5[,"value"]~ gg.Temp.5[,"Hour"],
     xlab = "Time", 
     ylab = "Temp (°C)", 
     main = paste(gg.Temp.5[1,"CET"], "to", gg.Temp.5[40787,"CET"],", 8m"))
abline(v = gg.Temp.5[c(27500,33000),"Hour"])

#Add temperature profiles
plot(gg.depth.1 ~ gg.temp.1, ylim = rev(range(gg.depth.1)),
     xlab = "Temperature", ylab = "Depth",
     main = paste(DoY$gg[[2]]))
abline(h=8)
plot(gg.depth.2 ~ gg.temp.2, ylim = rev(range(gg.depth.2)),
     xlab = "Temperature", ylab = "Depth",
     main = paste(DoY$gg[[3]]))
abline(h=8)

#Add DO profiles
plot(gg.depth.1 ~ gg.DO.1, ylim = rev(range(gg.depth.1)),
     xlab = "DO", ylab = "Depth",)
abline(h=8)
plot(gg.depth.2 ~ gg.DO.2, ylim = rev(range(gg.depth.2)),
     xlab = "DO", ylab = "Depth",)
abline(h=8)

dev.off()


#Create objects for lake hs
hs.full <- filter(.data = Full_data, lake == "hs")
hs.DO.full <- filter(.data = hs.full, depth == 5) %>% filter(parameter == "DO_mgL")
hs.Temp.full <- filter(.data = hs.full, depth == 5) %>% filter(parameter == "T_C")
hs.serial = c(1329,1322) #Both loggers capture the same event
hs.Jz.5m = vector(length = 2)

#Extract both profiles from Rinko file
hs.depth.1 <- Rinko[which(Rinko$month == 6 & Rinko$lake == "hs"), "depth"]
hs.depth.2 <- Rinko[which(Rinko$month == 8 & Rinko$lake == "hs"), "depth"]
hs.temp.1 <- Rinko[which(Rinko$month == 6 & Rinko$lake == "hs"), "T_C"]
hs.temp.2 <- Rinko[which(Rinko$month == 8 & Rinko$lake == "hs"), "T_C"]
hs.DO.1 <- Rinko[which(Rinko$month == 6 & Rinko$lake == "hs"), "DO_mgL"]
hs.DO.2 <- Rinko[which(Rinko$month == 8 & Rinko$lake == "hs"), "DO_mgL"]


pdf("./hs.loggers.pdf", width= 7)
for(j in 1:1){
  par(mfrow=c(3,2))
hs.DO.5 <- mutate(hs.DO.full, year = substring(CET, 1, 4)) %>% mutate(Hour = seq(1,length(year), 1)) %>% filter(serial == hs.serial[j])
hs.Temp.5 <- mutate(hs.Temp.full, year = substring(CET, 1, 4)) %>% mutate(Hour = seq(1,length(year), 1)) %>% filter(serial == hs.serial[1])
plot(hs.DO.5[,"value"]~ hs.DO.5[,"Hour"],
     xlab = "Time", 
     ylab = "DO (mg/L)", 
     main = paste(hs.DO.5[4600,"CET"], "to", hs.DO.5[5600,"CET"],", 5m"))
if(j==1) hs.lm = lm(hs.DO.5[c(4600:5600),"value"]~hs.DO.5[c(4600:5600),"Hour"])
if(j==2) hs.lm = lm(hs.DO.5[c(1:250),"value"]~hs.DO.5[c(1:250),"Hour"])
abline(hs.lm, col = "blue", lwd = 2)
hs.Jz.5m[j] = summary(hs.lm)$coefficients[2,1]*-24

plot(hs.Temp.5[,"value"]~ hs.Temp.5[,"Hour"],
     xlab = "Time", 
     ylab = "Temp (°C)", 
     main = paste(hs.Temp.5[1,"CET"], "to", hs.Temp.5[5848,"CET"],", 5m"))
abline(v = hs.Temp.5[c(4600,5600),"Hour"])

#Add temperature profiles
plot(hs.depth.1 ~ hs.temp.1, ylim = rev(range(hs.depth.1)),
     xlab = "Temperature", ylab = "Depth",
     main = paste(DoY$hs[[2]]))
abline(h=c(5,8))
plot(hs.depth.2 ~ hs.temp.2, ylim = rev(range(hs.depth.2)),
     xlab = "Temperature", ylab = "Depth",
     main = paste(DoY$hs[[3]]))
abline(h=c(5,8))

#Add DO profiles
plot(hs.depth.1 ~ hs.DO.1, ylim = rev(range(hs.depth.1)),
     xlab = "DO", ylab = "Depth",)
abline(h=c(5,8))
plot(hs.depth.2 ~ hs.DO.2, ylim = rev(range(hs.depth.2)),
     xlab = "DO", ylab = "Depth",)
abline(h=c(5,8))
}
dev.off()

#Lake St
#Find the points that are aligned going deeper in the lake
St.jz.Depth.lm <- lm(InterpolatedMatrix$st[c(145:165,300:319),1] ~ InterpolatedMatrix$st[c(145:165,300:319),5])

#Model the theoritical Jz if everything was smooth
St.Depth.jz.lm <- lm(InterpolatedMatrix$st[c(145:165,300:319),5] ~ InterpolatedMatrix$st[c(145:165,300:319),1])
St.jz.mod = c(summary(St.Depth.jz.lm)$coefficients[1,1] + summary(St.Depth.jz.lm)$coefficients[2,1]*c(15:32))
St.alpha = filter(Bats, lake =="st")%>% slice(c(15:32)) %>% select(alpha)

#Find the real Jz
St.Jz = InterpolatedMatrix$st[seq(146,316,10),5]


#plot the results
pdf("./St.Jz.alpha.pdf",width=10)
par(mfrow=c(1,3))
plot(St.Jz ~ St.alpha[,1], las =1, ylab = "Jz", xlab = expression(alpha(z)),
     main = "Lake St, Jz")
plot(InterpolatedMatrix$st[,1] ~ InterpolatedMatrix$st[,5],
     ylim=rev(range(InterpolatedMatrix$st[,1])),
     xlab = "Jz", ylab = "Depth")
abline(St.jz.Depth.lm)
points(InterpolatedMatrix$st[c(145:165,300:319),1] ~ InterpolatedMatrix$st[c(145:165,300:319),5],
       col = "blue", pch = 16)
plot(St.jz.mod ~ St.alpha[,1], las =1, ylab = "Jz", xlab = expression(alpha(z)),
     main = "Lake St, modeled Jz")

dev.off()



#Add Jz to Bathymetry data
Bats = Bats[Bats$lake != "WS",]
Bats = Bats[Bats$lake != "gg_test",]
Bats = Bats[Bats$lake != "PS",]
Bats$lake = factor(Bats$lake)
Bats$ID = paste(Bats$lake, Bats$depth_1m, sep="-")

Bats = merge(Bats, Output.Jz[,c("Jz", "ID")], by = "ID")

ar.Dwb = 45.7*sqrt(5.14)/(21.4 + sqrt(5.14))
sc.Dwb = 45.7*sqrt(8.57)/(21.4 + sqrt(8.57))
hs.Dwb = 45.7*sqrt(1.31)/(21.4 + sqrt(1.31))
fukuNO.Dwb = 45.7*sqrt(0.0039)/(21.4 + sqrt(0.0039))
fukuSW.Dwb = 45.7*sqrt(0.0039)/(21.4 + sqrt(0.0039))
gg.Dwb = 45.7*sqrt(0.66)/(21.4 + sqrt(0.66))
st.Dwb = 45.7*sqrt(4.12)/(21.4 + sqrt(4.12))

pdf(file = "./Arendsee.JvJa.pdf")
par(mfrow = c(2,2))
plot(InterpolatedMatrix$ar$Depth ~ InterpolatedMatrix$ar$Temp,
     ylim = rev(range(InterpolatedMatrix$ar$Depth)),
     xlab = "T (°C)",
     ylab = "D (m)",
     las = 1)
abline(h=28)
abline(h = 38)
plot(InterpolatedMatrix$ar$Depth ~ InterpolatedMatrix$ar$DO_end,
     ylim = rev(range(InterpolatedMatrix$ar$Depth)),
     xlab = "DO (mg/L)",
     ylab = "D (m)",
     las = 1)
abline(h=28)
abline(h = 38)
mtext("Lake Arendsee", side = 3, at = -3.2, line = 2)

plot(InterpolatedMatrix$ar$Depth ~ InterpolatedMatrix$ar$Jz,
     ylim = rev(range(InterpolatedMatrix$ar$Depth)),
     xlab = "Jz",
     ylab = "D (m)",
     las = 1)
abline(h=28)
abline(h = 38)
plot(Bats$Jz[Bats$lake=="ar" & Bats$depth_1m > 28 & Bats$depth_1m < 38] ~ Bats$alpha[Bats$lake=="ar" & Bats$depth_1m > 28 & Bats$depth_1m < 38],
     xlab = "alpha",
     ylab = "Jz",
     las = 1)
abline(lm(Bats$Jz[Bats$lake=="ar" & Bats$depth_1m > 28 & Bats$depth_1m < 38] ~ Bats$alpha[Bats$lake=="ar" & Bats$depth_1m > 28 & Bats$depth_1m < 38]))
dev.off()
ar.lm = lm(Bats$Jz[Bats$lake=="ar" & Bats$depth_1m > 28 & Bats$depth_1m < 38] ~ Bats$alpha[Bats$lake=="ar" & Bats$depth_1m > 28 & Bats$depth_1m < 38])
ar.coef = summary(ar.lm)$coef
ar.Jz = ar.coef[1,1]
ar.Ja = ar.coef[2,1]

Ar.modeled = matrix(nrow = length(which(Bats$lake == "ar")), ncol = 91)
Ar.modeled[,1] = Bats$depth_1m[Bats$lake == "ar"]
Ar.modeled[,2] = 8

for(i in 3:ncol(Ar.modeled)){
  Ar.modeled[,i] = 8 - (ar.Jz + ar.Ja * Bats$alpha[Bats$lake == "ar"])*i
}
Ar.modeled = Ar.modeled[order(Ar.modeled[,1], decreasing = F),]

output.anox <- ifelse(Ar.modeled <= 2, 1, 0)
output.anox.a <- output.anox
for(j in 2:ncol(output.anox.a)){
  temp = output.anox.a[,j-1]+output.anox.a[,j]
  temp = ifelse(temp > output.anox.a[,j-1], temp, 0)
  output.anox.a[,j] = temp}

mypalette = c("white", brewer.pal(9, "YlOrBr"))
if(log(max(output.anox.a),base = 1.5) > length(mypalette)) mypalette = c(mypalette, rep("#662506",round(log(max(output.anox.a),base=1.5))-length(mypalette)) )
par(mfrow=c(1,1))
plot(log(output.anox.a, base = 1.5),
     col = mypalette,
     border= NA,
     key = NULL,
     main = paste0("Hypoxic days in lake Arendsee, modeled"),
     axis.col = NULL,
     axis.row = NULL,
     ylab = "Depth (m)",
     xlab = "Time (day of year)")
axis(side = 2, tick = T,at = c(2,47), labels = c(47, 0), las=1)
axis(side = 1, tick = T,at = c(1,90), labels = c(1, 90),line = 1)

#Find the second portion of the relationship in Arendsee (non-linear)
ar.deep.jz.data = Bats$Jz[Bats$lake=="ar" & Bats$depth_1m > 38 & Bats$depth_1m < 48]
ar.deep.alpha.data = Bats$alpha[Bats$lake=="ar" & Bats$depth_1m > 38 & Bats$depth_1m < 48]

ar.exp.lm <- nlsLM(ar.deep.jz.data ~ Jv + Ja*log(ar.deep.alpha.data),
                   start = list(Jv = .06, Ja = -0.008))

ar.deep.jv = summary(ar.exp.lm)$parameters[1,1]
ar.deep.ja = summary(ar.exp.lm)$parameters[2,1]

Ar.bats = Bats[Bats$lake == "ar",]
Ar.bats = Ar.bats[order(Ar.bats$depth_1m),]
Ar.modeled.complex = matrix(nrow = nrow(Ar.bats), ncol = 91)
Ar.modeled.complex[,1] = Ar.bats$depth_1m[Bats$lake == "ar"]
Ar.modeled.complex[,2] = 8


#Use the first relationship everywhere
for(i in 3:ncol(Ar.modeled.complex)){
Ar.modeled.complex[,i] = 8 - (ar.Jz + ar.Ja * Ar.bats$alpha)*i
}
#Correct the deeper values
for(i in 3:ncol(Ar.modeled.complex)){
Ar.modeled.complex[c(39:49),i] = Ar.modeled.complex[c(39:49),i]+ (ar.deep.jv + ar.deep.ja*log(Ar.bats$alpha[Ar.bats$depth_1m>=38]))*i
}
Ar.modeled.complex[Ar.modeled.complex<0] = 0
output.anox.complex <- ifelse(Ar.modeled.complex <= 2, 1, 0)
output.anox.a.complex <- output.anox.complex
for(j in 2:ncol(output.anox.a.complex)){
  temp = output.anox.a.complex[,j-1]+output.anox.a.complex[,j]
  temp = ifelse(temp > output.anox.a.complex[,j-1], temp, 0)
  output.anox.a.complex[,j] = temp}

mypalette = c("white", brewer.pal(9, "YlOrBr"))
if(log(max(output.anox.a.complex),base = 1.5) > length(mypalette)) mypalette = c(mypalette, rep("#662506",round(log(max(output.anox.a.complex),base=1.5))-length(mypalette)) )
par(mfrow=c(1,1))
plot(log(output.anox.a.complex, base = 1.5),
     col = mypalette,
     border= NA,
     key = NULL,
     main = paste0("Hypoxic days in lake Arendsee, complex modeling"),
     axis.col = NULL,
     axis.row = NULL,
     ylab = "Depth (m)",
     xlab = "Time (day of year)")
axis(side = 2, tick = T,at = c(2,47), labels = c(47, 0), las=1)
axis(side = 1, tick = T,at = c(1,90), labels = c(1, 90),line = 1)


plot(Bats$Jz[Bats$lake=="sc" & Bats$depth_1m > sc.Dwb] ~ Bats$alpha[Bats$lake=="sc" & Bats$depth_1m > sc.Dwb])
plot(Bats$Jz[Bats$lake=="hs" & Bats$depth_1m > hs.Dwb] ~ Bats$alpha[Bats$lake=="hs" & Bats$depth_1m > hs.Dwb])
plot(Bats$Jz[Bats$lake=="fukuNO" & Bats$depth_1m > fukuNO.Dwb] ~ Bats$alpha[Bats$lake=="fukuNO" & Bats$depth_1m > fukuNO.Dwb])
plot(Bats$Jz[Bats$lake=="fukuSW" & Bats$depth_1m > fukuSW.Dwb] ~ Bats$alpha[Bats$lake=="fukuSW" & Bats$depth_1m > fukuSW.Dwb])
plot(Bats$Jz[Bats$lake=="gg" & Bats$depth_1m > gg.Dwb] ~ Bats$alpha[Bats$lake=="gg" & Bats$depth_1m > gg.Dwb])
plot(Bats$Jz[Bats$lake=="st" & Bats$depth_1m > st.Dwb] ~ Bats$alpha[Bats$lake=="st" & Bats$depth_1m > st.Dwb])

#Plot the results
pdf(file = "Hypoxia.pdf")
for(i in 1:length(output.anox.a)){
  mypalette = c("white", brewer.pal(9, "YlOrBr"))
  if(log(max(output.anox.a[[i]]),base = 1.5) > length(mypalette)) mypalette = c(mypalette, rep("#662506",round(log(max(output.anox.a[[i]]),base=1.5))-length(mypalette)) )
  
  plot(log(output.anox.a[[i]], base = 1.5),
       col = mypalette,
       border= NA,
       key = NULL,
       main = paste0("Hypoxic days in lake ", List.lakes[i]),
       axis.col = NULL,
       axis.row = NULL,
       ylab = "Depth (m)",
       xlab = "Time (day of year)")
  axis(side = 2, tick = T,at = c(1,DoY[[i]][[1]][4]), labels = c(max(InterpolatedMatrix[[i]][,1]), min(InterpolatedMatrix[[i]][,1])), las=1)
  axis(side = 1, tick = T,at = c(1,DoY[[i]][[1]][3]), labels = c(DoY[[i]][[1]][1], DoY[[i]][[1]][2]),line = 1)}
dev.off()

pdf("Jz.pdf", width=10, height = 7)
for(i in 1:length(InterpolatedMatrix)){
  par(mfrow = c(1,2))
  plot(InterpolatedMatrix[[i]][,1] ~ InterpolatedMatrix[[i]][,3],
       xlab = "DO (mg/L)",
       ylab = "Depth",
       main = paste(List.lakes[i]),
       col = "blue",
       pch = 16,
       xlim = c(0,14),
       ylim = rev(range(InterpolatedMatrix[[i]][,1])))
  points(InterpolatedMatrix[[i]][,1] ~ InterpolatedMatrix[[i]][,4],
         xlim = c(0,14),
       ylim = rev(range(InterpolatedMatrix[[i]][,1])),
       col = "red",
       pch = 16)
  legend("bottomright",legend= c(paste(DoY[[i]][[2]]),  paste(DoY[[i]][[3]])), text.col = c("Blue","red"))

  plot(InterpolatedMatrix[[i]][,1] ~ InterpolatedMatrix[[i]][,5],
       xlab = "Jz",
       ylab = "Depth",
       main = List.lakes[i],
       ylim = rev(range(InterpolatedMatrix[[i]][,1])))}
dev.off()




anox.df = list()
for(i in 1:length(DoY)){
  for(j in 1:2){
    true.id = paste(DoY[[i]][[4]], j, InterpolatedMatrix[[i]][,1], sep = "-")
    if(j == 1){
      Anox.age = output.anox.a[[i]][,1]
      anox.df[[i]] = data.frame(true.id, Anox.age)
    }
    if(j == 2){
      Anox.age = as.vector(tail(t(output.anox.a[[i]]),1))
      anox.df[[i]] = rbind(anox.df[[i]], data.frame(true.id, Anox.age))  
    }

  }
}
anox.df = do.call(rbind, anox.df)
# 
# Chemistry = merge(x = Chemistry, y = anox.df, by = "true.id")
# Chemistry$MatchID = paste(Chemistry$lake, Chemistry$depth, sep="-")
# 
# DeltaNH4 = Chemistry$Na_d
# DeltaNH4[Chemistry$campaign.no == 1] = NA
# 
# Chemistry$NH4_fia[match(x = Chemistry$MatchID, table = Chemistry$MatchID) & Chemistry$campaign.no == 2]
# 
# 
# match(x = Chemistry$MatchID, table = Chemistry$MatchID)
# DeltaNH4[Chemistry$campaign.no == 2] = 
#   
# 
# 
# plot(Chemistry$DO_mgL ~ Chemistry$Anox.age) #Works because threshold used was 2mgL
# plot(Chemistry$SRP ~ Chemistry$Anox.age)
# plot(Chemistry$NH4_fia ~ Chemistry$Anox.age)
# plot(Chemistry$NH4_fia ~ Chemistry$Anox.age, bg = Chemistry$lake, pch = 21)
# plot(Chemistry$conc.CH4.insitu ~ Chemistry$Anox.age)
# plot(Chemistry$conc.CO2.insitu ~ Chemistry$Anox.age)
# plot(Chemistry$conc.CO2.corrected ~ Chemistry$Anox.age)

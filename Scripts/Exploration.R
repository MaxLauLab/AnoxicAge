library(RColorBrewer)
#library(plot.matrix)
library(dplyr)
library(segmented)
library(minpack.lm)
library(tidyr)
library(ggplot2)
setwd("D:/Postdoc/Allemagne/Github/AnoxicAge")
load("../AnoxicAge.RData")

theme_klima<- function(base_size = 14, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size=14,colour="black"),
      axis.text.y = element_text(size=14,colour="black", margin=margin(0,5,0,0), vjust = 0.5),
      axis.ticks =  element_line(colour = "black"), 
      axis.title.x= element_text(size=21,colour="black",vjust=-2),
      axis.title.y= element_text(size=21,angle=90,colour="black",vjust = 6),
      panel.background = element_rect(fill="white",size = 1, colour = "black"), 
      panel.border =element_blank(),  
      panel.grid.major = element_line(colour = "lightgrey"), 
      #panel.grid.minor = element_line(colour = "lightgrey"),
      panel.grid.minor = element_blank(), 
      plot.background = element_rect(fill="white",color=NA), 
      plot.title =element_text(size=14,colour="black"), 
      plot.margin = unit(c(1,  1, 1, 2), "lines"),
      legend.background=element_rect(fill='white',color=NA),
      legend.title=element_text(size=14,colour="black"),
      legend.text=element_text(size=14,colour="black"),
      legend.key = element_rect( fill = 'lightgrey',color='white'),
      legend.key.size = unit(c(1.2, 1.2), "lines"),
      axis.line.x = element_line(color="black", size = 1.5),
      axis.line.y = element_line(color="black", size = 1.5),
      #factettes
      strip.background = element_rect(colour = 'lightgrey',size = 1,fill = 'lightgrey'), #for boxes of facettes
      panel.spacing = unit(0.6 , "lines"),   #panel margins
      strip.text.x = element_text(size = 12,colour = 'black'),
      strip.text.y = element_text(size = 12,colour = 'black',angle = 90)
    )
}

#########################################################################################
###########################Calculate Jz Arendsee using loggers###########################
#########################################################################################

#################################THIS SECTION WORKS BY ITSELF############################
##################################CAN BE ITS OWN SCRIPT WHEN#############################
#################################OTHER SECTIONS ARE VERIFIED#############################

Full_data <- read.csv("../full_database.csv")
Bats <- read.csv("./Data/Raw/Arendsee.alpha.csv")
ar.depths = c(30,35,40,45,47)
ar.year = c(2016,2017,2018,2019,2020)
Ar.full <- filter(.data = Full_data, lake == "ar")

Ar.Temp <- filter(Ar.full, depth == 30 | depth == 35 | depth == 40 | depth == 45 | depth == 47 ) %>% 
  filter(parameter == "DO_mgL" | parameter == "T_C")

Ar.Temp = Ar.Temp[,-1]

write.csv(x = Ar.Temp,file = "../Logger.ar.csv", fileEncoding = "UTF-8")

#Find when it destratifies. Exploration only
{
pdf("./Exploration output/Ar.loggers.Temp.pdf", width=15, height=5)
par(mfrow=c(1,5))
for(i in 1:length(ar.depths)){
  Ar.Temp <- filter(Ar.full, depth == ar.depths[i]) %>% 
    filter(parameter == "T_C") %>% 
    filter(value < 7) %>%
    mutate(year = substring(CET, 1, 4)) %>% 
    mutate(mm_dd = substring(CET,6,10)) %>%
    mutate(mm = substring(mm_dd,1,2))
  
  for(j in 1:length(ar.year)){
    if(i==5 & j==1) {
      plot(1~1, col = "white")
      legend("center", legend="NA", bty = "n")
      next
    }
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
}


#Look at O2 time series. Exploration only
{
  pdf("./Exploration output/Ar.loggers.DO.pdf", width=12, height=5)
  par(mfrow=c(1,5))
  for(i in 1:length(ar.depths)){
    Ar.DO <- filter(Ar.full, depth == ar.depths[i]) %>% 
      filter(parameter == "DO_mgL") %>% 
      mutate(year = substring(CET, 1, 4)) %>% 
      mutate(mm_dd = substring(CET,6,10)) %>%
      mutate(mm = substring(mm_dd,1,2))
    
    for(j in 1:length(ar.year)){
      if(i==5 & j==1) {
        plot(1~1, col = "white")
        legend("center", legend="NA", bty = "n")
        next
      }
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
}

#Jz determination with multiple methods
#Create matrix
Jz.mat = as.data.frame(matrix(nrow=length(ar.depths), ncol = length(ar.year)))
colnames(Jz.mat) = ar.year
rownames(Jz.mat) = ar.depths

#Select time stamps
Ar.full <- filter(.data = Full_data, lake == "ar")
ar.from <- matrix(c(1500,2000,1919,2000,1500,
                    1500,2000,707,2000,1500,
                    1500,2000,2000,2000,1500,
                    1500,2000,1000,2000,1500,
                    2000,2000,2000,2000,1500), nrow=5, ncol=5, byrow = T)

ar.to <- matrix(c(7300,7500,7400,7844,7800,
                  7300,7200,6000,7000,7000,
                  5600,6000,6000,5800,5500,
                  4600,5000,3300,5200,4000,
                  4200,4500,4200,5000,4000), nrow=5, ncol=5, byrow = T)
Ar.seg.dates = matrix(nrow=25,ncol=4)
colnames(Ar.seg.dates) = c("First date", "First DO", "Second date", "Second DO")
rownames(Ar.seg.dates) = as.vector(outer(ar.year, ar.depths, paste, sep=" "))
rownames(Ar.seg.dates) = paste0(rownames(Ar.seg.dates), "m")

#Create AIC table for comparison
AIC.table = matrix(nrow=25, ncol=5)
colnames(AIC.table) = c("AIC.lin", "AIC.2", "AIC.3", "AIC.4", "AIC.exp")
rownames(AIC.table) = rownames(Ar.seg.dates)

#Look at different regressions between O2 and time
#Using linear, exponential or segmented regressions
Jz.seg.list = list()
Jz.seg.coef = list()
{
  counter=1
pdf("./Exploration output/Ar.loggers.SegLm.TEST.pdf", width=8, height=2.5)
par(mfrow=c(1,5))
for(i in 1:length(ar.depths)){
  Ar.DO <- filter(Ar.full, depth == ar.depths[i]) %>% 
    filter(parameter == "DO_mgL") %>% 
    mutate(year = substring(CET, 1, 4)) %>% 
    mutate(mm_dd = substring(CET,6,10)) %>%
    mutate(mm = substring(mm_dd,1,2))
  
  for(j in 1:length(ar.year)){
    if(i==5 & j==1) {
      plot(1~1, col = "white")
      legend("center", legend="NA", bty = "n")
      counter = counter+1
      next    }
    Ar.DO.yr <- filter(Ar.DO, year == ar.year[j])
    Ar.DO.yr <- Ar.DO.yr[c(ar.from[i,j]:ar.to[i,j]),] %>% mutate(Dec.Day = seq(1,length(year), 1)/24)
    Ar.DO.yr$Season <- ifelse(as.numeric(Ar.DO.yr$mm) <= 5, "Spring", ifelse(as.numeric(Ar.DO.yr$mm) >= 9, "Autumn", "Summer"))

    
    # plot(Ar.DO.yr[,"value"]~ Ar.DO.yr[,"Dec.Day"],
    #      xlab = "Time",
    #      ylab = "DO (mg/L)",
    #      main = paste0(ar.year[j]," ", ar.depths[i], "m"))
    
    #Try piecewise regressions
    #Piecewise regression (find the inflection point)
    #Linear model
    Dec.Day = Ar.DO.yr$Dec.Day
    value = Ar.DO.yr$value
    lin.mod = lm(value ~ Dec.Day)
    
    
    
    Dec.Day.segLm = c(seq(-400,0,1/24),Dec.Day, seq(Dec.Day[length(Dec.Day)]+1/24,Dec.Day[length(Dec.Day)]+400,1/24))
    value.segLm = c(rep(max(value),9601),value, rep(min(value),9600))
    lin.mod.segLm = lm(value.segLm ~ Dec.Day.segLm)
    
    plot(value~ Dec.Day,
         xlab = "Time",
         ylab = "DO (mg/L)",
         main = paste0(ar.year[j]," ", ar.depths[i], "m"))
    
    abline(lin.mod, lwd = 2, col = "cyan")
    
    AIC.table[counter,1] = round(AIC(lin.mod))
    #Piecewise model
    psi = c(50) #else psi = c(30)
    segmented.mod <- segmented(lin.mod.segLm, psi=psi)
    # plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "red", lwd=2)
    AIC.table[counter,2] = round(AIC(segmented.mod))
    
    psi = c(30,85) #else psi = c(30)
    segmented.mod <- segmented(lin.mod.segLm, psi=psi)
    plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "chartreuse3", lwd=2)
    AIC.table[counter,3] = round(AIC(segmented.mod))
    
    Jz.mat[i,j] = summary(lin.mod)$coefficients[2,1]*-1 #Multiply by -24 to have a positive consumption rate per day
    Jz.seg.list[[counter]] = segmented.mod
    # Ar.seg.dates[counter,1] = Ar.DO.yr[summary(Jz.seg.list[[counter]])$psi[1,2]*24, "mm_dd"]
    # Ar.seg.dates[counter,2] = Ar.DO.yr[summary(Jz.seg.list[[counter]])$psi[1,2]*24, "value"]
    # Ar.seg.dates[counter,3] = Ar.DO.yr[summary(Jz.seg.list[[counter]])$psi[2,2]*24, "mm_dd"]
    # Ar.seg.dates[counter,4] = Ar.DO.yr[summary(Jz.seg.list[[counter]])$psi[2,2]*24, "value"]
    Jz.seg.coef[[counter]] = segmented.mod$coefficients
    legend("topright", legend = c(Ar.seg.dates[counter,1],Ar.seg.dates[counter,3]))
    
    # psi = c(30,50,88) #else psi = c(30)
    # segmented.mod <- segmented(lin.mod.segLm, psi=psi)
    # plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "blue", lwd=2)
    # AIC.table[counter,4] = round(AIC(segmented.mod))
    
    # exp.mod <- nlsLM(value ~ a*exp(-k*Dec.Day) + b,
    #                  start = list(a = 0.5, k = 0.5, b = 12))
    # exp.param = summary(exp.mod)$parameters
    # curve(exp.param[1] * exp(-exp.param[2] * x) + exp.param[3], add=T, col = "yellow", lwd=2) 
    # AIC.table[counter,5] = round(AIC(exp.mod))
    counter = counter+1
  }
}
dev.off()
}


#Compare different methods
#Note. Multiple breakpoints can provide a better fit, but can be overkill to model O2 data
AIC.table

#Aggregated to daily values
Jz.mat.day = as.data.frame(matrix(nrow=length(ar.depths), ncol = length(ar.year)))
colnames(Jz.mat.day) = ar.year
rownames(Jz.mat.day) = ar.depths
Jz.mat.day.err <- Jz.mat.day

ar.from.day <- round(matrix(c(1500,2000,1919,2000,1500,
                    1500,2000,707,2000,1500,
                    1500,2000,2000,2000,1500,
                    1500,2000,1000,2000,1500,
                    2000,2000,2000,2000,2000), nrow=5, ncol=5, byrow = T)/24)
ar.to.day <- round(matrix(c(7300,7500,7400,7300,7300,
                  7300,7200,6000,7000,7200,
                  6600,7000,7000,6800,6000,
                  5000,5000,4800,5000,4600,
                  4200,4500,4400,4800,4400), nrow=5, ncol=5, byrow = T)/24)
Ar.seg.dates.day = matrix(nrow=25,ncol=4)
colnames(Ar.seg.dates.day) = c("First date", "First DO", "Second date", "Second DO")
rownames(Ar.seg.dates.day) = as.vector(outer(ar.year, ar.depths, paste, sep=" "))
rownames(Ar.seg.dates.day) = paste0(rownames(Ar.seg.dates.day), "m")

AIC.table.day = matrix(nrow=25, ncol=5)
colnames(AIC.table.day) = c("AIC.lin", "AIC.2", "AIC.3", "AIC.4", "AIC.exp")
rownames(AIC.table.day) = rownames(Ar.seg.dates.day)

Jz.seg.list.day = list()
Jz.seg.coef.day = list()
{
counter=1
pdf("./Exploration output/Ar.loggers.SegLm.day.100-200.TEST.pdf", width=8, height=3)
par(mfrow=c(1,5))
for(i in 1:length(ar.depths)){
  Ar.DO <- filter(Ar.full, depth == ar.depths[i]) %>% 
    filter(parameter == "DO_mgL") %>% 
    mutate(year = substring(CET, 1, 4),
           mm = substring(CET, 6, 7),
           dd = substring(CET, 9, 10),
           mm_dd = substring(CET,6,10))
  Ar.DO$DOY = as.numeric(strftime(Ar.DO$CET, format = "%j"))
  Ar.DO.day = Ar.DO %>% group_by(year, DOY) %>% summarize(value = mean(value))
  
  for(j in 1:length(ar.year)){
    if(i==5 & j==1) {
      plot(1~1, col = "white")
      legend("center", legend="NA", bty = "n")
      Jz.seg.coef.day[[counter]] = list(c(0,0,0,0,0,0))
      counter = counter+1
      next    }
    Ar.DO.yr.day <- filter(Ar.DO.day, year == ar.year[j])
#    Ar.DO.yr.day <- Ar.DO.yr.day[which(Ar.DO.yr.day$DOY==DOY.ini | Ar.DO.yr.day$DOY==DOY.end),]
    Ar.DO.yr.day <- Ar.DO.yr.day[which(Ar.DO.yr.day$DOY>=ar.from.day[i,j] & Ar.DO.yr.day$DOY<=ar.to.day[i,j]),]
    # Ar.DO.yr.day$Dec.Day = c(1:length(Ar.DO.yr.day$year))
    # Ar.DO.yr.day$Season <- ifelse(as.numeric(Ar.DO.yr.day$mm) <= 5, "Spring", ifelse(as.numeric(Ar.DO.yr.day$mm) >= 9, "Autumn", "Summer"))
    Ar.DO.yr.day = as.data.frame(Ar.DO.yr.day)
    
    # plot(Ar.DO.yr.day[,"value"]~ Ar.DO.yr.day[,"DOY"],
    #      xlab = "Time",
    #      ylab = "DO (mg/L)",
    #      main = paste0(ar.year[j]," ", ar.depths[i], "m"))
    
    
    #Try piecewise regressions
    #Piecewise regression (find the inflection point)
    #Linear model
    Dec.Day = Ar.DO.yr.day$DOY
    value = Ar.DO.yr.day$value
    # print(ar.year[j]); print(length(value))
    lin.mod = lm(value ~ Dec.Day)
    
    Dec.Day.segLm = c(seq(Dec.Day[1]-400,Dec.Day[1]-1,1),Dec.Day, seq(Dec.Day[length(Dec.Day)]+1,Dec.Day[length(Dec.Day)]+400,1))
    value.segLm = c(rep(max(value),400),value, rep(min(value),400))
    lin.mod.segLm = lm(value.segLm ~ Dec.Day.segLm)
    
    plot(value~ Dec.Day,
         xlab = "Time",
         ylab = "DO (mg/L)",
         main = paste0(ar.year[j]," ", ar.depths[i], "m"))
    
    abline(lin.mod, lwd = 2, col = "cyan")
    
    AIC.table.day[counter,1] = round(AIC(lin.mod))
    #Piecewise model
    psi = c(150) #else psi = c(30)
    segmented.mod <- segmented(lin.mod.segLm, psi=psi)
    AIC.table.day[counter,2] = round(AIC(segmented.mod))
    
    psi = c(150,170) #else psi = c(30)
    segmented.mod <- segmented(lin.mod.segLm, psi=psi)
    plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "chartreuse3", lwd=2)
    AIC.table.day[counter,3] = round(AIC(segmented.mod))
    
    Jz.mat.day[i,j] = summary(lin.mod)$coefficients[2,1]*-1 #Multiply by -24 to have a positive consumption rate per day
    Jz.mat.day.err[i,j] = summary(lin.mod)$coefficients[2,2]
    Jz.seg.list.day[[counter]] = segmented.mod
    Ar.seg.dates.day[counter,2] = Ar.DO.yr.day[round(summary(Jz.seg.list.day[[counter]])$psi[1,2]), "value"]
    Ar.seg.dates.day[counter,4] = Ar.DO.yr.day[round(summary(Jz.seg.list.day[[counter]])$psi[2,2]), "value"]
    Jz.seg.coef.day[[counter]] = segmented.mod$coefficients
    legend("topright", legend = c(Ar.seg.dates[counter,1],Ar.seg.dates[counter,3]))
    
    psi = c(130,150,170) #else psi = c(30)
    segmented.mod <- segmented(lin.mod.segLm, psi=psi)
    AIC.table.day[counter,4] = round(AIC(segmented.mod))
    counter = counter+1
  }
}
dev.off()
}
AIC.table.day

#How related are Jz using hourly and daily data
plot(c(as.matrix(Jz.mat[,-6])) ~ c(as.matrix(Jz.mat.day)))
Hr.Day.lm = lm(c(as.matrix(Jz.mat[,-6])) ~ c(as.matrix(Jz.mat.day)))
abline(Hr.Day.lm)
summary(Hr.Day.lm)

#Create vector of both (they are well correlated) to compare with segmented Jz
JzLogger.vec.lin = c(c(as.matrix(Jz.mat)),c(as.matrix(Jz.mat.day)))
#Remove NA
JzLogger.vec.lin = JzLogger.vec.lin[-which(is.na(JzLogger.vec.lin))]

#Calcule the Livingstone models with the hourly data
Ar.Jz.mat = matrix(unlist(Jz.seg.coef),nrow=6)
rownames(Ar.Jz.mat) = c("Intercept", "First slope", "Second slope", "Third slope", "Useless", "No_Use")
colnames(Ar.Jz.mat) = rownames(Ar.seg.dates)[-21]

Ar.Jz = vector(length=24)
#Select which slopes to choose from segmented regression
# Jz.index = c(0,0,1,0,0,
#              0,1,1,0,0,
#              0,0,0,0,0,
#              0,0,1,0,2,
#              1,0,0,0)
for(i in 1:length(Ar.Jz))
{
  # if(Jz.index[i] == 1) Ar.Jz[i] = Ar.Jz.mat[2,i]
  # if(Jz.index[i] == 0) Ar.Jz[i] = Ar.Jz.mat[2,i] + Ar.Jz.mat[3,i]
  # if(Jz.index[i] == 2) Ar.Jz[i] = Ar.Jz.mat[2,i] + Ar.Jz.mat[3,i] + Ar.Jz.mat[4,i]
  Ar.Jz[i] = Ar.Jz.mat[2,i] + Ar.Jz.mat[3,i]
}

Ar.Jz.2016 = Ar.Jz[c(1,6,11,16)]*-1
Ar.Jz.2017 = Ar.Jz[c(2,7,12,17,21)]*-1
Ar.Jz.2018 = Ar.Jz[c(2,7,12,17,21)+1]*-1
Ar.Jz.2019 = Ar.Jz[c(2,7,12,17,21)+2]*-1
Ar.Jz.2020 = Ar.Jz[c(2,7,12,17,21)+3]*-1
#Slice is written for depths, but needs +1 to account for 0m deep (it selects row numbers)
Jz.mat[,6] = Bats  %>% slice(c(30,35,40,45,47)) %>% select(alpha)

#Linear relationships
Ar.Living.2016.lin = lm(Ar.Jz.2016 ~ Jz.mat[-5,6]) #R2 = 0.91
Ar.Living.2017.lin = lm(Ar.Jz.2017 ~ Jz.mat[,6]) #R2 = 0.97
Ar.Living.2018.lin = lm(Ar.Jz.2018 ~ Jz.mat[,6]) #R2 = 0.92
Ar.Living.2019.lin = lm(Ar.Jz.2019 ~ Jz.mat[,6]) #R2 = 0.89
Ar.Living.2020.lin = lm(Ar.Jz.2020 ~ Jz.mat[,6]) #R2 = 0.88

#Log relationships
temp.object = Jz.mat[-5,6]
Ar.Living.2016.log = nlsLM(Ar.Jz.2016 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100)) #R2 = 0.99

temp.object = Jz.mat[,6]
Ar.Living.2017.log = nlsLM(Ar.Jz.2017 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100)) #R2 = 0.95

Ar.Living.2018.log = nlsLM(Ar.Jz.2018 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100)) #R2 = 0.96

Ar.Living.2019.log = nlsLM(Ar.Jz.2019 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100)) #R2 = 0.92

Ar.Living.2020.log = nlsLM(Ar.Jz.2020 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100)) #R2 = 0.94


#Exponential plateau relationships
temp.object = Jz.mat[-5,6]
temp.max = max(Ar.Jz.2016)
temp.min = min(Ar.Jz.2016)
Ar.Living.2016.exp = nlsLM(Ar.Jz.2016 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=temp.min, k=5),
                           lower = c(1.5*temp.min, 0, 0),
                           upper = c(2*temp.max,temp.max,15)) #0.995
pred <- predict(Ar.Living.2016.exp, temp.object)
rss <- sum((pred - Ar.Jz.2016) ^ 2)
tss <- sum((temp - mean(Ar.Jz.2016)) ^ 2)
rsq <- 1 - rss/tss

temp.object = Jz.mat[,6]
temp.max = max(Ar.Jz.2017)
temp.min = min(Ar.Jz.2017)
Ar.Living.2017.exp = nlsLM(Ar.Jz.2017 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=temp.min, k=5),
                           lower = c(1.5*temp.min, 0, 0),
                           upper = c(2*temp.max,temp.max,15)) #R2 = 0.999

pred <- predict(Ar.Living.2017.exp, temp.object)
rss <- sum((pred - Ar.Jz.2017) ^ 2)
tss <- sum((temp - mean(Ar.Jz.2017)) ^ 2)
rsq <- 1 - rss/tss


temp.max = max(Ar.Jz.2018)
temp.min = min(Ar.Jz.2018)
Ar.Living.2018.exp = nlsLM(Ar.Jz.2018 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=temp.min, k=5),
                           lower = c(1.5*temp.min, 0, 0),
                           upper = c(2*temp.max,temp.max,15)) #R2 = 0.97

pred <- predict(Ar.Living.2018.exp, temp.object)
rss <- sum((pred - Ar.Jz.2018) ^ 2)
tss <- sum((temp - mean(Ar.Jz.2018)) ^ 2)
rsq <- 1 - rss/tss

temp.max = max(Ar.Jz.2019)
temp.min = min(Ar.Jz.2019)
Ar.Living.2019.exp = nlsLM(Ar.Jz.2019 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=temp.min, k=5),
                           lower = c(1.5*temp.min, 0, 0),
                           upper = c(2*temp.max,temp.max,15)) #R2 = 0.9998
pred <- predict(Ar.Living.2019.exp, temp.object)
rss <- sum((pred - Ar.Jz.2019) ^ 2)
tss <- sum((temp - mean(Ar.Jz.2019)) ^ 2)
rsq <- 1 - rss/tss


temp.max = max(Ar.Jz.2020)
temp.min = min(Ar.Jz.2020)
Ar.Living.2020.exp = nlsLM(Ar.Jz.2020 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=temp.min, k=5),
                           lower = c(1.5*temp.min, 0, 0),
                           upper = c(2*temp.max,temp.max,15)) #R2 = 0.9999
pred <- predict(Ar.Living.2020.exp, temp.object)
rss <- sum((pred - Ar.Jz.2020) ^ 2)
tss <- sum((temp - mean(Ar.Jz.2020)) ^ 2)
rsq <- 1 - rss/tss


rm(temp.object)



#Plot for hourly resolution
{
pdf("./Output/Ar.Jz-Alpha.hourly.SegLm-LOGGER.TEST.pdf", height = 3.5)
par(mfrow=c(1,3))
par(mar=(c(5,5,4,1)+0.2))
plot(Ar.Jz.2017 ~ Jz.mat[, 6], las =1,
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     ylim = c(0.04, 0.30),
     main = "Linear", col ="blue",pch=16)
abline(lm(Ar.Jz.2017 ~ Jz.mat[, 6]), col ="blue")
points(Ar.Jz.2018 ~ Jz.mat[, 6], col ="chartreuse3",pch=16)
# abline(lm(Ar.Jz.2018 ~ Jz.mat[, 6]), col ="chartreuse3")
points(Ar.Jz.2019 ~ Jz.mat[, 6], col = "red",pch=16)
# abline(lm(Ar.Jz.2019 ~ Jz.mat[, 6]), col ="red")
# points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
# abline(lm(Ar.Jz.2016 ~ Jz.mat[-5, 6]), col ="black")
points(Ar.Jz.2020 ~ Jz.mat[,6], col="pink", pch = 16)
# abline(lm(Ar.Jz.2020 ~ Jz.mat[, 6]), col ="pink")
legend("topleft", legend = c("2017","2018","2019","2020"),
       text.col = c("blue", "chartreuse3","red","pink"))

#Log-linear
plot(Ar.Jz.2017 ~ Jz.mat[, 6], las =1,
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     ylim = c(0.04, 0.30),
     main = "Log-linear", col ="blue",pch=16)
# curve(coef(Ar.Living.2017.log)[1]*log10(x*coef(Ar.Living.2017.log)[2]),
#       add=T, col ="blue")
points(Ar.Jz.2018 ~ Jz.mat[, 6], col ="chartreuse3",pch=16)
# curve(coef(Ar.Living.2018.log)[1]*log10(x*coef(Ar.Living.2018.log)[2]),
#       add=T, col ="chartreuse3")
points(Ar.Jz.2019 ~ Jz.mat[, 6], col = "red",pch=16)
# curve(coef(Ar.Living.2019.log)[1]*log10(x*coef(Ar.Living.2019.log)[2]),
#       add=T, col ="red")
# points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
# curve(coef(Ar.Living.2016.log)[1]*log10(x*coef(Ar.Living.2016.log)[2]),
#       add=T, col ="black")
points(Ar.Jz.2020 ~ Jz.mat[,6], col="pink", pch = 16)
# curve(coef(Ar.Living.2020.log)[1]*log10(x*coef(Ar.Living.2020.log)[2]),
#       add=T, col ="pink")

plot(Ar.Jz.2017 ~ Jz.mat[, 6], las =1,
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     ylim = c(0.04, 0.30),
     main = "Exponential plateau", col ="blue",pch=16)
curve(coef(Ar.Living.2017.exp)[1] - (coef(Ar.Living.2017.exp)[1] - coef(Ar.Living.2017.exp)[2])*
        exp(-coef(Ar.Living.2017.exp)[3]*x),
      add=T, col ="blue")
points(Ar.Jz.2018 ~ Jz.mat[, 6], col ="chartreuse3",pch=16)
curve(coef(Ar.Living.2018.exp)[1] - (coef(Ar.Living.2018.exp)[1] - coef(Ar.Living.2018.exp)[2])*
        exp(-coef(Ar.Living.2018.exp)[3]*x),
      add=T, col ="chartreuse3", lty = 2)
points(Ar.Jz.2019 ~ Jz.mat[, 6], col = "red",pch=16)
curve(coef(Ar.Living.2019.exp)[1] - (coef(Ar.Living.2019.exp)[1] - coef(Ar.Living.2019.exp)[2])*
        exp(-coef(Ar.Living.2019.exp)[3]*x),
      add=T, col ="red")
# points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
# curve(coef(Ar.Living.2016.exp)[1] - (coef(Ar.Living.2016.exp)[1] - coef(Ar.Living.2016.exp)[2])*
#         exp(-coef(Ar.Living.2016.exp)[3]*x),
#       add=T, col ="black")
points(Ar.Jz.2020 ~ Jz.mat[,6], col="pink", pch = 16)
curve(coef(Ar.Living.2020.exp)[1] - (coef(Ar.Living.2020.exp)[1] - coef(Ar.Living.2020.exp)[2])*
        exp(-coef(Ar.Living.2020.exp)[3]*x),
      add=T, col ="pink")
dev.off()
}



#Calculate the Livingstone model with daily data
Ar.Jz.mat.day = matrix(unlist(Jz.seg.coef.day),nrow=6)
rownames(Ar.Jz.mat.day) = c("Intercept", "First slope", "Second slope", "Third slope", "Useless", "No_Use")
colnames(Ar.Jz.mat.day) = rownames(Ar.seg.dates.day)

Ar.Jz.day = vector(length=25)
# Jz.index.day = c(0,1,1,2,0,
#                  0,1,0,2,0,
#                  2,0,0,0,2,
#                  2,0,0,0,0,
#                  0,0,0,2,0)
for(i in 1:length(Ar.Jz.day))
{
  # if(Jz.index.day[i]==0) Ar.Jz.day[i] = Ar.Jz.mat.day[2,i] + Ar.Jz.mat.day[3,i]
  # if(Jz.index.day[i]==1) Ar.Jz.day[i] = Ar.Jz.mat.day[2,i] 
  # if(Jz.index.day[i]==2) Ar.Jz.day[i] = Ar.Jz.mat.day[2,i] + Ar.Jz.mat.day[3,i] + Ar.Jz.mat.day[4,i]
  Ar.Jz.day[i] = Ar.Jz.mat.day[2,i] + Ar.Jz.mat.day[3,i]
}

Ar.Jz.day[Ar.Jz.day==0] = NA

Ar.Jz.day.2016 = Ar.Jz.day[c(1,6,11,16,21)]*-1
Ar.Jz.day.2017 = Ar.Jz.day[c(1,6,11,16,21)+1]*-1
Ar.Jz.day.2018 = Ar.Jz.day[c(1,6,11,16,21)+2]*-1
Ar.Jz.day.2019 = Ar.Jz.day[c(1,6,11,16,21)+3]*-1
Ar.Jz.day.2020 = Ar.Jz.day[c(1,6,11,16,21)+4]*-1
Jz.mat[,6] = Bats %>% slice(c(30,35,40,45,47)) %>% select(alpha)

#With the linear O2 consumption rates
Ar.Jz.day.2016.lin = Jz.mat.day[,1] #From the linear O2 consumption, not SegLm
Ar.Jz.day.2017.lin = Jz.mat.day[,2]
Ar.Jz.day.2018.lin = Jz.mat.day[,3]
Ar.Jz.day.2019.lin = Jz.mat.day[,4]
Ar.Jz.day.2020.lin = Jz.mat.day[,5]

#RMSE = sqrt(mean(summary(Object)$residuals^2))
#AIC = AIC(Object)
#R2 for log and exponential plateau
pred <- predict(Ar.Jz.alpha.YSI.2021.log, alpha)
# rss <- sum((pred - temp) ^ 2)
# tss <- sum((temp - mean(temp)) ^ 2)
# rsq <- 1 - rss/tss

#Jz ~ Alpha, linear
Ar.Living.2016.day = summary(lm(Ar.Jz.day.2016.lin ~ Jz.mat[,6]))
#R2 = ; AIC = ; RMSE = 
Ar.Living.2017.day = summary(lm(Ar.Jz.day.2017.lin ~ Jz.mat[,6]))
#R2 = 0.88; AIC = -25.3; RMSE = 0.0106
Ar.Living.2018.day = summary(lm(Ar.Jz.day.2018.lin ~ Jz.mat[,6]))
#R2 = 0.97; AIC = -26.12632; RMSE = 0.0097
Ar.Living.2019.day = summary(lm(Ar.Jz.day.2019.lin ~ Jz.mat[,6]))
#R2 = 0.8856; AIC = -27.42955; RMSE = 0.008549407
Ar.Living.2020.day = summary(lm(Ar.Jz.day.2020.lin ~ Jz.mat[,6]))
#R2 = 0.96; AIC = -28.97831; RMSE = 0.007322751

#Jz ~ Alpha, log linear, with linear O2 consumption
Ar.Living.2016.day.lin.log = nlsLM(Ar.Jz.day.2016.lin ~ b*log10(k*Jz.mat[-5,6]),
                                   start = list(b = 0.08, k = 200))
Ar.Living.2017.day.lin.log = nlsLM(Ar.Jz.day.2017.lin ~ b*log10(k*Jz.mat[,6]),
                                   start = list(b = 0.08, k = 200))
#R2 = 0.97; AIC = -32.2; RMSE = 0.005280211
Ar.Living.2018.day.lin.log = nlsLM(Ar.Jz.day.2018.lin ~ b*log10(k*Jz.mat[,6]),
                                   start = list(b = 0.08, k = 200))
#R2 = 0.98; AIC = -28.32138; RMSE = 0.007819951
Ar.Living.2019.day.lin.log = nlsLM(Ar.Jz.day.2019.lin ~ b*log10(k*Jz.mat[,6]),
                                   start = list(b = 0.08, k = 200))
#R2 = 0.99; AIC = -40.3215; RMSE = 0.002355297
Ar.Living.2020.day.lin.log = nlsLM(Ar.Jz.day.2020.lin ~ b*log10(k*Jz.mat[,6]),
                                   start = list(b = 0.08, k = 200))
#R2 = 0.99; AIC = -38.08294; RMSE = 0.002946219

#Jz ~ Alpha, exponential, with linear O2 consumption
Ar.Living.2017.day.lin.exp = nlsLM(Ar.Jz.day.2017.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                   start = list(j.m = max(Ar.Jz.day.2017.lin), b = 0.2, k = 1))
#R2 = 0.986; AIC = -33.92481; RMSE = 0.003655875
Ar.Living.2018.day.lin.exp = nlsLM(Ar.Jz.day.2018.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                   start = list(j.m = max(Ar.Jz.day.2018.lin), b = 0.2, k = 1))
#R2 = 0.997; AIC = -35.16316; RMSE = 0.003230059
Ar.Living.2019.day.lin.exp = nlsLM(Ar.Jz.day.2019.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                   start = list(j.m = max(Ar.Jz.day.2019.lin), b = 0.2, k = 1))
#R2 = 0.999; AIC = -48.7609; RMSE = .000829217
Ar.Living.2020.day.lin.exp = nlsLM(Ar.Jz.day.2020.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                   start = list(j.m = max(Ar.Jz.day.2020.lin), b = 0.2, k = 1))
#R2 = 0.996; AIC = -39.03195; RMSE = 0.00219377

#Jz ~ Alpha, log linear, with segmented regression O2 consumption
Ar.Living.2016.day.SegLm.log = nlsLM(Ar.Jz.day.2016 ~ b*log10(k*Jz.mat[-5,6]),
                                     start = list(b = 0.08, k = 100))
Ar.Living.2017.day.SegLm.log = nlsLM(Ar.Jz.day.2017 ~ b*log10(k*Jz.mat[,6]),
                                     start = list(b = 0.08, k = 100))
Ar.Living.2018.day.SegLm.log = nlsLM(Ar.Jz.day.2018 ~ b*log10(k*Jz.mat[,6]),
                                     start = list(b = 0.08, k = 100))
Ar.Living.2019.day.SegLm.log = nlsLM(Ar.Jz.day.2019 ~ b*log10(k*Jz.mat[,6]),
                                     start = list(b = 0.08, k = 100))
Ar.Living.2020.day.SegLm.log = nlsLM(Ar.Jz.day.2020 ~ b*log10(k*Jz.mat[,6]),
                                     start = list(b = 0.08, k = 100))


#Jz ~ Alpha, exponential plateau, with segmented regression O2 consumption
# Ar.Living.2016.day.SegLm.exp = nlsLM(Ar.Jz.day.2016 ~ j.m - (j.m-b)*exp(-k*Jz.mat[-5,4]),
#                                      start = list(j.m = max(Ar.Jz.day.2016, na.rm=T), b = 0.2, k = 1))
Ar.Living.2017.day.SegLm.exp = nlsLM(Ar.Jz.day.2017 ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                     start = list(j.m = max(Ar.Jz.day.2017), b = 0.2, k = 10))

Ar.Living.2018.day.SegLm.exp = nlsLM(Ar.Jz.day.2018 ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                     start = list(j.m = max(Ar.Jz.day.2018), b = min(Ar.Jz.day.2018), k = 10),
                                     lower = c(1.4*max(Ar.Jz.day.2018),0,0),
                                     upper = c(2*max(Ar.Jz.day.2018), max(Ar.Jz.day.2018),15)) #R2 = 0.929

pred <- predict(Ar.Living.2018.day.SegLm.exp, Jz.mat[,6])
rss <- sum((pred - Ar.Jz.day.2018) ^ 2)
tss <- sum((temp - mean(Ar.Jz.day.2018)) ^ 2)
rsq <- 1 - rss/tss

Ar.Living.2019.day.SegLm.exp = nlsLM(Ar.Jz.day.2019 ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                     start = list(j.m = max(Ar.Jz.day.2019), b = 0.2, k = 10))
Ar.Living.2020.day.SegLm.exp = nlsLM(Ar.Jz.day.2020 ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                     start = list(j.m = max(Ar.Jz.day.2020), b = 0.2, k = 10))


#Compare linear O2 consumption with segmented one
{
pdf("./Output/Ar.Jz-Alpha.day.LinVsSegLm-LOGGER.TEST.pdf")
par(mfrow=c(2,2))
par(mar = c(5,5,4,2)+0.2)
#Linear O2 consumption
plot(Ar.Jz.day.2017.lin ~ Jz.mat[, 6], las =1,
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     ylim = c(0.04, 0.3),
     xlim = c(0.02, 0.3),
     main = expression(Linear~O[2]~consumption),
     col ="blue",pch=16)
curve(coef(Ar.Living.2017.day.lin.log)[1]*log10(x*coef(Ar.Living.2017.day.lin.log)[2]),
      add=T, col ="blue")
points(Ar.Jz.day.2016.lin ~ Jz.mat[,6], col="black", pch = 16)
curve(coef(Ar.Living.2016.day.lin.log)[1]*log10(x*coef(Ar.Living.2016.day.lin.log)[2]),
      add=T, col ="black")
points(Ar.Jz.day.2018.lin ~ Jz.mat[, 6], col ="chartreuse3",pch=16)
curve(coef(Ar.Living.2018.day.lin.log)[1]*log10(x*coef(Ar.Living.2018.day.lin.log)[2]),
      add=T, col ="chartreuse3")
points(Ar.Jz.day.2019.lin ~ Jz.mat[, 6], col = "red",pch=16)
curve(coef(Ar.Living.2019.day.lin.log)[1]*log10(x*coef(Ar.Living.2019.day.lin.log)[2]),
      add=T, col ="red")
points(Ar.Jz.day.2020.lin ~ Jz.mat[, 6], col = "pink",pch=16)
curve(coef(Ar.Living.2020.day.lin.log)[1]*log10(x*coef(Ar.Living.2020.day.lin.log)[2]),
      add=T, col ="pink")

legend("topleft", legend = c("2016","2017","2018","2019","2020"),
       text.col = c("black","blue", "chartreuse3","red","pink"))


#Segmented O2 consumption
plot(Ar.Jz.day.2017 ~ Jz.mat[, 6], las =1,
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     ylim = c(0.04, 0.3),
     xlim = c(0.02, 0.3),
     main = expression(Segmented~O[2]~consumption),
     col ="blue",pch=16)
curve(coef(Ar.Living.2017.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2017.day.SegLm.log)[2]),
      add=T, col ="blue")
points(Ar.Jz.day.2016 ~ Jz.mat[,6], col="black", pch = 16)
curve(coef(Ar.Living.2016.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2016.day.SegLm.log)[2]),
      add=T, col ="black")
points(Ar.Jz.day.2018 ~ Jz.mat[, 6], col ="chartreuse3",pch=16)
curve(coef(Ar.Living.2018.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2018.day.SegLm.log)[2]),
      add=T, col ="chartreuse3")
points(Ar.Jz.day.2019 ~ Jz.mat[, 6], col = "red",pch=16)
curve(coef(Ar.Living.2019.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2019.day.SegLm.log)[2]),
      add=T, col ="red")
points(Ar.Jz.day.2020 ~ Jz.mat[, 6], col = "pink",pch=16)
curve(coef(Ar.Living.2020.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2020.day.SegLm.log)[2]),
      add=T, col ="pink")


#Plot exponential plateau results
plot(Ar.Jz.day.2017.lin ~ Jz.mat[, 6], las =1,
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     ylim = c(0.04, 0.3),
     xlim = c(0.02, 0.3),
     main = expression(Linear~O[2]~consumption),
     col ="blue",pch=16)
curve(coef(Ar.Living.2017.day.lin.exp)[1] - (coef(Ar.Living.2017.day.lin.exp)[1] - 
                                                 coef(Ar.Living.2017.day.lin.exp)[2]) * 
        exp(-coef(Ar.Living.2017.day.lin.exp)[3]*x),
      add=T, col ="blue")
points(Ar.Jz.day.2018.lin ~ Jz.mat[, 6], col ="chartreuse3",pch=16)
curve(coef(Ar.Living.2018.day.lin.exp)[1] - (coef(Ar.Living.2018.day.lin.exp)[1] - 
                                                 coef(Ar.Living.2018.day.lin.exp)[2]) * 
        exp(-coef(Ar.Living.2018.day.lin.exp)[3]*x),
      add=T, col ="chartreuse3")
points(Ar.Jz.day.2019.lin ~ Jz.mat[, 6], col = "red",pch=16)
curve(coef(Ar.Living.2019.day.lin.exp)[1] - (coef(Ar.Living.2019.day.lin.exp)[1] - 
                                                 coef(Ar.Living.2019.day.lin.exp)[2]) * 
        exp(-coef(Ar.Living.2019.day.lin.exp)[3]*x),
      add=T, col ="red")

points(Ar.Jz.day.2020.lin ~ Jz.mat[, 6], col = "pink",pch=16)
curve(coef(Ar.Living.2020.day.lin.exp)[1] - (coef(Ar.Living.2020.day.lin.exp)[1] - 
                                                 coef(Ar.Living.2020.day.lin.exp)[2]) * 
        exp(-coef(Ar.Living.2020.day.lin.exp)[3]*x),
      add=T, col ="pink")

#Plot exponential plateau results
plot(Ar.Jz.day.2017 ~ Jz.mat[, 6], las =1,
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     ylim = c(0.04, 0.3),
     xlim = c(0.02, 0.3),
     main = expression(Segmented~O[2]~consumption),
     col ="blue",pch=16)
curve(coef(Ar.Living.2017.day.SegLm.exp)[1] - (coef(Ar.Living.2017.day.SegLm.exp)[1] - 
                                                 coef(Ar.Living.2017.day.SegLm.exp)[2]) * 
        exp(-coef(Ar.Living.2017.day.SegLm.exp)[3]*x),
      add=T, col ="blue")
points(Ar.Jz.day.2018 ~ Jz.mat[, 6], col ="chartreuse3",pch=16)
curve(coef(Ar.Living.2018.day.SegLm.exp)[1] - (coef(Ar.Living.2018.day.SegLm.exp)[1] - 
                                                 coef(Ar.Living.2018.day.SegLm.exp)[2]) * 
        exp(-coef(Ar.Living.2018.day.SegLm.exp)[3]*x),
      add=T, col ="chartreuse3")
points(Ar.Jz.day.2019 ~ Jz.mat[, 6], col = "red",pch=16)
curve(coef(Ar.Living.2019.day.SegLm.exp)[1] - (coef(Ar.Living.2019.day.SegLm.exp)[1] - 
                                                 coef(Ar.Living.2019.day.SegLm.exp)[2]) * 
        exp(-coef(Ar.Living.2019.day.SegLm.exp)[3]*x),
      add=T, col ="red")

points(Ar.Jz.day.2020 ~ Jz.mat[, 6], col = "pink",pch=16)
curve(coef(Ar.Living.2020.day.SegLm.exp)[1] - (coef(Ar.Living.2020.day.SegLm.exp)[1] - 
                                                 coef(Ar.Living.2020.day.SegLm.exp)[2]) * 
        exp(-coef(Ar.Living.2020.day.SegLm.exp)[3]*x),
      add=T, col ="pink")

dev.off()
}


#Test relationship between Linear and SegLm Jz
Jz.Logger.SegLm = c(Ar.Jz.2016,Ar.Jz.2017,Ar.Jz.2018,Ar.Jz.2019,Ar.Jz.2020, 
                    Ar.Jz.day.2016[-5],Ar.Jz.day.2017,Ar.Jz.day.2018,Ar.Jz.day.2019,Ar.Jz.day.2020)



#######################################################################################
###############################With only 3 loggers#####################################
#######################################################################################

ar.depths.short = c(30,45,47)
ar.year = c(2016,2017,2018,2019,2020)
Ar.full <- filter(.data = Full_data, lake == "ar")


#Jz determination with multiple methods
#Create matrix
Jz.mat.short = as.data.frame(matrix(nrow=length(ar.depths.short), ncol = length(ar.year)))
colnames(Jz.mat.short) = ar.year
rownames(Jz.mat.short) = ar.depths.short

#Select time stamps
Ar.full <- filter(.data = Full_data, lake == "ar")
ar.from.short <- matrix(c(1500,2000,1919,2000,1500,
                    1500,2000,1000,2000,1500,
                    2000,2000,2000,2000,1500), nrow=3, ncol=5, byrow = T)

ar.to.short <- matrix(c(7300,7500,7400,7844,7800,
                  4600,5000,3300,5200,4000,
                  4200,4500,4200,5000,4000), nrow=3, ncol=5, byrow = T)
Ar.seg.dates.short = matrix(nrow=15,ncol=4)
colnames(Ar.seg.dates.short) = c("First date", "First DO", "Second date", "Second DO")
rownames(Ar.seg.dates.short) = as.vector(outer(ar.year, ar.depths.short, paste, sep=" "))
rownames(Ar.seg.dates.short) = paste0(rownames(Ar.seg.dates.short), "m")

#Create AIC table for comparison
AIC.table.short = matrix(nrow=15, ncol=5)
colnames(AIC.table.short) = c("AIC.lin", "AIC.2", "AIC.3", "AIC.4", "AIC.exp")
rownames(AIC.table.short) = rownames(Ar.seg.dates.short)

#Look at different regressions between O2 and time
#Using linear, exponential or segmented regressions
Jz.seg.list.short = list()
Jz.seg.coef.short = list()
{
  counter=1
  pdf("./Exploration output/Ar.loggers.SegLm.TEST.short.pdf", width=8, height=2.5)
  par(mfrow=c(1,5))
  for(i in 1:length(ar.depths.short)){
    Ar.DO <- filter(Ar.full, depth == ar.depths.short[i]) %>% 
      filter(parameter == "DO_mgL") %>% 
      mutate(year = substring(CET, 1, 4)) %>% 
      mutate(mm_dd = substring(CET,6,10)) %>%
      mutate(mm = substring(mm_dd,1,2))
    
    for(j in 1:length(ar.year)){
      if(i==3 & j==1) {
        plot(1~1, col = "white")
        legend("center", legend="NA", bty = "n")
        counter = counter+1
        next    }
      Ar.DO.yr <- filter(Ar.DO, year == ar.year[j])
      Ar.DO.yr <- Ar.DO.yr[c(ar.from.short[i,j]:ar.to.short[i,j]),] %>% mutate(Dec.Day = seq(1,length(year), 1)/24)
      Ar.DO.yr$Season <- ifelse(as.numeric(Ar.DO.yr$mm) <= 5, "Spring", ifelse(as.numeric(Ar.DO.yr$mm) >= 9, "Autumn", "Summer"))
      
      #Linear model
      Dec.Day = Ar.DO.yr$Dec.Day
      value = Ar.DO.yr$value
      lin.mod = lm(value ~ Dec.Day)

      Dec.Day.segLm = c(seq(-400,0,1/24),Dec.Day, seq(Dec.Day[length(Dec.Day)]+1/24,Dec.Day[length(Dec.Day)]+400,1/24))
      value.segLm = c(rep(max(value),9601),value, rep(min(value),9600))
      lin.mod.segLm = lm(value.segLm ~ Dec.Day.segLm)
      
      plot(value~ Dec.Day,
           xlab = "Time",
           ylab = "DO (mg/L)",
           main = paste0(ar.year[j]," ", ar.depths.short[i], "m"))
      
      abline(lin.mod, lwd = 2, col = "cyan")
      
      AIC.table[counter,1] = round(AIC(lin.mod))
      #Piecewise model
      psi = c(50) #else psi = c(30)
      segmented.mod <- segmented(lin.mod.segLm, psi=psi)
      # plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "red", lwd=2)
      AIC.table[counter,2] = round(AIC(segmented.mod))
      
      psi = c(30,85) #else psi = c(30)
      segmented.mod <- segmented(lin.mod.segLm, psi=psi)
      plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "chartreuse3", lwd=2)
      AIC.table[counter,3] = round(AIC(segmented.mod))
      
      Jz.mat.short[i,j] = summary(lin.mod)$coefficients[2,1]*-1 #Multiply by -24 to have a positive consumption rate per day
      Jz.seg.list.short[[counter]] = segmented.mod
     Jz.seg.coef.short[[counter]] = segmented.mod$coefficients
      legend("topright", legend = c(Ar.seg.dates[counter,1],Ar.seg.dates[counter,3]))

      counter = counter+1
    }
  }
  dev.off()
}


#Calcule the Livingstone models with the hourly data
Ar.Jz.mat.short = matrix(unlist(Jz.seg.coef.short),nrow=6)
rownames(Ar.Jz.mat.short) = c("Intercept", "First slope", "Second slope", "Third slope", "Useless", "No_Use")
colnames(Ar.Jz.mat.short) = rownames(Ar.seg.dates.short)[-11]

Ar.Jz.short = vector(length=14)
#Select which slopes to choose from segmented regression
# Jz.index = c(0,0,1,0,0,
#              0,1,1,0,0,
#              0,0,0,0,0,
#              0,0,1,0,2,
#              1,0,0,0)
for(i in 1:length(Ar.Jz.short))
{
  # if(Jz.index[i] == 1) Ar.Jz[i] = Ar.Jz.mat[2,i]
  # if(Jz.index[i] == 0) Ar.Jz[i] = Ar.Jz.mat[2,i] + Ar.Jz.mat[3,i]
  # if(Jz.index[i] == 2) Ar.Jz[i] = Ar.Jz.mat[2,i] + Ar.Jz.mat[3,i] + Ar.Jz.mat[4,i]
  Ar.Jz.short[i] = Ar.Jz.mat.short[2,i] + Ar.Jz.mat.short[3,i]
}

Ar.Jz.short.2016 = Ar.Jz.short[c(1,6)]*-1
Ar.Jz.short.2017 = Ar.Jz.short[c(2,7,11)]*-1
Ar.Jz.short.2018 = Ar.Jz.short[c(2,7,11)+1]*-1
Ar.Jz.short.2019 = Ar.Jz.short[c(2,7,11)+2]*-1
Ar.Jz.short.2020 = Ar.Jz.short[c(2,7,11)+3]*-1
#Slice is written for depths, but needs +1 to account for 0m deep (it selects row numbers)
Jz.mat.short[,6] = Bats %>% slice(c(30,45,47)) %>% select(alpha)

#Log relationships
temp.object = Jz.mat.short[,6]
Ar.Living.short.2017.log = nlsLM(Ar.Jz.short.2017 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))
Ar.Living.short.2018.log = nlsLM(Ar.Jz.short.2018 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))
Ar.Living.short.2019.log = nlsLM(Ar.Jz.short.2019 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))
Ar.Living.short.2020.log = nlsLM(Ar.Jz.short.2020 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))


#Exponential plateau relationships
temp.object = Jz.mat.short[,6]
temp.max = max(Ar.Jz.short.2017)
Ar.Living.short.2017.exp = nlsLM(Ar.Jz.short.2017 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=0.05, k=5))
temp.max = max(Ar.Jz.short.2018)
temp.min = min(Ar.Jz.short.2018)
Ar.Living.short.2018.exp = nlsLM(Ar.Jz.short.2018 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=temp.min, k=5),
                           lower = c(1.4*temp.max,0,0),
                           upper = c(2*temp.max,temp.max,15))
temp.max = max(Ar.Jz.short.2019)
Ar.Living.short.2019.exp = nlsLM(Ar.Jz.short.2019 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=0.05, k=5))
temp.max = max(Ar.Jz.short.2020)
Ar.Living.short.2020.exp = nlsLM(Ar.Jz.short.2020 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=0.05, k=5))
rm(temp.object)

# {
#   png("./Output/Fig. Sx Three vs five loggers.png", width = 8.36, height = 4.4, units = "in", res = 300)
#   
# # png("./Output/Three loggers only.png", width = 8.36, height = 4.4, units = "in", res = 300)
# par(mfrow=c(1,2))
# par(mar=c(4,5,3,1)+.1)
# plot(Ar.Jz.2017 ~ Jz.mat[,6],
#      las = 1, pch = 16,
#      ylim = c(0,0.25),
#      ylab = expression(Jz~(mg~O[2]~L^-1)),
#      xlab = expression(alpha*(z)),
#      col = "blue",
#      main = "Log-linear")
# curve(coef(Ar.Living.short.2017.log)[1]*log10(x*coef(Ar.Living.short.2017.log)[2]),
#       add=T, col ="blue")
# curve(coef(Ar.Living.2017.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2017.day.SegLm.log)[2]),
#       add=T, col ="blue", lty=2)
# points(Ar.Jz.2018 ~ Jz.mat[,6], pch = 16, col = "chartreuse3")
# curve(coef(Ar.Living.short.2018.log)[1]*log10(x*coef(Ar.Living.short.2018.log)[2]),
#       add=T, col ="chartreuse3")
# curve(coef(Ar.Living.2018.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2018.day.SegLm.log)[2]),
#       add=T, col ="chartreuse3", lty=2)
# points(Ar.Jz.2019 ~ Jz.mat[,6], pch = 16, col = "red")
# curve(coef(Ar.Living.short.2019.log)[1]*log10(x*coef(Ar.Living.short.2019.log)[2]),
#       add=T, col ="red")
# curve(coef(Ar.Living.2019.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2019.day.SegLm.log)[2]),
#       add=T, col ="red", lty=2)
# points(Ar.Jz.2020 ~ Jz.mat[,6], pch = 16, col = "pink")
# curve(coef(Ar.Living.short.2020.log)[1]*log10(x*coef(Ar.Living.short.2020.log)[2]),
#       add=T, col ="pink")
# curve(coef(Ar.Living.2020.day.SegLm.log)[1]*log10(x*coef(Ar.Living.2020.day.SegLm.log)[2]),
#       add=T, col ="pink", lty=2)
# 
# legend("topleft", legend = c("2017","2018","2019","2020"),
#        text.col = c("blue", "chartreuse3","red","pink"))
# legend("bottomright", lty = c(1,2), legend = c("3 loggers fit", "5 loggers fit"))
# 
# 
# plot(Ar.Jz.2017 ~ Jz.mat[,6],
#      las = 1, pch = 16,
#      ylim = c(0,0.25),
#      ylab = expression(Jz~(mg~O[2]~L^-1)),
#      xlab = expression(alpha*(z)),
#      col = "blue",
#      main = "Exponential plateau")
# curve(coef(Ar.Living.short.2017.exp)[1] - (coef(Ar.Living.short.2017.exp)[1] - 
#                                              coef(Ar.Living.short.2017.exp)[2]) * 
#         exp(-coef(Ar.Living.short.2017.exp)[3]*x),
#       add=T, col ="blue")
# curve(coef(Ar.Living.2017.day.SegLm.exp)[1] - (coef(Ar.Living.2017.day.SegLm.exp)[1] - 
#                                              coef(Ar.Living.2017.day.SegLm.exp)[2]) * 
#         exp(-coef(Ar.Living.2017.day.SegLm.exp)[3]*x),
#       add=T, col ="blue", lty=2)
# 
# points(Ar.Jz.2018 ~ Jz.mat[,6], pch = 16, col = "chartreuse3")
# curve(coef(Ar.Living.short.2018.exp)[1] - (coef(Ar.Living.short.2018.exp)[1] - 
#                                              coef(Ar.Living.short.2018.exp)[2]) * 
#         exp(-coef(Ar.Living.short.2018.exp)[3]*x),
#       add=T, col ="chartreuse3")
# curve(coef(Ar.Living.2018.day.SegLm.exp)[1] - (coef(Ar.Living.2018.day.SegLm.exp)[1] - 
#                                                  coef(Ar.Living.2018.day.SegLm.exp)[2]) * 
#         exp(-coef(Ar.Living.2018.day.SegLm.exp)[3]*x),
#       add=T, col ="chartreuse3", lty=2)
# points(Ar.Jz.2019 ~ Jz.mat[,6], pch = 16, col = "red")
# curve(coef(Ar.Living.short.2019.exp)[1] - (coef(Ar.Living.short.2019.exp)[1] - 
#                                              coef(Ar.Living.short.2019.exp)[2]) * 
#         exp(-coef(Ar.Living.short.2019.exp)[3]*x),
#       add=T, col ="red")
# curve(coef(Ar.Living.2019.day.SegLm.exp)[1] - (coef(Ar.Living.2019.day.SegLm.exp)[1] - 
#                                                  coef(Ar.Living.2019.day.SegLm.exp)[2]) * 
#         exp(-coef(Ar.Living.2019.day.SegLm.exp)[3]*x),
#       add=T, col ="red", lty=2)
# points(Ar.Jz.2020 ~ Jz.mat[,6], pch = 16, col = "pink")
# curve(coef(Ar.Living.short.2020.exp)[1] - (coef(Ar.Living.short.2020.exp)[1] - 
#                                              coef(Ar.Living.short.2020.exp)[2]) * 
#         exp(-coef(Ar.Living.short.2020.exp)[3]*x),
#       add=T, col ="pink")
# curve(coef(Ar.Living.2020.day.SegLm.exp)[1] - (coef(Ar.Living.2020.day.SegLm.exp)[1] - 
#                                                  coef(Ar.Living.2020.day.SegLm.exp)[2]) * 
#         exp(-coef(Ar.Living.2020.day.SegLm.exp)[3]*x),
#       add=T, col ="pink", lty=2)
# dev.off()
# }



##########################################################################################
###################Calculate Jz Arendsee using high temporal resolution###################
##########################################################################################

#################################THIS SECTION WORKS BY ITSELF############################
##################################CAN BE ITS OWN SCRIPT WHEN#############################
#################################OTHER SECTIONS ARE VERIFIED#############################

#Load all file names
YSI.list = list.files("./Data/Raw/Arendsee-cleaned")
Bats <- read.csv("./Data/Raw/Arendsee.alpha.csv")
#Keep only csv files
YSI.list = YSI.list[grep(pattern = ".csv",list.files("./Data/Raw/Arendsee-cleaned"))]
YSI.yrs = c(2017,2018,2019,2020,2021)
Jz.YSI.list = list()
Jz.YSI.list.err = list()
Jz.YSI.lin.list = list()
Jz.YSI.lin.list.err = list()


#These cutoff were chosen a posteriori using the breakpoint thresholds, then manually adjusted
YSI.17.from = c(70, 73, 79,79,80,79,79,78,78,79,77,77,79,84,85,86,87,90,100)
YSI.17.to = c(315,314,308,306,306,308,309,306,299,307,307,305,297,279,270,262,250,229,202)

YSI.18.from = c(91,93,94,94,95,96,96,97,96,96,95,105,110,110,110,110,110,110)
YSI.18.to = c(330,328,339,338,336,339,329,326,326,327,328,300,275,260,250,239,223,210)

YSI.19.from = c(100,102,101,102,101,101,101,102,102,101,101,101,99,98,95,95,94,93)
YSI.19.to = c(314,301,308,305,304,300,311,305,300,280,278,270,266,259,250,239,230,210)

YSI.20.from = c(105,107,108,106,106,106,107,107,107,107,107,107,107,108,107,107,105,105)
YSI.20.to = c(253,251,251,253,252,250,250,245,240,233,230,219,212,206,200,195,189,181)

YSI.21.from = c(118,120,123,123,123,120,120,120,120,117,119,119,119,119,120,120,125,125)
YSI.21.to = c(255,250,255,255,255,244,241,230,225,222,222,218,214,205,199,188,183,175)
  
for(i in 1:length(YSI.yrs)){
#Read the file
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


#Find Jz for each depth
Jz.YSI = vector(length = length(unique(Ar.long.deep$Depth_m)))
Jz.YSI.err = Jz.YSI
Jz.YSI.lin = Jz.YSI
Jz.YSI.lin.err = Jz.YSI.lin
#Note. After looking at the pdf once, the second slope in the piecewise regression is always right
pdf(paste0("./Exploration output/Ar.YSI.",YSI.yrs[i],".pdf"))
for(k in 1:length(Jz.YSI)){
  Ar.temp <- Ar.long.deep[Ar.long.deep$Depth_m == unique(Ar.long.deep$Depth_m)[k],]
  if(YSI.yrs[i] == 2017) {
    Ar.temp = Ar.temp[Ar.temp$DOY>YSI.17.from[k] & Ar.temp$DOY < YSI.17.to[k],]
    psi = c(100,200)}
  if(YSI.yrs[i] == 2018) {
    Ar.temp = Ar.temp[Ar.temp$DOY>YSI.18.from[k] & Ar.temp$DOY <  YSI.18.to[k],] 
    psi = c(110,200)}
  if(YSI.yrs[i] == 2019) {
    Ar.temp = Ar.temp[Ar.temp$DOY>YSI.19.from[k] & Ar.temp$DOY < YSI.19.to[k],]
    psi = c(100,200)}
  if(YSI.yrs[i] == 2020) {
    Ar.temp = Ar.temp[Ar.temp$DOY>YSI.20.from[k] & Ar.temp$DOY <  YSI.20.to[k],]
    psi = c(110,200)}
  if(YSI.yrs[i] == 2021) {
    Ar.temp = Ar.temp[Ar.temp$DOY>YSI.21.from[k] & Ar.temp$DOY < YSI.21.to[k],]
    psi = c(110,200)}
  
  
  
  Dec.Day = Ar.temp$DOY
  value = Ar.temp$DO_mgL
  
  if(any(is.na(Dec.Day))) {
    Dec.Day.na = which(is.na(Dec.Day))
    Dec.Day = Dec.Day[-Dec.Day.na]
    value = value[-Dec.Day.na]
  }
  if(any(is.na(value))) {
    value.na = which(is.na(value))
    Dec.Day = Dec.Day[-value.na]
    value = value[-value.na]
  }
  
  lin.mod = lm(value[value != min(value)] ~ Dec.Day[value != min(value)])
  print(c(length(value[value != min(value)]),unique(Ar.long.deep$Depth_m)[k]))
  
  Dec.Day.segLm = c(seq(Dec.Day[1]-200,Dec.Day[1]-1,1),Dec.Day, seq(Dec.Day[length(Dec.Day)]+1,Dec.Day[length(Dec.Day)]+300,1))
  value.dummy = max(value)
  value.segLm = c(rep(value.dummy,200),value, rep(min(value),300))
  lin.mod.segLm = lm(value.segLm ~ Dec.Day.segLm)
  
  plot(value ~ Dec.Day, las = 1,
       main = paste(unique(Ar.long.deep$Depth_m)[k], "m", sep=" "),
       ylab = expression(DO~mg~L^-1),
       xlab = "Day of year",
       ylim = c(0,13))
  abline(lin.mod, lwd = 2, col = "cyan")
  
  #Piecewise model
  segmented.mod <- segmented(lin.mod.segLm, psi=psi)
  plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "chartreuse3", lwd=2)
  legend("topright", legend = c(paste("First breakpoint is", round(segmented.mod$psi[1,2]), "day", sep = " "),
                                paste("Second breakpoint is", round(segmented.mod$psi[2,2]), "day", sep = " ")))
  
  Jz.YSI[k] = (segmented.mod$coefficients[2] + segmented.mod$coefficients[3])*-1
  Jz.YSI.err[k] = summary(segmented.mod)$coef[2,2] + summary(segmented.mod)$coef[3,2]
  Jz.YSI.lin[k] = summary(lin.mod)$coef[2,1] * -1
  Jz.YSI.lin.err[k] = summary(lin.mod)$coef[2,2]
}
dev.off()
Jz.YSI.list[[i]] = Jz.YSI
Jz.YSI.list.err[[i]] = Jz.YSI.err
Jz.YSI.lin.list[[i]] = Jz.YSI.lin
Jz.YSI.lin.list.err[[i]] = Jz.YSI.lin.err

#Select the right alphas
#2017: 33:48
#2018-2020: 30:47
if(i == 1){
  alpha.YSI.list=list()
  alpha.YSI.list[[1]] = Bats %>% slice(c(33:48)) %>% select(alpha)
}
if(i == 2) alpha.YSI.list[[2]] = Bats %>% slice(c(30:47)) %>% select(alpha)
}


#Plot both methods to see adequation. 2021 should be really different based on graph
#And both methods seem wrong (SegLm overestimate Jz and Lm underestimate it); the shape 
#of O2 consumption at depth is highly unusual

# {
#   png("./Output/Fig. Sx SegLm vs Linear Jz.png", width = 11.4, height = 8, units = "in", res=300)
#   #pdf("./Output/Fig. Sx SegLm vs Linear Jz.pdf")
#   par(mfrow=c(2,3))
#   for(i in 1:5){
#     plot(Jz.YSI.list[[i]] ~ Jz.YSI.lin.list[[i]], las = 1,
#          ylab = "Jz (Segmented)",
#          xlab = "Jz (Linear)",
#          main = c(2017,2018,2019,2020,2021)[i])
#     abline(lm(Jz.YSI.list[[i]] ~ Jz.YSI.lin.list[[i]]))
#     abline(0,1, lty = 2)
#   }
#   plot(JzLogger.vec.lin[1:24] ~ Jz.Logger.SegLm[1:24] ,
#        ylab = "Jz (linear fit)", 
#        xlab = "Jz (segmented fit)",
#        las = 1,
#        main = "Loggers")
#   abline(lm(JzLogger.vec.lin[1:24] ~Jz.Logger.SegLm[1:24]))
#   abline(0,1, lty=2)
#   dev.off()}

{
  png("./Output/Fig. S1 SegLm vs Linear Jz.png", width = 8.36, height = 4.4, units = "in", res=300)
  #pdf("./Output/Fig. S1 SegLm vs Linear Jz.pdf")
  par(mfrow=c(1,2))
  plot(Jz.YSI.lin.list[[1]] ~ Jz.YSI.list[[1]],
       ylab = "Jz (linear fit)", 
       xlab = "Jz (segmented fit)",
       ylim = c(0.03, 0.3),
       xlim = c(0.03, 0.45),
       las = 1, pch = 16, col = "blue",
       main = "YSI profiler")
  for(i in 2:5){
    points(Jz.YSI.lin.list[[i]] ~ Jz.YSI.list[[i]],
         pch = 16, col = c("chartreuse3","red","pink","#73BAE6")[i-1])  }
  abline(lm(unlist(Jz.YSI.lin.list) ~ unlist(Jz.YSI.list)))
  abline(0,1, lty = 2)
  
  legend("topleft", legend = c("2016","2017","2018","2019","2020","2021"),
         text.col = c("black","blue", "chartreuse3","red","pink","#73BAE6"))
  
  legend("bottomright", lty = c(1,2), legend = c("regression fit", "1:1 fit"))
  
  plot(JzLogger.vec.lin[1:24] ~ Jz.Logger.SegLm[1:24] ,
       xlim = c(0.025, 0.25),
       ylim = c(0.04, 0.2),
       ylab = "Jz (linear fit)", 
       xlab = "Jz (segmented fit)",
       las = 1, pch = 16,
       col = c("black","black","black","black",
               "blue","blue","blue","blue","blue",
               "chartreuse3","chartreuse3","chartreuse3","chartreuse3","chartreuse3",
               "red","red","red","red","red",
               "pink","pink","pink","pink","pink"),
       main = "Loggers")
  abline(lm(JzLogger.vec.lin[1:24] ~Jz.Logger.SegLm[1:24]))
  abline(0,1, lty=2)
  dev.off()
  }

summary(lm(JzLogger.vec.lin[1:24]~Jz.Logger.SegLm[1:24])) #R2 = 0.9599



#RMSE = sqrt(mean(summary(Object)$residuals^2))
#AIC = AIC(Object)

#Linear regressions
Ar.Jz.alpha.YSI.2017 = lm(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1]) 
#R2 = 0.993; AIC = -146.5; RMSE = 0.002059
Ar.Jz.alpha.YSI.2018 = lm(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1]) 
#R2 = 0.966; AIC = -119.55; RMSE = 0.007398443
Ar.Jz.alpha.YSI.2019 = lm(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1]) 
#R2 = 0.90; AIC = -133.856; RMSE = 0.004972443
Ar.Jz.alpha.YSI.2020 = lm(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1]) 
#R2 = 0.86; AIC = -109.9361; RMSE = 0.009663632
Ar.Jz.alpha.YSI.2021 = lm(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1]) 
#R2 = 0.98; AIC = -105.2641; RMSE = 0.01100277

#Log-linear and exponential plateau regressions

#2017
temp = Jz.YSI.list[[1]][-c(1:3)]
temp.max = max(temp)
alpha = alpha.YSI.list[[1]][,1]
Ar.Jz.alpha.YSI.2017.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200))
#R2 = 0.876; AIC = -100.1125; RMSE = 0.008782861
Ar.Jz.alpha.YSI.2017.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
#R2 = 0.9926; AIC = -144.405; RMSE = 0.002067128


#2018
temp = Jz.YSI.list[[2]]
temp.max = max(temp)
alpha = alpha.YSI.list[[2]][,1]
Ar.Jz.alpha.YSI.2018.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200)) 
#R2 = 0.9528; AIC = -113.2695; RMSE = 0.008808999
Ar.Jz.alpha.YSI.2018.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
#R2 = 0.9930; AIC = 145.7226; RMSE = 0.003382937



#2019
temp = Jz.YSI.list[[3]]
temp.max = max(temp)
Ar.Jz.alpha.YSI.2019.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200))
#R2 = 0.999; AIC = -221.2; RMSE = 0.00043
Ar.Jz.alpha.YSI.2019.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
#R2 = 0.993; AIC = -180.7806; RMSE = 0.00127



#2020
temp = Jz.YSI.list[[4]]
temp.max = max(temp)
Ar.Jz.alpha.YSI.2020.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200))
#R2 = 0.9918; AIC = ; RMSE = 
Ar.Jz.alpha.YSI.2020.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
#R2 = 0.997; AIC = -179.1 ; RMSE = 0.0013

#Test with shorter dataset, from 5 to 15 depths in hypolimnion. 
Ar.Jz.alpha.YSI.2020.log.short = list()
for(i in 5:15){
  temp = Jz.YSI.list[[4]][1:i]
  temp.max = max(temp)
  alpha.short = alpha[1:i]
  Ar.Jz.alpha.YSI.2020.log.short[[i-4]] = nlsLM(temp ~ b*log10(k*alpha.short),
                                 start = list(b=0.08, k=200))
}

#Repeat, but with exponential plateau model
Ar.Jz.alpha.YSI.2020.exp.short = list()
for(i in 5:15){
  temp = Jz.YSI.list[[4]][1:i]
  temp.max = max(temp)
  temp.min = min(temp)
  alpha.short = alpha[1:i]
  Ar.Jz.alpha.YSI.2020.exp.short[[i-4]] = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha.short),
                                                start = list(t.m = temp.max, b=temp.min, k=5),
                                                lower = c(1.4*temp.min, 0,0),
                                                upper = c(2*temp.max, temp.max,15)) #THERE WAS NO LOWER BEFORE
}


#2021
temp = Jz.YSI.list[[5]]
temp.max = max(temp)
Ar.Jz.alpha.YSI.2021.log = nlsLM(temp ~ b*log10(k*alpha), 
                                 start = list(b=0.08, k=200))
#R2 = 0.937, AIC = -81.67; RMSE = 0.0212
Ar.Jz.alpha.YSI.2021.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
#R2 = 0.994, AIC = -122.2; RMSE = 0.0065


# pred <- predict(Ar.Jz.alpha.YSI.2021.log, alpha)
# rss <- sum((pred - temp) ^ 2)
# tss <- sum((temp - mean(temp)) ^ 2)
# rsq <- 1 - rss/tss

rm(temp)
rm(temp.max)
rm(alpha)


#To calculate R²
# pred <- predict(Ar.Jz.alpha.YSI.2017.log, alpha.YSI.list[[1]][,1])
# rss <- sum((pred - temp) ^ 2)
# tss <- sum((temp - mean(temp)) ^ 2)
# rsq <- 1 - rss/tss

#Plot the results (without 2017 as it is weird, but also follows an exponential fit)
{
pdf("./Output/Ar.Jz-Alpha.HighTemporalRes.TEST.pdf", width = 9, height = 3.25)
par(mfrow=c(1,3))
par(mar = c(5,5,4,2)+0.2)

plot(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1],
     las = 1,
     xlim = c(0,0.25),
     ylim = c(0.05, 0.40),
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     main = "Linear",
     pch = 16, col = "chartreuse3")
# abline(Ar.Jz.alpha.YSI.2018, col = "chartreuse3")
points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
       pch = 16, col = "blue")
 abline(Ar.Jz.alpha.YSI.2017, col = "blue")
points(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "red")
# abline(Ar.Jz.alpha.YSI.2019, col = "red")
points(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "pink")
# abline(Ar.Jz.alpha.YSI.2020, col = "pink")
points(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "#73BAE6")
# abline(Ar.Jz.alpha.YSI.2021, col = "#73BAE6")
legend("topleft", legend = c("2017","2018","2019","2020", "2021"),
       text.col = c("blue","chartreuse3","red","pink", "#73BAE6"))


plot(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1],
     las = 1,
     xlim = c(0,0.25),
     ylim = c(0.05, 0.40),
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     main = "Log-linear",
     pch = 16, col = "chartreuse3")
# curve(coef(Ar.Jz.alpha.YSI.2018.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2018.log)[2]),
#       add=T, col ="chartreuse3")
points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
       pch = 16, col = "blue")
# curve(coef(Ar.Jz.alpha.YSI.2017.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2017.log)[2]),
#       add=T, col ="blue")
points(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "red")
curve(coef(Ar.Jz.alpha.YSI.2019.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2019.log)[2]),
      add=T, col ="red")
points(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "pink")
curve(coef(Ar.Jz.alpha.YSI.2020.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2020.log)[2]),
      add=T, col ="pink")
points(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "#73BAE6")
# curve(coef(Ar.Jz.alpha.YSI.2021.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2021.log)[2]),
#       add=T, col ="#73BAE6")

plot(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1],
     las = 1,
     xlim = c(0,0.25),
     ylim = c(0.05, 0.40),
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     main = "Exponential plateau",
     pch = 16, col = "chartreuse3")

curve(coef(Ar.Jz.alpha.YSI.2018.exp)[1] - (coef(Ar.Jz.alpha.YSI.2018.exp)[1] - 
        coef(Ar.Jz.alpha.YSI.2018.exp)[2])*
        exp(-1*coef(Ar.Jz.alpha.YSI.2018.exp)[3]*x), 
      add=T, col = "chartreuse3")

points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
       pch = 16, col = "blue")
curve(coef(Ar.Jz.alpha.YSI.2017.exp)[1] - (coef(Ar.Jz.alpha.YSI.2017.exp)[1] - 
                                             coef(Ar.Jz.alpha.YSI.2017.exp)[2])*
        exp(-1*coef(Ar.Jz.alpha.YSI.2017.exp)[3]*x), 
      add=T, col = "blue")

points(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "red")
curve(coef(Ar.Jz.alpha.YSI.2019.exp)[1] - (coef(Ar.Jz.alpha.YSI.2019.exp)[1] - 
        coef(Ar.Jz.alpha.YSI.2019.exp)[2])*
        exp(-1*coef(Ar.Jz.alpha.YSI.2019.exp)[3]*x), 
      add=T, col = "red")

points(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "pink")
curve(coef(Ar.Jz.alpha.YSI.2020.exp)[1] - (coef(Ar.Jz.alpha.YSI.2020.exp)[1] - 
        coef(Ar.Jz.alpha.YSI.2020.exp)[2])*
        exp(-1*coef(Ar.Jz.alpha.YSI.2020.exp)[3]*x), 
      add=T, col = "pink")

points(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "#73BAE6")
curve(coef(Ar.Jz.alpha.YSI.2021.exp)[1] - (coef(Ar.Jz.alpha.YSI.2021.exp)[1] - 
        coef(Ar.Jz.alpha.YSI.2021.exp)[2])*
        exp(-1*coef(Ar.Jz.alpha.YSI.2021.exp)[3]*x), 
      add=T, col = "#73BAE6")

dev.off()
}


#Comparison of different thickness of oxic hypolimnion
#This simulates the second profile being anoxic, thus creating a bad Jz estimation
png("./Output/Fig S4 Comparison of Jz with shallower hypolimnion Jz.png", res = 300, units = "in", height = 4, width = 8)
par(mfrow=c(1,2))
par(mar=c(4,5,3,1)+.1)
plot(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
     ylim = c(0.05,0.2),
     xlim = c(0, 0.25),
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     main = "Log-linear",
     las = 1)
curve(coef(Ar.Jz.alpha.YSI.2020.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2020.log)[2]),
      add=T, col ="blue", lwd = 2, to = 0.235)
for(i in 1:11){
  curve(coef(Ar.Jz.alpha.YSI.2020.log.short[[i]])[1]*log10(x*coef(Ar.Jz.alpha.YSI.2020.log.short[[i]])[2]),
        add=T, col ="black", lwd = 1, to = 0.235)
}
legend("topleft", lty = 1, col= c("blue", "black"), legend = c("Full profile", "Short profiles"), cex = 0.6)
mtext(text = "a", side = 3, at = -0.01)

plot(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
     ylim = c(0.05,0.2),
     xlim = c(0, 0.25),
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     main = "Exponential plateau",
     las = 1)
curve(coef(Ar.Jz.alpha.YSI.2020.exp)[1] - (coef(Ar.Jz.alpha.YSI.2020.exp)[1] - 
                                             coef(Ar.Jz.alpha.YSI.2020.exp)[2])*
        exp(-1*coef(Ar.Jz.alpha.YSI.2020.exp)[3]*x), 
      add=T, col = "Blue", lwd = 2, to = 0.235)
for(i in 1:11){
  curve(coef(Ar.Jz.alpha.YSI.2020.exp.short[[i]])[1] - (coef(Ar.Jz.alpha.YSI.2020.exp.short[[i]])[1] - 
                                               coef(Ar.Jz.alpha.YSI.2020.exp.short[[i]])[2])*
          exp(-1*coef(Ar.Jz.alpha.YSI.2020.exp.short[[i]])[3]*x), 
        add=T, col = "Black", to = 0.235)
  # text(x = c(rep(c(0.245,0.255),5),0.255)[i], y =  coef(Ar.Jz.alpha.YSI.2020.exp.short[[i]])[1] - (coef(Ar.Jz.alpha.YSI.2020.exp.short[[i]])[1] - 
  #                                                                             coef(Ar.Jz.alpha.YSI.2020.exp.short[[i]])[2])*
  #                             exp(-1*coef(Ar.Jz.alpha.YSI.2020.exp.short[[i]])[3]*0.25), 
  #      labels = seq(5,15,1)[i], cex = 0.5)
}
# legend("topleft", legend=("n: number of depths considered"), cex = 0.5)
mtext(text = "b", side = 3, at = -0.01)

dev.off()


###########################################################################################
###########Calculate Jz Arendsee using low temporal resolution - 2 oxic profiles###########
###########################################################################################

#2017: 119-187 #Now 94-200
#2018: 133-199 #Now 122-182
#2019: 117-200 #Now 106-200
#2020: 124-179 #Now 114-182
#2020: 130-169 #Now 143-165

DOY.ini = c(94,122,108,114,115)
DOY.end = c(200,182,201,182, 169)

#Create R2 matrix
R2.mat = matrix(nrow=5, ncol = 3)
rownames(R2.mat) = c("2017", "2018", "2019", "2020", "2021")
colnames(R2.mat) = c("Lin", "Log-lin", "Exp")
AIC.mat <- RMSE.mat <- R2.mat

#With matching YSI profiles
YSI.list = list.files("./Data/Raw/Arendsee-cleaned")
#Keep only csv files
YSI.list = YSI.list[grep(pattern = ".csv",list.files("./Data/Raw/Arendsee-cleaned"))]

Bats <- read.csv("./Data/Raw/Arendsee.alpha.csv")
Jz.lowres.slice.list = list()
ar.alpha.list = list()

Ar.Jz.lm.best.list = list()
Jz.lowres = list()
for(i in 1:length(YSI.list)){
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
  
  #Original method
  # #Keep only matching date to Rinko profiles
  # Ar.long.deep.lowres <- Ar.long.deep[which(Ar.long.deep$DOY==DOY.ini[i] | Ar.long.deep$DOY==DOY.end[i]),]
  # Ar.long.deep.lowres.a <- Ar.long.deep.lowres %>% 
  #   group_by(Depth_m, DOY) %>% summarize(DO_mgL = mean(DO_mgL)) 
  # Jz.lowres = vector(length = nrow(Ar.long.deep.lowres.a)/2)
  # Depth.lowres = unique(Ar.long.deep.lowres.a$Depth_m)
  # DOY.lowres = max(Ar.long.deep.lowres.a$DOY) - min(Ar.long.deep.lowres.a$DOY)
  # 
  # 
  # for(a in 1:(nrow(Ar.long.deep.lowres.a)/2)){
  #   Jz.lowres[a] = (max(Ar.long.deep.lowres.a[Ar.long.deep.lowres.a$Depth_m==Depth.lowres[a],"DO_mgL"]) - 
  #                     min(Ar.long.deep.lowres.a[Ar.long.deep.lowres.a$Depth_m==Depth.lowres[a],"DO_mgL"]))/DOY.lowres
  # }

  # #TheilSen slopes
  # #Keep only profiles within 2 dates
  # Ar.long.deep = Ar.long.deep[Ar.long.deep$DOY >= DOY.ini[i] & Ar.long.deep$DOY <= DOY.end[i],]
  # 
  # #Keep only values higher than 2mgO2/L
  # Ar.long.deep = Ar.long.deep[Ar.long.deep$DO_mgL >=2,]
  # 
  # Depths = unique(Ar.long.deep$Depth_m)
  # Jz.lowres = matrix(nrow=length(Depths), ncol = 2)
  # colnames(Jz.lowres) = c("Slope", "MAD") 
  # for(j in 1:length(Depths)){
  #   Ar.long.deep.temp = Ar.long.deep[Ar.long.deep$Depth_m == Depths[j],]
  #   TheilSen.store = TheilSen(x = Ar.long.deep.temp$DOY, y = Ar.long.deep.temp$DO_mgL)
  #   Jz.lowres[j,1] = unlist(TheilSen.store[2])
  #   Jz.lowres[j,2] = unlist(TheilSen.store[3])
  # }
  
  
  #2 oxic profiles - 28 days apart
  #Keep only profiles within 2 dates
  Ar.long.deep = Ar.long.deep[Ar.long.deep$DOY >= DOY.ini[i] & Ar.long.deep$DOY <= DOY.end[i],]
  
  #Keep only values higher than 2mgO2/L
  Ar.long.deep = Ar.long.deep[Ar.long.deep$DO_mgL >=2,]
  
  Depths = unique(Ar.long.deep$Depth_m)
  Jz.lowres = matrix(nrow=length(Depths), ncol = 2)
  colnames(Jz.lowres) = c("Slope", "Error") 
  rownames(Jz.lowres) = Depths
  for(j in 1:length(Depths)){
    Ar.long.deep.temp = Ar.long.deep[Ar.long.deep$Depth_m == Depths[j],]
    Ar.long.deep.temp = Ar.long.deep.temp %>% group_by(DOY) %>% summarize(DO_mgL = mean(DO_mgL))
    
    #Fill in missing days (linear interpolation)
    Ar.long.filled = t(Ar.long.deep.temp)
    colnames(Ar.long.filled) = Ar.long.filled[1,]
    Ar.long.filled = t(as.matrix(Ar.long.filled[-1,]))
    Ar.long.filled = interpolation_fct(Ar.long.filled)
    
    #Calculate Jz between each pair of points
    Slope.temp = vector(length=length(Ar.long.filled)-28)
    for(k in 1:(length(Ar.long.filled)-28)){
      Slope.temp[k] = (Ar.long.filled[,k]-Ar.long.filled[,k+28])/28
    }
    Jz.lowres[j,1] = mean(Slope.temp)
    Jz.lowres[j,2] = sd(Slope.temp)/sqrt(length(Slope.temp))
    # print(c(length(Slope.temp), Depths[j]))
  }
slice.beg = c(30,30,30,30,30)
slice.end = c(47,47,47,47,47)

# Jz.lowres.end = c(8,3,3,3,4)


ar.alpha <- Bats%>% slice(slice.beg[i]:slice.end[i]) %>% select(alpha)
# if(i == 1) ar.alpha = as.matrix(ar.alpha[-11,])
ar.alpha.list[[i]] = ar.alpha
Jz.lowres.slice = Jz.lowres #[-c(1:Jz.lowres.end[i])]# Remove the first few  values because they are identical (likely because sediments to volume is too low)
if(i == 1) Jz.lowres.slice = Jz.lowres.slice[-19,]
Jz.lowres.slice.list[[i]] = Jz.lowres.slice

plot(Jz.lowres.slice[,1] ~ ar.alpha[,1])
Ar.Jz.lowres = lm(Jz.lowres.slice[,1] ~ ar.alpha[,1])
# Ar.Jz.lowres.log = lm(Jz.lowres.slice ~ log10(ar.alpha[,1]))

#Save relevant comparison metrics
R2.mat[i,1] = summary(Ar.Jz.lowres)$r.squared
AIC.mat[i,1] = AIC(Ar.Jz.lowres)
RMSE.mat[i,1] = sqrt(mean(Ar.Jz.lowres$residuals^2))

temp = Jz.lowres.slice[,1]
temp.max = max(Jz.lowres.slice[,1])
temp.min = min(Jz.lowres.slice[,1])
Ar.Jz.lowres.log = nlsLM(temp ~ b*log10(k*ar.alpha[,1]),
                                 start = list(b=0.08, k=200))

#Calculate R2
pred <- predict(Ar.Jz.lowres.log, ar.alpha[,1])
rss <- sum((pred - temp) ^ 2)
tss <- sum((temp - mean(temp)) ^ 2)
rsq <- 1 - rss/tss

#Save relevant comparison metrics
R2.mat[i,2] = rsq
AIC.mat[i,2] = AIC(Ar.Jz.lowres.log)
RMSE.mat[i,2] = sqrt(mean(summary(Ar.Jz.lowres.log)$residuals^2))

Ar.Jz.lowres.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*ar.alpha[,1]),
                         start = list(t.m = 1.6*temp.max, b=temp.min, k=5))
pred <- predict(Ar.Jz.lowres.exp, ar.alpha[,1])
rss <- sum((pred - temp) ^ 2)
tss <- sum((temp - mean(temp)) ^ 2)
rsq <- 1 - rss/tss

#Save relevant comparison metrics
R2.mat[i,3] = rsq
AIC.mat[i,3] = AIC(Ar.Jz.lowres.exp)
RMSE.mat[i,3] = sqrt(mean(summary(Ar.Jz.lowres.exp)$residuals^2))

# pred <- predict(Ar.Jz.alpha.YSI.2021.log, alpha)
# rss <- sum((pred - temp) ^ 2)
# tss <- sum((temp - mean(temp)) ^ 2)
# rsq <- 1 - rss/tss
Ar.Jz.lm.best.list[[i]] = list(Ar.Jz.lowres, Ar.Jz.lowres.log, Ar.Jz.lowres.exp)
}

best.2017.short.lm = lm(Jz.lowres.slice.list[[1]][,1] ~ ar.alpha.list[[1]][,1])

{
  pdf("./Output/Ar.Jz-Alpha.BestLowRes.ST.pdf", width = 8, height = 3.3)
  par(mfrow=c(1,3))
  par(mar = c(5,5,4,2)+0.2)
  
  plot(Jz.lowres.slice.list[[1]][,1] ~ ar.alpha.list[[1]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.3),
       xlab = expression(alpha(z)),
       ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "Linear",
       pch = 16, col = "blue")
  # abline(best.2017.short.lm, col = "blue")
  points(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.list[[2]][,1],
         pch = 16, col = "chartreuse3")
  # abline(Ar.Jz.lm.best.list[[2]][[1]], col = "chartreuse3")
  points(Jz.lowres.slice.list[[3]][,1] ~ ar.alpha.list[[3]][,1],
         pch = 16, col = "red")
  # abline(Ar.Jz.lm.best.list[[3]][[1]], col = "red")
  points(Jz.lowres.slice.list[[4]][,1] ~ ar.alpha.list[[4]][,1],
         pch = 16, col = "pink")
  # abline(Ar.Jz.lm.best.list[[4]][[1]], col = "pink")
  points(Jz.lowres.slice.list[[5]][,1] ~ ar.alpha.list[[5]][,1],
         pch = 16, col = "#00B0F0")
  # abline(Ar.Jz.lm.best.list[[5]][[1]], col = "#00B0F0")
  
  legend("topleft", legend = c("2017","2018","2019","2020", "2021"),
         text.col = c("blue","chartreuse3","red","pink", "#00B0F0"))
  
  plot(Jz.lowres.slice.list[[1]][,1] ~ ar.alpha.list[[1]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.3),
       xlab = expression(alpha(z)),
       ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "Log-linear",
       pch = 16, col = "blue")
  # curve(coef(Ar.Jz.lm.best.list[[1]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[1]][[2]])[2]),
  #        add=T, col ="blue", lty = 2)
  points(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.list[[2]][,1],
         pch = 16, col = "chartreuse3")
  curve(coef(Ar.Jz.lm.best.list[[2]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[2]][[2]])[2]),
        add=T, col ="chartreuse3", lty = 2)
  points(Jz.lowres.slice.list[[3]][,1] ~ ar.alpha.list[[2]][,1],
         pch = 16, col = "red")
  curve(coef(Ar.Jz.lm.best.list[[3]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[3]][[2]])[2]),
        add=T, col ="red", lty = 2)
  points(Jz.lowres.slice.list[[4]][,1] ~ ar.alpha.list[[2]][,1],
         pch = 16, col = "pink")
  curve(coef(Ar.Jz.lm.best.list[[4]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[4]][[2]])[2]),
        add=T, col ="pink")
  points(Jz.lowres.slice.list[[5]][,1] ~ ar.alpha.list[[5]][,1],
         pch = 16, col = "#00B0F0")
  # curve(coef(Ar.Jz.lm.best.list[[5]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[5]][[2]])[2]),
  #       add=T, col ="#00B0F0")

  plot(Jz.lowres.slice.list[[1]][,1] ~ ar.alpha.list[[1]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.3),
       xlab = expression(alpha(z)),
       ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "Exponential plateau",
       pch = 16, col = "blue")
  curve(coef(Ar.Jz.lm.best.list[[1]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[1]][[3]])[1] - 
         coef(Ar.Jz.lm.best.list[[1]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[1]][[3]])[3]*x), 
        add=T, col = "blue")
  
  points(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.list[[2]][,1], pch = 16, col = "chartreuse3")
  curve(coef(Ar.Jz.lm.best.list[[2]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[2]][[3]])[1] - 
          coef(Ar.Jz.lm.best.list[[2]][[3]])[2])*
                exp(-1*coef(Ar.Jz.lm.best.list[[2]][[3]])[3]*x), 
        add=T, col = "chartreuse3")
  
  points(Jz.lowres.slice.list[[3]][,1] ~ ar.alpha.list[[3]][,1], pch = 16, col = "red")
  curve(coef(Ar.Jz.lm.best.list[[3]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[3]][[3]])[1] - 
                                             coef(Ar.Jz.lm.best.list[[3]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[3]][[3]])[3]*x), 
        add=T, col = "red")
  
  points(Jz.lowres.slice.list[[4]][,1] ~ ar.alpha.list[[4]][,1], pch = 16, col = "pink")
  curve(coef(Ar.Jz.lm.best.list[[4]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[4]][[3]])[1] -
                                                   coef(Ar.Jz.lm.best.list[[4]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[4]][[3]])[3]*x),
        add=T, col = "pink")
  
  points(Jz.lowres.slice.list[[5]][,1] ~ ar.alpha.list[[5]][,1], pch = 16, col = "#00B0F0")
  # curve(coef(Ar.Jz.lm.best.list[[5]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[5]][[3]])[1] - 
  #                                                  coef(Ar.Jz.lm.best.list[[5]][[3]])[2])*
  #         exp(-1*coef(Ar.Jz.lm.best.list[[5]][[3]])[3]*x), 
  #       add=T, col = "#00B0F0")
  
  dev.off()
}

# {
#   pdf("./Output/Fig.SX Ar.Jz-Alpha.BestLowRes.48m.pdf", width = 5, height = 5)
#   par(mar = c(5,5,4,2)+0.2)
#   plot(Jz.lowres.slice.list[[1]] ~ ar.alpha.list[[1]][,1],
#        las = 1,
#        xlim = c(0,0.50),
#        ylim = c(0.05, 0.15),
#        xlab = expression(alpha(z)),
#        ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
#        main = "",
#        pch = 16, col = "black")
#   abline(Ar.Jz.lm.best.list[[1]][[1]], col = "black")
#   
#   curve(coef(Ar.Jz.lm.best.list[[1]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[1]][[3]])[1] - 
#                                                    coef(Ar.Jz.lm.best.list[[1]][[3]])[2])*
#           exp(-1*coef(Ar.Jz.lm.best.list[[1]][[3]])[3]*x), 
#         add=T, col = "black", lty = 2)
#   
#   legend("topleft", legend = c("Linear fit", "exponential plateau fit"),
#          text.col = c("black","black"), lty = c(1,2))
#   dev.off()
# }


#Print Figure 3 (YSI, loggers and 2 casts) in a single plot
{
  pdf("./Output/Figure3.pdf", width = 4.5, height = 4)
  par(mfrow=c(3,3))
  par(mar=c(4,5,1,1)+.1)
  ############################################Loggers########################################
  plot(Ar.Jz.day.2017.lin ~ Jz.mat[, 6], las =1,
       # xlab = expression(alpha(z)),
       xlab = "",
       xaxt = "n",
       xlim = c(0,0.25),
       ylab = "",#expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       ylim = c(0.04, 0.25),cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       main = "Linear", col ="blue",pch=16)
  # abline(lm(Ar.Jz.2017 ~ Jz.mat[, 6]), col ="blue", lty =2)
  points(Ar.Jz.day.2018.lin ~ Jz.mat[, 6], col ="chartreuse3",pch=16,cex = 1.8)
  abline(lm(Ar.Jz.day.2018.lin ~ Jz.mat[, 6]), col ="chartreuse3", lty = 2)
  points(Ar.Jz.day.2019.lin ~ Jz.mat[, 6], col = "red",pch=16,cex = 1.8)
  # abline(lm(Ar.Jz.2019 ~ Jz.mat[, 6]), col ="red")
  # points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
  # abline(lm(Ar.Jz.2016 ~ Jz.mat[-5, 6]), col ="black")
  points(Ar.Jz.day.2020.lin ~ Jz.mat[,6], col="pink", pch = 16,cex = 1.8)
  # abline(lm(Ar.Jz.2020 ~ Jz.mat[, 6]), col ="pink")
  
  #Log-linear
  plot(Ar.Jz.day.2017.lin ~ Jz.mat[, 6], las =1,
       # xlab = expression(alpha(z)),
       # ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       xlim = c(0,0.25),
       ylim = c(0.04, 0.25),cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       main = "Log-linear", col ="blue",pch=16)
  curve(coef(Ar.Living.2017.day.lin.log )[1]*log10(x*coef(Ar.Living.2017.day.lin.log)[2]),
        add=T, col ="blue", lty = 2)
  points(Ar.Jz.day.2018.lin ~ Jz.mat[, 6], col ="chartreuse3",pch=16,cex = 1.8)
  curve(coef(Ar.Living.2018.day.lin.log )[1]*log10(x*coef(Ar.Living.2018.day.lin.log )[2]),
        add=T, col ="chartreuse3", lty = 2)
  points(Ar.Jz.day.2019.lin ~ Jz.mat[, 6], col = "red",pch=16,cex = 1.8)
  curve(coef(Ar.Living.2019.day.lin.log)[1]*log10(x*coef(Ar.Living.2019.day.lin.log)[2]),
        add=T, col ="red")
  # points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
  # curve(coef(Ar.Living.2016.log)[1]*log10(x*coef(Ar.Living.2016.log)[2]),
  #       add=T, col ="black")
  points(Ar.Jz.day.2020.lin ~ Jz.mat[,6], col="pink", pch = 16,cex = 1.8)
  curve(coef(Ar.Living.2020.day.lin.log)[1]*log10(x*coef(Ar.Living.2020.day.lin.log)[2]),
        add=T, col ="pink")
  
  #Exponential plateau
  plot(Ar.Jz.day.2017.lin ~ Jz.mat[, 6], las =1,
       # xlab = expression(alpha(z)),
       # ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       xlim = c(0,0.25),
       ylim = c(0.04, 0.25),cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       main = "Exponential plateau", col ="blue",pch=16)
  curve(coef(Ar.Living.2017.day.lin.exp )[1] - (coef(Ar.Living.2017.day.lin.exp )[1] - coef(Ar.Living.2017.day.lin.exp )[2])*
          exp(-coef(Ar.Living.2017.day.lin.exp )[3]*x),
        add=T, col ="blue")
  points(Ar.Jz.day.2018.lin ~ Jz.mat[, 6], col ="chartreuse3",pch=16,cex = 1.8)
  curve(coef(Ar.Living.2018.day.lin.exp)[1] - (coef(Ar.Living.2018.day.lin.exp)[1] - coef(Ar.Living.2018.day.lin.exp)[2])*
          exp(-coef(Ar.Living.2018.day.lin.exp)[3]*x),
        add=T, col ="chartreuse3", lty = 2)
  points(Ar.Jz.day.2019.lin ~ Jz.mat[, 6], col = "red",pch=16,cex = 1.8)
  curve(coef(Ar.Living.2019.day.lin.exp)[1] - (coef(Ar.Living.2019.day.lin.exp)[1] - coef(Ar.Living.2019.day.lin.exp)[2])*
          exp(-coef(Ar.Living.2019.day.lin.exp)[3]*x),
        add=T, col ="red")
  # points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
  # curve(coef(Ar.Living.2016.exp)[1] - (coef(Ar.Living.2016.exp)[1] - coef(Ar.Living.2016.exp)[2])*
  #         exp(-coef(Ar.Living.2016.exp)[3]*x),
  #       add=T, col ="black")
  points(Ar.Jz.day.2020.lin ~ Jz.mat[,6], col="pink", pch = 16,cex = 1.8)
  curve(coef(Ar.Living.2020.day.lin.exp)[1] - (coef(Ar.Living.2020.day.lin.exp)[1] - coef(Ar.Living.2020.day.lin.exp)[2])*
          exp(-coef(Ar.Living.2020.day.lin.exp)[3]*x),
        add=T, col ="pink")
  ###############################################YSI#########################################
  plot(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.40),
       xaxt = "n",
       xlab = "",
       # xlab = expression(alpha(z)),
       ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "",cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       pch = 16, col = "chartreuse3")
  abline(Ar.Jz.alpha.YSI.2018, col = "chartreuse3", lty = 2)
  points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
         pch = 16, col = "blue",cex = 1.8)
  abline(Ar.Jz.alpha.YSI.2017, col = "blue")
  points(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "red",cex = 1.8)
  # abline(Ar.Jz.alpha.YSI.2019, col = "red")
  points(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "pink",cex = 1.8)
  # abline(Ar.Jz.alpha.YSI.2020, col = "pink")
  points(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "#73BAE6",cex = 1.8)
  abline(Ar.Jz.alpha.YSI.2021, col = "#73BAE6", lty = 2)
  # legend("topleft", legend = c("2017","2018","2019","2020", "2021"),
  #        text.col = c("blue","chartreuse3","red","pink", "#73BAE6"))
  
  
  plot(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.40),
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       # xlab = expression(alpha(z)),
       # ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "",cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       pch = 16, col = "chartreuse3")
  # curve(coef(Ar.Jz.alpha.YSI.2018.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2018.log)[2]),
  #       add=T, col ="chartreuse3")
  points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
         pch = 16, col = "blue",cex = 1.8)
  # curve(coef(Ar.Jz.alpha.YSI.2017.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2017.log)[2]),
  #       add=T, col ="blue")
  points(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "red",cex = 1.8)
  curve(coef(Ar.Jz.alpha.YSI.2019.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2019.log)[2]),
        add=T, col ="red")
  points(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "pink",cex = 1.8)
  curve(coef(Ar.Jz.alpha.YSI.2020.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2020.log)[2]),
        add=T, col ="pink")
  points(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "#73BAE6",cex = 1.8)
  # curve(coef(Ar.Jz.alpha.YSI.2021.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2021.log)[2]),
  #       add=T, col ="#73BAE6")
  
  plot(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.40),
       xaxt = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       # xlab = expression(alpha(z)),
       # ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "",cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       pch = 16, col = "chartreuse3")
  
  curve(coef(Ar.Jz.alpha.YSI.2018.exp)[1] - (coef(Ar.Jz.alpha.YSI.2018.exp)[1] - 
                                               coef(Ar.Jz.alpha.YSI.2018.exp)[2])*
          exp(-1*coef(Ar.Jz.alpha.YSI.2018.exp)[3]*x), 
        add=T, col = "chartreuse3")
  
  points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
         pch = 16, col = "blue",cex = 1.8)
  curve(coef(Ar.Jz.alpha.YSI.2017.exp)[1] - (coef(Ar.Jz.alpha.YSI.2017.exp)[1] - 
                                               coef(Ar.Jz.alpha.YSI.2017.exp)[2])*
          exp(-1*coef(Ar.Jz.alpha.YSI.2017.exp)[3]*x), 
        add=T, col = "blue")
  
  points(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "red",cex = 1.8)
  curve(coef(Ar.Jz.alpha.YSI.2019.exp)[1] - (coef(Ar.Jz.alpha.YSI.2019.exp)[1] - 
                                               coef(Ar.Jz.alpha.YSI.2019.exp)[2])*
          exp(-1*coef(Ar.Jz.alpha.YSI.2019.exp)[3]*x), 
        add=T, col = "red")
  
  points(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "pink",cex = 1.8)
  curve(coef(Ar.Jz.alpha.YSI.2020.exp)[1] - (coef(Ar.Jz.alpha.YSI.2020.exp)[1] - 
                                               coef(Ar.Jz.alpha.YSI.2020.exp)[2])*
          exp(-1*coef(Ar.Jz.alpha.YSI.2020.exp)[3]*x), 
        add=T, col = "pink")
  
  points(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1],
         pch = 16, col = "#73BAE6",cex = 1.8)
  curve(coef(Ar.Jz.alpha.YSI.2021.exp)[1] - (coef(Ar.Jz.alpha.YSI.2021.exp)[1] - 
                                               coef(Ar.Jz.alpha.YSI.2021.exp)[2])*
          exp(-1*coef(Ar.Jz.alpha.YSI.2021.exp)[3]*x), 
        add=T, col = "#73BAE6")

  #########################################2 oxic profiles#################################
  plot(Jz.lowres.slice.list[[1]][,1] ~ ar.alpha.list[[1]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.2),
       xlab = "",
       ylab = "",#expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "",cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       pch = 16, col = "blue")
  # abline(best.2017.short.lm, col = "blue")
  points(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.list[[2]][,1],
         pch = 16, cex = 1.8,col = "chartreuse3")
  abline(Ar.Jz.lm.best.list[[2]][[1]], col = "chartreuse3")
  points(Jz.lowres.slice.list[[3]][,1] ~ ar.alpha.list[[3]][,1],
         pch = 16, cex = 1.8,col = "red")
  # abline(Ar.Jz.lm.best.list[[3]][[1]], col = "red")
  points(Jz.lowres.slice.list[[4]][,1] ~ ar.alpha.list[[4]][,1],
         pch = 16, cex = 1.8,col = "pink")
  # abline(Ar.Jz.lm.best.list[[4]][[1]], col = "pink")
  points(Jz.lowres.slice.list[[5]][,1] ~ ar.alpha.list[[5]][,1],
         pch = 16, cex = 1.8,col = "#00B0F0")
  abline(Ar.Jz.lm.best.list[[5]][[1]], col = "#00B0F0", lty = 2)
  
  plot(Jz.lowres.slice.list[[1]][,1] ~ ar.alpha.list[[1]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.2),
       xlab = expression(alpha(z)),
       ylab = "",
       yaxt = "n",cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       pch = 16, col = "blue")
  curve(coef(Ar.Jz.lm.best.list[[1]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[1]][[2]])[2]),
         add=T, col ="blue", lty = 2)
  points(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.list[[2]][,1],
         pch = 16, cex = 1.8,col = "chartreuse3")
  # curve(coef(Ar.Jz.lm.best.list[[2]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[2]][[2]])[2]),
  #       add=T, col ="chartreuse3", lty = 2)
  points(Jz.lowres.slice.list[[3]][,1] ~ ar.alpha.list[[2]][,1],
         pch = 16, cex = 1.8,col = "red")
  curve(coef(Ar.Jz.lm.best.list[[3]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[3]][[2]])[2]),
        add=T, col ="red", lty = 2)
  points(Jz.lowres.slice.list[[4]][,1] ~ ar.alpha.list[[2]][,1],
         pch = 16, cex = 1.8,col = "pink")
  curve(coef(Ar.Jz.lm.best.list[[4]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[4]][[2]])[2]),
        add=T, col ="pink")
  points(Jz.lowres.slice.list[[5]][,1] ~ ar.alpha.list[[5]][,1],
         pch = 16, cex = 1.8,col = "#00B0F0")
  # curve(coef(Ar.Jz.lm.best.list[[5]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[5]][[2]])[2]),
  #       add=T, col ="#00B0F0")
  
  plot(Jz.lowres.slice.list[[1]][,1] ~ ar.alpha.list[[1]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.2),
       xlab = "",
       ylab = "",
       yaxt = "n",cex = 1.8, cex.axis=0.9, cex.lab = 0.8,cex.main = 1.2,
       pch = 16, col = "blue")
  curve(coef(Ar.Jz.lm.best.list[[1]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[1]][[3]])[1] - 
                                                   coef(Ar.Jz.lm.best.list[[1]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[1]][[3]])[3]*x), 
        add=T, col = "blue")
  
  points(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.list[[2]][,1], pch = 16, cex = 1.8,col = "chartreuse3")
  curve(coef(Ar.Jz.lm.best.list[[2]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[2]][[3]])[1] - 
                                                   coef(Ar.Jz.lm.best.list[[2]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[2]][[3]])[3]*x), 
        add=T, col = "chartreuse3")
  
  points(Jz.lowres.slice.list[[3]][,1] ~ ar.alpha.list[[3]][,1], pch = 16, cex = 1.8,col = "red")
  curve(coef(Ar.Jz.lm.best.list[[3]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[3]][[3]])[1] - 
                                                   coef(Ar.Jz.lm.best.list[[3]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[3]][[3]])[3]*x), 
        add=T, col = "red")
  
  points(Jz.lowres.slice.list[[4]][,1] ~ ar.alpha.list[[4]][,1], pch = 16, cex = 1.8,col = "pink")
  curve(coef(Ar.Jz.lm.best.list[[4]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[4]][[3]])[1] -
                                                   coef(Ar.Jz.lm.best.list[[4]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[4]][[3]])[3]*x),
        add=T, col = "pink")
  
  points(Jz.lowres.slice.list[[5]][,1] ~ ar.alpha.list[[5]][,1], pch = 16, cex = 1.8,col = "#00B0F0")
  curve(coef(Ar.Jz.lm.best.list[[5]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[5]][[3]])[1] -
                                                   coef(Ar.Jz.lm.best.list[[5]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[5]][[3]])[3]*x),
        add=T, col = "#00B0F0")
  
  dev.off()
}


# Figure 5, How good are only 2 profiles
##########################2 oxic profiles#########################

DOY.ini.monte = 122
DOY.end.monte = 182

#With matching YSI profiles
YSI.list = list.files("./Data/Raw/Arendsee-cleaned")
#Keep only csv files
YSI.list = YSI.list[grep(pattern = ".csv",list.files("./Data/Raw/Arendsee-cleaned"))]

Bats <- read.csv("./Data/Raw/Arendsee.alpha.csv")
Jz.lowres.slice.list.monte = list()
ar.alpha.list.monte = list()

Ar.Jz.lm.best.list.monte = list()
Jz.lowres.monte = list()
Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[2]))

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

#2 oxic profiles - 28 days apart
#Keep only profiles within 2 dates
Ar.long.deep = Ar.long.deep[Ar.long.deep$DOY >= DOY.ini.monte & Ar.long.deep$DOY <= DOY.end.monte,]

#Keep only values higher than 2mgO2/L
Ar.long.deep = Ar.long.deep[Ar.long.deep$DO_mgL >=2,]

Depths = unique(Ar.long.deep$Depth_m)
Jz.lowres.monte = matrix(nrow = length(Depths), ncol = 33)
rownames(Jz.lowres.monte) = Depths
for(j in 1:length(Depths)){
  Ar.long.deep.temp = Ar.long.deep[Ar.long.deep$Depth_m == Depths[j],]
  Ar.long.deep.temp = Ar.long.deep.temp %>% group_by(DOY) %>% summarize(DO_mgL = mean(DO_mgL))
  
  #Fill in missing days (linear interpolation)
  Ar.long.filled = t(Ar.long.deep.temp)
  colnames(Ar.long.filled) = Ar.long.filled[1,]
  Ar.long.filled = t(as.matrix(Ar.long.filled[-1,]))
  Ar.long.filled = interpolation_fct(Ar.long.filled)
  
  #Calculate Jz between each pair of points
  Slope.temp = vector(length=length(Ar.long.filled)-28)
  for(k in 1:(length(Ar.long.filled)-28)){
    Slope.temp[k] = (Ar.long.filled[,k]-Ar.long.filled[,k+28])/28
  }
  
  Jz.lowres.monte[j,] = Slope.temp
  # print(c(length(Slope.temp), Depths[j]))
}


ar.alpha.monte <- Bats%>% slice(30:47) %>% select(alpha)


matrix.perm = matrix(c(1,2,3,1,2,4,1,2,5,2,3,4,2,3,5,3,4,5), ncol = 3, byrow = T)
{
  pdf("./Output/Figure 5.pdf", width = 4.5, height = 3)
  par(mfrow=c(2,3))
  par(mar=c(4,5,1,1)+.1)
  plot(Ar.Jz.day.2018.lin ~ Jz.mat[, 6], las = 1,
       xlab = "",
       ylab = expression(Jz~(mg~O[2]~L^-1~d^-1)),
       ylim = c(0.04, 0.2), xaxt = "n",
       cex = 1.4, cex.axis=0.8, cex.lab = 0.9)
  abline(lm(Ar.Jz.day.2018.lin ~ Jz.mat[, 6]), col = "Blue", lwd = 2)
  for(i in 1:nrow(matrix.perm)){
    abline(lm(Ar.Jz.day.2018.lin[matrix.perm[i,]] ~ Jz.mat[matrix.perm[i,], 6]), lty = 1, col = alpha("black", 0.4))
  }
  
  
  # polygon(x = c(Loggers.X, rev(Loggers.X)), y = c(Loggers.lin.quants[1,], rev(Loggers.lin.quants[2,])),
  #         col = alpha("black", 0.1), border = NA)
  
  plot(Ar.Jz.day.2018.lin ~ Jz.mat[, 6], las = 1,
       xlab = "",
       ylab = "", yaxt = "n",xaxt = "n",
       ylim = c(0.04, 0.2),
       cex = 1.4, cex.axis=0.8, cex.lab = 0.9)
  main = nlsLM(Ar.Jz.day.2018.lin ~ b*log10(k*Jz.mat[,6]),
               start = list(b = 0.08, k = 200))
  curve(coef(main)[1]*log10(x*coef(main)[2]), add=T, col = "Blue", lwd = 2)
  for(i in 1:nrow(matrix.perm)){
    alpha.temp = Jz.mat[matrix.perm[i,],6]
    Jz.temp = Ar.Jz.day.2018.lin[matrix.perm[i,]]
    temp = nlsLM(Jz.temp ~ b*log10(k*alpha.temp),
                 start = list(b = 0.08, k = 200))
    curve(coef(temp)[1]*log10(x*coef(temp)[2]), add=T, lty = 1, col = alpha("black", 0.4))
  }
  
  
  # polygon(x = c(Loggers.X, rev(Loggers.X)), y = c(Loggers.log.quants[1,], rev(Loggers.log.quants[2,])),
  #         col = alpha("black", 0.1), border = NA)
  
  plot(Ar.Jz.day.2018.lin ~ Jz.mat[, 6], las = 1,
       xlab = "",
       ylab = "", yaxt = "n",xaxt = "n",
       ylim = c(0.04, 0.2),
       cex = 1.4, cex.axis=0.8, cex.lab = 0.9)
  
  main = nlsLM(Ar.Jz.day.2018.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
               start = list(j.m = max(Ar.Jz.day.2018.lin), b = 0.2, k = 1))
  curve(coef(main)[1] - (coef(main)[1] - coef(main)[2]) * exp(-coef(main)[3]*x),
        add=T, col = "Blue", lwd = 2)
  
  for(i in 1:nrow(matrix.perm)){
    alpha.temp = Jz.mat[matrix.perm[i,],6]
    Jz.temp = Ar.Jz.day.2018.lin[matrix.perm[i,]]
    temp = nlsLM(Jz.temp ~ j.m - (j.m-b)*exp(-k*alpha.temp),
                 start = list(j.m = max(Jz.temp), b = 0.2, k = 1))
    curve(coef(temp)[1] - (coef(temp)[1] - coef(temp)[2]) * exp(-coef(temp)[3]*x),
          add=T, lty = 1, col = alpha("black", 0.4))
  }
  # polygon(x = c(Loggers.X, rev(Loggers.X)), y = c(Loggers.exp.quants[1,], rev(Loggers.exp.quants[2,])),
  #         col = alpha("black", 0.1), border = NA)
  
  #2 oxic profiles
  plot(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.monte$alpha, las = 1,
       xlab = expression(alpha(z)),
       ylab = expression(Jz~(mg~O[2]~L^-1~d^-1)),
       ylim = c(0.04, 0.2),
       cex = 1.4, cex.axis=0.8, cex.lab = 0.9)
  abline(Ar.Jz.lm.best.list[[2]][[1]], col = "Blue", lwd = 2)
  
  for(i in 1:ncol(Jz.lowres.monte)){
    abline(lm(Jz.lowres.monte[,i] ~ ar.alpha.monte$alpha), lty = 1, col = alpha("black", 0.4))
  }
  
  # polygon(x = c(Oxic.X, rev(Oxic.X)), y = c(Oxic.lin.quants[1,], rev(Oxic.lin.quants[2,])),
  #         col = alpha("black", 0.1), border = NA)
  
  plot(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.monte$alpha, las = 1,
       xlab = expression(alpha(z)),
       ylab = "", yaxt = "n",
       ylim = c(0.04, 0.2),
       cex = 1.4, cex.axis=0.8, cex.lab = 0.9)
  Jz.temp = Jz.lowres.slice.list[[2]][,1]
  alpha.temp = ar.alpha.monte$alpha
  main = nlsLM(Jz.temp ~ b*log10(k*alpha.temp),
               start = list(b = 0.08, k = 200))
  curve(coef(main)[1]*log10(x*coef(main)[2]), add=T, col = "Blue", lwd = 2)
  
  for(i in 1:ncol(Jz.lowres.monte)){
    Jz.temp = Jz.lowres.monte[,i]
    temp = nlsLM(Jz.temp ~ b*log10(k*alpha.temp),
                 start = list(b = 0.08, k = 200))
    curve(coef(temp)[1]*log10(x*coef(temp)[2]), add=T, lty = 1, col = alpha("black", 0.4))
  }
  
  # polygon(x = c(Oxic.X, rev(Oxic.X)), y = c(Oxic.log.quants[1,], rev(Oxic.log.quants[2,])),
  #         col = alpha("black", 0.1), border = NA)
  
  plot(Jz.lowres.slice.list[[2]][,1] ~ ar.alpha.monte$alpha, las = 1,
       xlab = expression(alpha(z)),
       ylab = "", yaxt = "n",
       ylim = c(0.04, 0.2),
       cex = 1.4, cex.axis=0.8, cex.lab = 0.9)
  Jz.temp = Jz.lowres.slice.list[[2]][,1]
  
  main = nlsLM(Jz.temp ~ j.m - (j.m-b)*exp(-k*alpha.temp),
               start = list(j.m = max(Ar.Jz.day.2018.lin), b = 0.2, k = 1))
  curve(coef(main)[1] - (coef(main)[1] - coef(main)[2]) * exp(-coef(main)[3]*x),
        add=T, col = "Blue", lwd = 2)
  
  for(i in 1:ncol(Jz.lowres.monte)){
    Jz.temp = Jz.lowres.monte[,i]
    temp = nlsLM(Jz.temp ~ j.m - (j.m-b)*exp(-k*alpha.temp),
                 start = list(j.m = max(Jz.temp), b = 0.3, k = 1))
    curve(coef(temp)[1] - (coef(temp)[1] - coef(temp)[2]) * exp(-coef(temp)[3]*x),
          add=T, lty = 1, col = alpha("black", 0.4))
  }
  
  # polygon(x = c(Oxic.X, rev(Oxic.X)), y = c(Oxic.exp.quants[1,], rev(Oxic.exp.quants[2,])),
  #         col = alpha("black", 0.1), border = NA)
  
  dev.off()
}

source("./Scripts/O2_DecayRate.R")

# #Useful parameters to model O2 profiles and anoxic age
# #Select alpha from 30m to 47m deep
# ar.alpha <- Bats %>% slice(30:47) %>% select(alpha)
# ar.depth = c(30:47)
# 
# #Select the matching Chemistry depths and time
# ar.chem <- filter(Chemistry, lake =="ar") %>% filter(depth == 30 | depth == 40 | depth==45 | depth==46 | depth==47 | depth==48)
# 
# #In the first visit of 2020, the maximum sampled depth is 48m in the chemistry dataset, but the YSI
# #profile stoped at 47m. I ASSUME here that the 48m is in fact 47m
# ar.chem.1 <- filter(ar.chem, date == "2020-05-26") 
# ar.chem.2 <- filter(ar.chem, date == "2020-08-25")
# 
# 
# #Calculate mean CO2 and CH4 concentrations
# ar.chem.1.co2 = group_by(ar.chem.1, depth) %>% summarize(CO2 = mean(conc.CO2.corrected))
# ar.chem.1.ch4 = group_by(ar.chem.1, depth) %>% summarize(CH4 = mean(conc.CH4.insitu))
# ar.chem.2.co2 = group_by(ar.chem.2, depth) %>% summarize(CO2 = mean(conc.CO2.corrected))
# ar.chem.2.ch4 = group_by(ar.chem.2, depth) %>% summarize(CH4 = mean(conc.CH4.insitu))
# 
# #Remove duplicated lines for other variables
# ar.chem.1.single = ar.chem.1[,-c(42:50)]
# ar.chem.1.single = unique(ar.chem.1.single)
# ar.chem.2.single = ar.chem.2[,-c(42:50)]
# ar.chem.2.single = unique(ar.chem.2.single)

#Depth used in 2020
# aa.depth = c(30,40,45,46,47)

#########################################################################################
#############################Anoxic age matrices using all fits##########################
#########################################################################################
source("./Scripts/O2_DecayRate.R")
INSERT_FCT_NAME <- function(b, k, Model, alpha, JzMax, data.source, year){
  O2.consumption.rate <- O2.decay(b = b, k = k, Model = Model, alpha = alpha, JzMax = JzMax)
  nday = 231 #Numbers of days to simulate
  dbeg = 135 #First day of simulated stratification
  
  #Create empty matrix with the good dimensions
  O2.modelled.matrix = as.data.frame(matrix(nrow=nrow(O2.consumption.rate), ncol = nday))
  #Set first profile at 12.3 mg O2/L
  O2.modelled.matrix[,1] = 12.3
  
  #Model daily profiles based on consumption rate
  for(i in 2:ncol(O2.modelled.matrix)){
    O2.modelled.matrix[,i] = 12.3 - O2.consumption.rate[,1]*(i-1)
  }
  #Change negative values to 0
  O2.modelled.matrix[O2.modelled.matrix<0] = 0
  
  #Anoxic age threshold, here 2 for hypoxia
  O2.threshold = 2
  
  #CHange values below trehsold to 1, others to 0
  O2.modelled.matrix.bin <- ifelse(O2.modelled.matrix <= O2.threshold, 1, 0)
  
  #Create new object to calculate anoxic age
  O2.modelled.anoxicage <- O2.modelled.matrix.bin
  for(j in 2:ncol(O2.modelled.anoxicage)){
    temp = O2.modelled.anoxicage[,j-1]+O2.modelled.anoxicage[,j]
    temp = ifelse(temp > O2.modelled.anoxicage[,j-1], temp, 0)
    O2.modelled.anoxicage[,j] = temp
  }
  
  #Add a column for depths and colnames
  O2.modelled.anoxicage = cbind(c(30:47), O2.modelled.anoxicage)
  colnames(O2.modelled.anoxicage)[1] = "Depth"
  colnames(O2.modelled.anoxicage)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))
  
  #Transform into dataframe
  O2.modelled.anoxicage = as.data.frame(O2.modelled.anoxicage)
  
  #Transform to long format
  O2.modelled.anoxicage.long = pivot_longer(O2.modelled.anoxicage,
                                                cols = 2:ncol(O2.modelled.anoxicage),
                                                names_to = "DOY", values_to = "HypoxAge")
  O2.modelled.anoxicage.long = as.data.frame(O2.modelled.anoxicage.long)
  
  #See results in ugly graph
  print(ggplot(O2.modelled.anoxicage.long)+
    geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
    scale_y_reverse()+
    scale_fill_gradient(low="white", high="red")+
    ggtitle(year))
  
  
  
  #Align the modeled oxygen profile with the real data to associate anoxic age with 
  #reduced compounds concentration using the lowest sum of squared difference
  
  if(data.source == "YSI") {
    year.i = grep(year, YSI.list)
    Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[year.i]))
  }  
  
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
  #In 2020, Arendsee was sampled on 2020-05-26 (DOY 147) and 2020-08-25	
  Ar.first.profile = Ar.long.deep[,-2] %>% filter(mm == "05", dd == "26") %>% 
    group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))
  
  # Ar.second.profile = Ar.long.deep[,-2] %>% filter(mm == "08", dd == "25") %>% 
  #   group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))
  
  Match.temp.1 = O2.modelled.matrix
  for(i in 1:ncol(O2.modelled.matrix)){
    Match.temp.1[,i] = O2.modelled.matrix[,i] - Ar.first.profile[,2]
  }
  
  Match.temp.1.abs = (Match.temp.1)^2
  Match.day.1 = which(colSums(Match.temp.1.abs) == min(colSums(Match.temp.1.abs)))
  Match.day.1.RMSE = sqrt(sum(Match.temp.1.abs[Match.day.1])/length(Match.temp.1.abs[Match.day.1]))
  
  O2.modelled.anoxicage = O2.modelled.anoxicage[,-c(2:(Match.day.1-1))]
  colnames(O2.modelled.anoxicage)[c(2:ncol(O2.modelled.anoxicage))] = seq(147,ncol(O2.modelled.anoxicage)+146,1)
  output = list()
  output[[1]] = O2.modelled.anoxicage
  output[[2]] = Match.day.1
  output[[3]] = Match.day.1.RMSE
  return(output)
  }


#Anoxic age for Loggers - 2017
{
Loggers.linear.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Living.2017.day)[1,1],
                                         k = coef(Ar.Living.2017.day)[2,1],
                                         Model = "Livingstone",
                                         alpha = ar.alpha,
                                         data.source = "YSI",
                                         year = 2017)

Loggers.log.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Living.2017.day.lin.log)[1],
                                      k = coef(Ar.Living.2017.day.lin.log)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2017)

Loggers.exp.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Living.2017.day.lin.exp)[2],
                                      k = coef(Ar.Living.2017.day.lin.exp)[3],
                                      JzMax = coef(Ar.Living.2017.day.lin.exp)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2017)}

#Anoxic age for Loggers - 2018
{Loggers.linear.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Living.2018.day)[1,1],
                                         k = coef(Ar.Living.2018.day)[2,1],
                                         Model = "Livingstone",
                                         alpha = ar.alpha,
                                         data.source = "YSI",
                                         year = 2018)

Loggers.log.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Living.2018.day.lin.log)[1],
                                      k = coef(Ar.Living.2018.day.lin.log)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2018)

Loggers.exp.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Living.2018.day.lin.exp)[2],
                                      k = coef(Ar.Living.2018.day.lin.exp)[3],
                                      JzMax = coef(Ar.Living.2018.day.lin.exp)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2018)}

#Anoxic age for Loggers - 2019
{Loggers.linear.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Living.2019.day)[1,1],
                                         k = coef(Ar.Living.2019.day)[2,1],
                                         Model = "Livingstone",
                                         alpha = ar.alpha,
                                         data.source = "YSI",
                                         year = 2019)

Loggers.log.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Living.2019.day.lin.log)[1],
                                      k = coef(Ar.Living.2019.day.lin.log)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2019)

Loggers.exp.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Living.2019.day.lin.exp)[2],
                                      k = coef(Ar.Living.2019.day.lin.exp)[3],
                                      JzMax = coef(Ar.Living.2019.day.lin.exp)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2019)}

#Anoxic age for Loggers - 2020
{Loggers.linear.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Living.2020.day)[1,1],
                                         k = coef(Ar.Living.2020.day)[2,1],
                                         Model = "Livingstone",
                                         alpha = ar.alpha,
                                         data.source = "YSI",
                                         year = 2020)

Loggers.log.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Living.2020.day.lin.log)[1],
                                      k = coef(Ar.Living.2020.day.lin.log)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2020)

Loggers.exp.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Living.2020.day.lin.exp)[2],
                                      k = coef(Ar.Living.2020.day.lin.exp)[3],
                                      JzMax = coef(Ar.Living.2020.day.lin.exp)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2020)
}


#Anoxic age for High temporal Res (YSI) - 2017
{YSI.linear.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2017)[1],
                                     k = coef(Ar.Jz.alpha.YSI.2017)[2],
                                     Model = "Livingstone",
                                     alpha = ar.alpha,
                                     data.source = "YSI",
                                     year = 2017)

YSI.log.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2017.log)[1],
                                  k = coef(Ar.Jz.alpha.YSI.2017.log)[2],
                                  Model = "Log",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2017)

YSI.exp.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2017.exp)[2],
                                  k = coef(Ar.Jz.alpha.YSI.2017.exp)[3],
                                  JzMax = coef(Ar.Jz.alpha.YSI.2017.exp)[1],
                                  Model = "Exp",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2017)}

#Anoxic age for High temporal Res (YSI) - 2018
{YSI.linear.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2018)[1],
                                     k = coef(Ar.Jz.alpha.YSI.2018)[2],
                                     Model = "Livingstone",
                                     alpha = ar.alpha,
                                     data.source = "YSI",
                                     year = 2018)

YSI.log.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2018.log)[1],
                                  k = coef(Ar.Jz.alpha.YSI.2018.log)[2],
                                  Model = "Log",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2018)

YSI.exp.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2018.exp)[2],
                                  k = coef(Ar.Jz.alpha.YSI.2018.exp)[3],
                                  JzMax = coef(Ar.Jz.alpha.YSI.2018.exp)[1],
                                  Model = "Exp",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2018)}

#Anoxic age for High temporal Res (YSI) - 2019
{YSI.linear.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2019)[1],
                                     k = coef(Ar.Jz.alpha.YSI.2019)[2],
                                     Model = "Livingstone",
                                     alpha = ar.alpha,
                                     data.source = "YSI",
                                     year = 2019)

YSI.log.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2019.log)[1],
                                  k = coef(Ar.Jz.alpha.YSI.2019.log)[2],
                                  Model = "Log",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2019)

YSI.exp.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2019.exp)[2],
                                  k = coef(Ar.Jz.alpha.YSI.2019.exp)[3],
                                  JzMax = coef(Ar.Jz.alpha.YSI.2019.exp)[1],
                                  Model = "Exp",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2019)}

#Anoxic age for High temporal Res (YSI) - 2020
{YSI.linear.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2020)[1],
                                     k = coef(Ar.Jz.alpha.YSI.2020)[2],
                                     Model = "Livingstone",
                                     alpha = ar.alpha,
                                     data.source = "YSI",
                                     year = 2020)

YSI.log.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2020.log)[1],
                                  k = coef(Ar.Jz.alpha.YSI.2020.log)[2],
                                  Model = "Log",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2020)

YSI.exp.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2020.exp)[2],
                                  k = coef(Ar.Jz.alpha.YSI.2020.exp)[3],
                                  JzMax = coef(Ar.Jz.alpha.YSI.2020.exp)[1],
                                  Model = "Exp",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2020)
}
#Anoxic age for High temporal Res (YSI) - 2021
{YSI.linear.AA.2021 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2021)[1],
                                     k = coef(Ar.Jz.alpha.YSI.2021)[2],
                                     Model = "Livingstone",
                                     alpha = ar.alpha,
                                     data.source = "YSI",
                                     year = 2021)

YSI.log.AA.2021 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2021.log)[1],
                                  k = coef(Ar.Jz.alpha.YSI.2021.log)[2],
                                  Model = "Log",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2021)

YSI.exp.AA.2021 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2021.exp)[2],
                                  k = coef(Ar.Jz.alpha.YSI.2021.exp)[3],
                                  JzMax = coef(Ar.Jz.alpha.YSI.2021.exp)[1],
                                  Model = "Exp",
                                  alpha = ar.alpha,
                                  data.source = "YSI",
                                  year = 2021)}


#Anoxic age for Best case (YSI) - 2017
{Best.linear.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[1]][[1]])[1],
                                      k = coef(Ar.Jz.lm.best.list[[1]][[1]])[2],
                                      Model = "Livingstone",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2017)

Best.log.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[1]][[2]])[1],
                                   k = coef(Ar.Jz.lm.best.list[[1]][[2]])[2],
                                   Model = "Log",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2017)

Best.exp.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[1]][[3]])[2],
                                   k = coef(Ar.Jz.lm.best.list[[1]][[3]])[3],
                                   JzMax = coef(Ar.Jz.lm.best.list[[1]][[3]])[1],
                                   Model = "Exp",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2017)}

#Anoxic age for Best case (YSI) - 2018
{Best.linear.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[2]][[1]])[1],
                                      k = coef(Ar.Jz.lm.best.list[[2]][[1]])[2],
                                      Model = "Livingstone",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2018)

Best.log.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[2]][[2]])[1],
                                   k = coef(Ar.Jz.lm.best.list[[2]][[2]])[2],
                                   Model = "Log",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2018)

Best.exp.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[2]][[3]])[2],
                                   k = coef(Ar.Jz.lm.best.list[[2]][[3]])[3],
                                   JzMax = coef(Ar.Jz.lm.best.list[[2]][[3]])[1],
                                   Model = "Exp",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2018)}

#Anoxic age for Best case (YSI) - 2019
{Best.linear.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[3]][[1]])[1],
                                      k = coef(Ar.Jz.lm.best.list[[3]][[1]])[2],
                                      Model = "Livingstone",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2019)

Best.log.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[3]][[2]])[1],
                                   k = coef(Ar.Jz.lm.best.list[[3]][[2]])[2],
                                   Model = "Log",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2019)

Best.exp.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[3]][[3]])[2],
                                   k = coef(Ar.Jz.lm.best.list[[3]][[3]])[3],
                                   JzMax = coef(Ar.Jz.lm.best.list[[3]][[3]])[1],
                                   Model = "Exp",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2019)}

#Anoxic age for Best case (YSI) - 2020
{Best.linear.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[4]][[1]])[1],
                                      k = coef(Ar.Jz.lm.best.list[[4]][[1]])[2],
                                      Model = "Livingstone",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2020)

Best.log.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[4]][[2]])[1],
                                   k = coef(Ar.Jz.lm.best.list[[4]][[2]])[2],
                                   Model = "Log",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2020)

Best.exp.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[4]][[3]])[2],
                                   k = coef(Ar.Jz.lm.best.list[[4]][[3]])[3],
                                   JzMax = coef(Ar.Jz.lm.best.list[[4]][[3]])[1],
                                   Model = "Exp",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2020)}

#Anoxic age for Best case (YSI) - 2021
{Best.linear.AA.2021 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[5]][[1]])[1],
                                      k = coef(Ar.Jz.lm.best.list[[5]][[1]])[2],
                                      Model = "Livingstone",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2021)

Best.log.AA.2021 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[5]][[2]])[1],
                                   k = coef(Ar.Jz.lm.best.list[[5]][[2]])[2],
                                   Model = "Log",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2021)

Best.exp.AA.2021 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[5]][[3]])[2],
                                   k = coef(Ar.Jz.lm.best.list[[5]][[3]])[3],
                                   JzMax = coef(Ar.Jz.lm.best.list[[5]][[3]])[1],
                                   Model = "Exp",
                                   alpha = ar.alpha,
                                   data.source = "YSI",
                                   year = 2021)}


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

#Home function to interpolate data on missing days. This function is not super flexible.
#Use at own risks

source('./Scripts/Matrix_Inter.R', encoding = 'UTF-8')

interpolated.2017 = interpolation_fct(output[[1]])
interpolated.2018 = interpolation_fct(output[[2]][,-283])
interpolated.2019 = interpolation_fct(output[[3]])
interpolated.2020 = interpolation_fct(output[[4]])
interpolated.2021 = interpolation_fct(output[[5]])

# #Change to 1 or 0 if hypoxic or not
# YSIprofil.bin = list()
# YSIprofil.bin.cumul = list()
# for(i in 1:4)
# {
#   data.temp = output[[i]]
#   YSIprofil.bin[[i]] <- ifelse(data.temp <= O2.threshold, 1, 0)
# }
O2.threshold = 2
interpolated.2017.bin <- ifelse(interpolated.2017 <= O2.threshold, 1, 0)
interpolated.2018.bin <- ifelse(interpolated.2018 <= O2.threshold, 1, 0)
interpolated.2019.bin <- ifelse(interpolated.2019 <= O2.threshold, 1, 0)
interpolated.2020.bin <- ifelse(interpolated.2020 <= O2.threshold, 1, 0)
interpolated.2021.bin <- ifelse(interpolated.2021 <= O2.threshold, 1, 0)


YSI.aa.2017 <- t(apply(interpolated.2017.bin, 1, function(X) X==1))
YSI.aa.2018 <- t(apply(interpolated.2018.bin, 1, function(X) X==1))
YSI.aa.2019 <- t(apply(interpolated.2019.bin, 1, function(X) X==1))
YSI.aa.2020 <- t(apply(interpolated.2020.bin, 1, function(X) X==1))
YSI.aa.2021 <- t(apply(interpolated.2021.bin, 1, function(X) X==1))

#write.csv(x = YSI.aa.2017, file = "./Output/YSI.aa.2017.csv", fileEncoding = "UTF-8")
#write.csv(x = YSI.aa.2018, file = "./Output/YSI.aa.2018.csv", fileEncoding = "UTF-8")
#write.csv(x = YSI.aa.2019, file = "./Output/YSI.aa.2019.csv", fileEncoding = "UTF-8")
#write.csv(x = YSI.aa.2021, file = "./Output/YSI.aa.2021.csv", fileEncoding = "UTF-8")

#Find first day of continuous hypoxia for each year and each depth
First.aa.doy.fct <- function(x){
  output = vector(length=nrow(x))
  for(i in 1:nrow(x)){
    output[i] = min(which(x[i,]==TRUE)) + as.numeric(colnames(x)[1])
  }
  return(output)
}

#Observed profiles
YSI.aa.2017.cut = First.aa.doy.fct(YSI.aa.2017[,-c(1:120)]) #Remove first few months if there was still anoxia from previous year
YSI.aa.2018.cut = First.aa.doy.fct(YSI.aa.2018[,-c(1:120)])
YSI.aa.2019.cut = First.aa.doy.fct(YSI.aa.2019[,-c(1:120)])
YSI.aa.2020.cut = First.aa.doy.fct(YSI.aa.2020[,-c(1:120)])
YSI.aa.2021.cut = First.aa.doy.fct(YSI.aa.2021[,-c(1:120)])

###Modelled profiles
#Loggers-2017
{Loggers.linear.AA.2017.bin = t(apply(Loggers.linear.AA.2017[[1]][,-1], 1, function(X) X==1))
Loggers.log.AA.2017.bin = t(apply(Loggers.log.AA.2017[[1]][,-1], 1, function(X) X==1))
Loggers.exp.AA.2017.bin = t(apply(Loggers.exp.AA.2017[[1]][,-1], 1, function(X) X==1))

Loggers.linear.AA.2017.FirstaaDOY = First.aa.doy.fct(Loggers.linear.AA.2017.bin)
Loggers.log.AA.2017.FirstaaDOY = First.aa.doy.fct(Loggers.log.AA.2017.bin)
Loggers.exp.AA.2017.FirstaaDOY = First.aa.doy.fct(Loggers.exp.AA.2017.bin)}

#Loggers-2018
{Loggers.linear.AA.2018.bin = t(apply(Loggers.linear.AA.2018[[1]][,-1], 1, function(X) X==1))
Loggers.log.AA.2018.bin = t(apply(Loggers.log.AA.2018[[1]][,-1], 1, function(X) X==1))
Loggers.exp.AA.2018.bin = t(apply(Loggers.exp.AA.2018[[1]][,-1], 1, function(X) X==1))

Loggers.linear.AA.2018.FirstaaDOY = First.aa.doy.fct(Loggers.linear.AA.2018.bin)
Loggers.log.AA.2018.FirstaaDOY = First.aa.doy.fct(Loggers.log.AA.2018.bin)
Loggers.exp.AA.2018.FirstaaDOY = First.aa.doy.fct(Loggers.exp.AA.2018.bin)}

#Loggers-2019
{Loggers.linear.AA.2019.bin = t(apply(Loggers.linear.AA.2019[[1]][,-1], 1, function(X) X==1))
Loggers.log.AA.2019.bin = t(apply(Loggers.log.AA.2019[[1]][,-1], 1, function(X) X==1))
Loggers.exp.AA.2019.bin = t(apply(Loggers.exp.AA.2019[[1]][,-1], 1, function(X) X==1))

Loggers.linear.AA.2019.FirstaaDOY = First.aa.doy.fct(Loggers.linear.AA.2019.bin)
Loggers.log.AA.2019.FirstaaDOY = First.aa.doy.fct(Loggers.log.AA.2019.bin)
Loggers.exp.AA.2019.FirstaaDOY = First.aa.doy.fct(Loggers.exp.AA.2019.bin)}

#Loggers-2020
{Loggers.linear.AA.2020.bin = t(apply(Loggers.linear.AA.2020[[1]][,-1], 1, function(X) X==1))
Loggers.log.AA.2020.bin = t(apply(Loggers.log.AA.2020[[1]][,-1], 1, function(X) X==1))
Loggers.exp.AA.2020.bin = t(apply(Loggers.exp.AA.2020[[1]][,-1], 1, function(X) X==1))

Loggers.linear.AA.2020.FirstaaDOY = First.aa.doy.fct(Loggers.linear.AA.2020.bin)
Loggers.log.AA.2020.FirstaaDOY = First.aa.doy.fct(Loggers.log.AA.2020.bin)
Loggers.exp.AA.2020.FirstaaDOY = First.aa.doy.fct(Loggers.exp.AA.2020.bin)}

#YSI - 2017
{YSI.linear.AA.2017.bin = t(apply(YSI.linear.AA.2017[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2017.bin = t(apply(YSI.log.AA.2017[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2017.bin = t(apply(YSI.exp.AA.2017[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2017.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2017.bin)
YSI.log.AA.2017.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2017.bin)
YSI.exp.AA.2017.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2017.bin)}

#YSI - 2018
{YSI.linear.AA.2018.bin = t(apply(YSI.linear.AA.2018[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2018.bin = t(apply(YSI.log.AA.2018[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2018.bin = t(apply(YSI.exp.AA.2018[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2018.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2018.bin)
YSI.log.AA.2018.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2018.bin)
YSI.exp.AA.2018.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2018.bin)}

#YSI - 2019
{YSI.linear.AA.2019.bin = t(apply(YSI.linear.AA.2019[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2019.bin = t(apply(YSI.log.AA.2019[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2019.bin = t(apply(YSI.exp.AA.2019[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2019.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2019.bin)
YSI.log.AA.2019.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2019.bin)
YSI.exp.AA.2019.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2019.bin)}

#YSI - 2020
{YSI.linear.AA.2020.bin = t(apply(YSI.linear.AA.2020[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2020.bin = t(apply(YSI.log.AA.2020[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2020.bin = t(apply(YSI.exp.AA.2020[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2020.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2020.bin)
YSI.log.AA.2020.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2020.bin)
YSI.exp.AA.2020.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2020.bin)}

#YSI - 2021
{YSI.linear.AA.2021.bin = t(apply(YSI.linear.AA.2021[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2021.bin = t(apply(YSI.log.AA.2021[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2021.bin = t(apply(YSI.exp.AA.2021[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2021.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2021.bin)
YSI.log.AA.2021.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2021.bin)
YSI.exp.AA.2021.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2021.bin)}

#Best-2017
{Best.linear.AA.2017.bin = t(apply(Best.linear.AA.2017[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2017.bin = t(apply(Best.log.AA.2017[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2017.bin = t(apply(Best.exp.AA.2017[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2017.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2017.bin)
Best.log.AA.2017.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2017.bin)
Best.exp.AA.2017.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2017.bin)}

#Best-2018
{Best.linear.AA.2018.bin = t(apply(Best.linear.AA.2018[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2018.bin = t(apply(Best.log.AA.2018[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2018.bin = t(apply(Best.exp.AA.2018[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2018.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2018.bin)
Best.log.AA.2018.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2018.bin)
Best.exp.AA.2018.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2018.bin)}

#Best-2019
{Best.linear.AA.2019.bin = t(apply(Best.linear.AA.2019[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2019.bin = t(apply(Best.log.AA.2019[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2019.bin = t(apply(Best.exp.AA.2019[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2019.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2019.bin)
Best.log.AA.2019.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2019.bin)
Best.exp.AA.2019.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2019.bin)}

#Best-2020
{Best.linear.AA.2020.bin = t(apply(Best.linear.AA.2020[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2020.bin = t(apply(Best.log.AA.2020[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2020.bin = t(apply(Best.exp.AA.2020[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2020.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2020.bin)
Best.log.AA.2020.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2020.bin)
Best.exp.AA.2020.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2020.bin)}

#Best-2021
{Best.linear.AA.2021.bin = t(apply(Best.linear.AA.2021[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2021.bin = t(apply(Best.log.AA.2021[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2021.bin = t(apply(Best.exp.AA.2021[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2021.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2021.bin)
Best.log.AA.2021.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2021.bin)
Best.exp.AA.2021.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2021.bin)
}


#Export all vectors to make the table in Excel
#Combine into multiple and 1 big table
{First.2017.all = rbind(Observed2017 = YSI.aa.2017.cut[-19],
                       Loggers.linear.AA.2017.FirstaaDOY,
                       Loggers.log.AA.2017.FirstaaDOY,
                       Loggers.exp.AA.2017.FirstaaDOY,
                       YSI.linear.AA.2017.FirstaaDOY,
                       YSI.log.AA.2017.FirstaaDOY,
                       YSI.exp.AA.2017.FirstaaDOY,
                       Best.linear.AA.2017.FirstaaDOY,
                       Best.log.AA.2017.FirstaaDOY,
                       Best.exp.AA.2017.FirstaaDOY)
First.2018.all = rbind(Observed2018 = YSI.aa.2018.cut,
                       Loggers.linear.AA.2018.FirstaaDOY,
                       Loggers.log.AA.2018.FirstaaDOY,
                       Loggers.exp.AA.2018.FirstaaDOY,
                       YSI.linear.AA.2018.FirstaaDOY,
                       YSI.log.AA.2018.FirstaaDOY,
                       YSI.exp.AA.2018.FirstaaDOY,
                       Best.linear.AA.2018.FirstaaDOY,
                       Best.log.AA.2018.FirstaaDOY,
                       Best.exp.AA.2018.FirstaaDOY)
First.2019.all = rbind(Observed2019 = YSI.aa.2019.cut,
                       Loggers.linear.AA.2019.FirstaaDOY,
                       Loggers.log.AA.2019.FirstaaDOY,
                       Loggers.exp.AA.2019.FirstaaDOY,
                       YSI.linear.AA.2019.FirstaaDOY,
                       YSI.log.AA.2019.FirstaaDOY,
                       YSI.exp.AA.2019.FirstaaDOY,
                       Best.linear.AA.2019.FirstaaDOY,
                       Best.log.AA.2019.FirstaaDOY,
                       Best.exp.AA.2019.FirstaaDOY)
First.2020.all = rbind(Observed2020 = YSI.aa.2020.cut,
                       Loggers.linear.AA.2020.FirstaaDOY,
                       Loggers.log.AA.2020.FirstaaDOY,
                       Loggers.exp.AA.2020.FirstaaDOY,
                       YSI.linear.AA.2020.FirstaaDOY,
                       YSI.log.AA.2020.FirstaaDOY,
                       YSI.exp.AA.2020.FirstaaDOY,
                       Best.linear.AA.2020.FirstaaDOY,
                       Best.log.AA.2020.FirstaaDOY,
                       Best.exp.AA.2020.FirstaaDOY)
First.2021.all = rbind(Observed2021 = YSI.aa.2021.cut,
                       YSI.linear.AA.2021.FirstaaDOY,
                       YSI.log.AA.2021.FirstaaDOY,
                       YSI.exp.AA.2021.FirstaaDOY,
                       Best.linear.AA.2021.FirstaaDOY,
                       Best.log.AA.2021.FirstaaDOY,
                       Best.exp.AA.2021.FirstaaDOY)


First.DOY.all = rbind(First.2017.all, First.2018.all, First.2019.all, First.2020.all,First.2021.all)
}
# write.csv(file = "./Output/First.DOY.all.csv", x = First.DOY.all, fileEncoding = "UTF-8")

#Plot the results
#2017
{
  #jpeg("./Output/AA.DOY.Obs-Mod-2017.jpg", height=8, width=8, res = 300, units="in")
  pdf("./Output/FigS6-FirstDayAnoxia2017.pdf", height=8, width=8)
  par(mfrow=c(3,3))
  par(mar=c(4,5,1,0.5)+.1)
  for(i in 2:nrow(First.2017.all)){
    if(i == 2){
      plot(First.2017.all[1,] ~ First.2017.all[i,],
           xlab = "",
           xlim = c(180,350),
           ylab = "First day of anoxia (observed)",
           xaxt = "n",
           las = 1, cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = c("Linear model"))
      legend("topleft", lty = 1, legend = ("1:1 line"))
      abline(0,1)
    }
    if(i == 3 | i ==4){
      plot(First.2017.all[1,] ~ First.2017.all[i,],
           xlab = "",
           xlim = c(180,350),
           ylab = "",
           xaxt="n",
           yaxt="n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = c("Log-linear model", "Exponential plateau model")[i-2])
      abline(0,1)
    }
    if(i == 5){
      plot(First.2017.all[1,] ~ First.2017.all[i,],
           xlab = "",
           xlim = c(180,350),
           ylab = "First day of anoxia (observed)",
           xaxt = "n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
    if(i == 6 | i== 7){
      plot(First.2017.all[1,] ~ First.2017.all[i,],
           xlab = "",
           xlim = c(180,350),
           ylab = "",
           xaxt="n",
           yaxt="n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
    if(i == 8){
      plot(First.2017.all[1,] ~ First.2017.all[i,],
           xlim = c(180,350),
           xlab = "First day of anoxia (modelled)",
           ylab = "First day of anoxia (observed)",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    if(i == 9| i == 10){
      plot(First.2017.all[1,] ~ First.2017.all[i,],
           xlim = c(180,350),
           xlab = "First day of anoxia (modelled)",
           ylab = "",
           yaxt = "n",
           las = 1,cex = 1.8, cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
  }
  dev.off()
}

#2018
{
  #jpeg("./Output/AA.DOY.Obs-Mod-2018.jpg", height=8, width=8, res = 300, units="in")
  pdf("./Output/FigS7-FirstDayAnoxia2018.pdf", height=8, width=8)
  par(mfrow=c(3,3))
  par(mar=c(4,5,1,0.5)+.1)
  for(i in 2:nrow(First.2018.all)){
    if(i == 2){
      plot(First.2018.all[1,] ~ First.2018.all[i,],
           xlab = "",
           xlim = c(190,350),
           ylab = "First day of anoxia (observed)",
           xaxt = "n",
           las = 1, cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = c("Linear model"))
      legend("topleft", lty = 1, legend = ("1:1 line"))
      abline(0,1)
    }
    if(i == 3 | i ==4){
      plot(First.2018.all[1,] ~ First.2018.all[i,],
           xlab = "",
           xlim = c(190,350),
           ylab = "",
           xaxt="n",
           yaxt="n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = c("Log-linear model", "Exponential plateau model")[i-2])
      abline(0,1)
    }
    if(i == 5){
      plot(First.2018.all[1,] ~ First.2018.all[i,],
           xlab = "",
           xlim = c(190,350),
           ylab = "First day of anoxia (observed)",
           xaxt = "n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
    if(i == 6 | i== 7){
      plot(First.2018.all[1,] ~ First.2018.all[i,],
           xlab = "",
           xlim = c(190,350),
           ylab = "",
           xaxt="n",
           yaxt="n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
    if(i == 8){
      plot(First.2018.all[1,] ~ First.2018.all[i,],
           xlim = c(190,350),
           xlab = "First day of anoxia (modelled)",
           ylab = "First day of anoxia (observed)",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    if(i == 9| i == 10){
      plot(First.2018.all[1,] ~ First.2018.all[i,],
           xlim = c(190,350),
           xlab = "First day of anoxia (modelled)",
           ylab = "",
           yaxt = "n",
           las = 1,cex = 1.8, cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
  }
  dev.off()
}

#2019
{
  #jpeg("./Output/AA.DOY.Obs-Mod-2019.jpg", height=8, width=8, res = 300, units="in")
  pdf("./Output/Fig. 4-FirstDayAnoxia2019.pdf", height=4, width=4)
par(mfrow=c(3,3))
par(mar=c(4,5,1,0.5)+.1)
for(i in 2:nrow(First.2019.all)){
  if(i == 2){
  plot(First.2019.all[1,] ~ First.2019.all[i,],
       xlab = "",
       xlim = c(190,350),
       ylab = "",
       xaxt = "n",
       las = 1, cex = 1.4, cex.axis=0.8, cex.lab = 0.9,
       main = c("Linear model"))
    # legend("topleft", lty = 1, legend = ("1:1 line"))
  abline(0,1)
  }
  if(i == 3 | i ==4){
    plot(First.2019.all[1,] ~ First.2019.all[i,],
         xlab = "",
         xlim = c(190,350),
         ylab = "",
         xaxt="n",
         yaxt="n",
         las = 1,cex = 1.4, cex.axis=0.8, cex.lab = 0.9,
         main = c("Log-linear model", "Exponential plateau model")[i-2])
    abline(0,1)
  }

  if(i == 5){
    plot(First.2019.all[1,] ~ First.2019.all[i,],
         xlab = "",
         xlim = c(190,350),
         ylab = "First day of anoxia (observed)",
         xaxt = "n",
         las = 1,cex = 1.4, cex.axis=0.8, cex.lab = 0.9,
         main = "")
    abline(0,1)
  }
  
  if(i == 6 | i== 7){
    plot(First.2019.all[1,] ~ First.2019.all[i,],
         xlab = "",
         xlim = c(190,350),
         ylab = "",
         xaxt="n",
         yaxt="n",
         las = 1,cex = 1.4, cex.axis=0.8, cex.lab = 0.9,
         main = "")
    abline(0,1)
  }
  
  if(i == 8| i == 10){
    plot(First.2019.all[1,] ~ First.2019.all[i,],
         xlim = c(190,350),
         xlab = "",
         ylab = "",
         las = 1,cex = 1.4, cex.axis=0.8, cex.lab = 0.9,
         main = "")
    abline(0,1)
  }
    if(i == 9){
      plot(First.2019.all[1,] ~ First.2019.all[i,],
           xlim = c(190,350),
           xlab = "First day of anoxia (modelled)",
           ylab = "",
           yaxt = "n",
           las = 1,cex = 1.4, cex.axis=0.8, cex.lab = 0.9,
           main = "")
      abline(0,1)
    }
  
}
dev.off()
}

#2020
{
  #jpeg("./Output/AA.DOY.Obs-Mod-2020.jpg", height=8, width=8, res = 290, units="in")
  pdf("./Output/FigS8-FirstDayAnoxia2020.pdf", height=8, width=8)
  par(mfrow=c(3,3))
  par(mar=c(4,5,1,0.5)+.1)
  for(i in 2:nrow(First.2020.all)){
    if(i == 2){
      plot(First.2020.all[1,] ~ First.2020.all[i,],
           xlab = "",
           xlim = c(170,290),
           ylab = "First day of anoxia (observed)",
           xaxt = "n",
           las = 1, cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = c("Linear model"))
      legend("topleft", lty = 1, legend = ("1:1 line"))
      abline(0,1)
    }
    if(i == 3 | i ==4){
      plot(First.2020.all[1,] ~ First.2020.all[i,],
           xlab = "",
           xlim = c(170,290),
           ylab = "",
           xaxt="n",
           yaxt="n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = c("Log-linear model", "Exponential plateau model")[i-2])
      abline(0,1)
    }
    if(i == 5){
      plot(First.2020.all[1,] ~ First.2020.all[i,],
           xlab = "",
           xlim = c(170,290),
           ylab = "First day of anoxia (observed)",
           xaxt = "n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
    if(i == 6 | i== 7){
      plot(First.2020.all[1,] ~ First.2020.all[i,],
           xlab = "",
           xlim = c(170,290),
           ylab = "",
           xaxt="n",
           yaxt="n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
    if(i == 8){
      plot(First.2020.all[1,] ~ First.2020.all[i,],
           xlim = c(170,290),
           xlab = "First day of anoxia (modelled)",
           ylab = "First day of anoxia (observed)",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    if(i == 9| i == 10){
      plot(First.2020.all[1,] ~ First.2020.all[i,],
           xlim = c(170,290),
           xlab = "First day of anoxia (modelled)",
           ylab = "",
           yaxt = "n",
           las = 1,cex = 1.8, cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
  }
  dev.off()
}

#2021
{
  #jpeg("./Output/AA.DOY.Obs-Mod-2021.jpg", height=8, width=8, res = 290, units="in")
  pdf("./Output/FigS9-FirstDayAnoxia2021.pdf", height=5.6, width=8)
  par(mfrow=c(2,3))
  par(mar=c(4,5,1,0.5)+.1)
  for(i in 2:nrow(First.2021.all)){
    if(i == 2){
      plot(First.2021.all[1,] ~ First.2021.all[i,],
           xlab = "",
           xlim = c(170,290),
           ylab = "First day of anoxia (observed)",
           xaxt = "n",
           las = 1, cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = c("Linear model"))
      legend("topleft", lty = 1, legend = ("1:1 line"))
      abline(0,1)
    }
    if(i == 3 | i ==4){
      plot(First.2021.all[1,] ~ First.2021.all[i,],
           xlab = "",
           xlim = c(170,290),
           ylab = "",
           xaxt="n",
           yaxt="n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = c("Log-linear model", "Exponential plateau model")[i-2])
      abline(0,1)
    }
    if(i == 5){
      plot(First.2021.all[1,] ~ First.2021.all[i,],
           xlab = "First day of anoxia (modelled)",
           xlim = c(170,290),
           ylab = "First day of anoxia (observed)",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
    if(i == 6 | i== 7){
      plot(First.2021.all[1,] ~ First.2021.all[i,],
           xlab = "First day of anoxia (modelled)",
           xlim = c(170,290),
           ylab = "",
           yaxt="n",
           las = 1,cex = 1.8,cex.axis=1.3, cex.lab = 1.4,
           main = "")
      abline(0,1)
    }
    
  }
  dev.off()
}



#########################################################################################
##################################Load nutrient data#####################################
#########################################################################################

NH4_SRP.data = read.csv("./Data/Raw/SRP-NH4_Ar.csv")
NH4_SRP.data$DOY = as.numeric(strftime(NH4_SRP.data$Date, format = "%j"))

#Remove 2016 as we don't have YSI profiles
NH4_SRP.data <- NH4_SRP.data[-grep("2016",NH4_SRP.data$Date),]

#Add column for anoxic age
NH4_SRP.data$AnoxA <- NA

ar.alpha <- Bats %>% slice(30:50)
#2017
{#Create new anoxic age vector with higher resolution
O2.consumption.rate <- O2.decay(b = coef(Ar.Jz.alpha.YSI.2017.exp)[2], 
                                k = coef(Ar.Jz.alpha.YSI.2017.exp)[3],
                                JzMax = coef(Ar.Jz.alpha.YSI.2017.exp)[1],
                                Model = "Exp",
                                alpha = ar.alpha[,2])
nday = 271 #Numbers of days to simulate
dbeg = 135 #First day of simulated stratification

#Create empty matrix with the good dimensions
O2.modelled.matrix = as.data.frame(matrix(nrow=length(O2.consumption.rate), ncol = nday))
#Set first profile at 12.3 mg O2/L
O2.modelled.matrix[,1] = 12.3

#Model daily profiles based on consumption rate
for(i in 2:ncol(O2.modelled.matrix)){
  O2.modelled.matrix[,i] = 12.3 - O2.consumption.rate*(i-1)
}
#Change negative values to 0
O2.modelled.matrix[O2.modelled.matrix<0] = 0

#Anoxic age threshold, here 2 for hypoxia
O2.threshold = 2

#CHange values below trehsold to 1, others to 0
O2.modelled.matrix.bin <- ifelse(O2.modelled.matrix <= O2.threshold, 1, 0)

#Create new object to calculate anoxic age
O2.modelled.anoxicage <- O2.modelled.matrix.bin
for(j in 2:ncol(O2.modelled.anoxicage)){
  temp = O2.modelled.anoxicage[,j-1]+O2.modelled.anoxicage[,j]
  temp = ifelse(temp > O2.modelled.anoxicage[,j-1], temp, 0)
  O2.modelled.anoxicage[,j] = temp
}

#Add a column for depths and colnames
O2.modelled.anoxicage = cbind(ar.alpha[,1], O2.modelled.anoxicage)
colnames(O2.modelled.anoxicage)[1] = "Depth"
colnames(O2.modelled.anoxicage)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

#Transform into dataframe
O2.modelled.anoxicage = as.data.frame(O2.modelled.anoxicage)

#Transform to long format
O2.modelled.anoxicage.long = pivot_longer(O2.modelled.anoxicage,
                                          cols = 2:ncol(O2.modelled.anoxicage),
                                          names_to = "DOY", values_to = "HypoxAge")
O2.modelled.anoxicage.long = as.data.frame(O2.modelled.anoxicage.long)

#See results in ugly graph
print(ggplot(O2.modelled.anoxicage.long)+
        geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
        scale_y_reverse()+
        scale_fill_gradient(low="white", high="red")+
        ggtitle(2017))

#Align the modeled oxygen profile with the real data to associate anoxic age with 
#reduced compounds concentration using the lowest sum of squared difference
year.i = grep(2017, YSI.list)
Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[year.i]))
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
#In 2020, Arendsee was sampled on 2020-05-26 (DOY 147) and 2020-08-25	
Ar.first.profile = Ar.long.deep[,-2] %>% filter(mm == "05", dd == "26") %>% 
  group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))

# Ar.second.profile = Ar.long.deep[,-2] %>% filter(mm == "08", dd == "25") %>% 
#   group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))

Match.temp.1 = O2.modelled.matrix[-c(19,21),]
for(i in 1:ncol(O2.modelled.matrix)){
  Match.temp.1[,i] = O2.modelled.matrix[,i] - Ar.first.profile[,2]
}

Match.temp.1.abs = (Match.temp.1)^2
Match.day.1 = which(colSums(Match.temp.1.abs) == min(colSums(Match.temp.1.abs)))
Match.day.1.RMSE = sqrt(sum(Match.temp.1.abs[Match.day.1])/length(Match.temp.1.abs[Match.day.1]))

O2.modelled.anoxicage.2017 = O2.modelled.anoxicage[,-c(2:(Match.day.1-1))]
colnames(O2.modelled.anoxicage.2017)[c(2:ncol(O2.modelled.anoxicage.2017))] = seq(147,ncol(O2.modelled.anoxicage.2017)+145,1)

#Transform to long format
O2.modelled.anoxicage.2017.long = pivot_longer(O2.modelled.anoxicage.2017,
                                               cols = 2:ncol(O2.modelled.anoxicage.2017),
                                               names_to = "DOY",
                                               values_to = "AnoxAge")

#We use these values ONLY to fill gaps from observed anoxic age
#Change TRUE/FALSE with 1/0
YSI.aa.2017[YSI.aa.2017==FALSE] = 0
YSI.aa.2017[YSI.aa.2017==TRUE] = 1

#Compile anoxic age
YSI.aa.2017.cumul = YSI.aa.2017[,-c(1:120)]
for(j in 2:ncol(YSI.aa.2017.cumul)){
temp = YSI.aa.2017.cumul[,j-1]+YSI.aa.2017.cumul[,j]
temp = ifelse(temp > YSI.aa.2017.cumul[,j-1], temp, 0)
YSI.aa.2017.cumul[,j] = temp
}

YSI.aa.2017.cumul = cbind(Depth=c(30:48),YSI.aa.2017.cumul)

YSI.aa.2017.cumul.long = pivot_longer(as.data.frame(YSI.aa.2017.cumul),
                                      cols = 2:ncol(YSI.aa.2017.cumul), 
                                      names_to = "DOY",
                                      values_to = "AnoxAge")

#Join modelled 48.5m to observed anoxic age
Combined.long = rbind(YSI.aa.2017.cumul.long,O2.modelled.anoxicage.2017.long[O2.modelled.anoxicage.2017.long$Depth==48.5,])

#See if it makes sense
Combined.aa.2017 = pivot_wider(Combined.long, names_from = DOY, values_from = AnoxAge)
#It does, continue with the long format

#Subset nutrient data by years
NH4_SRP.data.2017 = NH4_SRP.data[grep("2017", NH4_SRP.data$Date),]
NH4_SRP.data.2017[NH4_SRP.data.2017$Depth_m=="48.5", "Depth_m"] = 48

#Match calculated anoxic age with nutrient data
for(i in 1:nrow(NH4_SRP.data.2017)){
  if(NH4_SRP.data.2017[i,"DOY"] < 147){
    NH4_SRP.data.2017[i,"AnoxA"] = 0
    next
  }
  NH4_SRP.data.2017[i,"AnoxA"] = Combined.long [Combined.long $Depth==NH4_SRP.data.2017[i,"Depth_m"]&
                                                  Combined.long $DOY==NH4_SRP.data.2017[i,"DOY"],"AnoxAge"]
}

#Manually fix anoxic age for 48m deep; at 147DOY, it has been 21 days it is <2
# fix(NH4_SRP.data.2017) #DOY 143 = 17 and DOY 129 = 3

#Remove early dates (DOY < 120)
NH4_SRP.data.2017 = NH4_SRP.data.2017[-which(NH4_SRP.data.2017$DOY<120),]
}

#Repeat steps for 2018 and 2019
#2018
{
#Create new anoxic age vector with higher resolution
O2.consumption.rate <- O2.decay(b = coef(Ar.Jz.alpha.YSI.2018.exp)[2], 
                                k = coef(Ar.Jz.alpha.YSI.2018.exp)[3],
                                JzMax = coef(Ar.Jz.alpha.YSI.2018.exp)[1],
                                Model = "Exp",
                                alpha = ar.alpha[,2])
nday = 271 #Numbers of days to simulate
dbeg = 135 #First day of simulated stratification

#Create empty matrix with the good dimensions
O2.modelled.matrix = as.data.frame(matrix(nrow=length(O2.consumption.rate), ncol = nday))
#Set first profile at 12.3 mg O2/L
O2.modelled.matrix[,1] = 12.3

#Model daily profiles based on consumption rate
for(i in 2:ncol(O2.modelled.matrix)){
  O2.modelled.matrix[,i] = 12.3 - O2.consumption.rate*(i-1)
}
#Change negative values to 0
O2.modelled.matrix[O2.modelled.matrix<0] = 0

#Anoxic age threshold, here 2 for hypoxia
O2.threshold = 2

#CHange values below trehsold to 1, others to 0
O2.modelled.matrix.bin <- ifelse(O2.modelled.matrix <= O2.threshold, 1, 0)

#Create new object to calculate anoxic age
O2.modelled.anoxicage <- O2.modelled.matrix.bin
for(j in 2:ncol(O2.modelled.anoxicage)){
  temp = O2.modelled.anoxicage[,j-1]+O2.modelled.anoxicage[,j]
  temp = ifelse(temp > O2.modelled.anoxicage[,j-1], temp, 0)
  O2.modelled.anoxicage[,j] = temp
}

#Add a column for depths and colnames
O2.modelled.anoxicage = cbind(ar.alpha[,1], O2.modelled.anoxicage)
colnames(O2.modelled.anoxicage)[1] = "Depth"
colnames(O2.modelled.anoxicage)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))

#Transform into dataframe
O2.modelled.anoxicage = as.data.frame(O2.modelled.anoxicage)

#Transform to long format
O2.modelled.anoxicage.long = pivot_longer(O2.modelled.anoxicage,
                                          cols = 2:ncol(O2.modelled.anoxicage),
                                          names_to = "DOY", values_to = "HypoxAge")
O2.modelled.anoxicage.long = as.data.frame(O2.modelled.anoxicage.long)

#See results in ugly graph
print(ggplot(O2.modelled.anoxicage.long)+
        geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
        scale_y_reverse()+
        scale_fill_gradient(low="white", high="red")+
        ggtitle(2018))

#Align the modeled oxygen profile with the real data to associate anoxic age with 
#reduced compounds concentration using the lowest sum of squared difference
year.i = grep(2018, YSI.list)
Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[year.i]))
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
#In 2020, Arendsee was sampled on 2020-05-26 (DOY 147) and 2020-08-25	
Ar.first.profile = Ar.long.deep[,-2] %>% filter(mm == "05", dd == "26") %>% 
  group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))

# Ar.second.profile = Ar.long.deep[,-2] %>% filter(mm == "08", dd == "25") %>% 
#   group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))

Match.temp.1 = O2.modelled.matrix[-c(19:21),]
for(i in 1:ncol(O2.modelled.matrix)){
  Match.temp.1[,i] = O2.modelled.matrix[,i] - Ar.first.profile[,2]
}

Match.temp.1.abs = (Match.temp.1)^2
Match.day.1 = which(colSums(Match.temp.1.abs) == min(colSums(Match.temp.1.abs)))
Match.day.1.RMSE = sqrt(sum(Match.temp.1.abs[Match.day.1])/length(Match.temp.1.abs[Match.day.1]))

O2.modelled.anoxicage.2018 = O2.modelled.anoxicage[,-c(2:(Match.day.1-1))]
colnames(O2.modelled.anoxicage.2018)[c(2:ncol(O2.modelled.anoxicage.2018))] = seq(147,ncol(O2.modelled.anoxicage.2018)+145,1)

#Transform to long format
O2.modelled.anoxicage.2018.long = pivot_longer(O2.modelled.anoxicage.2018,
                                               cols = 2:ncol(O2.modelled.anoxicage.2018),
                                               names_to = "DOY",
                                               values_to = "AnoxAge")

#We use these values ONLY to fill gaps from observed anoxic age
#Change TRUE/FALSE with 1/0
YSI.aa.2018[YSI.aa.2018==FALSE] = 0
YSI.aa.2018[YSI.aa.2018==TRUE] = 1

#Compile anoxic age
YSI.aa.2018.cumul = YSI.aa.2018[,-c(1:120)]
for(j in 2:ncol(YSI.aa.2018.cumul)){
  temp = YSI.aa.2018.cumul[,j-1]+YSI.aa.2018.cumul[,j]
  temp = ifelse(temp > YSI.aa.2018.cumul[,j-1], temp, 0)
  YSI.aa.2018.cumul[,j] = temp
}

YSI.aa.2018.cumul = cbind(Depth=c(30:47),YSI.aa.2018.cumul)

YSI.aa.2018.cumul.long = pivot_longer(as.data.frame(YSI.aa.2018.cumul),
                                      cols = 2:ncol(YSI.aa.2018.cumul), 
                                      names_to = "DOY",
                                      values_to = "AnoxAge")

#Join modelled 48.5m to observed anoxic age
Combined.long = rbind(YSI.aa.2018.cumul.long,
                      O2.modelled.anoxicage.2018.long[O2.modelled.anoxicage.2018.long$Depth==47.5,],
                      O2.modelled.anoxicage.2018.long[O2.modelled.anoxicage.2018.long$Depth==48,],
                      O2.modelled.anoxicage.2018.long[O2.modelled.anoxicage.2018.long$Depth==48.5,])

#See if it makes sense
Combined.aa.2018 = pivot_wider(Combined.long, names_from = DOY, values_from = AnoxAge)
#It does, continue with the long format

#Subset nutrient data by years
NH4_SRP.data.2018 = NH4_SRP.data[grep("2018", NH4_SRP.data$Date),]

#Match calculated anoxic age with nutrient data
for(i in 1:nrow(NH4_SRP.data.2018)){
  if(NH4_SRP.data.2018[i,"DOY"] < 147){
    NH4_SRP.data.2018[i,"AnoxA"] = 0
    next
  }
  NH4_SRP.data.2018[i,"AnoxA"] = Combined.long[Combined.long $Depth==NH4_SRP.data.2018[i,"Depth_m"]&
                                                  Combined.long $DOY==NH4_SRP.data.2018[i,"DOY"],"AnoxAge"]
}

#Remove early dates (DOY<120)
NH4_SRP.data.2018 = NH4_SRP.data.2018[-which(NH4_SRP.data.2018$DOY<120),]

}
#Nothing to fix in 2018

#With 2019
{#Create new anoxic age vector with higher resolution
  O2.consumption.rate <- O2.decay(b = coef(Ar.Jz.alpha.YSI.2019.exp)[2], 
                                  k = coef(Ar.Jz.alpha.YSI.2019.exp)[3],
                                  JzMax = coef(Ar.Jz.alpha.YSI.2019.exp)[1],
                                  Model = "Exp",
                                  alpha = ar.alpha[,2])
  nday = 271 #Numbers of days to simulate
  dbeg = 135 #First day of simulated stratification
  
  #Create empty matrix with the good dimensions
  O2.modelled.matrix = as.data.frame(matrix(nrow=length(O2.consumption.rate), ncol = nday))
  #Set first profile at 12.3 mg O2/L
  O2.modelled.matrix[,1] = 12.3
  
  #Model daily profiles based on consumption rate
  for(i in 2:ncol(O2.modelled.matrix)){
    O2.modelled.matrix[,i] = 12.3 - O2.consumption.rate*(i-1)
  }
  #Change negative values to 0
  O2.modelled.matrix[O2.modelled.matrix<0] = 0
  
  #Anoxic age threshold, here 2 for hypoxia
  O2.threshold = 2
  
  #CHange values below trehsold to 1, others to 0
  O2.modelled.matrix.bin <- ifelse(O2.modelled.matrix <= O2.threshold, 1, 0)
  
  #Create new object to calculate anoxic age
  O2.modelled.anoxicage <- O2.modelled.matrix.bin
  for(j in 2:ncol(O2.modelled.anoxicage)){
    temp = O2.modelled.anoxicage[,j-1]+O2.modelled.anoxicage[,j]
    temp = ifelse(temp > O2.modelled.anoxicage[,j-1], temp, 0)
    O2.modelled.anoxicage[,j] = temp
  }
  
  #Add a column for depths and colnames
  O2.modelled.anoxicage = cbind(ar.alpha[,1], O2.modelled.anoxicage)
  colnames(O2.modelled.anoxicage)[1] = "Depth"
  colnames(O2.modelled.anoxicage)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))
  
  #Transform into dataframe
  O2.modelled.anoxicage = as.data.frame(O2.modelled.anoxicage)
  
  #Transform to long format
  O2.modelled.anoxicage.long = pivot_longer(O2.modelled.anoxicage,
                                            cols = 2:ncol(O2.modelled.anoxicage),
                                            names_to = "DOY", values_to = "HypoxAge")
  O2.modelled.anoxicage.long = as.data.frame(O2.modelled.anoxicage.long)
  
  #See results in ugly graph
  print(ggplot(O2.modelled.anoxicage.long)+
          geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
          scale_y_reverse()+
          scale_fill_gradient(low="white", high="red")+
          ggtitle(2019))
  
  #Align the modeled oxygen profile with the real data to associate anoxic age with 
  #reduced compounds concentration using the lowest sum of squared difference
  year.i = grep(2019, YSI.list)
  Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[year.i]))
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
  #In 2020, Arendsee was sampled on 2020-05-26 (DOY 147) and 2020-08-25	
  Ar.first.profile = Ar.long.deep[,-2] %>% filter(mm == "05", dd == "26") %>% 
    group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))
  
  # Ar.second.profile = Ar.long.deep[,-2] %>% filter(mm == "08", dd == "25") %>% 
  #   group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))
  
  Match.temp.1 = O2.modelled.matrix[-c(19:21),]
  for(i in 1:ncol(O2.modelled.matrix)){
    Match.temp.1[,i] = O2.modelled.matrix[,i] - Ar.first.profile[,2]
  }
  
  Match.temp.1.abs = (Match.temp.1)^2
  Match.day.1 = which(colSums(Match.temp.1.abs) == min(colSums(Match.temp.1.abs)))
  Match.day.1.RMSE = sqrt(sum(Match.temp.1.abs[Match.day.1])/length(Match.temp.1.abs[Match.day.1]))
  
  O2.modelled.anoxicage.2019 = O2.modelled.anoxicage[,-c(2:(Match.day.1-1))]
  colnames(O2.modelled.anoxicage.2019)[c(2:ncol(O2.modelled.anoxicage.2019))] = seq(147,ncol(O2.modelled.anoxicage.2019)+145,1)
  
  
  #Change a few values at 47.5 and 48m deep based on independant profiles 
  O2.modelled.anoxicage.2019[19,c(43:234)] = seq(1,192,1)
  O2.modelled.anoxicage.2019[20,c(42:234)] = seq(1,193,1)
  O2.modelled.anoxicage.2019[21,c(41:234)] = seq(1,194,1)
  
  #Transform to long format
  O2.modelled.anoxicage.2019.long = pivot_longer(O2.modelled.anoxicage.2019,
                                                 cols = 2:ncol(O2.modelled.anoxicage.2019),
                                                 names_to = "DOY",
                                                 values_to = "AnoxAge")
  

  
  

  #We use these values ONLY to fill gaps from observed anoxic age
  #Change TRUE/FALSE with 1/0
  YSI.aa.2019[YSI.aa.2019==FALSE] = 0
  YSI.aa.2019[YSI.aa.2019==TRUE] = 1
  
  #Compile anoxic age
  YSI.aa.2019.cumul = YSI.aa.2019[,-c(1:120)]
  for(j in 2:ncol(YSI.aa.2019.cumul)){
    temp = YSI.aa.2019.cumul[,j-1]+YSI.aa.2019.cumul[,j]
    temp = ifelse(temp > YSI.aa.2019.cumul[,j-1], temp, 0)
    YSI.aa.2019.cumul[,j] = temp
  }
  
  YSI.aa.2019.cumul = cbind(Depth=c(30:47),YSI.aa.2019.cumul)
  
  YSI.aa.2019.cumul.long = pivot_longer(as.data.frame(YSI.aa.2019.cumul),
                                        cols = 2:ncol(YSI.aa.2019.cumul), 
                                        names_to = "DOY",
                                        values_to = "AnoxAge")
  
  #Join modelled 48.5m to observed anoxic age
  Combined.long = rbind(YSI.aa.2019.cumul.long,
                        O2.modelled.anoxicage.2019.long[O2.modelled.anoxicage.2019.long$Depth==47.5,],
                        O2.modelled.anoxicage.2019.long[O2.modelled.anoxicage.2019.long$Depth==48,])
  
  #See if it makes sense
  Combined.aa.2019 = pivot_wider(Combined.long, names_from = DOY, values_from = AnoxAge)
  #Some depths are weird, but let's see

  
  #Subset nutrient data by years
  NH4_SRP.data.2019 = NH4_SRP.data[grep("2019", NH4_SRP.data$Date),]
  NH4_SRP.data.2019[NH4_SRP.data.2019$Depth_m==47.8,"Depth_m"] = 48
  NH4_SRP.data.2019[NH4_SRP.data.2019$Depth_m==47.6,"Depth_m"] = 48
  NH4_SRP.data.2019[NH4_SRP.data.2019$Depth_m==47.5,"Depth_m"] = 47
  
  #Match calculated anoxic age with nutrient data
  for(i in 1:nrow(NH4_SRP.data.2019)){
    if(NH4_SRP.data.2019[i,"DOY"] < 147){
      NH4_SRP.data.2019[i,"AnoxA"] = 0
      next
    }
    NH4_SRP.data.2019[i,"AnoxA"] = Combined.long[Combined.long $Depth==NH4_SRP.data.2019[i,"Depth_m"]&
                                                   Combined.long $DOY==NH4_SRP.data.2019[i,"DOY"],"AnoxAge"]
  }
  
  #Remove early dates (DOY<120)
  NH4_SRP.data.2019 = NH4_SRP.data.2019[-which(NH4_SRP.data.2019$DOY<120),]
 
  #Remove 1 line that looks like an outlier (lowest values for no apparent reason)
  NH4_SRP.data.2019 =  NH4_SRP.data.2019[-which(NH4_SRP.data.2019$SRP_mgL==min(NH4_SRP.data.2019$SRP_mgL)),]

  }

#With 2020
{#Create new anoxic age vector with higher resolution
  ar.alpha.2020.NH4_SRP = rbind(ar.alpha[c(1:13),], c(42.5,alpha_42.5), ar.alpha[c(14:21),])
  O2.consumption.rate <- O2.decay(b = coef(Ar.Jz.alpha.YSI.2020.exp)[2], 
                                  k = coef(Ar.Jz.alpha.YSI.2020.exp)[3],
                                  JzMax = coef(Ar.Jz.alpha.YSI.2020.exp)[1],
                                  Model = "Exp",
                                  alpha = ar.alpha.2020.NH4_SRP[,2])
  nday = 271 #Numbers of days to simulate
  dbeg = 135 #First day of simulated stratification
  
  #Create empty matrix with the good dimensions
  O2.modelled.matrix = as.data.frame(matrix(nrow=length(O2.consumption.rate), ncol = nday))
  #Set first profile at 12.3 mg O2/L
  O2.modelled.matrix[,1] = 12.3
  
  #Model daily profiles based on consumption rate
  for(i in 2:ncol(O2.modelled.matrix)){
    O2.modelled.matrix[,i] = 12.3 - O2.consumption.rate*(i-1)
  }
  #Change negative values to 0
  O2.modelled.matrix[O2.modelled.matrix<0] = 0
  
  #Anoxic age threshold, here 2 for hypoxia
  O2.threshold = 2
  
  #CHange values below trehsold to 1, others to 0
  O2.modelled.matrix.bin <- ifelse(O2.modelled.matrix <= O2.threshold, 1, 0)
  
  #Create new object to calculate anoxic age
  O2.modelled.anoxicage <- O2.modelled.matrix.bin
  for(j in 2:ncol(O2.modelled.anoxicage)){
    temp = O2.modelled.anoxicage[,j-1]+O2.modelled.anoxicage[,j]
    temp = ifelse(temp > O2.modelled.anoxicage[,j-1], temp, 0)
    O2.modelled.anoxicage[,j] = temp
  }
  
  #Add a column for depths and colnames
  O2.modelled.anoxicage = cbind(ar.alpha.2020.NH4_SRP[,1], O2.modelled.anoxicage)
  colnames(O2.modelled.anoxicage)[1] = "Depth"
  colnames(O2.modelled.anoxicage)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))
  
  #Transform into dataframe
  O2.modelled.anoxicage = as.data.frame(O2.modelled.anoxicage)
  
  #Transform to long format
  O2.modelled.anoxicage.long = pivot_longer(O2.modelled.anoxicage,
                                            cols = 2:ncol(O2.modelled.anoxicage),
                                            names_to = "DOY", values_to = "HypoxAge")
  O2.modelled.anoxicage.long = as.data.frame(O2.modelled.anoxicage.long)
  
  #See results in ugly graph
  print(ggplot(O2.modelled.anoxicage.long)+
          geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
          scale_y_reverse()+
          scale_fill_gradient(low="white", high="red")+
          ggtitle(2020))
  
  #Align the modeled oxygen profile with the real data to associate anoxic age with 
  #reduced compounds concentration using the lowest sum of squared difference
  year.i = grep(2020, YSI.list)
  Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[year.i]))
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
  #In 2020, Arendsee was sampled on 2020-05-26 (DOY 147) and 2020-08-25	
  Ar.first.profile = Ar.long.deep[,-2] %>% filter(mm == "05", dd == "26") %>% 
    group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))
  
  # Ar.second.profile = Ar.long.deep[,-2] %>% filter(mm == "08", dd == "25") %>% 
  #   group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))
  
  Match.temp.1 = O2.modelled.matrix[-c(14,20:22),]
  for(i in 1:ncol(O2.modelled.matrix)){
    Match.temp.1[,i] = O2.modelled.matrix[,i] - Ar.first.profile[,2]
  }
  
  Match.temp.1.abs = (Match.temp.1)^2
  Match.day.1 = which(colSums(Match.temp.1.abs) == min(colSums(Match.temp.1.abs)))
  Match.day.1.RMSE = sqrt(sum(Match.temp.1.abs[Match.day.1])/length(Match.temp.1.abs[Match.day.1]))
  
  O2.modelled.anoxicage.2020 = O2.modelled.anoxicage[,-c(2:(Match.day.1-1))]
  colnames(O2.modelled.anoxicage.2020)[c(2:ncol(O2.modelled.anoxicage.2020))] = seq(147,ncol(O2.modelled.anoxicage.2020)+145,1)
  
  
  # #Change a few values at 47.5 and 48m deep based on independant profiles 
  # O2.modelled.anoxicage.2020[19,c(43:234)] = seq(1,192,1)
  # O2.modelled.anoxicage.2020[20,c(42:234)] = seq(1,193,1)
  # O2.modelled.anoxicage.2020[21,c(41:234)] = seq(1,194,1)
  # 
  #Transform to long format
  O2.modelled.anoxicage.2020.long = pivot_longer(O2.modelled.anoxicage.2020,
                                                 cols = 2:ncol(O2.modelled.anoxicage.2020),
                                                 names_to = "DOY",
                                                 values_to = "AnoxAge")
  
  
  
  
  
  #We use these values ONLY to fill gaps from observed anoxic age
  #Change TRUE/FALSE with 1/0
  YSI.aa.2020[YSI.aa.2020==FALSE] = 0
  YSI.aa.2020[YSI.aa.2020==TRUE] = 1
  
  #Compile anoxic age
  YSI.aa.2020.cumul = YSI.aa.2020[,-c(1:120)]
  for(j in 2:ncol(YSI.aa.2020.cumul)){
    temp = YSI.aa.2020.cumul[,j-1]+YSI.aa.2020.cumul[,j]
    temp = ifelse(temp > YSI.aa.2020.cumul[,j-1], temp, 0)
    YSI.aa.2020.cumul[,j] = temp
  }
  
  YSI.aa.2020.cumul = cbind(Depth=c(30:47),YSI.aa.2020.cumul)
  
  YSI.aa.2020.cumul.long = pivot_longer(as.data.frame(YSI.aa.2020.cumul),
                                        cols = 2:ncol(YSI.aa.2020.cumul), 
                                        names_to = "DOY",
                                        values_to = "AnoxAge")
  

  #Join modelled 48.5m to observed anoxic age
  Combined.long = rbind(YSI.aa.2020.cumul.long,
                        O2.modelled.anoxicage.2020.long[O2.modelled.anoxicage.2020.long$Depth==48,])
  
  #See if it makes sense
  Combined.aa.2020 = pivot_wider(Combined.long, names_from = DOY, values_from = AnoxAge)

  
  #Select NH4 and SRP from other campaign dataset
  NH4_SRP.data.2020 <- filter(Chemistry, lake =="ar") %>% filter(depth >=30)
  NH4_SRP.data.2020 = NH4_SRP.data.2020[,c(2,28,30,31,3,4)]
  
  #Add DOY column
  NH4_SRP.data.2020$DOY = 0
  #Set DOY according to dates
  NH4_SRP.data.2020[grep("05-26", NH4_SRP.data.2020$date),"DOY"] = 147
  NH4_SRP.data.2020[grep("08-25", NH4_SRP.data.2020$date),"DOY"] = 238
    
  #Change 42.5 by 43m because it makes no difference with anoxic age
  NH4_SRP.data.2020[NH4_SRP.data.2020$depth==42.5,"depth"] = 42
  
  #Add anoxic age column to NH$-SRP table
  NH4_SRP.data.2020$AnoxA = 0
  
  #Match calculated anoxic age with nutrient data
  for(i in 1:nrow(NH4_SRP.data.2020)){
    if(NH4_SRP.data.2020[i,"DOY"] < 147){
      NH4_SRP.data.2020[i,"AnoxA"] = 0
      next
    }
    NH4_SRP.data.2020[i,"AnoxA"] = Combined.long[Combined.long $Depth==NH4_SRP.data.2020[i,"depth"]&
                                                   Combined.long $DOY==NH4_SRP.data.2020[i,"DOY"],"AnoxAge"]
  }
  
  #Change colnames to match other data source
  colnames(NH4_SRP.data.2020)[c(1,2,4,5,6)] = c("Date","Depth_m","O2_mgL","SRP_mgL","NH4.N_mgL")
  
  #Change SRP into mgL (was in ugL)
  NH4_SRP.data.2020[,5] = NH4_SRP.data.2020[,5]/1000
  
  # #Remove 1 line that looks like an outlier (lowest values for no apparent reason)
  # NH4_SRP.data.2020 =  NH4_SRP.data.2020[-which(NH4_SRP.data.2020$SRP_mgL==min(NH4_SRP.data.2020$SRP_mgL)),]
}

#With 2021
{#Create new anoxic age vector with higher resolution
  O2.consumption.rate <- O2.decay(b = coef(Ar.Jz.alpha.YSI.2021.exp)[2], 
                                  k = coef(Ar.Jz.alpha.YSI.2021.exp)[3],
                                  JzMax = coef(Ar.Jz.alpha.YSI.2021.exp)[1],
                                  Model = "Exp",
                                  alpha = ar.alpha[,2])
  nday = 271 #Numbers of days to simulate
  dbeg = 135 #First day of simulated stratification
  
  #Create empty matrix with the good dimensions
  O2.modelled.matrix = as.data.frame(matrix(nrow=length(O2.consumption.rate), ncol = nday))
  #Set first profile at 12.3 mg O2/L
  O2.modelled.matrix[,1] = 12.3
  
  #Model daily profiles based on consumption rate
  for(i in 2:ncol(O2.modelled.matrix)){
    O2.modelled.matrix[,i] = 12.3 - O2.consumption.rate*(i-1)
  }
  #Change negative values to 0
  O2.modelled.matrix[O2.modelled.matrix<0] = 0
  
  #Anoxic age threshold, here 2 for hypoxia
  O2.threshold = 2
  
  #CHange values below trehsold to 1, others to 0
  O2.modelled.matrix.bin <- ifelse(O2.modelled.matrix <= O2.threshold, 1, 0)
  
  #Create new object to calculate anoxic age
  O2.modelled.anoxicage <- O2.modelled.matrix.bin
  for(j in 2:ncol(O2.modelled.anoxicage)){
    temp = O2.modelled.anoxicage[,j-1]+O2.modelled.anoxicage[,j]
    temp = ifelse(temp > O2.modelled.anoxicage[,j-1], temp, 0)
    O2.modelled.anoxicage[,j] = temp
  }
  
  #Add a column for depths and colnames
  O2.modelled.anoxicage = cbind(ar.alpha[,1], O2.modelled.anoxicage)
  colnames(O2.modelled.anoxicage)[1] = "Depth"
  colnames(O2.modelled.anoxicage)[2:(nday+1)] = paste(seq(dbeg,dbeg+nday-1,1))
  
  #Transform into dataframe
  O2.modelled.anoxicage = as.data.frame(O2.modelled.anoxicage)
  
  #Transform to long format
  O2.modelled.anoxicage.long = pivot_longer(O2.modelled.anoxicage,
                                            cols = 2:ncol(O2.modelled.anoxicage),
                                            names_to = "DOY", values_to = "HypoxAge")
  O2.modelled.anoxicage.long = as.data.frame(O2.modelled.anoxicage.long)
  
  #See results in ugly graph
  print(ggplot(O2.modelled.anoxicage.long)+
          geom_raster(aes(x=as.numeric(DOY), y=Depth, fill=HypoxAge))+
          scale_y_reverse()+
          scale_fill_gradient(low="white", high="red")+
          ggtitle(2021))
  
  #Align the modeled oxygen profile with the real data to associate anoxic age with 
  #reduced compounds concentration using the lowest sum of squared difference
  year.i = grep(2021, YSI.list)
  Ar.YSI = read.csv(paste0("./Data/Raw/Arendsee-cleaned/",YSI.list[year.i]))
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
  #In 2021, Arendsee was sampled on 2021-05-26 (DOY 147) and 2021-08-25	
  Ar.first.profile = Ar.long.deep[,-2] %>% filter(mm == "05", dd == "26") %>% 
    group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))
  
  # Ar.second.profile = Ar.long.deep[,-2] %>% filter(mm == "08", dd == "25") %>% 
  #   group_by(Depth_m) %>% summarize(DO_mgL = mean(DO_mgL))
  
  Match.temp.1 = O2.modelled.matrix[-c(19:21),]
  for(i in 1:ncol(O2.modelled.matrix)){
    Match.temp.1[,i] = O2.modelled.matrix[,i] - Ar.first.profile[,2]
  }
  
  Match.temp.1.abs = (Match.temp.1)^2
  Match.day.1 = which(colSums(Match.temp.1.abs) == min(colSums(Match.temp.1.abs)))
  Match.day.1.RMSE = sqrt(sum(Match.temp.1.abs[Match.day.1])/length(Match.temp.1.abs[Match.day.1]))
  
  O2.modelled.anoxicage.2021 = O2.modelled.anoxicage[,-c(2:(Match.day.1-1))]
  colnames(O2.modelled.anoxicage.2021)[c(2:ncol(O2.modelled.anoxicage.2021))] = seq(147,ncol(O2.modelled.anoxicage.2021)+145,1)
  
  
  # #Change a few values at 47.5 and 48m deep based on independant profiles 
  # O2.modelled.anoxicage.2021[19,c(43:234)] = seq(1,192,1)
  # O2.modelled.anoxicage.2021[20,c(42:234)] = seq(1,193,1)
  # O2.modelled.anoxicage.2021[21,c(41:234)] = seq(1,194,1)
  # 
  #Transform to long format
  O2.modelled.anoxicage.2021.long = pivot_longer(O2.modelled.anoxicage.2021,
                                                 cols = 2:ncol(O2.modelled.anoxicage.2021),
                                                 names_to = "DOY",
                                                 values_to = "AnoxAge")
  
  
  
  
  
  #We use these values ONLY to fill gaps from observed anoxic age
  #Change TRUE/FALSE with 1/0
  YSI.aa.2021[YSI.aa.2021==FALSE] = 0
  YSI.aa.2021[YSI.aa.2021==TRUE] = 1
  
  #Compile anoxic age
  YSI.aa.2021.cumul = YSI.aa.2021[,-c(1:120)]
  for(j in 2:ncol(YSI.aa.2021.cumul)){
    temp = YSI.aa.2021.cumul[,j-1]+YSI.aa.2021.cumul[,j]
    temp = ifelse(temp > YSI.aa.2021.cumul[,j-1], temp, 0)
    YSI.aa.2021.cumul[,j] = temp
  }
  
  YSI.aa.2021.cumul = cbind(Depth=c(30:47),YSI.aa.2021.cumul)
  
  YSI.aa.2021.cumul.long = pivot_longer(as.data.frame(YSI.aa.2021.cumul),
                                        cols = 2:ncol(YSI.aa.2021.cumul), 
                                        names_to = "DOY",
                                        values_to = "AnoxAge")
  
  
  #Join modelled 47.5m to observed anoxic age
  Combined.long = rbind(YSI.aa.2021.cumul.long,
                        O2.modelled.anoxicage.2021.long[O2.modelled.anoxicage.2021.long$Depth==47.5,])
  
  
  #See if it makes sense
  Combined.aa.2021 = pivot_wider(Combined.long, names_from = DOY, values_from = AnoxAge)
  
  #Create NH4-SRP dataset from yet another campaign file
  NH4_SRP.data.2021 <- data.frame(Date = "27-07-2021",
                                  Depth_m = c(30,43,45,46,47),
                                  T_C = NA,
                                  O2_mgL = NA,
                                  SRP_mgL = c(153,266,299,318,326)/1000,
                                  NH4.N_mgL = c(0.07,1.13,1.46,1.53,1.66),
                                  DOY = 208,
                                  AnoxA = 0,
                                  year = 3,
                                  cex = 1)

  #Match calculated anoxic age with nutrient data
  for(i in 1:nrow(NH4_SRP.data.2021)){
    if(NH4_SRP.data.2021[i,"DOY"] < 147){
      NH4_SRP.data.2021[i,"AnoxA"] = 0
      next
    }
    NH4_SRP.data.2021[i,"AnoxA"] = Combined.long[Combined.long $Depth==NH4_SRP.data.2021[i,"Depth_m"]&
                                                   Combined.long $DOY==NH4_SRP.data.2021[i,"DOY"],"AnoxAge"]
  }

  # #Remove 1 line that looks like an outlier (lowest values for no apparent reason)
  # NH4_SRP.data.2020 =  NH4_SRP.data.2020[-which(NH4_SRP.data.2020$SRP_mgL==min(NH4_SRP.data.2020$SRP_mgL)),]
}

# #plot results
# pdf("./Exploration output/NH4-SRP_2017.pdf", width=8, height=4)
# par(mfrow=c(1,2))
# par(mar=c(4,5,2,1)+.1)
# plot(NH4_SRP.data.2017$SRP_mgL ~ NH4_SRP.data.2017$AnoxA,
#      las = 1,
#      ylab = expression(SRP~(mgL^-1)),
#      xlab = "Anoxic age (days)")
# plot(NH4_SRP.data.2017$NH4.N_mgL ~ NH4_SRP.data.2017$AnoxA,
#      las = 1,
#      ylab = expression(NH[4]^"+"~(mg~L^-1)),
#      xlab = "Anoxic age (days)")
# dev.off()
# 
# pdf("./Exploration output/NH4-SRP_2018.pdf", width=8, height=4)
# par(mfrow=c(1,2))
# par(mar=c(4,5,2,1)+.1)
# plot(NH4_SRP.data.2018$SRP_mgL ~ NH4_SRP.data.2018$AnoxA,
#      las = 1,
#      ylab = expression(SRP~(mgL^-1)),
#      xlab = "Anoxic age (days)",
#      pch = 16,
#      col = c("#73BAE6","#C7144C","#33A02C","#FFD700","#C742B5")[as.numeric(as.factor(NH4_SRP.data.2018$Depth_m))])
# plot(NH4_SRP.data.2018$NH4.N_mgL ~ NH4_SRP.data.2018$AnoxA,
#      las = 1,
#      ylab = expression(NH[4]^"+"~(mg~L^-1)),
#      xlab = "Anoxic age (days)",
#      pch = 16,
#      col = c("#73BAE6","#C7144C","#33A02C","#FFD700","#C742B5")[as.numeric(as.factor(NH4_SRP.data.2018$Depth_m))])
# dev.off()
# 
# pdf("./Exploration output/NH4-SRP_2019.pdf", width=8, height=4)
# par(mfrow=c(1,2))
# par(mar=c(4,5,2,1)+.1)
# plot(NH4_SRP.data.2019$SRP_mgL ~ NH4_SRP.data.2019$AnoxA,
#      las = 1,
#      ylab = expression(SRP~(mgL^-1)),
#      xlab = "Anoxic age (days)")
# plot(NH4_SRP.data.2019$NH4.N_mgL ~ NH4_SRP.data.2019$AnoxA,
#      las = 1,
#      ylab = expression(NH[4]^"+"~(mg~L^-1)),
#      xlab = "Anoxic age (days)")
# dev.off()
# 
# pdf("./Exploration output/NH4-SRP_2020.pdf", width=8, height=4)
# par(mfrow=c(1,2))
# par(mar=c(4,5,2,1)+.1)
# plot(NH4_SRP.data.2020$SRP_mgL ~ NH4_SRP.data.2020$AnoxA,
#      las = 1,
#      ylab = expression(SRP~(mgL^-1)),
#      xlab = "Anoxic age (days)")
# plot(NH4_SRP.data.2020$NH4.N_mgL ~ NH4_SRP.data.2020$AnoxA,
#      las = 1,
#      ylab = expression(NH[4]^"+"~(mg~L^-1)),
#      xlab = "Anoxic age (days)")
# dev.off()
# 
# 
# pdf("./Exploration output/NH4-SRP_2021.pdf", width=8, height=4)
# par(mfrow=c(1,2))
# par(mar=c(4,5,2,1)+.1)
# plot(NH4_SRP.data.2021$SRP_mgL ~ NH4_SRP.data.2021$AnoxA,
#      las = 1,
#      ylab = expression(SRP~(mgL^-1)),
#      xlab = "Anoxic age (days)")
# plot(NH4_SRP.data.2021$NH4.N_mgL ~ NH4_SRP.data.2021$AnoxA,
#      las = 1,
#      ylab = expression(NH[4]^"+"~(mg~L^-1)),
#      xlab = "Anoxic age (days)")
# dev.off()


#Add a column for pch values
NH4_SRP.data.2017$year = 1
NH4_SRP.data.2017$cex = 1
NH4_SRP.data.2019$year = 2
NH4_SRP.data.2019$cex = 2
NH4_SRP.data.2020$year = 3
NH4_SRP.data.2020$cex = 1


NH4_SRP.data.all = rbind(NH4_SRP.data.2017,
                         NH4_SRP.data.2019,
                         NH4_SRP.data.2020)
# plot(NH4_SRP.data.all$SRP_mgL ~ NH4_SRP.data.all$AnoxA,
#      las = 1,
#      ylab = expression(SRP~(mgL^-1)),
#      xlab = "Anoxic age (days)", pch = NH4_SRP.data.all$year)
# legend("bottomright", pch = c(0,1,2), legend = c("2017", "2019", "2020"))
# plot(NH4_SRP.data.all$NH4.N_mgL ~ NH4_SRP.data.all$AnoxA,
#      las = 1,
#      ylab = expression(NH[4]^"+"~(mg~L^-1)),
#      xlab = "Anoxic age (days)", pch = NH4_SRP.data.all$year)

#Test without anoxic age = 0
pdf("./Output/Fig. 1, Nutrients-AA, no boxplot.pdf", width=8, height=4)
par(mfrow=c(1,2))
par(mar=c(4,5,1,2)+0.1)
plot(NH4_SRP.data.all$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA!=0],
     las = 1,
     ylim = c(0.1,0.36),
     ylab = expression(SRP~(mg~L^-1)),
     xlab = "Anoxic age (days)",
     pch = c(15,16,17)[NH4_SRP.data.all$year[NH4_SRP.data.all$AnoxA!=0]],
     col = c("#73BAE6","#C7144C", "#FFD700")[NH4_SRP.data.all$year[NH4_SRP.data.all$AnoxA!=0]],
     cex = NH4_SRP.data.all$cex[NH4_SRP.data.all$AnoxA!=0])
legend("bottomright",
       pch = c(22,21,24),
       pt.cex = c(1,2,1),
       legend = c("2017", "2019","2020"),
       pt.bg = c("#73BAE6","#C7144C", "#FFD700"))
plot(NH4_SRP.data.all$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA!=0],
     las = 1,
     ylim = c(0,1.1),
     ylab = expression(NH[4]^"+"~(mg~L^-1)),
     xlab = "Anoxic age (days)",
     pch = c(15,16,17)[NH4_SRP.data.all$year[NH4_SRP.data.all$AnoxA!=0]],
     col = c("#73BAE6","#C7144C", "#FFD700")[NH4_SRP.data.all$year[NH4_SRP.data.all$AnoxA!=0]],
     cex = NH4_SRP.data.all$cex[NH4_SRP.data.all$AnoxA!=0])
dev.off()

pdf("./Output/Fig. 1, Nutrients-AA.Boxplot.pdf", width=8, height=4)
par(mfrow=c(1,2))
par(mar=c(4,5,1,2)+0.1)
boxplot(NH4_SRP.data.all$SRP_mgL[NH4_SRP.data.all$AnoxA==0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA==0],
     las = 1,
     ylim = c(0.1,0.36),
     ylab = expression(SRP~(mg~L^-1)),
     xlab = "Anoxic age (days)",
     pch = NH4_SRP.data.all$year[NH4_SRP.data.all$AnoxA!=0],
     cex = NH4_SRP.data.all$cex[NH4_SRP.data.all$AnoxA!=0])
boxplot(NH4_SRP.data.all$NH4.N_mgL[NH4_SRP.data.all$AnoxA==0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA==0],
     las = 1,
     ylim = c(0,1.1),
     ylab = expression(NH[4]^"+"~(mg~L^-1)),
     xlab = "Anoxic age (days)",
     pch = NH4_SRP.data.all$year[NH4_SRP.data.all$AnoxA!=0],
     cex = NH4_SRP.data.all$cex[NH4_SRP.data.all$AnoxA!=0])
dev.off()

#Add alpha in the dataframe
NH4_SRP.data.all$alpha = ar.alpha[match(x = NH4_SRP.data.all$Depth_m, table = ar.alpha[,1]),2]


SRP.anox.lm = lm(NH4_SRP.data.all$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA!=0])
SRP.all.lm = lm(NH4_SRP.data.all$SRP_mgL ~ NH4_SRP.data.all$AnoxA)
NH4.anox.lm = lm(NH4_SRP.data.all$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA!=0])
NH4.all.lm = lm(NH4_SRP.data.all$NH4.N_mgL ~ NH4_SRP.data.all$AnoxA)

SRP.all.DOY.lm = lm(NH4_SRP.data.all$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$DOY[NH4_SRP.data.all$AnoxA!=0])
NH4.all.DOY.lm = lm(NH4_SRP.data.all$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$DOY[NH4_SRP.data.all$AnoxA!=0])

#multiple regressions in anoxia. mlm = AnoxA, d = Depth, t = time
SRP.anox.mlm.d = lm(NH4_SRP.data.all$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA!=0]+NH4_SRP.data.all$Depth_m[NH4_SRP.data.all$AnoxA!=0])
NH4.anox.mlm.d = lm(NH4_SRP.data.all$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA!=0]+NH4_SRP.data.all$Depth_m[NH4_SRP.data.all$AnoxA!=0])

SRP.anox.mlm.t = lm(NH4_SRP.data.all$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA!=0] +NH4_SRP.data.all$DOY[NH4_SRP.data.all$AnoxA!=0])
NH4.anox.mlm.t = lm(NH4_SRP.data.all$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$AnoxA[NH4_SRP.data.all$AnoxA!=0]+NH4_SRP.data.all$DOY[NH4_SRP.data.all$AnoxA!=0])

SRP.anox.d.t = lm(NH4_SRP.data.all$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$Depth_m[NH4_SRP.data.all$AnoxA!=0] +NH4_SRP.data.all$DOY[NH4_SRP.data.all$AnoxA!=0])
NH4.anox.d.t = lm(NH4_SRP.data.all$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.all$Depth_m[NH4_SRP.data.all$AnoxA!=0]+NH4_SRP.data.all$DOY[NH4_SRP.data.all$AnoxA!=0])


#Simple lm
summary(SRP.anox.lm) #R2 = 0.4779
summary(NH4.anox.lm) #R2 = 0.442
summary(SRP.all.lm) #R2 = 0.6189
summary(NH4.all.lm) #R2 = 0.547

summary(SRP.all.DOY.lm) #R2 = 0.002
summary(NH4.all.DOY.lm) #R2 = 0.002
#Multiple lm
summary(SRP.anox.mlm.d) #R2adj = 0.57
summary(NH4.anox.mlm.d) #R2adj = 0.52
summary(SRP.anox.mlm.t) #R2adj = 0.5477
summary(NH4.anox.mlm.t) #R2adj = 0.5758
summary(SRP.anox.d.t) #R2adj = 0.4395
summary(NH4.anox.d.t) #R2adj = 0.319



#Relationships with scaled predictive variables
NH4_SRP.data.sc = cbind(scale(NH4_SRP.data.all$Depth_m,center = T, scale = T),
                        scale(NH4_SRP.data.all$DOY,center = T, scale = T),
                        scale(NH4_SRP.data.all$AnoxA,center = T, scale = T),
                        NH4_SRP.data.all$SRP_mgL,
                        NH4_SRP.data.all$NH4.N_mgL)
  
colnames(NH4_SRP.data.sc) = c("Depth_m","DOY","AnoxA","SRP_mgL","NH4.N_mgL")
NH4_SRP.data.sc = as.data.frame(NH4_SRP.data.sc)


SRP.anox.lm.sc = lm(NH4_SRP.data.sc$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$AnoxA[NH4_SRP.data.all$AnoxA!=0])
SRP.all.lm.sc = lm(NH4_SRP.data.sc$SRP_mgL ~ NH4_SRP.data.sc$AnoxA)
NH4.anox.lm.sc = lm(NH4_SRP.data.sc$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$AnoxA[NH4_SRP.data.all$AnoxA!=0])
NH4.all.lm.sc = lm(NH4_SRP.data.sc$NH4.N_mgL ~ NH4_SRP.data.sc$AnoxA)

SRP.all.DOY.lm.sc = lm(NH4_SRP.data.sc$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$DOY[NH4_SRP.data.all$AnoxA!=0])
NH4.all.DOY.lm.sc = lm(NH4_SRP.data.sc$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$DOY[NH4_SRP.data.all$AnoxA!=0])

#multiple regressions in anoxia. mlm = AnoxA, d = Depth, t = time
SRP.anox.mlm.d.sc = lm(NH4_SRP.data.sc$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$AnoxA[NH4_SRP.data.all$AnoxA!=0]+NH4_SRP.data.sc$Depth_m[NH4_SRP.data.all$AnoxA!=0])
NH4.anox.mlm.d.sc = lm(NH4_SRP.data.sc$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$AnoxA[NH4_SRP.data.all$AnoxA!=0]+NH4_SRP.data.sc$Depth_m[NH4_SRP.data.all$AnoxA!=0])

SRP.anox.mlm.t.sc = lm(NH4_SRP.data.sc$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$AnoxA[NH4_SRP.data.all$AnoxA!=0] +NH4_SRP.data.sc$DOY[NH4_SRP.data.all$AnoxA!=0])
NH4.anox.mlm.t.sc = lm(NH4_SRP.data.sc$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$AnoxA[NH4_SRP.data.all$AnoxA!=0]+NH4_SRP.data.sc$DOY[NH4_SRP.data.all$AnoxA!=0])

SRP.anox.d.t.sc = lm(NH4_SRP.data.sc$SRP_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$Depth_m[NH4_SRP.data.all$AnoxA!=0] +NH4_SRP.data.sc$DOY[NH4_SRP.data.all$AnoxA!=0])
NH4.anox.d.t.sc = lm(NH4_SRP.data.sc$NH4.N_mgL[NH4_SRP.data.all$AnoxA!=0] ~ NH4_SRP.data.sc$Depth_m[NH4_SRP.data.all$AnoxA!=0]+NH4_SRP.data.sc$DOY[NH4_SRP.data.all$AnoxA!=0])

#Simple lm
summary(SRP.anox.lm.sc) #R2 = 0.4779
summary(NH4.anox.lm.sc) #R2 = 0.4403
summary(SRP.all.lm.sc) #R2 = 0.6189
summary(NH4.all.lm.sc) #R2 = 0.547

summary(SRP.all.DOY.lm.sc) #R2 = 0.0023
summary(NH4.all.DOY.lm.sc) #R2 = 0.0028
#Multiple lm
summary(SRP.anox.mlm.d.sc) #R2adj = 0.5725
summary(NH4.anox.mlm.d.sc) #R2adj = 0.522
summary(SRP.anox.mlm.t.sc) #R2adj = 0.5477
summary(NH4.anox.mlm.t.sc) #R2adj = 0.5758
summary(SRP.anox.d.t.sc) #R2adj = 0.4395
summary(NH4.anox.d.t.sc) #R2adj = 0.319






###########################################################################################
#########################################BONES########################################
#####################################################################################










Match.temp.1 = ar.2017.YSI.Log
for(i in 1:ncol(ar.2017.YSI.Log)){
  Match.temp.1[,i] = ar.2017.YSI.Log[,i] - Ar.first.profile[,2]
}

Match.temp.1.abs = (Match.temp.1)^2
Match.day.1 = which(colSums(Match.temp.1.abs) == min(colSums(Match.temp.1.abs)))
Match.day.1.RMSE = sqrt(sum(Match.temp.1.abs[33])/length(Match.temp.1.abs[33]))

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
Match.day.1.Living.RMSE = sqrt(sum(Match.temp.1.abs.Living[Match.day.1.Living])/length(Match.temp.1.abs.Living[Match.day.1.Living]))


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
Match.day.1.low.RMSE = sqrt(sum(Match.temp.1.low.abs[Match.day.1.low])/length(Match.temp.1.low.abs[Match.day.1.low]))



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
Match.day.1.low.living.RMSE = sqrt(sum(Match.temp.1.low.abs.living[Match.day.1.low.living])/length(Match.temp.1.low.abs.living[Match.day.1.low.living]))



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

Match.temp.1.lowbest.abs = (Match.temp.1.lowbest)^2
Match.day.1.lowbest = which(colSums(Match.temp.1.lowbest.abs) == min(colSums(Match.temp.1.lowbest.abs)))
Match.day.1.lowbest.RMSE = sqrt(sum(Match.temp.1.lowbest.abs[Match.day.1.lowbest])/length(Match.temp.1.lowbest.abs[Match.day.1.lowbest]))



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



####################################Low temporal resolution, best case, living################################

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
Match.day.1.lowbest.living.RMSE = sqrt(sum(Match.temp.1.lowbest.abs.living[Match.day.1.lowbest.living])/length(Match.temp.1.lowbest.abs.living[Match.day.1.lowbest.living]))




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




#Observed anoxic age 2020
interpolated.2020.bin.cumul <- interpolated.2020.bin
for(j in 2:ncol(interpolated.2020.bin.cumul)){
  temp = interpolated.2020.bin.cumul[,j-1]+interpolated.2020.bin.cumul[,j]
  temp = ifelse(temp > interpolated.2020.bin.cumul[,j-1], temp, 0)
  interpolated.2020.bin.cumul[,j] = temp
}

#Test with different O2 thresholds
interpolated.2020.bin.5 <- ifelse(interpolated.2020 <= 5, 1, 0)
interpolated.2020.bin.cumul.5 <- interpolated.2020.bin.5
for(j in 2:ncol(interpolated.2020.bin.cumul.5)){
  temp = interpolated.2020.bin.cumul.5[,j-1]+interpolated.2020.bin.cumul.5[,j]
  temp = ifelse(temp > interpolated.2020.bin.cumul.5[,j-1], temp, 0)
  interpolated.2020.bin.cumul.5[,j] = temp
}

x.graph = c(interpolated.2020.bin.cumul.5[c(1,11,16,17,18),147],interpolated.2020.bin.cumul.5[c(1,11,16,17,18),238])
y.graph = c(ar.chem.1.single$SRP,ar.chem.2.single$SRP)
plot(y.graph ~ x.graph)



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
                    P = ar.chem.2.single$P_d, Ca = ar.chem.1.single$Ca_d,
                    DOY = 147)
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
                    P = ar.chem.2.single$P_d, Ca = ar.chem.2.single$Ca_d,
                    DOY = 207)

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



par(mfrow=c(2,3))
par(mar=c(4,5,1,1)+.1)
plot(CH4~DOY, data=aa.matrix, pch = 16, las = 1,
     xlim = c(145,215), xlab = "Day of year", col = "black",
     ylab = expression(CH[4]~(mu*mol~L^-1)),
     cex.lab = 1.3, cex.axis=1.3)
plot(CH4~aa.depth, data=aa.matrix, pch = 16, las = 1,
     xlim = c(30,50), xlab = "Depth (m)", col = "black",
     ylab = expression(CH[4]~(mu*mol~L^-1)),
     cex.lab = 1.3, cex.axis=1.3)
plot(CH4~aa.highres.log, data=aa.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", col = "black",
     ylab = expression(CH[4]~(mu*mol~L^-1)),
     cex.lab = 1.3, cex.axis=1.3)

plot(SRP~DOY, data=aa.matrix, pch = 16, las = 1,
     xlim = c(145,215), xlab = "Day of year", col = "black",
     ylab = expression(SRP~(mu*mol~L^-1)),
     cex.lab = 1.3, cex.axis=1.3)
plot(SRP~aa.depth, data=aa.matrix, pch = 16, las = 1,
     xlim = c(30,50), xlab = "Depth (m)", col = "black",
     ylab = expression(SRP~(mu*mol~L^-1)),
     cex.lab = 1.3, cex.axis=1.3)
plot(SRP~aa.highres.log, data=aa.matrix, pch = 16, las = 1,
     xlim = c(0,90), xlab = "Anoxic age (d)", col = "black",
     ylab = expression(SRP~(mu*mol~L^-1)),
     cex.lab = 1.3, cex.axis=1.3)









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






  # 
#   YSIprofil.bin.cumul.temp = YSIprofil.bin[[i]] 
# 
# 
# for(j in 2:ncol(YSIprofil.bin.cumul.temp)){
#   temp = YSIprofil.bin.cumul.temp[,j-1]+YSIprofil.bin.cumul.temp[,j]
#   temp = ifelse(temp > YSIprofil.bin.cumul.temp[,j-1], temp, 0)
#   YSIprofil.bin.cumul.temp[,j] = temp
# }
#   YSIprofil.bin.cumul[[i]] = YSIprofil.bin.cumul.temp
# }


####################################################################################################
#############################################Bones##################################################
####################################################################################################


#This step could be made automatic with all the objects produced earlier
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





###########################################################################################
##########################Comparing JZ~alpha among methods (Fig. 1)########################
###########################################################################################  

###################################### Maybe not needed anymore#########################
# 
# pdf("./Method Comparisons, two axis.pdf", width=10)
# par(mfrow=c(1,2))
# par(mar=c(5,5.1,2,3)+0.1)
# plot(Jz.mat.day[,2] ~ Jz.mat[,4],
#      las = 1,
#      ylim = c(0.05,0.20),
#      xlim = c(0.02,0.2),
#      xaxt="n",
#      xlab = "",
#      ylab = expression(Jz~(mg~O[2]~L^-1~d^-1)),
#      main = "2017",
#      pch = 16,
#      col = "#9BBB59")
# axis(1,c(0.02,0.05,0.1,0.15,0.2),line=1,col="red",col.ticks="red",col.axis="red")
# mtext(expression(alpha(z)),1,line=1,at=0,col="red")
# axis(1,c(Jz.mat[,4],0.2),
#      labels=c(30,35,40,45,47,48),line=3,col="blue",col.ticks="blue",col.axis="blue")
# mtext("Depth (m)",1,line=3,at=-0.01,col="blue")
# 
# abline(lm(Jz.mat.day[,2] ~ Jz.mat[,4]), lty = 2,col = "#9BBB59")
# bob=Jz.mat.day[,2]
# bob2=nlsLM(bob~a*log10(b*Jz.mat[,4]),
#            start = list(a=0.03, b=150))
# curve(coef(bob2)[1] * log10(coef(bob2)[2]*x), add=T, col = "#9BBB59")
# 
# points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
#        pch = 16,
#        col = "#00B0F0")
# abline(lm(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1]), lty = 2,col = "#00B0F0")
# bob=Jz.YSI.list[[1]][-c(1:3)]
# bob2=nlsLM(bob~a*log10(b*alpha.YSI.list[[1]][,1]),
#            start = list(a=0.03, b=150))
# curve(coef(bob2)[1] * log10(coef(bob2)[2]*x), add=T, col = "#00B0F0")
# 
# points(Jz.lowYSI.list[[1]] ~ alpha.lowYSI.list[[1]][,1],
#        pch = 16,
#        col = "#002060")
# abline(lm(Jz.lowYSI.list[[1]] ~ alpha.lowYSI.list[[1]][,1]), lty = 2,col = "#002060")
# bob=Jz.lowYSI.list[[1]]
# bob2=nlsLM(bob~a*log10(b*alpha.lowYSI.list[[1]][,1]),
#            start = list(a=0.03, b=150))
#  curve(coef(bob2)[1] * log10(coef(bob2)[2]*x), add=T, col = "#002060")
# legend("bottomright",
#        legend = c("DO Loggers", "High temporal res", "Low temporal res"),
#        text.col = c("#9BBB59", "#00B0F0","#002060"))
# 
# 
# plot(Jz.mat.day[,2] ~ Jz.mat[,4],
#      las = 1,
#      ylim = c(0.05,0.20),
#      xlim = c(0.02,0.2),
#      xlab = "",
#      ylab = expression(Jz~(mg~O[2]~L^-1~d^-1)),
#      xaxt="n",
#      main = "DO loggers",
#      pch = 16,
#      col = "#9BBB59")
# abline(lm(Jz.mat.day[,2] ~ Jz.mat[,4]), lty = 2,col = "#9BBB59")
# bob=Jz.mat.day[,2]
# bob2=nlsLM(bob~a*log10(b*Jz.mat[,4]),
#            start = list(a=0.03, b=150))
#  curve(coef(bob2)[1] * log10(coef(bob2)[2]*x), add=T, col = "#9BBB59")
# axis(1,c(0.02,0.05,0.1,0.15,0.2),line=1,col="red",col.ticks="red",col.axis="red")
# mtext(expression(alpha(z)),1,line=1,at=0,col="red")
# axis(1,c(Jz.mat[,4],0.2),
#      labels=c(30,35,40,45,47,48),line=3,col="blue",col.ticks="blue",col.axis="blue")
# mtext("Depth (m)",1,line=3,at=-0.01,col="blue")
# 
# points(Jz.mat.day[,3] ~ Jz.mat[,4],
#      las = 1,
#      ylim = c(0.05,0.20),
#      xlab = expression(alpha(z)),
#      ylab = "Jz",
#      main = "",
#        pch = 16,
#        col = "#00B0F0")
# abline(lm(Jz.mat.day[,3] ~ Jz.mat[,4]), lty = 2,col = "#00B0F0")
# bob=Jz.mat.day[,3]
# bob2=nlsLM(bob~a*log10(b*Jz.mat[,4]),
#            start = list(a=0.03, b=150))
#  curve(coef(bob2)[1] * log10(coef(bob2)[2]*x), add=T, col = "#00B0F0")
# 
# 
# points(Jz.mat.day[,4] ~ Jz.mat[,4],
#      las = 1,
#      ylim = c(0.05,0.20),
#      xlab = expression(alpha(z)),
#      ylab = "Jz",
#      main = "",
#      pch = 16,
#        col = "#002060")
# abline(lm(Jz.mat.day[,4] ~ Jz.mat[,4]), lty = 2,col = "#002060")
# bob=Jz.mat.day[,4]
# bob2=nlsLM(bob~a*log10(b*Jz.mat[,4]),
#            start = list(a=0.03, b=150))
#  curve(coef(bob2)[1] * log10(coef(bob2)[2]*x), add=T, col = "#002060")
# legend("bottomright",
#        legend = c("2017", "2018", "2019"),
#        text.col = c("#9BBB59", "#00B0F0","#002060"))
# dev.off()
# 



###########################################################################################
####################################Calculate anoxix age###################################
###########################################################################################  

#List of all objects for linear, log-linear and exponential plateau relationships
# Loggers, daily
# Ar.Living.2016.day (linear)
# Ar.Living.2017.day (linear)
# Ar.Living.2018.day (linear)
# Ar.Living.2019.day (linear)
# 
# Ar.Living.2016.day.SegLm.log (log)
# Ar.Living.2017.day.SegLm.log (log)
# Ar.Living.2018.day.SegLm.log (log)
# Ar.Living.2019.day.SegLm.log (log)
# 
# Ar.Living.2017.day.SegLm.exp (exp)
# Ar.Living.2018.day.SegLm.exp (exp)
# Ar.Living.2019.day.SegLm.exp (exp)
# 
# High temporal resolution
# Ar.Jz.alpha.YSI.2017 (linear)
# Ar.Jz.alpha.YSI.2018 (linear)
# Ar.Jz.alpha.YSI.2019 (linear)
# Ar.Jz.alpha.YSI.2020 (linear)
# 
# Ar.Jz.alpha.YSI.2017.log
# Ar.Jz.alpha.YSI.2018.log
# Ar.Jz.alpha.YSI.2019.log
# Ar.Jz.alpha.YSI.2020.log
# 
# Ar.Jz.alpha.YSI.2017.exp
# Ar.Jz.alpha.YSI.2018.exp
# Ar.Jz.alpha.YSI.2019.exp
# Ar.Jz.alpha.YSI.2020.exp
# 
# Rinko (2020)
# Ar.Jz.alpha.rinko (linear)
# Ar.log.lm (log)
# Ar.exp.lm (exp)
# 
# Best case (2017:2020)
# Ar.Jz.lm.best.list (linear, log and exp, all years)


##########################################################################################
###################Calculate Jz Arendsee using low temporal resolution####################
##########################################################################################

#################################THIS SECTION WORKS BY ITSELF############################
##################################CAN BE ITS OWN SCRIPT WHEN#############################
#################################OTHER SECTIONS ARE VERIFIED#############################


Rinko <- read.csv("./Data/Raw/rinkos.csv", row.names=1)
Interpolation <- function(V1, V2, n){
  output = matrix(nrow = length(V1), ncol=n)
  for(i in 1:n){
    output[,i] = V1 + (V2-V1)/n * i
  }
  return(output)
}

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


ar.alpha <- Bats%>% slice(c(30:38)) %>% select(alpha) #30 to 38 meters because the rate goes down after
ar.alpha.rinko <- ar.alpha
#With Rinko profiles calculated at the begining of this script
Ar.Jz.rinko =filter(Output.Jz, lake == "ar") %>%
  filter(depth_1m>=30) %>%
  filter(depth_1m<=38) %>%
  select(Jz)
Ar.Jz.rinko = as.vector(Ar.Jz.rinko[,1])

#Linear relationship
Ar.Jz.alpha.rinko = lm(Ar.Jz.rinko ~ ar.alpha.rinko[,1]) #R2 = 0.94
summary(Ar.Jz.alpha.rinko)
#Log-linear
Ar.log.lm <- nlsLM(Ar.Jz.rinko ~ a*log10(k*ar.alpha[,1]),
                   start = list(a=0.08, k=200)) #R2 = 0.969


#Exponential
Ar.exp.lm = nlsLM(Ar.Jz.rinko ~ j.m - (j.m - b) * exp(-k*ar.alpha[,1]),
                  start = list(j.m = 0.15, b=0.05, k=5)) #0.98
# pred <- predict(Ar.exp.lm, ar.alpha[,1])
# rss <- sum((pred - Ar.Jz.rinko) ^ 2)
# tss <- sum((Ar.Jz.rinko - mean(Ar.Jz.rinko)) ^ 2)
# rsq <- 1 - rss/tss

{
  # pdf("./Output/Ar.Jz-Alpha.Rinko.pdf", width = 5.3, height = 5.3)
  png("./Output/Ar.Jz-Alpha.Rinko.YSIcomparison.png", width = 5.3, height = 5.3, units = "in", res=300)
  # png("./Output/Ar.Jz-Alpha.Rinko.png", width = 5.3, height = 5.3, units = "in", res=300) #Use this line when removing YSI points
  par(mfrow=c(1,1))
  par(mar=c(4,5,1,1)+.1)
  plot(Ar.Jz.rinko ~ ar.alpha.rinko[,1],
       xlim = c(0.02, 0.25),
       ylim = c(0.07,0.4),
       las = 1,
       xlab = expression(alpha(z)),
       ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)))
  abline(Ar.Jz.alpha.rinko, col = "black")
  curve(coef(Ar.log.lm)[1]*log10(x*coef(Ar.log.lm)[2]), add=T, col ="blue")
  curve(coef(Ar.exp.lm)[1] - (coef(Ar.exp.lm)[1] - coef(Ar.exp.lm)[2])*
          exp(-1*coef(Ar.exp.lm)[3]*x), 
        add=T, col = "chartreuse3")
  
  #Remove this legend if comparing with YSI data
  # legend("topleft", legend = c("Linear", "Log-linear", "Exponential plateau"),
  #        text.col = c("black","blue", "chartreuse3"))
  
  #Comparison with YSI data from the same year
  points(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
         pch = 21, bg = "red")
  curve(coef(Ar.Jz.alpha.YSI.2020.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2020.log)[2]),
        add=T, col ="red")
  
  legend("topright", legend = c("Linear", "Log-linear", "Exponential plateau", "YSI log-linear"),
         text.col = c("black","blue", "chartreuse3", "red"))
  
  legend("topleft", legend = c("YSI data", "Rinko data"),
         pch = 21, pt.bg = c("red","white"))
  dev.off()
}


#With matching YSI profiles
YSI.list = list.files("./Data/Raw/Arendsee-cleaned")
#Keep only csv files
YSI.list = YSI.list[grep(pattern = ".csv",list.files("./Data/Raw/Arendsee-cleaned"))]
YSI.yrs = c(2017,2018,2019,2020,2021)
Ar.Jz.lm.list = list()
Jz.lowYSI.list = list()
alpha.lowYSI.list = list()
for(i in 1:5){
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
  
  #Keep only matching date to Rinko profiles
  #Alpha.select was determined a posteriori with Jz~alpha graph. Once Jz is decreasing due to poor
  #temporal resolution, we cut the selection
  if(i==1) {DOY.ini = DoY$ar[[1]][1]-1
  DOY.end = DoY$ar[[1]][2]
  alpha.select = c(30:39)}
  if(i==2){ DOY.ini = DoY$ar[[1]][1]
  DOY.end = DoY$ar[[1]][2]+2
  alpha.select = c(30:44)}
  if(i==3){ DOY.ini = DoY$ar[[1]][1]
  DOY.end = DoY$ar[[1]][2]
  alpha.select = c(30:44)}
  if(i==4){ DOY.ini = DoY$ar[[1]][1]
  DOY.end = DoY$ar[[1]][2]
  alpha.select = c(30:39)}
  if(i==5){ DOY.ini = DoY$ar[[1]][1]
  DOY.end = DoY$ar[[1]][2]
  alpha.select = c(30:37)}
  
  
  Ar.long.deep.lowres <- Ar.long.deep[which(Ar.long.deep$DOY==DOY.ini | Ar.long.deep$DOY==DOY.end),]
  Ar.long.deep.lowres.a <- Ar.long.deep.lowres %>% 
    group_by(Depth_m, DOY) %>% summarize(DO_mgL = mean(DO_mgL)) 
  Jz.lowres = vector(length = nrow(Ar.long.deep.lowres.a)/2)
  Depth.lowres = unique(Ar.long.deep.lowres.a$Depth_m)
  DOY.lowres = max(Ar.long.deep.lowres.a$DOY) - min(Ar.long.deep.lowres.a$DOY)
  for(a in 1:(nrow(Ar.long.deep.lowres.a)/2)){
    Jz.lowres[a] = (max(Ar.long.deep.lowres.a[Ar.long.deep.lowres.a$Depth_m==Depth.lowres[a],"DO_mgL"]) - 
                      min(Ar.long.deep.lowres.a[Ar.long.deep.lowres.a$Depth_m==Depth.lowres[a],"DO_mgL"]))/DOY.lowres
  }
  
  ar.alpha <- Bats%>% slice(alpha.select) %>% select(alpha)
  Jz.lowres.slice = Jz.lowres[1:length(alpha.select)]
  Ar.Jz.lowres = lm(Jz.lowres.slice ~ ar.alpha[,1])
  Ar.Jz.lowres.log = lm(Jz.lowres.slice ~ log10(ar.alpha[,1]))
  Ar.Jz.lm.list[[i]] = list(summary(Ar.Jz.lowres), summary(Ar.Jz.lowres.log))
  
  Jz.lowYSI.list[[i]] <- Jz.lowres.slice
  alpha.lowYSI.list[[i]] <- ar.alpha
  
  if(i == 4){
    #Compare O2 profiles in 2020 between YSI and Rinko
    compare.depth.1 = as.data.frame(Ar.long.deep.lowres.a[which(Ar.long.deep.lowres.a$DOY==147),"Depth_m"])
    compare.O2.1 = as.data.frame(Ar.long.deep.lowres.a[which(Ar.long.deep.lowres.a$DOY==147),"DO_mgL"])
    
    compare.depth.2 = as.data.frame(Ar.long.deep.lowres.a[which(Ar.long.deep.lowres.a$DOY==238),"Depth_m"])
    compare.O2.2 = as.data.frame(Ar.long.deep.lowres.a[which(Ar.long.deep.lowres.a$DOY==238),"DO_mgL"])
    
  }
  
}



#Compare Rinko and YSI O2 profiles
{
  pdf("./Exploration output/Rinko-YSI profiles.pdf", width = 8, height = 4)
  par(mfrow=c(1,2))
  plot(Rinko[Rinko$lake=="ar" & Rinko$month==5, "depth"] ~ 
         Rinko[Rinko$lake=="ar" & Rinko$month==5, "DO_mgL"],
       ylim = c(rev(range(Rinko[Rinko$lake=="ar" & Rinko$month==5, "depth"]))),
       xlab = "DO (mgL)",
       ylab = "Depth",
       pch = 16,
       col = "blue")
  points(compare.depth.1[,1] ~ compare.O2.1[,1],
         pch = 16,
         col = "red")
  legend("topleft",
         legend = c("Rinko", "YSI"),
         text.col = c("Blue", "Red"))
  
  plot(Rinko[Rinko$lake=="ar" & Rinko$month==8, "depth"] ~ 
         Rinko[Rinko$lake=="ar" & Rinko$month==8, "DO_mgL"],
       ylim = c(rev(range(Rinko[Rinko$lake=="ar" & Rinko$month==8, "depth"]))),
       xlab = "DO (mgL)",
       ylab = "Depth",
       pch = 16,
       col = "blue")
  points(compare.depth.2[,1] ~ compare.O2.2[,1],
         pch = 16,
         col = "red")
  dev.off()
}

#Rinko-2020
{Rinko.linear.AA.2020.bin = t(apply(Rinko.linear.AA.2020[[1]][,-1], 1, function(X) X==1))
  Rinko.log.AA.2020.bin = t(apply(Rinko.log.AA.2020[[1]][,-1], 1, function(X) X==1))
  Rinko.exp.AA.2020.bin = t(apply(Rinko.exp.AA.2020[[1]][,-1], 1, function(X) X==1))
  
  Rinko.linear.AA.2020.FirstaaDOY = First.aa.doy.fct(Rinko.linear.AA.2020.bin)
  Rinko.log.AA.2020.FirstaaDOY = First.aa.doy.fct(Rinko.log.AA.2020.bin)
  Rinko.exp.AA.2020.FirstaaDOY = First.aa.doy.fct(Rinko.exp.AA.2020.bin)}

#Anoxic age for low temporal Res (Rinko) - 2020
#Really off, but 2020-Rinko has a very poor depth resolution of Jz. It makes sense
{Rinko.linear.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.rinko)[1],
                                        k = coef(Ar.Jz.alpha.rinko)[2],
                                        Model = "Livingstone",
                                        alpha = ar.alpha,
                                        data.source = "YSI",
                                        year = 2020)
  
  #Only one somewhat OK as extrapolation or higher a(z) gave something reasonable
  Rinko.log.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.log.lm)[1],
                                      k = coef(Ar.log.lm)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2020)
  
  #Really off, but 2020-Rinko has a very poor depth resolution of Jz. It makes sense
  Rinko.exp.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.exp.lm)[2],
                                      k = coef(Ar.exp.lm)[3],
                                      JzMax = coef(Ar.exp.lm)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2020)}

,
Rinko.linear.AA.2020.FirstaaDOY,
Rinko.log.AA.2020.FirstaaDOY,
Rinko.exp.AA.2020.FirstaaDOY
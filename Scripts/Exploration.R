library(RColorBrewer)
library(plot.matrix)
library(dplyr)
library(segmented)
library(minpack.lm)
library(tidyr)
library(ggplot2)

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





setwd("D:/Postdoc/Allemagne/Github/AnoxicAge")
#########################################################################################
###########################Calculate Jz Arendsee using loggers###########################
#########################################################################################

#################################THIS SECTION WORKS BY ITSELF############################
##################################CAN BE ITS OWN SCRIPT WHEN#############################
#################################OTHER SECTIONS ARE VERIFIED#############################

Full_data <- read.csv("../full_database.csv")
logger.meta <- read.csv("./Data/loggermeta.csv")
Bats <- read.csv("./Data/Raw/bats.csv")
ar.depths = c(30,35,40,45,47)
ar.year = c(2016,2017,2018,2019,2020)
Ar.full <- filter(.data = Full_data, lake == "ar")

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
Jz.mat[,6] = filter(Bats, lake =="ar") %>% arrange(depth_1m)  %>% slice(c(30,35,40,45,47)+1) %>% select(alpha)

#Linear relationships
Ar.Living.2016.lin = lm(Ar.Jz.2016 ~ Jz.mat[-5,6])
Ar.Living.2017.lin = lm(Ar.Jz.2017 ~ Jz.mat[,6])
Ar.Living.2018.lin = lm(Ar.Jz.2018 ~ Jz.mat[,6])
Ar.Living.2019.lin = lm(Ar.Jz.2019 ~ Jz.mat[,6])
Ar.Living.2020.lin = lm(Ar.Jz.2020 ~ Jz.mat[,6])

#Log relationships
temp.object = Jz.mat[-5,6]
Ar.Living.2016.log = nlsLM(Ar.Jz.2016 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))
temp.object = Jz.mat[,6]
Ar.Living.2017.log = nlsLM(Ar.Jz.2017 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))
Ar.Living.2018.log = nlsLM(Ar.Jz.2018 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))
Ar.Living.2019.log = nlsLM(Ar.Jz.2019 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))
Ar.Living.2020.log = nlsLM(Ar.Jz.2020 ~ b*log10(k*temp.object),
                           start = list(b = 0.08, k = 100))


#Exponential plateau relationships
temp.object = Jz.mat[-5,6]
temp.max = max(Ar.Jz.2016)
Ar.Living.2016.exp = nlsLM(Ar.Jz.2016 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=0.05, k=5))
temp.object = Jz.mat[,6]
temp.max = max(Ar.Jz.2017)
Ar.Living.2017.exp = nlsLM(Ar.Jz.2017 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=0.05, k=5))
temp.max = max(Ar.Jz.2018)
Ar.Living.2018.exp = nlsLM(Ar.Jz.2018 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=0.05, k=5))
temp.max = max(Ar.Jz.2019)
Ar.Living.2019.exp = nlsLM(Ar.Jz.2019 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=0.05, k=5))
temp.max = max(Ar.Jz.2020)
Ar.Living.2020.exp = nlsLM(Ar.Jz.2020 ~ t.m - (t.m - b) * exp(-k*temp.object),
                           start = list(t.m = temp.max, b=0.05, k=5))
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
abline(lm(Ar.Jz.2018 ~ Jz.mat[, 6]), col ="chartreuse3")
points(Ar.Jz.2019 ~ Jz.mat[, 6], col = "red",pch=16)
abline(lm(Ar.Jz.2019 ~ Jz.mat[, 6]), col ="red")
points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
abline(lm(Ar.Jz.2016 ~ Jz.mat[-5, 6]), col ="black")
points(Ar.Jz.2020 ~ Jz.mat[,6], col="pink", pch = 16)
abline(lm(Ar.Jz.2020 ~ Jz.mat[, 6]), col ="pink")
legend("topleft", legend = c("2016","2017","2018","2019","2020"),
       text.col = c("black","blue", "chartreuse3","red","pink"))

#Log-linear
plot(Ar.Jz.2017 ~ Jz.mat[, 6], las =1,
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     ylim = c(0.04, 0.30),
     main = "Log-linear", col ="blue",pch=16)
curve(coef(Ar.Living.2017.log)[1]*log10(x*coef(Ar.Living.2017.log)[2]),
      add=T, col ="blue")
points(Ar.Jz.2018 ~ Jz.mat[, 6], col ="chartreuse3",pch=16)
curve(coef(Ar.Living.2018.log)[1]*log10(x*coef(Ar.Living.2018.log)[2]),
      add=T, col ="chartreuse3")
points(Ar.Jz.2019 ~ Jz.mat[, 6], col = "red",pch=16)
curve(coef(Ar.Living.2019.log)[1]*log10(x*coef(Ar.Living.2019.log)[2]),
      add=T, col ="red")
points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
curve(coef(Ar.Living.2016.log)[1]*log10(x*coef(Ar.Living.2016.log)[2]),
      add=T, col ="black")
points(Ar.Jz.2020 ~ Jz.mat[,6], col="pink", pch = 16)
curve(coef(Ar.Living.2020.log)[1]*log10(x*coef(Ar.Living.2020.log)[2]),
      add=T, col ="pink")

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
      add=T, col ="chartreuse3")
points(Ar.Jz.2019 ~ Jz.mat[, 6], col = "red",pch=16)
curve(coef(Ar.Living.2019.exp)[1] - (coef(Ar.Living.2019.exp)[1] - coef(Ar.Living.2019.exp)[2])*
        exp(-coef(Ar.Living.2019.exp)[3]*x),
      add=T, col ="red")
points(Ar.Jz.2016 ~ Jz.mat[-5,6], col="black", pch = 16)
curve(coef(Ar.Living.2016.exp)[1] - (coef(Ar.Living.2016.exp)[1] - coef(Ar.Living.2016.exp)[2])*
        exp(-coef(Ar.Living.2016.exp)[3]*x),
      add=T, col ="black")
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
Jz.mat[,6] = filter(Bats, lake =="ar") %>% slice(c(30,35,40,45,47)+1) %>% select(alpha)

#With the linear O2 consumption rates
Ar.Jz.day.2016.lin = Jz.mat.day[,1] #From the linear O2 consumption, not SegLm
Ar.Jz.day.2017.lin = Jz.mat.day[,2]
Ar.Jz.day.2018.lin = Jz.mat.day[,3]
Ar.Jz.day.2019.lin = Jz.mat.day[,4]
Ar.Jz.day.2020.lin = Jz.mat.day[,5]

#Jz ~ Alpha, linear
Ar.Living.2016.day = summary(lm(Ar.Jz.day.2016.lin ~ Jz.mat[,6]))
Ar.Living.2017.day = summary(lm(Ar.Jz.day.2017.lin ~ Jz.mat[,6]))
Ar.Living.2018.day = summary(lm(Ar.Jz.day.2018.lin ~ Jz.mat[,6]))
Ar.Living.2019.day = summary(lm(Ar.Jz.day.2019.lin ~ Jz.mat[,6]))
Ar.Living.2020.day = summary(lm(Ar.Jz.day.2020.lin ~ Jz.mat[,6]))

#Jz ~ Alpha, log linear, with linear O2 consumption
Ar.Living.2016.day.lin.log = nlsLM(Ar.Jz.day.2016.lin ~ b*log10(k*Jz.mat[-5,6]),
                                   start = list(b = 0.08, k = 200))
Ar.Living.2017.day.lin.log = nlsLM(Ar.Jz.day.2017.lin ~ b*log10(k*Jz.mat[,6]),
                                   start = list(b = 0.08, k = 200))
Ar.Living.2018.day.lin.log = nlsLM(Ar.Jz.day.2018.lin ~ b*log10(k*Jz.mat[,6]),
                                   start = list(b = 0.08, k = 200))
Ar.Living.2019.day.lin.log = nlsLM(Ar.Jz.day.2019.lin ~ b*log10(k*Jz.mat[,6]),
                                   start = list(b = 0.08, k = 200))
Ar.Living.2020.day.lin.log = nlsLM(Ar.Jz.day.2020.lin ~ b*log10(k*Jz.mat[,6]),
                                   start = list(b = 0.08, k = 200))

#Jz ~ Alpha, exponential, with linear O2 consumption
Ar.Living.2017.day.lin.exp = nlsLM(Ar.Jz.day.2017.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                   start = list(j.m = max(Ar.Jz.day.2017.lin), b = 0.2, k = 1))
Ar.Living.2018.day.lin.exp = nlsLM(Ar.Jz.day.2018.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                   start = list(j.m = max(Ar.Jz.day.2018.lin), b = 0.2, k = 1))
Ar.Living.2019.day.lin.exp = nlsLM(Ar.Jz.day.2019.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                   start = list(j.m = max(Ar.Jz.day.2019.lin), b = 0.2, k = 1))
Ar.Living.2020.day.lin.exp = nlsLM(Ar.Jz.day.2020.lin ~ j.m - (j.m-b)*exp(-k*Jz.mat[,6]),
                                   start = list(j.m = max(Ar.Jz.day.2020.lin), b = 0.2, k = 1))

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
                                     start = list(j.m = max(Ar.Jz.day.2018), b = 0.2, k = 10))
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

{
  png("./Output/Fig. Sx SegLm vs Linear Jz.png", width = 3.8, height = 4, units = "in", res=300)
plot(JzLogger.vec.lin[1:24] ~ Jz.Logger.SegLm[1:24] ,
     ylab = "Jz (linear fit)", 
     xlab = "Jz (segmented fit)",
     las = 1)
abline(lm(JzLogger.vec.lin[1:24] ~Jz.Logger.SegLm[1:24]))
abline(0,1, lty=2)
dev.off()}

summary(lm(JzLogger.vec.lin[1:24]~Jz.Logger.SegLm[1:24])) #R2 = 0.9582

##########################################################################################
###################Calculate Jz Arendsee using high temporal resolution###################
##########################################################################################

#################################THIS SECTION WORKS BY ITSELF############################
##################################CAN BE ITS OWN SCRIPT WHEN#############################
#################################OTHER SECTIONS ARE VERIFIED#############################

#Load all file names
YSI.list = list.files("./Data/Raw/Arendsee-cleaned")
Bats <- read.csv("./Data/Raw/bats.csv")
#Keep only csv files
YSI.list = YSI.list[grep(pattern = ".csv",list.files("./Data/Raw/Arendsee-cleaned"))]
YSI.yrs = c(2017,2018,2019,2020,2021)
Jz.YSI.list = list()


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
#Note. After looking at the pdf once, the second slope in the piecewise regression is always right
pdf(paste0("./Exploration output/Ar.YSI.",YSI.yrs[i],".pdf"))
for(k in 1:length(Jz.YSI)){
  Ar.temp <- Ar.long.deep[Ar.long.deep$Depth_m == unique(Ar.long.deep$Depth_m)[k],]
  if(YSI.yrs[i] == 2017) {
    Ar.temp = Ar.temp[Ar.temp$DOY>60 & Ar.temp$DOY < 325,]
    psi = c(100,200)}
  if(YSI.yrs[i] == 2018) {
    Ar.temp = Ar.temp[Ar.temp$DOY>100& Ar.temp$DOY < 330,] 
    psi = c(110,200)}
  if(YSI.yrs[i] == 2019) {
    Ar.temp = Ar.temp[Ar.temp$DOY>75 & Ar.temp$DOY < 337,]
    psi = c(100,200)}
  if(YSI.yrs[i] == 2020) {
    Ar.temp = Ar.temp[Ar.temp$DOY>98,]
    psi = c(110,200)}
  if(YSI.yrs[i] == 2021) {
    Ar.temp = Ar.temp[Ar.temp$DOY>79 & Ar.temp$DOY < 337,]
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
  
  lin.mod = lm(value ~ Dec.Day)
  
  Dec.Day.segLm = c(seq(Dec.Day[1]-200,Dec.Day[1]-1,1),Dec.Day, seq(Dec.Day[length(Dec.Day)]+1,Dec.Day[length(Dec.Day)]+300,1))
  value.dummy = mean(c(value[1], max(value)))
  value.segLm = c(rep(value.dummy,200),value, rep(min(value),300))
  lin.mod.segLm = lm(value.segLm ~ Dec.Day.segLm)
  
  plot(value.segLm ~ Dec.Day.segLm, las = 1,
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
  
}
dev.off()
Jz.YSI.list[[i]] = Jz.YSI

#Select the right alphas
#2017: 33:48
#2018-2020: 30:47
if(i == 1){
  alpha.YSI.list=list()
  alpha.YSI.list[[1]] = filter(Bats, lake =="ar") %>% arrange(depth_1m) %>% slice(c(33:48)+1) %>% select(alpha)
}
if(i == 2) alpha.YSI.list[[2]] = filter(Bats, lake =="ar") %>% arrange(depth_1m) %>% slice(c(30:47)+1) %>% select(alpha)
}

#Linear regressions
Ar.Jz.alpha.YSI.2017 = lm(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1])
Ar.Jz.alpha.YSI.2018 = lm(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1])
Ar.Jz.alpha.YSI.2019 = lm(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1])
Ar.Jz.alpha.YSI.2020 = lm(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1])
Ar.Jz.alpha.YSI.2021 = lm(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1])

#Log-linear and exponential plateau regressions
#2017
temp = Jz.YSI.list[[1]][-c(1:3)]
temp.max = max(temp)
alpha = alpha.YSI.list[[1]][,1]
Ar.Jz.alpha.YSI.2017.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200))
Ar.Jz.alpha.YSI.2017.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
#2018
temp = Jz.YSI.list[[2]]
temp.max = max(temp)
alpha = alpha.YSI.list[[2]][,1]
Ar.Jz.alpha.YSI.2018.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200))
Ar.Jz.alpha.YSI.2018.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
#2019
temp = Jz.YSI.list[[3]]
temp.max = max(temp)
Ar.Jz.alpha.YSI.2019.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200))
Ar.Jz.alpha.YSI.2019.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
#2020
temp = Jz.YSI.list[[4]]
temp.max = max(temp)
Ar.Jz.alpha.YSI.2020.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200))
Ar.Jz.alpha.YSI.2020.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))

#2021
temp = Jz.YSI.list[[5]]
temp.max = max(temp)
Ar.Jz.alpha.YSI.2021.log = nlsLM(temp ~ b*log10(k*alpha),
                                 start = list(b=0.08, k=200))
Ar.Jz.alpha.YSI.2021.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*alpha),
                                 start = list(t.m = temp.max, b=0.05, k=5))
# pred <- predict(Ar.Jz.alpha.YSI.2021.exp, alpha)
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
     xlim = c(0,0.2),
     ylim = c(0.05, 0.40),
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     main = "Linear",
     pch = 16, col = "chartreuse3")
# abline(Ar.Jz.alpha.YSI.2018, col = "chartreuse3")
points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
       pch = 16, col = "blue")
# abline(Ar.Jz.alpha.YSI.2017, col = "blue")
points(Jz.YSI.list[[3]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "red")
# abline(Ar.Jz.alpha.YSI.2019, col = "red")
points(Jz.YSI.list[[4]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "pink")
# abline(Ar.Jz.alpha.YSI.2020, col = "pink")
points(Jz.YSI.list[[5]] ~ alpha.YSI.list[[2]][,1],
       pch = 16, col = "#73BAE6")
# abline(Ar.Jz.alpha.YSI.2021, col = "#73BAE6")
legend("topleft", legend = c("2018","2019","2020", "2021"),
       text.col = c("chartreuse3","red","pink", "#73BAE6"))


plot(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1],
     las = 1,
     xlim = c(0,0.2),
     ylim = c(0.05, 0.40),
     xlab = expression(alpha(z)),
     ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
     main = "Log-linear",
     pch = 16, col = "chartreuse3")
curve(coef(Ar.Jz.alpha.YSI.2018.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2018.log)[2]),
      add=T, col ="chartreuse3")
points(Jz.YSI.list[[1]][-c(1:3)] ~ alpha.YSI.list[[1]][,1],
       pch = 16, col = "blue")
curve(coef(Ar.Jz.alpha.YSI.2017.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2017.log)[2]),
      add=T, col ="blue")
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
curve(coef(Ar.Jz.alpha.YSI.2021.log)[1]*log10(x*coef(Ar.Jz.alpha.YSI.2021.log)[2]),
      add=T, col ="#73BAE6")

plot(Jz.YSI.list[[2]] ~ alpha.YSI.list[[2]][,1],
     las = 1,
     xlim = c(0,0.2),
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


ar.alpha <- filter(Bats, lake =="ar") %>% arrange(depth_1m) %>% slice(c(30:38)+1) %>% select(alpha) #30 to 38 meters because the rate goes down after
ar.alpha.rinko <- ar.alpha
#With Rinko profiles calculated at the begining of this script
Ar.Jz.rinko =filter(Output.Jz, lake == "ar") %>%
  filter(depth_1m>=30) %>%
  filter(depth_1m<=38) %>%
  select(Jz)
Ar.Jz.rinko = as.vector(Ar.Jz.rinko[,1])

#Linear relationship
Ar.Jz.alpha.rinko = lm(Ar.Jz.rinko ~ ar.alpha.rinko[,1])
summary(Ar.Jz.alpha.rinko)
#Log-linear
Ar.log.lm <- nlsLM(Ar.Jz.rinko ~ a*log10(k*ar.alpha[,1]),
                   start = list(a=0.08, k=200))
pred <- predict(Ar.log.lm, ar.alpha[,1])
rss <- sum((pred - Ar.Jz.rinko) ^ 2)
tss <- sum((Ar.Jz.rinko - mean(Ar.Jz.rinko)) ^ 2)
rsq <- 1 - rss/tss

#Exponential
Ar.exp.lm = nlsLM(Ar.Jz.rinko ~ j.m - (j.m - b) * exp(-k*ar.alpha[,1]),
                                 start = list(j.m = 0.15, b=0.05, k=5))

{
# pdf("./Output/Ar.Jz-Alpha.Rinko.pdf", width = 5.3, height = 5.3)
png("./Output/Ar.Jz-Alpha.Rinko.YSIcomparison.png", width = 5.3, height = 5.3, units = "in", res=300)
# png("./Output/Ar.Jz-Alpha.Rinko.png", width = 5.3, height = 5.3, units = "in", res=300) #Use this line when removing YSI points
par(mfrow=c(1,1))
par(mar=c(4,5,1,1)+.1)
plot(Ar.Jz.rinko ~ ar.alpha.rinko[,1],
     xlim = c(0.02, 2),
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

legend("topright", legend = c("Linear", "Log-linear", "Exponential plateau", "YSI"),
       text.col = c("black","blue", "chartreuse3", "red"))

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

ar.alpha <- filter(Bats, lake =="ar") %>% arrange(depth_1m) %>% slice(alpha.select+1) %>% select(alpha)
Jz.lowres.slice = Jz.lowres[1:length(alpha.select)]
Ar.Jz.lowres = lm(Jz.lowres.slice ~ ar.alpha[,1])
Ar.Jz.lowres.log = lm(Jz.lowres.slice ~ log10(ar.alpha[,1]))
Ar.Jz.lm.list[[i]] = list(summary(Ar.Jz.lowres), summary(Ar.Jz.lowres.log))

Jz.lowYSI.list[[i]] <- Jz.lowres.slice
alpha.lowYSI.list[[i]] <- ar.alpha

}

#Compare O2 profiles in 2020 between YSI and Rinko
compare.depth.1 = as.data.frame(Ar.long.deep.lowres.a[which(Ar.long.deep.lowres.a$DOY==147),"Depth_m"])
compare.O2.1 = as.data.frame(Ar.long.deep.lowres.a[which(Ar.long.deep.lowres.a$DOY==147),"DO_mgL"])

compare.depth.2 = as.data.frame(Ar.long.deep.lowres.a[which(Ar.long.deep.lowres.a$DOY==238),"Depth_m"])
compare.O2.2 = as.data.frame(Ar.long.deep.lowres.a[which(Ar.long.deep.lowres.a$DOY==238),"DO_mgL"])


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

###########################################################################################
##############Calculate Jz Arendsee using low temporal resolution - Best case##############
###########################################################################################

#2017: 119-187
#2018: 133-199
#2019: 117-200
#2020: 124-179
#2020: 130 -169

DOY.ini = c(119,133,117,124, 130)
DOY.end = c(187,199,200,179, 169)

#With matching YSI profiles
YSI.list = list.files("./Data/Raw/Arendsee-cleaned")
#Keep only csv files
YSI.list = YSI.list[grep(pattern = ".csv",list.files("./Data/Raw/Arendsee-cleaned"))]

Bats <- read.csv("./Data/Raw/bats.csv")
Jz.lowres.slice.list = list()
ar.alpha.list = list()

Ar.Jz.lm.best.list = list()
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
  
  #Keep only matching date to Rinko profiles
  Ar.long.deep.lowres <- Ar.long.deep[which(Ar.long.deep$DOY==DOY.ini[i] | Ar.long.deep$DOY==DOY.end[i]),]
  Ar.long.deep.lowres.a <- Ar.long.deep.lowres %>% 
    group_by(Depth_m, DOY) %>% summarize(DO_mgL = mean(DO_mgL)) 
  Jz.lowres = vector(length = nrow(Ar.long.deep.lowres.a)/2)
  Depth.lowres = unique(Ar.long.deep.lowres.a$Depth_m)
  DOY.lowres = max(Ar.long.deep.lowres.a$DOY) - min(Ar.long.deep.lowres.a$DOY)
  
  
  for(a in 1:(nrow(Ar.long.deep.lowres.a)/2)){
    Jz.lowres[a] = (max(Ar.long.deep.lowres.a[Ar.long.deep.lowres.a$Depth_m==Depth.lowres[a],"DO_mgL"]) - 
                      min(Ar.long.deep.lowres.a[Ar.long.deep.lowres.a$Depth_m==Depth.lowres[a],"DO_mgL"]))/DOY.lowres
  }


slice.beg = c(38,33,33,33,34)
slice.end = c(48,47,47,47,47)

Jz.lowres.end = c(8,3,3,3,4)


ar.alpha <- filter(Bats, lake =="ar") %>% arrange(depth_1m) %>% slice(slice.beg[i]:slice.end[i]+1) %>% select(alpha)
ar.alpha.list[[i]] = ar.alpha
Jz.lowres.slice = Jz.lowres[-c(1:Jz.lowres.end[i])]# Remove the first few  values because they are identical (likely because sediments to volume is too low)
Jz.lowres.slice.list[[i]] = Jz.lowres.slice

plot(Jz.lowres.slice ~ ar.alpha[,1])
Ar.Jz.lowres = lm(Jz.lowres.slice ~ ar.alpha[,1])
# Ar.Jz.lowres.log = lm(Jz.lowres.slice ~ log10(ar.alpha[,1]))

temp = Jz.lowres.slice
temp.max = max(Jz.lowres.slice)
Ar.Jz.lowres.log = nlsLM(temp ~ b*log10(k*ar.alpha[,1]),
                                 start = list(b=0.08, k=200))
Ar.Jz.lowres.exp = nlsLM(temp ~ t.m - (t.m - b) * exp(-k*ar.alpha[,1]),
                                 start = list(t.m = temp.max, b=0.05, k=5))



Ar.Jz.lm.best.list[[i]] = list(Ar.Jz.lowres, Ar.Jz.lowres.log, Ar.Jz.lowres.exp)
}


{
  pdf("./Output/Ar.Jz-Alpha.BestLowRes.TEST.pdf", width = 8, height = 3.3)
  par(mfrow=c(1,3))
  par(mar = c(5,5,4,2)+0.2)
  
  plot(Jz.lowres.slice.list[[1]] ~ ar.alpha.list[[1]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.3),
       xlab = expression(alpha(z)),
       ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "Linear",
       pch = 16, col = "blue")
  # abline(Ar.Jz.lm.best.list[[1]][[1]], col = "blue")
  points(Jz.lowres.slice.list[[2]] ~ ar.alpha.list[[2]][,1],
         pch = 16, col = "chartreuse3")
  # abline(Ar.Jz.lm.best.list[[2]][[1]], col = "chartreuse3")
  points(Jz.lowres.slice.list[[3]] ~ ar.alpha.list[[3]][,1],
         pch = 16, col = "red")
  # abline(Ar.Jz.lm.best.list[[3]][[1]], col = "red")
  points(Jz.lowres.slice.list[[4]] ~ ar.alpha.list[[4]][,1],
         pch = 16, col = "pink")
  # abline(Ar.Jz.lm.best.list[[4]][[1]], col = "pink")
  points(Jz.lowres.slice.list[[5]] ~ ar.alpha.list[[5]][,1],
         pch = 16, col = "#00B0F0")
  # abline(Ar.Jz.lm.best.list[[5]][[1]], col = "#00B0F0")
  
  legend("topleft", legend = c("2017","2018","2019","2020", "2021"),
         text.col = c("blue","chartreuse3","red","pink", "#00B0F0"))
  
  plot(Jz.lowres.slice.list[[1]] ~ ar.alpha.list[[1]][,1],
       las = 1,
       xlim = c(0,0.25),
       ylim = c(0.05, 0.3),
       xlab = expression(alpha(z)),
       ylab = expression(J[z]~(mg~O[2]~L^-1~d^-1)),
       main = "Log-linear",
       pch = 16, col = "blue")
  curve(coef(Ar.Jz.lm.best.list[[1]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[1]][[2]])[2]),
        add=T, col ="blue")
  points(Jz.lowres.slice.list[[2]] ~ ar.alpha.list[[2]][,1],
         pch = 16, col = "chartreuse3")
  curve(coef(Ar.Jz.lm.best.list[[2]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[2]][[2]])[2]),
        add=T, col ="chartreuse3")
  points(Jz.lowres.slice.list[[3]] ~ ar.alpha.list[[2]][,1],
         pch = 16, col = "red")
  curve(coef(Ar.Jz.lm.best.list[[3]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[3]][[2]])[2]),
        add=T, col ="red")
  points(Jz.lowres.slice.list[[4]] ~ ar.alpha.list[[2]][,1],
         pch = 16, col = "pink")
  curve(coef(Ar.Jz.lm.best.list[[4]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[4]][[2]])[2]),
        add=T, col ="pink")
  points(Jz.lowres.slice.list[[5]] ~ ar.alpha.list[[5]][,1],
         pch = 16, col = "#00B0F0")
  curve(coef(Ar.Jz.lm.best.list[[5]][[2]])[1]*log10(x*coef(Ar.Jz.lm.best.list[[5]][[2]])[2]),
        add=T, col ="#00B0F0")

  plot(Jz.lowres.slice.list[[1]] ~ ar.alpha.list[[1]][,1],
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
  
  points(Jz.lowres.slice.list[[2]] ~ ar.alpha.list[[2]][,1], pch = 16, col = "chartreuse3")
  curve(coef(Ar.Jz.lm.best.list[[2]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[2]][[3]])[1] - 
          coef(Ar.Jz.lm.best.list[[2]][[3]])[2])*
                exp(-1*coef(Ar.Jz.lm.best.list[[2]][[3]])[3]*x), 
        add=T, col = "chartreuse3")
  
  points(Jz.lowres.slice.list[[3]] ~ ar.alpha.list[[3]][,1], pch = 16, col = "red")
  curve(coef(Ar.Jz.lm.best.list[[3]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[3]][[3]])[1] - 
                                             coef(Ar.Jz.lm.best.list[[3]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[3]][[3]])[3]*x), 
        add=T, col = "red")
  
  points(Jz.lowres.slice.list[[4]] ~ ar.alpha.list[[4]][,1], pch = 16, col = "pink")
  curve(coef(Ar.Jz.lm.best.list[[4]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[4]][[3]])[1] - 
                                                   coef(Ar.Jz.lm.best.list[[4]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[4]][[3]])[3]*x), 
        add=T, col = "pink")
  
  points(Jz.lowres.slice.list[[5]] ~ ar.alpha.list[[5]][,1], pch = 16, col = "#00B0F0")
  curve(coef(Ar.Jz.lm.best.list[[5]][[3]])[1] - (coef(Ar.Jz.lm.best.list[[5]][[3]])[1] - 
                                                   coef(Ar.Jz.lm.best.list[[5]][[3]])[2])*
          exp(-1*coef(Ar.Jz.lm.best.list[[5]][[3]])[3]*x), 
        add=T, col = "#00B0F0")
  
  dev.off()
}




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


source("./Scripts/O2_DecayRate.R")

#Useful parameters to model O2 profiles and anoxic age
#Select alpha from 30m to 47m deep
ar.alpha <- filter(Bats, lake =="ar") %>% arrange(depth_1m) %>% slice(31:48) %>% select(alpha)
ar.depth = c(30:47)

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
# aa.depth = c(30,40,45,46,47)

##################################High temporal resolution, log model####################

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
Loggers.linear.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Living.2017.day)[1,1],
                                         k = coef(Ar.Living.2017.day)[2,1],
                                         Model = "Livingstone",
                                         alpha = ar.alpha,
                                         data.source = "YSI",
                                         year = 2017)

Loggers.log.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Living.2017.day.SegLm.log)[1],
                                      k = coef(Ar.Living.2017.day.SegLm.log)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2017)

Loggers.exp.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Living.2017.day.SegLm.exp)[2],
                                      k = coef(Ar.Living.2017.day.SegLm.exp)[3],
                                      JzMax = coef(Ar.Living.2017.day.SegLm.exp)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2017)

#Anoxic age for Loggers - 2018
Loggers.linear.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Living.2018.day)[1,1],
                                         k = coef(Ar.Living.2018.day)[2,1],
                                         Model = "Livingstone",
                                         alpha = ar.alpha,
                                         data.source = "YSI",
                                         year = 2018)

Loggers.log.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Living.2018.day.SegLm.log)[1],
                                      k = coef(Ar.Living.2018.day.SegLm.log)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2018)

Loggers.exp.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Living.2018.day.SegLm.exp)[2],
                                      k = coef(Ar.Living.2018.day.SegLm.exp)[3],
                                      JzMax = coef(Ar.Living.2018.day.SegLm.exp)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2018)

#Anoxic age for Loggers - 2019
Loggers.linear.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Living.2019.day)[1,1],
                                         k = coef(Ar.Living.2019.day)[2,1],
                                         Model = "Livingstone",
                                         alpha = ar.alpha,
                                         data.source = "YSI",
                                         year = 2019)

Loggers.log.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Living.2019.day.SegLm.log)[1],
                                      k = coef(Ar.Living.2019.day.SegLm.log)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2019)

Loggers.exp.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Living.2019.day.SegLm.exp)[2],
                                      k = coef(Ar.Living.2019.day.SegLm.exp)[3],
                                      JzMax = coef(Ar.Living.2019.day.SegLm.exp)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2019)

#Anoxic age for Loggers - 2020
Loggers.linear.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Living.2020.day)[1,1],
                                         k = coef(Ar.Living.2020.day)[2,1],
                                         Model = "Livingstone",
                                         alpha = ar.alpha,
                                         data.source = "YSI",
                                         year = 2020)

Loggers.log.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Living.2020.day.SegLm.log)[1],
                                      k = coef(Ar.Living.2020.day.SegLm.log)[2],
                                      Model = "Log",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2020)

Loggers.exp.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Living.2020.day.SegLm.exp)[2],
                                      k = coef(Ar.Living.2020.day.SegLm.exp)[3],
                                      JzMax = coef(Ar.Living.2020.day.SegLm.exp)[1],
                                      Model = "Exp",
                                      alpha = ar.alpha,
                                      data.source = "YSI",
                                      year = 2020)



#Anoxic age for High temporal Res (YSI) - 2017
YSI.linear.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2017)[1],
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
                                  year = 2017)

#Anoxic age for High temporal Res (YSI) - 2018
YSI.linear.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2018)[1],
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
                                  year = 2018)

#Anoxic age for High temporal Res (YSI) - 2019
YSI.linear.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2019)[1],
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
                                  year = 2019)

#Anoxic age for High temporal Res (YSI) - 2020
YSI.linear.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2020)[1],
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

#Anoxic age for High temporal Res (YSI) - 2021
YSI.linear.AA.2021 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.YSI.2021)[1],
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
                                  year = 2021)

#Anoxic age for low temporal Res (Rinko) - 2020
#Really off, but 2020-Rinko has a very poor depth resolution of Jz. It makes sense
Rinko.linear.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.alpha.rinko)[1],
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
                                  year = 2020)

#Anoxic age for Best case (YSI) - 2017
Best.linear.AA.2017 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[1]][[1]])[1],
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
                                   year = 2017)

#Anoxic age for Best case (YSI) - 2018
Best.linear.AA.2018 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[2]][[1]])[1],
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
                                   year = 2018)

#Anoxic age for Best case (YSI) - 2019
Best.linear.AA.2019 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[3]][[1]])[1],
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
                                   year = 2019)

#Anoxic age for Best case (YSI) - 2020
Best.linear.AA.2020 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[4]][[1]])[1],
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
                                   year = 2020)

#Anoxic age for Best case (YSI) - 2021
Best.linear.AA.2021 = INSERT_FCT_NAME(b = coef(Ar.Jz.lm.best.list[[5]][[1]])[1],
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
                                   year = 2021)


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
Loggers.linear.AA.2017.bin = t(apply(Loggers.linear.AA.2017[[1]][,-1], 1, function(X) X==1))
Loggers.log.AA.2017.bin = t(apply(Loggers.log.AA.2017[[1]][,-1], 1, function(X) X==1))
Loggers.exp.AA.2017.bin = t(apply(Loggers.exp.AA.2017[[1]][,-1], 1, function(X) X==1))

Loggers.linear.AA.2017.FirstaaDOY = First.aa.doy.fct(Loggers.linear.AA.2017.bin)
Loggers.log.AA.2017.FirstaaDOY = First.aa.doy.fct(Loggers.log.AA.2017.bin)
Loggers.exp.AA.2017.FirstaaDOY = First.aa.doy.fct(Loggers.exp.AA.2017.bin)

#Loggers-2018
Loggers.linear.AA.2018.bin = t(apply(Loggers.linear.AA.2018[[1]][,-1], 1, function(X) X==1))
Loggers.log.AA.2018.bin = t(apply(Loggers.log.AA.2018[[1]][,-1], 1, function(X) X==1))
Loggers.exp.AA.2018.bin = t(apply(Loggers.exp.AA.2018[[1]][,-1], 1, function(X) X==1))

Loggers.linear.AA.2018.FirstaaDOY = First.aa.doy.fct(Loggers.linear.AA.2018.bin)
Loggers.log.AA.2018.FirstaaDOY = First.aa.doy.fct(Loggers.log.AA.2018.bin)
Loggers.exp.AA.2018.FirstaaDOY = First.aa.doy.fct(Loggers.exp.AA.2018.bin)

#Loggers-2019
Loggers.linear.AA.2019.bin = t(apply(Loggers.linear.AA.2019[[1]][,-1], 1, function(X) X==1))
Loggers.log.AA.2019.bin = t(apply(Loggers.log.AA.2019[[1]][,-1], 1, function(X) X==1))
Loggers.exp.AA.2019.bin = t(apply(Loggers.exp.AA.2019[[1]][,-1], 1, function(X) X==1))

Loggers.linear.AA.2019.FirstaaDOY = First.aa.doy.fct(Loggers.linear.AA.2019.bin)
Loggers.log.AA.2019.FirstaaDOY = First.aa.doy.fct(Loggers.log.AA.2019.bin)
Loggers.exp.AA.2019.FirstaaDOY = First.aa.doy.fct(Loggers.exp.AA.2019.bin)

#Loggers-2020
Loggers.linear.AA.2020.bin = t(apply(Loggers.linear.AA.2020[[1]][,-1], 1, function(X) X==1))
Loggers.log.AA.2020.bin = t(apply(Loggers.log.AA.2020[[1]][,-1], 1, function(X) X==1))
Loggers.exp.AA.2020.bin = t(apply(Loggers.exp.AA.2020[[1]][,-1], 1, function(X) X==1))

Loggers.linear.AA.2020.FirstaaDOY = First.aa.doy.fct(Loggers.linear.AA.2020.bin)
Loggers.log.AA.2020.FirstaaDOY = First.aa.doy.fct(Loggers.log.AA.2020.bin)
Loggers.exp.AA.2020.FirstaaDOY = First.aa.doy.fct(Loggers.exp.AA.2020.bin)

#YSI - 2017
YSI.linear.AA.2017.bin = t(apply(YSI.linear.AA.2017[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2017.bin = t(apply(YSI.log.AA.2017[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2017.bin = t(apply(YSI.exp.AA.2017[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2017.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2017.bin)
YSI.log.AA.2017.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2017.bin)
YSI.exp.AA.2017.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2017.bin)

#YSI - 2018
YSI.linear.AA.2018.bin = t(apply(YSI.linear.AA.2018[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2018.bin = t(apply(YSI.log.AA.2018[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2018.bin = t(apply(YSI.exp.AA.2018[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2018.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2018.bin)
YSI.log.AA.2018.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2018.bin)
YSI.exp.AA.2018.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2018.bin)

#YSI - 2019
YSI.linear.AA.2019.bin = t(apply(YSI.linear.AA.2019[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2019.bin = t(apply(YSI.log.AA.2019[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2019.bin = t(apply(YSI.exp.AA.2019[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2019.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2019.bin)
YSI.log.AA.2019.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2019.bin)
YSI.exp.AA.2019.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2019.bin)

#YSI - 2020
YSI.linear.AA.2020.bin = t(apply(YSI.linear.AA.2020[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2020.bin = t(apply(YSI.log.AA.2020[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2020.bin = t(apply(YSI.exp.AA.2020[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2020.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2020.bin)
YSI.log.AA.2020.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2020.bin)
YSI.exp.AA.2020.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2020.bin)

#YSI - 2021
YSI.linear.AA.2021.bin = t(apply(YSI.linear.AA.2021[[1]][,-1], 1, function(X) X==1))
YSI.log.AA.2021.bin = t(apply(YSI.log.AA.2021[[1]][,-1], 1, function(X) X==1))
YSI.exp.AA.2021.bin = t(apply(YSI.exp.AA.2021[[1]][,-1], 1, function(X) X==1))

YSI.linear.AA.2021.FirstaaDOY = First.aa.doy.fct(YSI.linear.AA.2021.bin)
YSI.log.AA.2021.FirstaaDOY = First.aa.doy.fct(YSI.log.AA.2021.bin)
YSI.exp.AA.2021.FirstaaDOY = First.aa.doy.fct(YSI.exp.AA.2021.bin)

#Rinko-2020
Rinko.linear.AA.2020.bin = t(apply(Rinko.linear.AA.2020[[1]][,-1], 1, function(X) X==1))
Rinko.log.AA.2020.bin = t(apply(Rinko.log.AA.2020[[1]][,-1], 1, function(X) X==1))
Rinko.exp.AA.2020.bin = t(apply(Rinko.exp.AA.2020[[1]][,-1], 1, function(X) X==1))

Rinko.linear.AA.2020.FirstaaDOY = First.aa.doy.fct(Rinko.linear.AA.2020.bin)
Rinko.log.AA.2020.FirstaaDOY = First.aa.doy.fct(Rinko.log.AA.2020.bin)
Rinko.exp.AA.2020.FirstaaDOY = First.aa.doy.fct(Rinko.exp.AA.2020.bin)

#Best-2017
Best.linear.AA.2017.bin = t(apply(Best.linear.AA.2017[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2017.bin = t(apply(Best.log.AA.2017[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2017.bin = t(apply(Best.exp.AA.2017[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2017.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2017.bin)
Best.log.AA.2017.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2017.bin)
Best.exp.AA.2017.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2017.bin)

#Best-2018
Best.linear.AA.2018.bin = t(apply(Best.linear.AA.2018[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2018.bin = t(apply(Best.log.AA.2018[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2018.bin = t(apply(Best.exp.AA.2018[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2018.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2018.bin)
Best.log.AA.2018.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2018.bin)
Best.exp.AA.2018.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2018.bin)

#Best-2019
Best.linear.AA.2019.bin = t(apply(Best.linear.AA.2019[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2019.bin = t(apply(Best.log.AA.2019[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2019.bin = t(apply(Best.exp.AA.2019[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2019.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2019.bin)
Best.log.AA.2019.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2019.bin)
Best.exp.AA.2019.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2019.bin)

#Best-2020
Best.linear.AA.2020.bin = t(apply(Best.linear.AA.2020[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2020.bin = t(apply(Best.log.AA.2020[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2020.bin = t(apply(Best.exp.AA.2020[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2020.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2020.bin)
Best.log.AA.2020.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2020.bin)
Best.exp.AA.2020.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2020.bin)

#Best-2021
Best.linear.AA.2021.bin = t(apply(Best.linear.AA.2021[[1]][,-1], 1, function(X) X==1))
Best.log.AA.2021.bin = t(apply(Best.log.AA.2021[[1]][,-1], 1, function(X) X==1))
Best.exp.AA.2021.bin = t(apply(Best.exp.AA.2021[[1]][,-1], 1, function(X) X==1))

Best.linear.AA.2021.FirstaaDOY = First.aa.doy.fct(Best.linear.AA.2021.bin)
Best.log.AA.2021.FirstaaDOY = First.aa.doy.fct(Best.log.AA.2021.bin)
Best.exp.AA.2021.FirstaaDOY = First.aa.doy.fct(Best.exp.AA.2021.bin)



#Export all vectors to make the table in Excel
First.2017.all = rbind(Observed2017 = YSI.aa.2017.cut[-19],
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
                       Best.exp.AA.2020.FirstaaDOY,
                       Rinko.linear.AA.2020.FirstaaDOY,
                       Rinko.log.AA.2020.FirstaaDOY,
                       Rinko.exp.AA.2020.FirstaaDOY)
First.2021.all = rbind(Observed2021 = YSI.aa.2021.cut,
                       YSI.linear.AA.2021.FirstaaDOY,
                       YSI.log.AA.2021.FirstaaDOY,
                       YSI.exp.AA.2021.FirstaaDOY,
                       Best.linear.AA.2021.FirstaaDOY,
                       Best.log.AA.2021.FirstaaDOY,
                       Best.exp.AA.2021.FirstaaDOY)


First.DOY.all = rbind(First.2017.all, First.2018.all, First.2019.all, First.2020.all,First.2021.all)

write.csv(file = "./Output/First.DOY.all.csv", x = First.DOY.all, fileEncoding = "UTF-8")

#2017
jpeg("./Output/AA.DOY.Obs-Mod-2017.jpg", height=8, width=8, res = 300, units="in")
par(mfrow=c(3,3))
for(i in 2:nrow(First.2017.all)){
  plot(First.2017.all[1,] ~ First.2017.all[i,],
       xlab = "First day of hypoxia (modelled)",
       ylab = "First day of hypoxia (observed)",
       las = 1,
       main = c("Loggers linear", "Loggers log", "Loggers exponential",
                "YSI linear", "YSI log", "YSI exponential",
                "Best linear", "Best log", "Best exponential")[i-1])
  abline(0,1)
}
dev.off()
#2018
jpeg("./Output/AA.DOY.Obs-Mod-2018.jpg", height=8, width=8, res = 300, units="in")
par(mfrow=c(3,3))
for(i in 2:nrow(First.2018.all)){
  plot(First.2018.all[1,] ~ First.2018.all[i,],
       xlab = "First day of hypoxia (modelled)",
       ylab = "First day of hypoxia (observed)",
       las = 1,
       main = c("Loggers linear", "Loggers log", "Loggers exponential",
                "YSI linear", "YSI log", "YSI exponential",
                "Best linear", "Best log", "Best exponential")[i-1])
  abline(0,1)
}
dev.off()

#2019
jpeg("./Output/AA.DOY.Obs-Mod-2019.jpg", height=8, width=8, res = 300, units="in")
par(mfrow=c(3,3))
for(i in 2:nrow(First.2019.all)){
  plot(First.2019.all[1,] ~ First.2019.all[i,],
       xlab = "First day of hypoxia (modelled)",
       ylab = "First day of hypoxia (observed)",
       las = 1,
       main = c("Loggers linear", "Loggers log", "Loggers exponential",
                "YSI linear", "YSI log", "YSI exponential",
                "Best linear", "Best log", "Best exponential")[i-1])
  abline(0,1)
}
dev.off()

#2020
jpeg("./Output/AA.DOY.Obs-Mod-2020_test.jpg", height=8, width=8, res = 300, units="in")
par(mfrow=c(4,3))
for(i in 2:nrow(First.2020.all)){
  plot(First.2020.all[1,] ~ First.2020.all[i,],
       xlab = "First day of hypoxia (modelled)",
       ylab = "First day of hypoxia (observed)",
       las = 1,
       main = c("Loggers linear", "Loggers log", "Loggers exponential",
                "YSI linear", "YSI log", "YSI exponential",
                "Best linear", "Best log", "Best exponential",
                "Rinko linear", "Rinko log", "Rinko exponential")[i-1])
  abline(0,1)
}
dev.off()


































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
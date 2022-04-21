##############################################################################
########################Anoxic Age lake Mendota###############################
##############################################################################
library(segmented)
library(tidyr)
library(caret)
setwd("D:/Postdoc/Allemagne/Github/AnoxicAge")
load("D:/Postdoc/Allemagne/Github/AnoxicAge/Data/Mendota/Mendota.RData")
source("./Scripts/O2_DecayRate.R")

data = read.csv("./Data/Mendota/Mendota.csv")

data = data[-which(data$flag_do_raw=="C"),]
data = data[,-grep("flag", colnames(data))]
plot(data$depth ~ data$fdom,
     ylim = rev(range(data$depth)),
     ylab = "Depth (m)",
     xlab = "FDOM")

# data.2017 = data[grep("2017", data$sampledate),] #Data starts at end of summer, no use for us
data.2018 = data[grep("2018", data$sampledate),]
data.2018$DOY = as.numeric(strftime(data.2018$sampledate, format = "%j"))
data.2018 = data.2018[data.2018$DOY>100,]
data.2020 = data[grep("2020", data$sampledate),]
data.2020$DOY = as.numeric(strftime(data.2020$sampledate, format = "%j"))
data.2020 = data.2020[data.2020$DOY>90,]

####################################Year 2018###########################################
depth.2018 = c(10:23)
AIC.table.2018 = matrix(nrow=length(depth.2018), ncol=3)
rownames(AIC.table.2018) = paste0(depth.2018,"m")
colnames(AIC.table.2018) = c("Linear", "1 break", "2 breaks")

Jz.list.2018=list()

pdf("./Data/Mendota/Output/Segmented.2018.O2.pdf")
for(i in 1:length(depth.2018)){
  data.2018.depth = data.2018[data.2018$depth==depth.2018[i],]
  
  Dec.Day = data.2018.depth$DOY
  value = data.2018.depth$do_raw
  lin.mod = lm(value ~ Dec.Day)
  
  plot(value ~ Dec.Day,
       xlab = "Day of year",
       ylab = "Oxygen (mg/L)",
       main = paste0(depth.2018[i], "m"))
  abline(lin.mod, lwd = 2, col = "cyan")
  
  AIC.table.2018[i,1] = round(AIC(lin.mod))
  #Piecewise model
  psi = c(125) #else psi = c(30)
  segmented.mod <- segmented(lin.mod, psi=psi)
  plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "red", lwd=2)
  AIC.table.2018[i,2] = round(AIC(segmented.mod))
  
  Jz.list.2018[[i]] =   segmented.mod #This will give a better estimate of reaching anoxia
  #than using a two breakpoint. The AIC is worse overall, but reaching 0 mg/L O2 looks
  #more accurate
  
  psi = c(125,200) #else psi = c(30)
  segmented.mod <- segmented(lin.mod, psi=psi)
  plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "blue", lwd=2)
  AIC.table.2018[i,3] = round(AIC(segmented.mod))
  
  
}
dev.off()

####################################Year 2020###########################################

depth.2020 = c(10:20) #Removed 23.5m deep because it was always anoxic
AIC.table.2020 = matrix(nrow=length(depth.2020), ncol=3)
rownames(AIC.table.2020) = paste0(depth.2020,"m")
colnames(AIC.table.2020) = c("Linear", "1 break", "2 breaks")

Jz.list.2020 = list()

pdf("./Data/Mendota/Output/Segmented.2020.O2.pdf")
for(i in 1:length(depth.2020)){
  data.2020.depth = data.2020[data.2020$depth==depth.2020[i],]
  
  Dec.Day = data.2020.depth$DOY
  value = data.2020.depth$do_raw
  lin.mod = lm(value ~ Dec.Day)
  
  plot(value ~ Dec.Day,
       xlab = "Day of year",
       ylab = "Oxygen (mg/L)",
       main = paste0(depth.2020[i], "m"))
  abline(lin.mod, lwd = 2, col = "cyan")
  
  AIC.table.2020[i,1] = round(AIC(lin.mod))
  #Piecewise model
  psi = c(125) #else psi = c(30)
  segmented.mod <- segmented(lin.mod, psi=psi)
  plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "red", lwd=2)
  AIC.table.2020[i,2] = round(AIC(segmented.mod))
  
  psi = c(125,200) #else psi = c(30)
  segmented.mod <- segmented(lin.mod, psi=psi)
  plot.segmented(segmented.mod, add=T, conf.level=.95, rug=F, col = "blue", lwd=2)
  AIC.table.2020[i,3] = round(AIC(segmented.mod))
  
  Jz.list.2020[[i]] =   segmented.mod
}
dev.off()




##########################Extract Jz for each depths and each year######################
getwd()
#read Mendota alpha values
alpha = read.csv("./Data/Mendota/Mendota.alpha.csv")

#Always take the first slope for 2018 and 2020
Jz.2018 = vector(length=length(Jz.list.2018)) 
for(i in 1:length(Jz.2018)){
  Jz.2018[i] = Jz.list.2018[[i]]$coefficients[2]*-1
}

Jz.2020 = vector(length=length(Jz.list.2020)) 
for(i in 1:length(Jz.2020)){
  Jz.2020[i] = Jz.list.2020[[i]]$coefficients[2]*-1
}

alpha.2018 = alpha[alpha$Depth > 9 & alpha$Depth < 23.5, 2]
alpha.2020 = alpha[alpha$Depth > 9 & alpha$Depth < 21, 2]


Jzlog.2018=nlsLM(Jz.2018~b*log10(k*alpha.2018),
           start = list(b=0.03, k=150))
Jzlog.2020=nlsLM(Jz.2020~b*log10(k*alpha.2020),
                 start = list(b=0.03, k=150))

#Try a Exponential plateau equation
Jzmax2018 = max(Jz.2018)
Jzdif2018 = Jzmax2018 - min(Jz.2018)

JzExp.plateau.2018=nlsLM(Jz.2018 ~ j.m - (j.m-b) * exp(-k*alpha.2018),
                         start = list(j.m = Jzmax2018, b = 0, k=6))
fit2018m = coef(JzExp.plateau.2018)[1]
fit2018b = coef(JzExp.plateau.2018)[2]
fit2018k = coef(JzExp.plateau.2018)[3]

Jzmax2020 = max(Jz.2020)
Jzdif2020 = Jzmax2020 - min(Jz.2020)

JzExp.plateau.2020=nlsLM(Jz.2020 ~ j.m - (j.m-b) * exp(-k*alpha.2020),
                         start = list(j.m = Jzmax2020, b = 0, k=6))
fit2020m = coef(JzExp.plateau.2020)[1]
fit2020b = coef(JzExp.plateau.2020)[2]
fit2020k = coef(JzExp.plateau.2020)[3]


#Plot all three curves together
png("./Output/Jz-alpha.Mendota.png", width = 8, height = 6, units = "in", res=300)
par(mfrow=c(1,2))
par(mar=c(5,5.2,3,1)+0.1)
plot(Jz.2018 ~ alpha.2018,
     xlab = expression(alpha(z)),
     ylab = expression(Jz~(mgO[2]~L^-1~d^-1)),
     main = "2018",
     ylim = c(0.15, 0.25),
     xlim = c(0,0.45), las = 1)
abline(lm(Jz.2018 ~ alpha.2018), lty=2, col="#9BBB59", lwd = 2)
curve(coef(Jzlog.2018)[1] * log10(coef(Jzlog.2018)[2]*x), add=T, col = "#00B0F0", lwd = 2)
curve(fit2018m - (fit2018m-fit2018b)*exp(-1*fit2018k*x), add=T, col = "#002060", lwd = 2)

plot(Jz.2020 ~ alpha.2020,
     xlab = expression(alpha(z)),
     ylab = expression(Jz~(mgO[2]~L^-1~d^-1)),
     main = "2020",
     ylim = c(0.15, 0.25),
     xlim = c(0,0.45), las = 1)
abline(lm(Jz.2020 ~ alpha.2020), lty=2, col="#9BBB59", lwd = 2)
curve(coef(Jzlog.2020)[1] * log10(coef(Jzlog.2020)[2]*x), add=T, col = "#00B0F0", lwd = 2)
curve(fit2020m - (fit2020m-fit2020b)*exp(-1*fit2020k*x), add=T, col = "#002060", lwd = 2)
#curve(fit2018m - (fit2018m-fit2018b)*exp(-1*fit2018k*x), add=T, col = "Red", lwd = 2)
dev.off()

##########################################################################################
###############################Model oxygen data with the different methods###############
##########################################################################################
#####
#2018

#Coefficients needed to model O2 decay
Living2018coef = coef(lm(Jz.2018 ~ alpha.2018))
Log2018coef = coef(Jzlog.2018)

#O2 consumption per day for each model
O2decayLiving2018 = O2.decay(b = Living2018coef[1],
                             k = Living2018coef[2],
                             Model = "Livingstone",
                             alpha = alpha.2018)

O2decayLog2018 = O2.decay(b = Log2018coef[1],
                             k = Log2018coef[2],
                             Model = "Log",
                             alpha = alpha.2018)

O2decayExp2018 = O2.decay(b = fit2018b,
                          k = fit2018k,
                          Model = "Exp",
                          alpha = alpha.2018,
                          JzMax = fit2018m)

#Create a matrix of modeled values using the three O2 consumption rates
nday = 150 #Numbers of days to simulate
dbeg = 135 #First day of simulated stratification

#Blank matrix of the right size
O2.2018.Living = as.data.frame(matrix(nrow=length(O2decayLiving2018), ncol = nday))
O2.2018.Log = as.data.frame(matrix(nrow=length(O2decayLog2018), ncol = nday))
O2.2018.Exp = as.data.frame(matrix(nrow=length(O2decayExp2018), ncol = nday))

#Model O2 data
O2.2018.Living[,1] = 12.3
for(i in 2:ncol(O2.2018.Living)){
  O2.2018.Living[,i] = 12.3 - O2decayLiving2018*(i-1)
}
O2.2018.Living[O2.2018.Living<0] = 0 #Change negative values to 0

O2.2018.Log[,1] = 12.3
for(i in 2:ncol(O2.2018.Log)){
  O2.2018.Log[,i] = 12.3 - O2decayLog2018*(i-1)
}
O2.2018.Log[O2.2018.Log<0] = 0 #Change negative values to 0

O2.2018.Exp[,1] = 12.3
for(i in 2:ncol(O2.2018.Exp)){
  O2.2018.Exp[,i] = 12.3 - O2decayExp2018*(i-1)
}
O2.2018.Exp[O2.2018.Exp<0] = 0 #Change negative values to 0

#####Find the closest match with real data using minimum sum of squared difference#####
#Remove unwanted rows of Profiles data
data.2018 = data.2018[data.2018$depth>9,]
data.2018.cut = data.2018[-c(1:42),]


Match.temp.1.Living = O2.2018.Living
for(i in 1:ncol(O2.2018.Living)){
  Match.temp.1.Living[,i] = O2.2018.Living[,i] - data.2018.cut$do_raw[c(1:14)]
}

Match.temp.1.Living.sqr = (Match.temp.1.Living)^2
Match.day.1.Living = which(colSums(Match.temp.1.Living.sqr) == min(colSums(Match.temp.1.Living.sqr)))
min(colSums(Match.temp.1.Living.sqr)) #2.471939

Match.temp.1.Log = O2.2018.Log
for(i in 1:ncol(O2.2018.Log)){
  Match.temp.1.Log[,i] = O2.2018.Log[,i] - data.2018.cut$do_raw[c(1:14)]
}

Match.temp.1.Log.sqr = (Match.temp.1.Log)^2
Match.day.1.Log = which(colSums(Match.temp.1.Log.sqr) == min(colSums(Match.temp.1.Log.sqr)))
min(colSums(Match.temp.1.Log.sqr)) #2.235701

Match.temp.1.Exp = O2.2018.Exp
for(i in 1:ncol(O2.2018.Exp)){
  Match.temp.1.Exp[,i] = O2.2018.Exp[,i] - data.2018.cut$do_raw[c(1:14)] #2018-5-4, DOY124
}

Match.temp.1.Exp.sqr = (Match.temp.1.Exp)^2
Match.day.1.Exp = which(colSums(Match.temp.1.Exp.sqr) == min(colSums(Match.temp.1.Exp.sqr)))
min(colSums(Match.temp.1.Exp.sqr)) #2.159794



#Find the closest match for one of the late profile to compare the difference between
#methods, and the "real" vs "+x Days" match

#Change negative values by 0
data.2018.cut[data.2018.cut$do_raw<0, "do_raw"] = 0

Match.day.2 = Match.day.1.Exp + 68

Match.temp.2.Living = O2.2018.Living
for(i in 1:ncol(O2.2018.Living)){
  Match.temp.2.Living[,i] = O2.2018.Living[,i] - data.2018.cut$do_raw[c(125:138)] #2018-7-11, DOY192
}

Match.temp.2.Living.sqr = (Match.temp.2.Living)^2
Match.day.2.Living = which(colSums(Match.temp.2.Living.sqr) == min(colSums(Match.temp.2.Living.sqr)))
min(colSums(Match.temp.2.Living.sqr)) #0.07143753

Match.temp.2.Log = O2.2018.Log
for(i in 1:ncol(O2.2018.Log)){
  Match.temp.2.Log[,i] = O2.2018.Log[,i] - data.2018.cut$do_raw[c(125:138)] #2018-7-11, DOY192
}

Match.temp.2.Log.sqr = (Match.temp.2.Log)^2
Match.day.2.Log = which(colSums(Match.temp.2.Log.sqr) == min(colSums(Match.temp.2.Log.sqr)))
min(colSums(Match.temp.2.Log.sqr)) #0.08995276

Match.temp.2.Exp = O2.2018.Exp
for(i in 1:ncol(O2.2018.Exp)){
  Match.temp.2.Exp[,i] = O2.2018.Exp[,i] - data.2018.cut$do_raw[c(125:138)] #2018-7-11, DOY192
}

Match.temp.2.Exp.sqr = (Match.temp.2.Exp)^2
Match.day.2.Exp = which(colSums(Match.temp.2.Exp.sqr) == min(colSums(Match.temp.2.Exp.sqr)))
min(colSums(Match.temp.2.Exp.sqr)) #0.1706655


#Find the closest match for one of the close profile to compare the difference between
#methods, and the "real" vs "+x Days" match

Match.day.3 = Match.day.1.Exp + 20

Match.temp.3.Living = O2.2018.Living
for(i in 1:ncol(O2.2018.Living)){
  Match.temp.3.Living[,i] = O2.2018.Living[,i] - data.2018.cut$do_raw[c(29:42)] #2018-6-1, DOY152
}

Match.temp.3.Living.sqr = (Match.temp.3.Living)^2
Match.day.3.Living = which(colSums(Match.temp.3.Living.sqr) == min(colSums(Match.temp.3.Living.sqr)))
min(colSums(Match.temp.3.Living.sqr)) #1.052928

Match.temp.3.Log = O2.2018.Log
for(i in 1:ncol(O2.2018.Log)){
  Match.temp.3.Log[,i] = O2.2018.Log[,i] - data.2018.cut$do_raw[c(29:42)] #2018-5-24, DOY144
}

Match.temp.3.Log.sqr = (Match.temp.3.Log)^2
Match.day.3.Log = which(colSums(Match.temp.3.Log.sqr) == min(colSums(Match.temp.3.Log.sqr)))
min(colSums(Match.temp.3.Log.sqr)) #0.7190232

Match.temp.3.Exp = O2.2018.Exp
for(i in 1:ncol(O2.2018.Exp)){
  Match.temp.3.Exp[,i] = O2.2018.Exp[,i] - data.2018.cut$do_raw[c(29:42)] #2018-5-24, DOY144
}

Match.temp.3.Exp.sqr = (Match.temp.3.Exp)^2
Match.day.3.Exp = which(colSums(Match.temp.3.Exp.sqr) == min(colSums(Match.temp.3.Exp.sqr)))
min(colSums(Match.temp.3.Exp.sqr)) #0.88079514

######################################Repeat for 2020#########################
#Coefficients needed to model O2 decay
Living2020coef = coef(lm(Jz.2020 ~ alpha.2020))
Log2020coef = coef(Jzlog.2020)

#O2 consumption per day for each model
O2decayLiving2020 = O2.decay(b = Living2020coef[1],
                             k = Living2020coef[2],
                             Model = "Livingstone",
                             alpha = alpha.2020)

O2decayLog2020 = O2.decay(b = Log2020coef[1],
                          k = Log2020coef[2],
                          Model = "Log",
                          alpha = alpha.2020)

O2decayExp2020 = O2.decay(b = fit2020b,
                          k = fit2020k,
                          Model = "Exp",
                          alpha = alpha.2020,
                          JzMax = fit2020m)

#Create a matrix of modeled values using the three O2 consumption rates
nday = 150 #Numbers of days to simulate
dbeg = 135 #First day of simulated stratification

#Blank matrix of the right size
O2.2020.Living = as.data.frame(matrix(nrow=length(O2decayLiving2020), ncol = nday))
O2.2020.Log = as.data.frame(matrix(nrow=length(O2decayLog2020), ncol = nday))
O2.2020.Exp = as.data.frame(matrix(nrow=length(O2decayExp2020), ncol = nday))

#Model O2 data
O2.2020.Living[,1] = 12.3
for(i in 2:ncol(O2.2020.Living)){
  O2.2020.Living[,i] = 12.3 - O2decayLiving2020*(i-1)
}
O2.2020.Living[O2.2020.Living<0] = 0 #Change negative values to 0

O2.2020.Log[,1] = 12.3
for(i in 2:ncol(O2.2020.Log)){
  O2.2020.Log[,i] = 12.3 - O2decayLog2020*(i-1)
}
O2.2020.Log[O2.2020.Log<0] = 0 #Change negative values to 0

O2.2020.Exp[,1] = 12.3
for(i in 2:ncol(O2.2020.Exp)){
  O2.2020.Exp[,i] = 12.3 - O2decayExp2020*(i-1)
}
O2.2020.Exp[O2.2020.Exp<0] = 0 #Change negative values to 0

#####Find the closest match with real data using minimum sum of squared difference#####
#Remove unwanted rows of Profiles data
data.2020 = data.2020[data.2020$depth>9,]
data.2020.cut = data.2020[-c(1:55),]


Match.temp.1.2020.Living = O2.2020.Living
for(i in 1:ncol(O2.2020.Living)){
  Match.temp.1.2020.Living[,i] = O2.2020.Living[,i] - data.2020.cut$do_raw[c(1:11)]
}

Match.temp.1.2020.Living.sqr = (Match.temp.1.2020.Living)^2
Match.day.1.2020.Living = which(colSums(Match.temp.1.2020.Living.sqr) == min(colSums(Match.temp.1.2020.Living.sqr)))
min(colSums(Match.temp.1.2020.Living.sqr)) #0.133046

Match.temp.1.2020.Log = O2.2020.Log
for(i in 1:ncol(O2.2020.Log)){
  Match.temp.1.2020.Log[,i] = O2.2020.Log[,i] - data.2020.cut$do_raw[c(1:11)]
}

Match.temp.1.2020.Log.sqr = (Match.temp.1.2020.Log)^2
Match.day.1.2020.Log = which(colSums(Match.temp.1.2020.Log.sqr) == min(colSums(Match.temp.1.2020.Log.sqr)))
min(colSums(Match.temp.1.2020.Log.sqr)) #0.2077184

Match.temp.1.2020.Exp = O2.2020.Exp
for(i in 1:ncol(O2.2020.Exp)){
  Match.temp.1.2020.Exp[,i] = O2.2020.Exp[,i] - data.2020.cut$do_raw[c(1:11)]  #2020-5-22, DOY143
}

Match.temp.1.2020.Exp.sqr = (Match.temp.1.2020.Exp)^2
Match.day.1.2020.Exp = which(colSums(Match.temp.1.2020.Exp.sqr) == min(colSums(Match.temp.1.2020.Exp.sqr)))
min(colSums(Match.temp.1.2020.Exp.sqr)) #0.2025038



#Find the closest match for one of the late profile to compare the difference between
#methods, and the "real" vs "+x Days" match

#Change negative values by 0

Match.day.2.2020 = Match.day.1.2020.Exp + 41

Match.temp.2.2020.Living = O2.2020.Living
for(i in 1:ncol(O2.2020.Living)){
  Match.temp.2.2020.Living[,i] = O2.2020.Living[,i] - data.2020.cut$do_raw[c(67:77)] #	2020-7-2	, DOY184
}

Match.temp.2.2020.Living.sqr = (Match.temp.2.2020.Living)^2
Match.day.2.2020.Living = which(colSums(Match.temp.2.2020.Living.sqr) == min(colSums(Match.temp.2.2020.Living.sqr)))
min(colSums(Match.temp.2.2020.Living.sqr)) #4.300592

Match.temp.2.2020.Log = O2.2020.Log
for(i in 1:ncol(O2.2020.Log)){
  Match.temp.2.2020.Log[,i] = O2.2020.Log[,i] - data.2020.cut$do_raw[c(67:77)] #	2020-7-2	, DOY184
}

Match.temp.2.2020.Log.sqr = (Match.temp.2.2020.Log)^2
Match.day.2.2020.Log = which(colSums(Match.temp.2.2020.Log.sqr) == min(colSums(Match.temp.2.2020.Log.sqr)))
min(colSums(Match.temp.2.2020.Log.sqr)) #4.325419

Match.temp.2.2020.Exp = O2.2020.Exp
for(i in 1:ncol(O2.2020.Exp)){
  Match.temp.2.2020.Exp[,i] = O2.2020.Exp[,i] - data.2020.cut$do_raw[c(67:77)] #	2020-7-2	, DOY184
}

Match.temp.2.2020.Exp.sqr = (Match.temp.2.2020.Exp)^2
Match.day.2.2020.Exp = which(colSums(Match.temp.2.2020.Exp.sqr) == min(colSums(Match.temp.2.2020.Exp.sqr)))
min(colSums(Match.temp.2.2020.Exp.sqr)) #4.423028


#Find the closest match for one of the close profile to compare the difference between
#methods, and the "real" vs "+x Days" match

Match.day.3.2020 = Match.day.1.2020.Exp + 20

Match.temp.3.2020.Living = O2.2020.Living
for(i in 1:ncol(O2.2020.Living)){
  Match.temp.3.2020.Living[,i] = O2.2020.Living[,i] - data.2020.cut$do_raw[c(56:66)] #2020-6-25, DOY177
}

Match.temp.3.2020.Living.sqr = (Match.temp.3.2020.Living)^2
Match.day.3.2020.Living = which(colSums(Match.temp.3.2020.Living.sqr) == min(colSums(Match.temp.3.2020.Living.sqr)))
min(colSums(Match.temp.3.2020.Living.sqr)) #6.866874

Match.temp.3.2020.Log = O2.2020.Log
for(i in 1:ncol(O2.2020.Log)){
  Match.temp.3.2020.Log[,i] = O2.2020.Log[,i] - data.2020.cut$do_raw[c(56:66)] #2020-6-25, DOY177
}

Match.temp.3.2020.Log.sqr = (Match.temp.3.2020.Log)^2
Match.day.3.2020.Log = which(colSums(Match.temp.3.2020.Log.sqr) == min(colSums(Match.temp.3.2020.Log.sqr)))
min(colSums(Match.temp.3.2020.Log.sqr)) #4.811883

Match.temp.3.2020.Exp = O2.2020.Exp
for(i in 1:ncol(O2.2020.Exp)){
  Match.temp.3.2020.Exp[,i] = O2.2020.Exp[,i] - data.2020.cut$do_raw[c(56:66)] #2020-6-25, DOY177
}

Match.temp.3.2020.Exp.sqr = (Match.temp.3.2020.Exp)^2
Match.day.3.2020.Exp = which(colSums(Match.temp.3.2020.Exp.sqr) == min(colSums(Match.temp.3.2020.Exp.sqr)))
min(colSums(Match.temp.3.2020.Exp.sqr)) #4.872196


################################Transform O2 profiles into anoxic age##################
#2018

O2.threshold = c(1,2)
O2.2018.Living.bin.cumul.list=list()
O2.2018.Log.bin.cumul.list=list()
O2.2018.Exp.bin.cumul.list=list()
for(i in 1:length(O2.threshold))
  {
#Livingstone model
O2.2018.Living.bin <- ifelse(O2.2018.Living <= O2.threshold[i], 1, 0)
O2.2018.Living.bin.cumul <- O2.2018.Living.bin
for(j in 2:ncol(O2.2018.Living.bin.cumul)){
  temp = O2.2018.Living.bin.cumul[,j-1]+O2.2018.Living.bin.cumul[,j]
  temp = ifelse(temp > O2.2018.Living.bin.cumul[,j-1], temp, 0)
  O2.2018.Living.bin.cumul[,j] = temp
}
#Add a column for depths
O2.2018.Living.bin.cumul = cbind(c(10:23), O2.2018.Living.bin.cumul)
#Change the column names
colnames(O2.2018.Living.bin.cumul)[1] = "Depth"

#Make sure column number 7 is Day of Year 124 to match with the real dates
colnames(O2.2018.Living.bin.cumul)[2:(nday+1)] = paste(seq(119,119+nday-1,1))

#Transform into dataframe
O2.2018.Living.bin.cumul.list[[i]] = as.data.frame(O2.2018.Living.bin.cumul)

#Log model
O2.2018.Log.bin <- ifelse(O2.2018.Log <= O2.threshold[i], 1, 0)
O2.2018.Log.bin.cumul <- O2.2018.Log.bin
for(j in 2:ncol(O2.2018.Log.bin.cumul)){
  temp = O2.2018.Log.bin.cumul[,j-1]+O2.2018.Log.bin.cumul[,j]
  temp = ifelse(temp > O2.2018.Log.bin.cumul[,j-1], temp, 0)
  O2.2018.Log.bin.cumul[,j] = temp
}
#Add a column for depths
O2.2018.Log.bin.cumul = cbind(c(10:23), O2.2018.Log.bin.cumul)
#Change the column names
colnames(O2.2018.Log.bin.cumul)[1] = "Depth"
colnames(O2.2018.Log.bin.cumul)[2:(nday+1)] = paste(seq(119,119+nday-1,1))

#Transform into dataframe
O2.2018.Log.bin.cumul.list[[i]] = as.data.frame(O2.2018.Log.bin.cumul)

#Exponential plateau model
O2.2018.Exp.bin <- ifelse(O2.2018.Exp <= O2.threshold[i], 1, 0)
O2.2018.Exp.bin.cumul <- O2.2018.Exp.bin
for(j in 2:ncol(O2.2018.Exp.bin.cumul)){
  temp = O2.2018.Exp.bin.cumul[,j-1]+O2.2018.Exp.bin.cumul[,j]
  temp = ifelse(temp > O2.2018.Exp.bin.cumul[,j-1], temp, 0)
  O2.2018.Exp.bin.cumul[,j] = temp
}
#Add a column for depths
O2.2018.Exp.bin.cumul = cbind(c(10:23), O2.2018.Exp.bin.cumul)
#Change the column names
colnames(O2.2018.Exp.bin.cumul)[1] = "Depth"
colnames(O2.2018.Exp.bin.cumul)[2:(nday+1)] = paste(seq(119,119+nday-1,1))

#Transform into dataframe
O2.2018.Exp.bin.cumul.list[[i]] = as.data.frame(O2.2018.Exp.bin.cumul)
}
names(O2.2018.Exp.bin.cumul.list) <- names(O2.2018.Log.bin.cumul.list) <- names(O2.2018.Living.bin.cumul.list) <- paste0("Threshold",O2.threshold)
##################Make dataframe with anoxic days and corresponding FDOM values###########

#Extract DOY from field database
DOY.data.2018 <- unique(data.2018.cut$DOY)

FDOM.living.2018.list = list()
FDOM.log.2018.list = list()
FDOM.exp.2018.list = list()

for(j in 1:length(O2.2018.Exp.bin.cumul.list))
{
  aa.DOY.Living = cbind(Depth = O2.2018.Living.bin.cumul.list[[j]][,1], alpha=alpha.2018,
                      O2.2018.Living.bin.cumul.list[[j]][,match(DOY.data.2018, colnames(O2.2018.Living.bin.cumul.list[[j]]))])

aa.DOY.Log = cbind(Depth = O2.2018.Log.bin.cumul.list[[j]][,1], alpha=alpha.2018,
                      O2.2018.Log.bin.cumul.list[[j]][,match(DOY.data.2018, colnames(O2.2018.Log.bin.cumul.list[[j]]))])

aa.DOY.Exp = cbind(Depth = O2.2018.Exp.bin.cumul.list[[j]][,1], alpha=alpha.2018,
                      O2.2018.Exp.bin.cumul.list[[j]][,match(DOY.data.2018, colnames(O2.2018.Exp.bin.cumul.list[[j]]))])

#Change to long format
aa.DOY.Living.long = pivot_longer(aa.DOY.Living, cols = c(3:18),
                                  names_to = "DOY", values_to = "AnoxicAge")
aa.DOY.Log.long = pivot_longer(aa.DOY.Log, cols = c(3:18),
                                  names_to = "DOY", values_to = "AnoxicAge")
aa.DOY.Exp.long = pivot_longer(aa.DOY.Exp, cols = c(3:18),
                                  names_to = "DOY", values_to = "AnoxicAge")



#Match FDOM values with depth and DOY
#DOY Match
FDOM.all = vector(length=14) #Create temporary vector
for(i in 1:length(DOY.data.2018)) {
  temp = data.2018.cut[data.2018.cut$DOY==DOY.data.2018[i],"fdom"] #Find the value
  if(temp != 14) temp = c(temp,rep(NA, 14-length(temp))) #Complete Vector for missing depths
  if(i==1) FDOM.all = temp else(FDOM.all = c(FDOM.all,temp)) #Combine with prior values
}


#Add fdom values to anoxic age df
aa.DOY.Living.long = aa.DOY.Living.long[order(aa.DOY.Living.long$DOY, decreasing = F),]
aa.DOY.Living.long$Fdom = FDOM.all
aa.DOY.Log.long = aa.DOY.Log.long[order(aa.DOY.Log.long$DOY, decreasing = F),]
aa.DOY.Log.long$Fdom = FDOM.all
aa.DOY.Exp.long = aa.DOY.Exp.long[order(aa.DOY.Exp.long$DOY, decreasing = F),]
aa.DOY.Exp.long$Fdom = FDOM.all

FDOM.living.2018.list[[j]] = as.data.frame(aa.DOY.Living.long)
FDOM.log.2018.list[[j]] = as.data.frame(aa.DOY.Log.long)
FDOM.exp.2018.list[[j]] = as.data.frame(aa.DOY.Exp.long)
}



#Test the regressions
#Livingstone model

Anoxic.lm.Living.list = list()
for(i in 1:length(FDOM.living.2018.list)){
Anoxic.lm.Living.list[[i]] = lm(Fdom ~ AnoxicAge, data = FDOM.living.2018.list[[i]][FDOM.living.2018.list[[i]]$AnoxicAge!=0,])
}

Multiple.lm.Living = lm(Fdom ~ AnoxicAge + Depth, data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,])
Anoxic.R2.living = round(summary(Anoxic.lm.Living)$r.squared,3)
Multiple.lm.Living.a = lm(Fdom ~ AnoxicAge + Depth+ alpha, data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,])
Multiple.lm.Living.f = lm(aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,"Fdom"]~ 
                            aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,"AnoxicAge"] +
                            aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,"Depth"]+
                            aa.DOY.Living.long[c(1:14),"Fdom"],)


#Log-linear model
Anoxic.lm.Log.list = list()
for(i in 1:length(FDOM.log.2018.list)){
  Anoxic.lm.Log.list[[i]] = lm(Fdom ~ AnoxicAge, data = FDOM.log.2018.list[[i]][FDOM.log.2018.list[[i]]$AnoxicAge!=0,])
}

Multiple.lm.Log = lm(Fdom ~ AnoxicAge + Depth, data = aa.DOY.Log.long[aa.DOY.Log.long$AnoxicAge!=0,])
Anoxic.R2.Log = round(summary(Anoxic.lm.Log)$r.squared,3)
Multiple.lm.Log.a = lm(Fdom ~ AnoxicAge + Depth + alpha, data = aa.DOY.Log.long[aa.DOY.Log.long$AnoxicAge!=0,])

#Exponential plateau
Anoxic.lm.Exp.list = list()
for(i in 1:length(FDOM.exp.2018.list)){
  Anoxic.lm.Exp.list[[i]] = lm(Fdom ~ AnoxicAge, data = FDOM.exp.2018.list[[i]][FDOM.exp.2018.list[[i]]$AnoxicAge!=0,])
}


Multiple.lm.Exp = lm(Fdom ~ AnoxicAge + Depth, data = aa.DOY.Exp.long[aa.DOY.Exp.long$AnoxicAge!=0,])
Anoxic.R2.Exp = round(summary(Anoxic.lm.Exp)$r.squared,3)
Multiple.lm.Exp.a = lm(Fdom ~ AnoxicAge + Depth + alpha, data = aa.DOY.Exp.long[aa.DOY.Exp.long$AnoxicAge!=0,])

#Depth
Depth.lm.Living = lm(Fdom ~ Depth, data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,])
Depth.R2.living = round(summary(Depth.lm.Living)$r.squared,3)

#alpha
alpha.lm.Living = lm(Fdom ~ alpha, data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,])



#All the summaries together
#Anoxic age
summary(Anoxic.lm.Living)
summary(Anoxic.lm.Log)
summary(Anoxic.lm.Exp)
#Depth
summary(Depth.lm.Living)
#Anoxic age + depth
summary(Multiple.lm.Living)
summary(Multiple.lm.Log)
summary(Multiple.lm.Exp)


#plot the results
{
png("./Output/Mendota.Fdom-aa.2018.png", width = 8, height = 6, units = "in", res=300)
par(mfrow=c(2,2))
# par(mfrow=c(1,3))
# plot(Fdom ~ jitter(AnoxicAge), data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge==0,],
#         las = 1,
#         ylim = c(13,17),
#         xlab = "Anoxic age (d)",
#         ylab = "FDOM",
#      bg = Depth, pch = 21)
plot(Fdom ~ AnoxicAge, data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,],
     las = 1,
     #ylim = c(14,17),
     xlim = c(0,75),
     xlab = "Anoxic age (d)",
     ylab = expression(F[365/480]~(QSE)),
     main = "Livingstone",
     #bg = Depth,
     pch = 21)
text(x=11, y = 16.8, labels = bquote(R^2 == .(Anoxic.R2.living)))
abline(Anoxic.lm.Living)
mtext("a", side = 3, at = -1)
# plot(Fdom ~ Depth, data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,],
#      las = 1,
#      ylim = c(14,17),
#      xlim = c(9,25),
#      xlab = "Depth (m)",
#      ylab = "FDOM",
#      main = "Livingstone",
#      bg = AnoxicAge, pch = 21)
# abline(Depth.lm.Living)
# text(x=11.4, y = 16.8, labels = bquote(R^2 == .(Depth.R2.living)))
# mtext("b", side = 3, at = 9)

plot(Fdom ~ AnoxicAge, data = aa.DOY.Log.long[aa.DOY.Log.long$AnoxicAge!=0,],
     las = 1,
     ylim = c(14,17),
     xlim = c(0,75),
     xlab = "Anoxic age (d)",
     ylab = expression(F[365/480]~(QSE)),
     main = "Log-linear",
     #bg = Depth,
     pch = 21)
text(x=11, y = 16.8, labels = bquote(R^2 == .(Anoxic.R2.Log)))
abline(Anoxic.lm.Log)
mtext("b", side = 3, at = -1)

plot(Fdom ~ AnoxicAge, data = aa.DOY.Exp.long[aa.DOY.Exp.long$AnoxicAge!=0,],
     las = 1,
     ylim = c(14,17),
     xlim = c(0,75),
     xlab = "Anoxic age (d)",
     ylab = expression(F[365/480]~(QSE)),
     main = "Exponential plateau",
     #bg = Depth,
     pch = 21)
text(x=11, y = 16.8, labels = bquote(R^2 == .(Anoxic.R2.Exp)))
abline(Anoxic.lm.Exp)
mtext("c", side = 3, at = -1)
plot(Fdom ~ Depth, data = aa.DOY.Exp.long[aa.DOY.Exp.long$AnoxicAge!=0,],
     las = 1,
     ylim = c(14,17),
     xlim = c(9,25),
     xlab = "Depth (m)",
     ylab = expression(F[365/480]~(QSE)),
     main = "Depth",
     #bg = AnoxicAge,
     pch = 21)
abline(Depth.lm.Living)
text(x=11.4, y = 16.8, labels = bquote(R^2 == .(Depth.R2.living)))
mtext("d", side = 3, at = 9)
dev.off()
}


#Repeat all steps for 2020
#Livingstone model
O2.2020.Living.bin.cumul.list=list()
O2.2020.Log.bin.cumul.list=list()
O2.2020.Exp.bin.cumul.list=list()

for(i in 1:length(O2.threshold))
{
O2.2020.Living.bin <- ifelse(O2.2020.Living <= O2.threshold[i], 1, 0)
O2.2020.Living.bin.cumul <- O2.2020.Living.bin
for(j in 2:ncol(O2.2020.Living.bin.cumul)){
  temp = O2.2020.Living.bin.cumul[,j-1]+O2.2020.Living.bin.cumul[,j]
  temp = ifelse(temp > O2.2020.Living.bin.cumul[,j-1], temp, 0)
  O2.2020.Living.bin.cumul[,j] = temp
}
#Add a column for depths
O2.2020.Living.bin.cumul = cbind(c(10:20), O2.2020.Living.bin.cumul)
#Change the column names
colnames(O2.2020.Living.bin.cumul)[1] = "Depth"

#Make sure column number 21 is Day of Year 143 to match with the real dates
colnames(O2.2020.Living.bin.cumul)[2:(nday+1)] = paste(seq(124,124+nday-1,1))

#Transform into dataframe
O2.2020.Living.bin.cumul.list[[i]] = as.data.frame(O2.2020.Living.bin.cumul)

#Log model
O2.2020.Log.bin <- ifelse(O2.2020.Log <= O2.threshold[i], 1, 0)
O2.2020.Log.bin.cumul <- O2.2020.Log.bin
for(j in 2:ncol(O2.2020.Log.bin.cumul)){
  temp = O2.2020.Log.bin.cumul[,j-1]+O2.2020.Log.bin.cumul[,j]
  temp = ifelse(temp > O2.2020.Log.bin.cumul[,j-1], temp, 0)
  O2.2020.Log.bin.cumul[,j] = temp
}
#Add a column for depths
O2.2020.Log.bin.cumul = cbind(c(10:20), O2.2020.Log.bin.cumul)
#Change the column names
colnames(O2.2020.Log.bin.cumul)[1] = "Depth"
colnames(O2.2020.Log.bin.cumul)[2:(nday+1)] = paste(seq(124,124+nday-1,1))

#Transform into dataframe
O2.2020.Log.bin.cumul.list[[i]] = as.data.frame(O2.2020.Log.bin.cumul)

#Exponential plateau model
O2.2020.Exp.bin <- ifelse(O2.2020.Exp <= O2.threshold[i], 1, 0)
O2.2020.Exp.bin.cumul <- O2.2020.Exp.bin
for(j in 2:ncol(O2.2020.Exp.bin.cumul)){
  temp = O2.2020.Exp.bin.cumul[,j-1]+O2.2020.Exp.bin.cumul[,j]
  temp = ifelse(temp > O2.2020.Exp.bin.cumul[,j-1], temp, 0)
  O2.2020.Exp.bin.cumul[,j] = temp
}
#Add a column for depths
O2.2020.Exp.bin.cumul = cbind(c(10:20), O2.2020.Exp.bin.cumul)
#Change the column names
colnames(O2.2020.Exp.bin.cumul)[1] = "Depth"
colnames(O2.2020.Exp.bin.cumul)[2:(nday+1)] = paste(seq(124,124+nday-1,1))

#Transform into dataframe
O2.2020.Exp.bin.cumul.list[[i]] = as.data.frame(O2.2020.Exp.bin.cumul)
}

##################Make dataframe with anoxic days and corresponding FDOM values###########
 
#Extract DOY from field database
data.2020.cut <- data.2020.cut[c(1:165),]
data.2020.cut <- data.2020.cut[data.2020.cut$depth !=23.5,]
DOY.data.2020 <- unique(data.2020.cut$DOY)


FDOM.living.2020.list = list()
FDOM.log.2020.list = list()
FDOM.exp.2020.list = list()

for(j in 1:length(O2.2020.Exp.bin.cumul.list))
{
aa.DOY.Living.2020 = cbind(Depth = O2.2020.Living.bin.cumul.list[[j]][,1], alpha = alpha.2020, 
                           O2.2020.Living.bin.cumul.list[[j]][,match(DOY.data.2020, colnames(O2.2020.Living.bin.cumul.list[[j]]))])

aa.DOY.Log.2020 = cbind(Depth = O2.2020.Log.bin.cumul.list[[j]][,1], alpha = alpha.2020, 
                   O2.2020.Log.bin.cumul.list[[j]][,match(DOY.data.2020, colnames(O2.2020.Log.bin.cumul.list[[j]]))])

aa.DOY.Exp.2020 = cbind(Depth = O2.2020.Exp.bin.cumul.list[[j]][,1], alpha = alpha.2020, 
                   O2.2020.Exp.bin.cumul.list[[j]][,match(DOY.data.2020, colnames(O2.2020.Exp.bin.cumul.list[[j]]))])

#Change to long format
aa.DOY.Living.2020.long = pivot_longer(aa.DOY.Living.2020, cols = c(3:17),
                                  names_to = "DOY", values_to = "AnoxicAge")
aa.DOY.Log.2020.long = pivot_longer(aa.DOY.Log.2020, cols = c(3:17),
                               names_to = "DOY", values_to = "AnoxicAge")
aa.DOY.Exp.2020.long = pivot_longer(aa.DOY.Exp.2020, cols = c(3:17),
                               names_to = "DOY", values_to = "AnoxicAge")



#Match FDOM values with depth and DOY
#DOY Match
FDOM.all = vector(length=11) #Create temporary vector. 11 = number of depths
for(i in 1:length(DOY.data.2020)) {
  temp = data.2020.cut[data.2020.cut$DOY==DOY.data.2020[i],"fdom"] #Find the value
  if(length(temp) != 11) temp = c(temp,rep(NA, 11-length(temp))) #Complete Vector for missing depths
  if(i==1) FDOM.all = temp else(FDOM.all = c(FDOM.all,temp)) #Combine with prior values
}


#Add fdom values to anoxic age df
aa.DOY.Living.2020.long = aa.DOY.Living.2020.long[order(aa.DOY.Living.2020.long$DOY, decreasing = F),]
aa.DOY.Living.2020.long$Fdom = FDOM.all
aa.DOY.Log.2020.long = aa.DOY.Log.2020.long[order(aa.DOY.Log.2020.long$DOY, decreasing = F),]
aa.DOY.Log.2020.long$Fdom = FDOM.all
aa.DOY.Exp.2020.long = aa.DOY.Exp.2020.long[order(aa.DOY.Exp.2020.long$DOY, decreasing = F),]
aa.DOY.Exp.2020.long$Fdom = FDOM.all

FDOM.living.2020.list[[j]] = aa.DOY.Living.2020.long
FDOM.log.2020.list[[j]] = aa.DOY.Log.2020.long
FDOM.exp.2020.list[[j]] = aa.DOY.Exp.2020.long
}

#Test the regressions
#Livingstone model
Anoxic.lm.Living.2020 = lm(Fdom ~ AnoxicAge, data = aa.DOY.Living.2020.long[aa.DOY.Living.2020.long$AnoxicAge!=0,])
Multiple.lm.Living.2020 = lm(Fdom ~ AnoxicAge + Depth, data = aa.DOY.Living.2020.long[aa.DOY.Living.2020.long$AnoxicAge!=0,])
Anoxic.R2.Living.2020 = round(summary(Anoxic.lm.Living.2020)$r.squared,3)
Multiple.lm.Living.2020.a = lm(Fdom ~ AnoxicAge + Depth + alpha, data = aa.DOY.Living.2020.long[aa.DOY.Living.2020.long$AnoxicAge!=0,])

#Log-linear model
Anoxic.lm.Log.2020 = lm(Fdom ~ AnoxicAge, data = aa.DOY.Log.2020.long[aa.DOY.Log.2020.long$AnoxicAge!=0,])
Multiple.lm.Log.2020 = lm(Fdom ~ AnoxicAge + Depth, data = aa.DOY.Log.2020.long[aa.DOY.Log.2020.long$AnoxicAge!=0,])
Anoxic.R2.Log.2020 = round(summary(Anoxic.lm.Log.2020)$r.squared,3)
Multiple.lm.Log.2020.a = lm(Fdom ~ AnoxicAge + Depth + alpha, data = aa.DOY.Log.2020.long[aa.DOY.Log.2020.long$AnoxicAge!=0,])

#Exponential plateau
Anoxic.lm.Exp.2020 = lm(Fdom ~ AnoxicAge, data = aa.DOY.Exp.2020.long[aa.DOY.Exp.2020.long$AnoxicAge!=0,])
Multiple.lm.Exp.2020 = lm(Fdom ~ AnoxicAge + Depth, data = aa.DOY.Exp.2020.long[aa.DOY.Exp.2020.long$AnoxicAge!=0,])
Multiple.lm.Exp.2020.a = lm(Fdom ~ AnoxicAge + Depth + alpha, data = aa.DOY.Exp.2020.long[aa.DOY.Exp.2020.long$AnoxicAge!=0,])
Anoxic.R2.Exp.2020 = round(summary(Anoxic.lm.Exp.2020)$r.squared,3)
Anoxic.R2.Exp.2020 = round(summary(Anoxic.lm.Exp.2020)$r.squared,3)
#With depth
Depth.lm.Living.2020 = lm(Fdom ~ Depth, data = aa.DOY.Living.2020.long[aa.DOY.Living.2020.long$AnoxicAge!=0,])
Depth.R2.Living.2020 = round(summary(Depth.lm.Living.2020)$r.squared,3)
#With alpha
Alpha.lm.Living.2020 = lm(Fdom ~ alpha, data = aa.DOY.Living.2020.long[aa.DOY.Living.2020.long$AnoxicAge!=0,])
Alpha.R2.Living.2020 = round(summary(Alpha.lm.Living.2020)$r.squared,3)


#All the summaries together
#Anoxic age
summary(Anoxic.lm.Living.2020)
summary(Anoxic.lm.Log.2020)
summary(Anoxic.lm.Exp.2020)
#Depth
summary(Depth.lm.Living.2020)
#Anoxic age + depth
summary(Multiple.lm.Living.2020)
summary(Multiple.lm.Log.2020)
summary(Multiple.lm.Exp.2020)





{
  pdf("./Data/Mendota/Output/Fdom-aa.2018-2020.pdf", width = 7, height = 3.5)
  # png("./Data/Mendota/Output/Fdom-aa.2018-2020.png", width = 7, height = 3.5, units = "in", res=300)
  par(mfrow=c(2,3))
  par(mar=c(4,5,1,1.5)+.1)
  plot(FDOM.exp.2018.list[[1]]$Fdom ~ FDOM.exp.2018.list[[1]]$DOY,
       xlab = "Time (DOY)",
       ylab = expression(F[365/480]~(QSE)),
       ylim = c(13,17),
       cex = 1.8,
       las = 1,cex.axis = 1.2, cex.lab=1.2)
  mtext("a", side = 3, at = 115)
  plot(FDOM.exp.2018.list[[1]]$Fdom[FDOM.exp.2018.list[[2]]$AnoxicAge!=0] ~ FDOM.exp.2018.list[[1]]$AnoxicAge[FDOM.exp.2018.list[[2]]$AnoxicAge!=0],
       xlab = "Anoxic age (days)",
       # ylab = expression(F[365/480]~(QSE)),
       ylim = c(13,17),
       ylab = "",
       yaxt = "n",
       cex = 1.8,
       las = 1,cex.axis = 1.2, cex.lab=1.2)
  mtext("b", side = 3, at = -5)
  boxplot(FDOM.exp.2018.list[[2]]$Fdom[FDOM.exp.2018.list[[2]]$AnoxicAge==0] ~ FDOM.exp.2018.list[[2]]$AnoxicAge[FDOM.exp.2018.list[[2]]$AnoxicAge==0],
       xlab = "Anoxic age (days, threshold = 2)",
       # ylab = expression(F[365/480]~(QSE)),
       ylim = c(13,17),
       ylab = "",
       yaxt = "n",
       las = 1,cex.axis = 1.2, cex.lab=1.2)
  
  plot(FDOM.exp.2020.list[[1]]$Fdom ~ FDOM.exp.2020.list[[1]]$DOY,
       xlab = "Time (DOY)",
       ylab = expression(F[365/480]~(QSE)),
       ylim = c(10,14),
       cex = 1.8,
       las = 1,cex.axis = 1.2, cex.lab=1.2)
  mtext("c", side = 3, at = 135)
  plot(FDOM.exp.2020.list[[1]]$Fdom[FDOM.exp.2020.list[[2]]$AnoxicAge!=0] ~ FDOM.exp.2020.list[[1]]$AnoxicAge[FDOM.exp.2020.list[[2]]$AnoxicAge!=0],
       xlab = "Anoxic age (days, threshold = 1)",
       # ylab = expression(F[365/480]~(QSE)),
       ylim = c(10,14),
       ylab = "",
       yaxt = "n",
       cex = 1.8,
       las = 1,cex.axis = 1.2, cex.lab=1.2)
  mtext("d", side = 3, at = -5)
  boxplot(FDOM.exp.2020.list[[2]]$Fdom[FDOM.exp.2020.list[[2]]$AnoxicAge==0] ~ FDOM.exp.2020.list[[2]]$AnoxicAge[FDOM.exp.2020.list[[2]]$AnoxicAge==0],
       xlab = "Anoxic age (days, threshold = 2)",
       ylim = c(10,14),
       # ylab = expression(F[365/480]~(QSE)),
       ylab = "",
       yaxt = "n",
       las = 1,cex.axis = 1.2, cex.lab=1.2)
  mtext("f", side = 3, at = -5)
  dev.off()
}
#

FDOM.AA.2018.1.lm <- lm(FDOM.exp.2018.list[[1]]$Fdom ~ FDOM.exp.2018.list[[1]]$AnoxicAge)
FDOM.DOY.2018.1.lm <- lm(FDOM.exp.2018.list[[1]]$Fdom ~ as.numeric(FDOM.exp.2018.list[[1]]$DOY))
FDOM.AA.2018.2.lm <- lm(FDOM.exp.2018.list[[2]]$Fdom ~ FDOM.exp.2018.list[[2]]$AnoxicAge)

FDOM.AA.2018.NoOxic1.lm <- lm(FDOM.exp.2018.list[[1]]$Fdom[FDOM.exp.2018.list[[1]]$AnoxicAge!=0] ~ FDOM.exp.2018.list[[1]]$AnoxicAge[FDOM.exp.2018.list[[1]]$AnoxicAge!=0])
FDOM.DOY.2018.NoOxic1.lm <- lm(FDOM.exp.2018.list[[1]]$Fdom[FDOM.exp.2018.list[[1]]$AnoxicAge!=0] ~ as.numeric(FDOM.exp.2018.list[[1]]$DOY[FDOM.exp.2018.list[[1]]$AnoxicAge!=0]))

FDOM.AA.2018.NoOxic2.lm <- lm(FDOM.exp.2018.list[[2]]$Fdom[FDOM.exp.2018.list[[2]]$AnoxicAge!=0] ~ FDOM.exp.2018.list[[2]]$AnoxicAge[FDOM.exp.2018.list[[2]]$AnoxicAge!=0])
FDOM.DOY.2018.NoOxic2.lm <- lm(FDOM.exp.2018.list[[2]]$Fdom[FDOM.exp.2018.list[[2]]$AnoxicAge!=0] ~ as.numeric(FDOM.exp.2018.list[[2]]$DOY[FDOM.exp.2018.list[[2]]$AnoxicAge!=0]))


FDOM.AA.2020.1.lm <- lm(FDOM.exp.2020.list[[1]]$Fdom ~ FDOM.exp.2020.list[[1]]$AnoxicAge)
FDOM.DOY.2020.1.lm <- lm(FDOM.exp.2020.list[[1]]$Fdom ~ as.numeric(FDOM.exp.2020.list[[1]]$DOY))
FDOM.AA.2020.2.lm <- lm(FDOM.exp.2020.list[[2]]$Fdom ~ FDOM.exp.2020.list[[2]]$AnoxicAge)

FDOM.AA.2020.NoOxic1.lm <- lm(FDOM.exp.2020.list[[1]]$Fdom[FDOM.exp.2020.list[[1]]$AnoxicAge!=0] ~ FDOM.exp.2020.list[[1]]$AnoxicAge[FDOM.exp.2020.list[[1]]$AnoxicAge!=0])
FDOM.DOY.2020.NoOxic1.lm <- lm(FDOM.exp.2020.list[[1]]$Fdom[FDOM.exp.2020.list[[1]]$AnoxicAge!=0] ~ as.numeric(FDOM.exp.2020.list[[1]]$DOY[FDOM.exp.2020.list[[1]]$AnoxicAge!=0]))

FDOM.AA.2020.NoOxic2.lm <- lm(FDOM.exp.2020.list[[2]]$Fdom[FDOM.exp.2020.list[[2]]$AnoxicAge!=0] ~ FDOM.exp.2020.list[[2]]$AnoxicAge[FDOM.exp.2020.list[[2]]$AnoxicAge!=0])
FDOM.DOY.2020.NoOxic2.lm <- lm(FDOM.exp.2020.list[[2]]$Fdom[FDOM.exp.2020.list[[2]]$AnoxicAge!=0] ~ as.numeric(FDOM.exp.2020.list[[2]]$DOY[FDOM.exp.2020.list[[2]]$AnoxicAge!=0]))

#Summaries
#2018
summary(FDOM.AA.2018.1.lm) #R2 = 0.59
summary(FDOM.DOY.2018.1.lm) #R2 = 0.84
summary(FDOM.AA.2018.2.lm) #R2 = 0.63

summary(FDOM.AA.2018.NoOxic1.lm) #R2 = 0.54
summary(FDOM.DOY.2018.NoOxic1.lm) #R2 = 0.28

summary(FDOM.AA.2018.NoOxic2.lm) #R2 = 0.56
summary(FDOM.DOY.2018.NoOxic2.lm) #R2 = 0.32

#2020
summary(FDOM.AA.2020.1.lm) #R2 = 0.72
summary(FDOM.DOY.2020.1.lm) #R2 = 0.80
summary(FDOM.AA.2020.2.lm) #R2 = 0.74

summary(FDOM.AA.2020.NoOxic1.lm) #R2 = 0.70
summary(FDOM.DOY.2020.NoOxic1.lm) #R2 = 0.35

summary(FDOM.AA.2020.NoOxic2.lm) #R2 = 0.66
summary(FDOM.DOY.2020.NoOxic2.lm) #R2 = 0.36






#











#plot the results
{
  png("./Output/Mendota.Fdom-aa.2020.png", width = 6, height = 7, units = "in", res=300)
  par(mfrow=c(2,2))
  # par(mfrow=c(1,3))
  # plot(Fdom ~ jitter(AnoxicAge), data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge==0,],
  #         las = 1,
  #         ylim = c(13,17),
  #         xlab = "Anoxic age (d)",
  #         ylab = "FDOM",
  #      bg = Depth, pch = 21)
  plot(Fdom ~ AnoxicAge, data = aa.DOY.Living.2020.long[aa.DOY.Living.2020.long$AnoxicAge!=0,],
       las = 1,
       ylim = c(11,14),
       xlim = c(0,75),
       xlab = "Anoxic age (d)",
       ylab = expression(F[365/480]~(QSE)),
       main = "Livingstone",
       #bg = Depth,
       pch = 21)
  text(x=11, y = 13.8, labels = bquote(R^2 == .(Anoxic.R2.Living.2020)))
  abline(Anoxic.lm.Living.2020)
  mtext("a", side = 3, at = -1)
  # plot(Fdom ~ Depth, data = aa.DOY.Living.long[aa.DOY.Living.long$AnoxicAge!=0,],
  #      las = 1,
  #      ylim = c(14,17),
  #      xlim = c(9,25),
  #      xlab = "Depth (m)",
  #      ylab = "FDOM",
  #      main = "Livingstone",
  #      bg = AnoxicAge, pch = 21)
  # abline(Depth.lm.Living)
  # text(x=11.4, y = 16.8, labels = bquote(R^2 == .(Depth.R2.living)))
  # mtext("b", side = 3, at = 9)
  
  plot(Fdom ~ AnoxicAge, data = aa.DOY.Log.2020.long[aa.DOY.Log.2020.long$AnoxicAge!=0,],
       las = 1,
       ylim = c(11,14),
       xlim = c(0,75),
       xlab = "Anoxic age (d)",
       ylab = expression(F[365/480]~(QSE)),
       main = "Log-linear",
       #bg = Depth,
       pch = 21)
  text(x=11, y = 13.8, labels = bquote(R^2 == .(Anoxic.R2.Log.2020)))
  abline(Anoxic.lm.Log.2020)
  mtext("b", side = 3, at = -1)
  
  plot(Fdom ~ AnoxicAge, data = aa.DOY.Exp.2020.long[aa.DOY.Exp.2020.long$AnoxicAge!=0,],
       las = 1,
       ylim = c(11,14),
       xlim = c(0,75),
       xlab = "Anoxic age (d)",
       ylab = expression(F[365/480]~(QSE)),
       main = "Exponential plateau",
       #bg = Depth,
       pch = 21)
  text(x=11, y = 13.8, labels = bquote(R^2 == .(Anoxic.R2.Exp.2020)))
  abline(Anoxic.lm.Exp.2020)
  mtext("c", side = 3, at = -1)
  plot(Fdom ~ Depth, data = aa.DOY.Exp.2020.long[aa.DOY.Exp.2020.long$AnoxicAge!=0,],
       las = 1,
       ylim = c(11,14),
       xlim = c(9,21),
       xlab = "Depth (m)",
       ylab = expression(F[365/480]~(QSE)),
       main = "Depth",
       #bg = AnoxicAge,
       pch = 21)
  abline(Depth.lm.Living.2020)
  text(x=11.4, y = 13.8, labels = bquote(R^2 == .(Depth.R2.Living.2020)))
  mtext("d", side = 3, at = 9)
  dev.off()
}


#Transform df to add DOY and scale variables
aa.DOY.Exp.long.scale = cbind(aa.DOY.Exp.long[,c(2,5)], scale(aa.DOY.Exp.long[,1]),aa.DOY.Exp.long[,3],scale(aa.DOY.Exp.long[,4]))
aa.DOY.Exp.long.scale[,4] = scale(as.numeric(aa.DOY.Exp.long.scale[,4]))
summary(lm(Fdom ~ as.numeric(DOY) + Depth + AnoxicAge, data = aa.DOY.Exp.long.scale))

aa.DOY.Exp.2020.long.scale = cbind(aa.DOY.Exp.2020.long[,c(2,5)], scale(aa.DOY.Exp.2020.long[,1]),aa.DOY.Exp.2020.long[,3],scale(aa.DOY.Exp.2020.long[,4]))
aa.DOY.Exp.2020.long.scale[,4] = scale(as.numeric(aa.DOY.Exp.2020.long.scale[,4]))
summary(lm(Fdom ~ as.numeric(DOY) + Depth + AnoxicAge, data = aa.DOY.Exp.2020.long.scale))

#Random forests
#Transform tible to df. It doesn't work with caret otherwise
aa.DOY.Exp.long.RF = aa.DOY.Exp.long.scale
aa.DOY.Exp.long.RF$DOY = as.numeric(aa.DOY.Exp.long.RF$DOY)
aa.DOY.Exp.long.RF = na.omit(aa.DOY.Exp.long.RF)
RF.df = as.data.frame(aa.DOY.Exp.long.RF)

#Metacontrol
control <- trainControl(method='repeatedcv',
                        number=10,
                        repeats=3,
                        search='grid')
#create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will #change number of entry variable at each split according to tunegrid.
tunegrid <- expand.grid(.mtry = (1:20))
#Random generate 15 mtry values with tuneLength = 15
set.seed(101)

#Split into train and test dataset
train = sample(1:nrow(RF.df), 0.7*nrow(RF.df))
test = which(!1:nrow(RF.df) %in% train)

#Compute random forest
rf_gridsearch <- caret::train(Fdom ~.,
                              data = RF.df[train,],
                              method = "rf",
                              metric = "RMSE",
                              tuneGrid = tunegrid,
                              trControl = control)
#Plot variable importance
plot(varImp(rf_gridsearch), main = "FDOM")

#Calculate precision of prediction using test dataset
predictions <- predict(rf_gridsearch, RF.df[test,])
RMSE <- sqrt(sum((predictions - RF.df[test,"Fdom"])^2)/length(predictions))
print(RMSE/mean(RF.df[test,"Fdom"], na.omit=T))

#2020
aa.DOY.Exp.2020.long.RF = aa.DOY.Exp.2020.long.scale
aa.DOY.Exp.2020.long.RF$DOY = as.numeric(aa.DOY.Exp.2020.long.RF$DOY)
RF.df.2020 = as.data.frame(aa.DOY.Exp.2020.long.RF)

#Split into train and test dataset
train.2020 = sample(1:nrow(RF.df.2020), 0.7*nrow(RF.df.2020))
test.2020 = which(!1:nrow(RF.df.2020) %in% train.2020)

#Compute random forest
rf_gridsearch_2020 <- caret::train(Fdom ~.,
                              data = RF.df.2020[train.2020,],
                              method = "rf",
                              metric = "RMSE",
                              tuneGrid = tunegrid,
                              trControl = control)
#Plot variable importance
plot(varImp(rf_gridsearch_2020), main = "FDOM")

#Calculate precision of prediction using test dataset
predictions.2020 <- predict(rf_gridsearch_2020, RF.df.2020[test.2020,])
RMSE.2020 <- sqrt(sum((predictions.2020 - RF.df.2020[test.2020,"Fdom"])^2)/length(predictions.2020))
print(RMSE.2020/mean(RF.df.2020[test.2020,"Fdom"], na.omit=T))


#Translation to minimum = 0
# aa.DOY.Exp.long.graph = aa.DOY.Exp.long
# aa.DOY.Exp.long.graph$Fdom = aa.DOY.Exp.long.graph$Fdom - min(aa.DOY.Exp.long.graph$Fdom, na.rm=T)
# 
# aa.DOY.Exp.2020.long.graph = aa.DOY.Exp.2020.long
# aa.DOY.Exp.2020.long.graph$Fdom = aa.DOY.Exp.2020.long.graph$Fdom - min(aa.DOY.Exp.2020.long.graph$Fdom, na.rm=T)


#Export results
png("./Data/Mendota/Output/FDOM.Mendota.Rel.png", units = "in", res=450, width = 9, height = 6.4)
par(mfrow=c(2,3))
par(mar=c(4,5,1,1)+.1)
plot(Fdom ~ as.numeric(DOY), data = aa.DOY.Exp.long,
     xlim = c(120,250),
     xlab = "Time (DOY)",
     ylab = expression(F[365/480]~(QSE)),
     las = 1, cex.axis = 1.4, cex.lab = 1.5)
mtext("a", side = 3, at = 110)
# plot(Fdom ~ Depth, data = aa.DOY.Exp.long,
#      xlim = c(10,24),
#      xlab = "Depth (m)",
#      ylab = "",
#      las = 1, cex.axis = 1.4, cex.lab = 1.5)
# mtext("b", side = 3, at = 9)
plot(Fdom ~ AnoxicAge, data = aa.DOY.Exp.long,
     xlab = "Anoxic age (d)",
     ylab = "",
     las = 1, cex.axis = 1.4, cex.lab = 1.5)
mtext("b", side = 3, at = -5)
plot(Fdom ~ AnoxicAge, data = aa.DOY.Exp.long,
     xlab = "Anoxic age (d)",
     ylab = "",
     las = 1, cex.axis = 1.4, cex.lab = 1.5)
mtext("c", side = 3, at = -5)


plot(Fdom ~ as.numeric(DOY), data = aa.DOY.Exp.2020.long,
     xlim = c(120,250),
     xlab = "Time (DOY)",
     ylab = expression(F[365/480]~(QSE)),
     las = 1, cex.axis = 1.4, cex.lab = 1.5)
mtext("d", side = 3, at = 130)

plot(Fdom ~ AnoxicAge, data = aa.DOY.Exp.2020.long,
     xlab = "Anoxic age (d)",
     ylab = "",
     las = 1, cex.axis = 1.4, cex.lab = 1.5)
mtext("e", side = 3, at = -5)
plot(Fdom ~ AnoxicAge, data = aa.DOY.Exp.2020.long,
     xlab = "Anoxic age (d)",
     ylab = "",
     las = 1, cex.axis = 1.4, cex.lab = 1.5)
mtext("f", side = 3, at = -5)
dev.off()

png("./Data/Mendota/Output/FDOM.RF.2018.png")
plot(varImp(rf_gridsearch), main = "",
     cex = 1.5)
dev.off()
png("./Data/Mendota/Output/FDOM.RF.2020.png")
plot(varImp(rf_gridsearch_2020), main = "",
     cex = 1.5)
dev.off()


summary(lm(Fdom ~ as.numeric(DOY), data=aa.DOY.Exp.long.graph))
summary(lm(Fdom ~ as.numeric(DOY), data=aa.DOY.Exp.2020.long.graph))

par(mfrow=c(1,2))
plot(Fdom ~ as.numeric(DOY), aa.DOY.Exp.long.graph, pch = 16)
points(Fdom ~ as.numeric(DOY), aa.DOY.Exp.2020.long.graph, col="red", pch = 16)
plot(Fdom ~ AnoxicAge, aa.DOY.Exp.long.graph, pch = 16)
points(Fdom ~ AnoxicAge, aa.DOY.Exp.2020.long.graph, col="red", pch = 16)

# png("./Output/FDOM.Mendota.Rel.2020.png", units = "in", res=450, width = 9, height = 3.2)
# par(mfrow=c(2,3))
# par(mar=c(4,5,1,1)+.1)
# 
# # plot(Fdom ~ Depth, data = aa.DOY.Exp.2020.long,
# #      xlim = c(10,20),
# #      xlab = "Depth (m)",
# #      ylab = "",
# #      las = 1, cex.axis = 1.4, cex.lab = 1.5)
# # mtext("b", side = 3, at = 9)
# 
# dev.off()
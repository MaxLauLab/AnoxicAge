library(RColorBrewer)
library(plot.matrix)
library(RColorBrewer)
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
names(InterpolatedMatrix)= List.lakes

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

pdf("Jz.pdf")
for(i in 1:length(InterpolatedMatrix)){
  par(mfrow = c(1,2))
  plot(InterpolatedMatrix[[i]][,1] ~ InterpolatedMatrix[[i]][,4],
       xlab = "DO end (mg/L)",
       ylab = "Depth",
       main = List.lakes[i],
       ylim = rev(range(InterpolatedMatrix[[i]][,1])))
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

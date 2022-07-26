


#################################################################################
###################### For other lakes, probably delete##########################
#################################################################################
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

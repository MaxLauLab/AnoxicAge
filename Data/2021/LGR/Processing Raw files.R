#################################################################################################
######################################Processing LGR raw data####################################
#################################################################################################

setwd("D:/Postdoc/Allemagne/Github/AnoxicAge/Data/2021/LGR")

list.files = list.files()
list.files = list.files[grep(pattern = ".txt",x = list.files)]

#Arendsee
#####
data = read.table(paste0("./",list.files[1]), skip = 1, sep=",", nrows = 2953, header=T)
colnames(data)[3:12] = c("CH4_ppm", "CH4_ppm_sd",
                         "CO2_ppm", "CO2_ppm_sd",
                         "H2O_ppm", "H2O_ppm_sd",
                         "CH4_dry_ppm", "CH4_dry_ppm_sd",
                         "CO2_dry_ppm", "CO2_dry_ppm_sd")

data$Time <- as.POSIXct(data$Time, format = "%Y/%m/%d %H:%M:%S")

# par(mfrow=c(1,1))
# plot(data$CO2_ppm ~ data$Time)
# 
# plot(data[data$Time > paste0("2021/07/27 ", "15:31:00") & data$Time < paste0("2021/07/27 ", "15:35:00"),"CO2_ppm"] ~ 
#        data[data$Time > paste0("2021/07/27 ", "15:31:00") & data$Time < paste0("2021/07/27 ", "15:35:00"),"Time"],
#      las = 1,
#      ylab = "CO2",
#      xlab = "Time")

Time.beg = c("15:20:30", "15:22:30", "15:24:30", "15:29:00", "15:31:15", "15:32:45", "15:34:45", "15:36:20", "15:37:55", "15:39:25", "15:40:50", "15:42:20", "15:43:56","15:45:45", "15:47:40", "15:49:10", "15:52:10", "15:53:40")
Time.end = c("15:21:15", "15:22:50", "15:24:55", "15:29:45", "15:32:15", "15:33:45", "15:35:45", "15:37:20", "15:38:55", "15:40:25", "15:41:50", "15:43:20", "15:45:00","15:46:45", "15:48:40", "15:50:10", "15:53:10", "15:54:40")

#Calcuating sd is possible but requires more fine tuning. Maybe next step if required
ar.CO2.med = vector(length=length(Time.beg))
ar.CH4.med = ar.CO2.med
# ar.CO2.sd <- ar.CH4.sd <- ar.CO2.med
for(i in 1:length(Time.beg)){
  ar.CO2.med[i] = median(data[data$Time > paste0("2021/07/27 ",Time.beg[i]) & data$Time < paste0("2021/07/27 ",Time.end[i]), "CO2_ppm"])
  # ar.CO2.sd[i] = sd(data[data$Time > paste0("2021/07/27 ",Time.beg[i]) & data$Time < paste0("2021/07/27 ",Time.end[i]), "CO2_ppm"])
  ar.CH4.med[i] = median(data[data$Time > paste0("2021/07/27 ",Time.beg[i]) & data$Time < paste0("2021/07/27 ",Time.end[i]), "CH4_ppm"])
  # ar.CH4.sd[i] = sd(data[data$Time > paste0("2021/07/27 ",Time.beg[i]) & data$Time < paste0("2021/07/27 ",Time.end[i]), "CH4_ppm"])
  }

ar.bags = c("B1", "M11", "B2", "M2", "B4", "B3", "M5", "B5", "M6", "B6", "M8", "B8", "M10", "B10", "M13", "B13", "M4", "B12")
Ar.mat = data.frame(Lake = "Ar", Bags = ar.bags, CO2 = ar.CO2.med, CH4 = ar.CH4.med)
#####


#Scharmutzel
#####
data = read.table(paste0("./",list.files[3]), skip = 1, sep=",", nrows = 2233, header=T)
colnames(data)[3:12] = c("CH4_ppm", "CH4_ppm_sd",
                         "CO2_ppm", "CO2_ppm_sd",
                         "H2O_ppm", "H2O_ppm_sd",
                         "CH4_dry_ppm", "CH4_dry_ppm_sd",
                         "CO2_dry_ppm", "CO2_dry_ppm_sd")

data$Time <- as.POSIXct(data$Time, format = "%Y/%m/%d %H:%M:%S")

par(mfrow=c(1,1))
plot(data$CO2_ppm ~ data$Time)

# plot(data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"CO2_ppm"] ~
#        data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"Time"],
#      las = 1,
#      ylab = "CO2",
#      xlab = "Time")

Time.beg = c("13:01:55", "13:04:02", "13:07:01", "13:08:37", "13:10:00", "13:11:20", "13:12:54", "13:14:35", "13:15:50", "13:17:00", "13:18:43", "13:20:09", "13:21:45","13:23:10", "13:24:49", "13:26:20")
Time.end = c("13:02:55", "13:05:02", "13:08:01", "13:09:37", "13:11:00", "13:12:20", "13:13:54", "13:15:35", "13:16:50", "13:18:00", "13:19:43", "13:21:09", "13:22:45","13:24:10", "13:25:49", "13:27:20")

sc.CO2.med = vector(length=length(Time.beg))
sc.CH4.med = sc.CO2.med
for(i in 1:length(Time.beg)){
  sc.CO2.med[i] = median(data[data$Time > paste0("2021/07/29 ",Time.beg[i]) & data$Time < paste0("2021/07/29 ",Time.end[i]), "CO2_ppm"])
  sc.CH4.med[i] = median(data[data$Time > paste0("2021/07/29 ",Time.beg[i]) & data$Time < paste0("2021/07/29 ",Time.end[i]), "CH4_ppm"])
}

sc.bags = c("M1", "B1", "M3", "B3", "M4", "B4", "M5", "B5", "M6", "B6", "M8", "B8", "M10", "B10", "M11", "B11")
sc.mat = data.frame(Lake = "Sc", Bags = sc.bags, CO2 = sc.CO2.med, CH4 = sc.CH4.med)
#####

#Grossglienicke
#####
data = read.table(paste0("./",list.files[4]), skip = 1, sep=",", nrows = 1725, header=T)
colnames(data)[3:12] = c("CH4_ppm", "CH4_ppm_sd",
                         "CO2_ppm", "CO2_ppm_sd",
                         "H2O_ppm", "H2O_ppm_sd",
                         "CH4_dry_ppm", "CH4_dry_ppm_sd",
                         "CO2_dry_ppm", "CO2_dry_ppm_sd")

data$Time <- as.POSIXct(data$Time, format = "%Y/%m/%d %H:%M:%S")

par(mfrow=c(1,1))
plot(data$CO2_ppm ~ data$Time)

# plot(data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"CO2_ppm"] ~
#        data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"Time"],
#      las = 1,
#      ylab = "CO2",
#      xlab = "Time")

Time.beg = c("12:51:51", "12:53:23", "12:54:53", "12:57:00", "12:58:35", "13:00:25", "13:01:44", "13:03:16", "13:04:35", "13:05:51", "13:07:18", "13:08:45")
Time.end = c("12:52:51", "12:54:23", "12:55:53", "12:58:00", "12:59:35", "13:01:25", "13:02:44", "13:04:16", "13:05:35", "13:06:51", "13:08:18", "13:09:45")

gg.CO2.med = vector(length=length(Time.beg))
gg.CH4.med = gg.CO2.med
for(i in 1:length(Time.beg)){
  gg.CO2.med[i] = median(data[data$Time > paste0("2021/07/30 ",Time.beg[i]) & data$Time < paste0("2021/07/30 ",Time.end[i]), "CO2_ppm"])
  gg.CH4.med[i] = median(data[data$Time > paste0("2021/07/30 ",Time.beg[i]) & data$Time < paste0("2021/07/30 ",Time.end[i]), "CH4_ppm"])
}

gg.bags = c("M1", "B1", "M3", "B3", "M4", "B4", "M5", "B5", "M6", "B6", "M8", "B8")
gg.mat = data.frame(Lake = "Gg", Bags = gg.bags, CO2 = gg.CO2.med, CH4 = gg.CH4.med)
#####

#Fuku
#####
data = read.table(paste0("./",list.files[5]), skip = 1, sep=",", header=T)
colnames(data)[3:12] = c("CH4_ppm", "CH4_ppm_sd",
                         "CO2_ppm", "CO2_ppm_sd",
                         "H2O_ppm", "H2O_ppm_sd",
                         "CH4_dry_ppm", "CH4_dry_ppm_sd",
                         "CO2_dry_ppm", "CO2_dry_ppm_sd")

data$Time <- as.POSIXct(data$Time, format = "%Y/%m/%d %H:%M:%S")

par(mfrow=c(1,1))
plot(data$CO2_ppm ~ data$Time)

# plot(data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"CO2_ppm"] ~
#        data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"Time"],
#      las = 1,
#      ylab = "CO2",
#      xlab = "Time")

Time.beg = c("15:44:11", "15:45:40", "15:47:16", "15:48:39", "15:50:05", "15:51:32", "15:53:08", "15:54:20", "15:55:35", "15:56:55", "15:58:30", "15:59:52", "16:01:15", "16:02:37")
Time.end = c("15:45:11", "15:46:40", "15:48:16", "15:49:39", "15:51:05", "15:52:32", "15:54:08", "15:55:20", "15:56:35", "15:57:55", "15:59:30", "16:00:52", "16:02:15", "16:03:37")

Fuku.CO2.med = vector(length=length(Time.beg))
Fuku.CH4.med = Fuku.CO2.med
for(i in 1:length(Time.beg)){
  Fuku.CO2.med[i] = median(data[data$Time > paste0("2021/08/02 ",Time.beg[i]) & data$Time < paste0("2021/08/02 ",Time.end[i]), "CO2_ppm"])
  Fuku.CH4.med[i] = median(data[data$Time > paste0("2021/08/02 ",Time.beg[i]) & data$Time < paste0("2021/08/02 ",Time.end[i]), "CH4_ppm"])
}

Fuku.bags = c("M13", "B13", "M3", "B3", "M5", "B5", "M10", "B10", "M6", "B6", "M12", "B12", "M4", "B4")
Fuku.mat = data.frame(Lake = c(rep("FukuNO", 6), rep("FukuSW", 8)), Bags = Fuku.bags, CO2 = Fuku.CO2.med, CH4 = Fuku.CH4.med)
#####

#Haussee
#####
data = read.table(paste0("./",list.files[6]), skip = 1, sep=",", header=T, nrow = 1900)
colnames(data)[3:12] = c("CH4_ppm", "CH4_ppm_sd",
                         "CO2_ppm", "CO2_ppm_sd",
                         "H2O_ppm", "H2O_ppm_sd",
                         "CH4_dry_ppm", "CH4_dry_ppm_sd",
                         "CO2_dry_ppm", "CO2_dry_ppm_sd")

data$Time <- as.POSIXct(data$Time, format = "%Y/%m/%d %H:%M:%S")

par(mfrow=c(1,1))
plot(data$CO2_ppm ~ data$Time)

# plot(data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"CO2_ppm"] ~
#        data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"Time"],
#      las = 1,
#      ylab = "CO2",
#      xlab = "Time")

Time.beg = c("15:01:42", "15:03:12", "15:04:34", "15:05:56", "15:07:16", "15:08:35", "15:10:09", "15:11:35", "15:13:14", "15:14:35", "15:16:10", "15:17:26", "15:19:05", "15:20:30", "15:21:52", "15:23:15")
Time.end = c("15:02:42", "15:04:12", "15:05:34", "15:06:56", "15:08:16", "15:09:35", "15:11:09", "15:12:35", "15:14:14", "15:15:35", "15:17:10", "15:18:26", "15:20:05", "15:21:30", "15:22:52", "15:24:15")

Hs.CO2.med = vector(length=length(Time.beg))
Hs.CH4.med = Hs.CO2.med
for(i in 1:length(Time.beg)){
  Hs.CO2.med[i] = median(data[data$Time > paste0("2021/08/03 ",Time.beg[i]) & data$Time < paste0("2021/08/03 ",Time.end[i]), "CO2_ppm"])
  Hs.CH4.med[i] = median(data[data$Time > paste0("2021/08/03 ",Time.beg[i]) & data$Time < paste0("2021/08/03 ",Time.end[i]), "CH4_ppm"])
}

Hs.bags = c("M1", "B1", "M3", "B3", "M4", "B4", "M5", "B5", "B6", "M6", "M10", "B10", "M11", "B11", "M13", "B13")
Hs.mat = data.frame(Lake = "Hs", Bags = Hs.bags, CO2 = Hs.CO2.med, CH4 = Hs.CH4.med)
#####

#Stechlin
#####
data1 = read.table(paste0("./",list.files[7]), skip = 1, sep=",", header=T)
data2 = read.table(paste0("./",list.files[8]), skip = 1, sep=",", header=T, nrows = 1221)
data = rbind(data1, data2)
colnames(data)[3:12] = c("CH4_ppm", "CH4_ppm_sd",
                         "CO2_ppm", "CO2_ppm_sd",
                         "H2O_ppm", "H2O_ppm_sd",
                         "CH4_dry_ppm", "CH4_dry_ppm_sd",
                         "CO2_dry_ppm", "CO2_dry_ppm_sd")

data$Time <- as.POSIXct(data$Time, format = "%Y/%m/%d %H:%M:%S")

par(mfrow=c(1,1))
plot(data$CO2_ppm ~ data$Time)

# plot(data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"CO2_ppm"] ~
#        data[data$Time > paste0("2021/07/29 ", "12:55:05") & data$Time < paste0("2021/07/29 ", "13:05:00"),"Time"],
#      las = 1,
#      ylab = "CO2",
#      xlab = "Time")

Time.beg = c("12:01:08", "12:02:26", "12:03:52", "12:05:32", "12:07:08", "12:19:16", "12:21:04", "12:22:38", "12:25:10", "12:26:45", "12:28:05", "12:29:56")
Time.end = c("12:02:08", "12:03:26", "12:04:52", "12:06:32", "12:07:44", "12:20:16", "12:22:04", "12:23:38", "12:26:10", "12:27:45", "12:29:05", "12:30:56")
St.CO2.med = vector(length=length(Time.beg))
St.CH4.med = St.CO2.med
for(i in 1:length(Time.beg)){
  St.CO2.med[i] = median(data[data$Time > paste0("2021/08/04 ",Time.beg[i]) & data$Time < paste0("2021/08/04 ",Time.end[i]), "CO2_ppm"])
  St.CH4.med[i] = median(data[data$Time > paste0("2021/08/04 ",Time.beg[i]) & data$Time < paste0("2021/08/04 ",Time.end[i]), "CH4_ppm"])
}

St.bags = c("M1", "B1", "M3", "B3", "B4", "M4", "M5", "B5", "B8", "B6", "M12", "B12")
St.mat = data.frame(Lake = "St", Bags = St.bags, CO2 = St.CO2.med, CH4 = St.CH4.med)
#####

#Combine all data together
Gases.mat = rbind(Ar.mat, sc.mat, gg.mat, Fuku.mat, Hs.mat, St.mat)

write.csv(x = Gases.mat, file = "./Gases.csv", row.names = FALSE, fileEncoding = "UTF-8")

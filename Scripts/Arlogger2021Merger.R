library(tidyr)
#Script to concatenate 2020 Ar logger files into a single file
setwd("D:/Postdoc/Allemagne/Github/AnoxicAge/Data/Raw")
Ar.30 <- read.csv("./30-0m#2872.dat", header=T, skip=6)
Ar.35 <- read.csv("./35-0m#2610.dat", header=T, skip=6)
Ar.40 <- read.csv("./40-0m#2607.dat", header=T, skip=6)
Ar.45 <- read.csv("./45-0m#2628.dat", header=T, skip=6)
Ar.47 <- read.csv("./47-0m#2878.dat", header=T, skip=6)

#Add Depth as a column
Ar.30 = cbind(Ar.30,Depth = 30)
Ar.35 = cbind(Ar.35,Depth = 35)
Ar.40 = cbind(Ar.40,Depth = 40)
Ar.45 = cbind(Ar.45,Depth = 45)
Ar.47 = cbind(Ar.47,Depth = 47)

Ar.2020 = rbind(Ar.30,Ar.35,Ar.40, Ar.45, Ar.47)

Ar.2020$Day.Month.Year = gsub("/","-",Ar.2020$Day.Month.Year)
Ar.2020$Day.Month.Year = strptime(Ar.2020$Day.Month.Year, format = "%d-%m-%y", tz = "CET")

Ar.2020$CET = paste(Ar.2020$Day.Month.Year, Ar.2020$Hour.Minute.Second)
colnames(Ar.2020)[c(4,5,6,7)] = c("T_C","DO_p100", "DO_mgL","depth")

#Add dummy columns to match full database
Ar.2020$UTC = "NA"
Ar.2020$lake = "ar"
Ar.2020$logger = "NA"
Ar.2020$class="NA"
Ar.2020$deployment_id="NA"
Ar.2020$deploy_start = "NA"
Ar.2020$deploy_end = "NA"
Ar.2020$serial = "NA"

Ar.2020.long = pivot_longer(Ar.2020, cols = 4:6,
                            names_to = "parameter", values_to = "value")

#Reorder dataframe
Ar.2020.long = Ar.2020.long[,c(6,5,15,7,8,14,9,10,11,12,13,4)]

write.csv(x = Ar.2020.long, file = "./Arendsee_Logger_2020_long.csv", row.names = F, fileEncoding = "UTF-8")

Full_data = read.csv("../../../full_database.csv")

Complete_data = rbind(Full_data,Ar.2020.long)

write.csv(x = Complete_data, "../../../full_database.csv", fileEncoding = "UTF-8")

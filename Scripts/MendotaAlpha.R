###############################################################################################
#######################################Lake Mendota alpha######################################
###############################################################################################


library(rLakeAnalyzer)
library(minpack.lm)
setwd("D:/Postdoc/Allemagne/Github/AnoxicAge/Data/Mendota")

approx.bathy(Zmax = 25.3, lkeArea = 39.4*10^6, Zmean = 12.8,method = "voldev")
Hypso = read.table("./Mendota.tsv", header=T)
colnames(Hypso) = c("Depth", "Area")



Areal.function = nlsLM(Area ~ Area[1] * ( 1 - Depth/max(Depth))^q,
                       data = Hypso, start = list(q=0.8),
                       lower = c(q=0.5),
                       upper = c(q=2))

newdepth = c(1:25)
alpha = summary(Areal.function)$coefficient[1]/(max(Hypso$Depth)-newdepth)

df.alpha = data.frame(Depth = newdepth, alpha = alpha)

write.csv(x = df.alpha, file = "Mendota.alpha.csv", row.names = F, fileEncoding = "UTF-8")

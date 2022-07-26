###############################################################################################
#######################################Lake Arendsee alpha######################################
###############################################################################################


library(rLakeAnalyzer)
library(minpack.lm)
setwd("D:/Postdoc/Allemagne/Github/AnoxicAge/Data")

Bats = read.csv("./Raw/bats.csv")
Bats.ar = filter(Bats, lake =="ar") %>% arrange(depth_1m)

Areal.function = nlsLM(area_1m ~ area_1m[1] * ( 1 - depth_1m/max(depth_1m))^q,
                       data = Bats.ar, start = list(q=0.8),
                       lower = c(q=0.05),
                       upper = c(q=2))

newdepth = c(1:47,47.5,48,48.5)
alpha = summary(Areal.function)$coefficient[1]/(max(Bats.ar$depth_1m)-newdepth)

df.alpha = data.frame(depth_1m = newdepth, alpha = alpha)

write.csv(x = df.alpha, file = "./Raw/Arendsee.alpha.csv", row.names = F, fileEncoding = "UTF-8")

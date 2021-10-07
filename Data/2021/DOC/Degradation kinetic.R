################################################################################
###################################DOC oxic degradation########################
##################################################################################

library(minpack.lm)
setwd("D:/Postdoc/Allemagne/Github/AnoxicAge/Data/2021/DOC")

data = read.csv("./DOC.oxic.csv")

#Temporary fix DOC value for Arendsee 46m to Arendsee 47.5m
#This lower value fits the whole profile of values
fix(data)

#Create vector with all lakes
all.lake = unique(data$Lake)
index.i = 1
output.mat = as.data.frame(matrix(nrow = length(all.lake)*2, ncol = 9))
colnames(output.mat) = c("Lake", "Depth", "BDOC", "k", "RDOC", "BDOC.err", "k.err", "RDOC.err", "R.sq")

pdf("./Oxic degradation curves.pdf")
par(mfrow=c(1,2))

for(i in 1:length(all.lake)){
  #Select 1 lake
  data.lake = data[data$Lake == all.lake[i],]

  #Find number of depths
  lake.depth = unique(data.lake$Depth)
    for(j in 1:length(lake.depth))
    {
      data.lake.depth = data.lake[data.lake$Depth == lake.depth[j],]
      DOC.min = min(data.lake.depth$DOC, na.rm=T)
      DOC.max = max(data.lake.depth$DOC, na.rm=T)
      DOC.delta = DOC.max - DOC.min
    
      G.model <- nlsLM(DOC ~ G1*exp(-k1*Timepoint) + G2,
                               data = data.lake.depth,
                              start = list(G1 = DOC.delta, k1 = 0.02, G2 = DOC.min),
                              lower = c(DOC.delta-3,0.0001, DOC.min-5),
                              upper = c(DOC.delta+6, .04, DOC.min+3))
    
      G.param = summary(G.model)$parameters
      G.R2 = 1- sum((data.lake.depth[,"DOC"] - predict(G.model))^2) / sum((data.lake.depth[,"DOC"]-mean(data.lake.depth[,"DOC"]))^2)
    
      #Store relevant info in output matrix
      output.mat[index.i, 1] = paste(all.lake[i])
      output.mat[index.i, 2] = lake.depth[j]
      output.mat[index.i, 3] = G.param[1,1]
      output.mat[index.i, 4] = G.param[2,1]
      output.mat[index.i, 5] = G.param[3,1]
      output.mat[index.i, 6] = G.param[1,2]
      output.mat[index.i, 7] = G.param[2,2]
      output.mat[index.i, 8] = G.param[3,2]
      output.mat[index.i, 9] = round(G.R2,2)
    
      plot(data.lake.depth[,"DOC"] ~ data.lake.depth[,"Timepoint"], 
           xlim = c(0,550), 
           ylim = c(DOC.min-0.5, DOC.max+0.5),
           main = paste(all.lake[i], lake.depth[j]),
          xlab = "Time (h)", 
         ylab = "DOC (mg/L)",
         # xaxt = "n",
         pch = 18,
         cex = .8,
         las = 1,
         cex.axis = 1.2,
         cex.lab = 1.2)
      curve(G.param[1] * exp(-G.param[2] * x) + G.param[3], add=T) 
      text(x = 400, y = DOC.max+0.25,  labels = paste("R² =", round(G.R2,2)))
      index.i = index.i+1
  }

  
}
dev.off()

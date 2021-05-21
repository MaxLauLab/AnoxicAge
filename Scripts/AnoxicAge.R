#This function interpolates DO profiles at 0.05m increment
#The output is a vector containing 1 and 0 for whether or not the depth is anoxic/hypoxic

#Depth is a profile of measured depth
#DO is a profile of measured oxygen. A few NA are accepted as data will be interpolated
#DO.threshold is the cutoff. Default is set at 2 for hypoxia
#increment is the increment for the interpolation. Default is 0.05m
Hypoxic.binary <- function(Depth, DO, DO.threshold = 2, increment = 0.05){
  #Create a vector o depth for the interpolation
  NewDepth = seq(min(Depth), max(Depth), increment)
  #Linearly interpolate DO data. See North and Livingstone 2013 L&O:Methods
  Data = approx(x=Depth, y=DO, xout=NewDepth)
  Data = as.data.frame(matrix(c(Data$x,Data$y),ncol = 2))
  colnames(Data) = c("Depth", "DO")
  
  #Determine if DO data is below the threshold
  Data$output = ifelse(Data$DO <= Threshold, 1, 0)
  
  #Export the values
  return(Data)
}


#Calculate
HypoxicAge <- function(Data.path = "/Data/Profiles/",DO.threshold = 2, increment = 0.05){
  Directory = getwd()
  File.List = list.files(path=paste0(Directory,Data.path))
  
  data = read.csv(paste0(Directory,Data.path,File.List[1]))
  Depth = data$depth
  DO = data$DO
  Hypox.temp = Hypoxic.binary(Depth = Depth, DO = DO, DO.threshold = DO.threshold, increment = increment)
  output = matrix(nrow = length(Hypox.temp), ncol = length(File.List)+1)
  colnames(output) = File.List
  output[,1] = Hypox.temp$Depth
  output[,2] = Hypox.temp$output
  
  for(i in 2:length(File.List)){
    data = read.csv(paste0(Directory,Data.path,File.List[i]))
    Depth = data$depth
    DO = data$DO
    
    Hypox.temp = Hypoxic.binary(Depth = Depth, DO = DO, DO.threshold = DO.threshold, increment = increment)
    output[,i+1] = Hypox.temp$output
  }

  #Loop to add anoxic values together. If a depth is no longer anoxic, value goes back to 0
  for(j in 3:ncol(output)){
    temp = output[,j-1]+output[,j]
    temp = ifelse(temp > output[,j-1], temp, 0)
    output[,j] = temp
  }
  return(output)
}

#Ploting the results
#m1 is the output of HypoxicAge
mypalette = c("white", brewer.pal(9, "YlOrBr"))
if(max(m1) > length(mypalette)) mypalette = c(mypalette, rep("#662506",max(m1)-length(mypalette)) )
yThick1 = min(m2[,1])
yThick3 = max(m2[,1])*10
yThick2 = (yThick1 + yThick3)/2

xThick1 = 1
xThick3 = dim(m1)[2]-1
xThick2 = (xThick1 + xThick3)/2

plot(m2[,-1],
     col = mypalette,
     border= NA,
     key = NULL,
     axis.col = NULL,
     axis.row = NULL,
     ylab = "Depth (m)",
     xlab = "Time (day of year)")
axis(side = 2, tick = T,at = c(1,yThick2,yThick3), labels = c(yThick3, yThick2, yThick1)/10)
axis(side = 1, tick = T,at = c(1,xThick2,xThick3), labels = c(xThick1, xThick2, xThick3))




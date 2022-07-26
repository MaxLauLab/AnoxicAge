##################################################################################
##################################Compiling CDOM##################################
##################################################################################

setwd("D:/Postdoc/Allemagne/Github/AnoxicAge/Data/2021/FDOM")
meta = readxl::read_xlsx("./Aqualog_hypoo2020_with_3.xlsx", skip = 3)

run.no = c(19,20,22,23,24)

meta = meta[meta$aqualog_run==19 | meta$aqualog_run==20 | meta$aqualog_run==22 | meta$aqualog_run==23 | meta$aqualog_run==24,]
meta = meta[-c(1:6),]

counter = 1
CDOM.mat = matrix(nrow = nrow(meta), ncol = 73)
colnames(CDOM.mat) = seq(600,240,-5)
rownames(CDOM.mat) = meta$site

for(i in 1:length(run.no)){
  list.files.temp = list.files(paste0("./run", run.no[i]))
  list.files.temp = list.files.temp[grep("ABS", list.files.temp)]
  for(j in 1:length(list.files.temp)){
    data = read.table(paste0("./run", run.no[i],"/",list.files.temp[j]))
    CDOM.mat[counter,] = data[,2]
    counter=counter+1
    }
}

write.csv(x = CDOM.mat, file = "./CDOM.all.csv", row.names = T, fileEncoding = "UTF-8")

interpolation_fct <- function(Input_matrix){
  
  diy_in_colums <- as.numeric(colnames(Input_matrix)) #actual sampling days

  total_range<-seq(min(diy_in_colums),max(diy_in_colums),1)
  matrix_result <-matrix(ncol=length(total_range),nrow=nrow(Input_matrix))
  colnames(matrix_result) = total_range
  
  for(i in 1:(length(total_range))){
    if(length(grep(colnames(matrix_result)[i], x = colnames(Input_matrix)))) matrix_result[,i] = Input_matrix[,grep(colnames(matrix_result)[i], x = colnames(Input_matrix))[1]]
  }
  
  counter = 0
  counter_up = 1
  for(i in 1:nrow(matrix_result)){
    for(j in 1:ncol(matrix_result)){
      if(is.na(matrix_result[i,j])) counter = j
      if(counter == 1){
        matrix_result[i,j] = matrix_result[i-1,j]
        counter = 0
      } 
      if(counter){ while(is.na(matrix_result[i,j+counter_up])) {
        counter_up = counter_up+1
        if((counter + counter_up) == ncol(matrix_result)) break}
        
      if(counter + counter_up == (ncol(matrix_result))){
        matrix_result[i,c(counter:(counter+counter_up))] = matrix_result[i,counter-1]
        counter_up=1
        counter = 0
      }
      }
      if(counter){
      inter_step = (matrix_result[,counter + counter_up] - matrix_result[,counter-1])/(counter_up+1)
      for(k in 0:(counter_up-1)){
        matrix_result[,counter + k] = matrix_result[,counter-1] + inter_step*(k+1)
      }
      counter_up=1
      counter = 0 }
    }
  }
  
  
  return(matrix_result)
  
  
}
# 
# 
# 
# 
# 
# 
# 
# 
# 
# for(i in 2:(length(diy_in_colums))){
#   if((diy_in_colums[i-1])==total_range[i-1]){
#     matrix_result[,diy_in_colums[i]]<-matrix_example[,i]
#     matrix_result[,diy_in_colums[i]-1]<-matrix_example[,i-1]
#     #matrix_result[,i-1]<-matrix_example[,i-1]
#   } else {
#     for(j in 1:nrow(matrix_example)){
#       somanymissing<-diy_in_colums[i]-diy_in_colums[i-1]+1
#       matrix_result[j,diy_in_colums[i]:diy_in_colums[i-1]] <- approx(x=c(diy_in_colums[i-1],diy_in_colums[i]),
#                                                                      y=c(matrix_example[j,i-1],matrix_example[j,i]),n=somanymissing)$y
#     }
#   }
# }
# matrix_result
# matrix_example
# diy_in_colums

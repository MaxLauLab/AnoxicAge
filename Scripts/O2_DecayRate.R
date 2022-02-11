#Create decay rate function
O2.decay <- function(b, k, Model, alpha, JzMax){
  if(Model == "Livingstone")  decay.rate = b + alpha * k
  if(Model == "Log") decay.rate = b * log10(k * alpha)
  if(Model == "Exp") decay.rate = JzMax - (JzMax - b) * exp(-k*alpha)
  return(decay.rate)
}
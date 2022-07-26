TheilSen <- function(x, y) {
  
  n <- length(x)
  medind1 <- floor(((n * (n - 1)) / 2 + 2) / 2)
  medind2 <- floor((n + 2) / 2)
  temp <-  t(sapply(1:n, function(z)  apply(cbind(x, y), 1 ,
                                            function(k) (k[2] - y[z]) /
                                              (k[1] - x[z]))))
  TS.sorted <- sort(as.vector(temp[lower.tri(temp)]))
  TSslope <- TS.sorted[medind1]
  TS.sorted = TS.sorted[TS.sorted!= -Inf]
  TS.sorted = TS.sorted[TS.sorted!= Inf]
  TSMAD = median(abs(TS.sorted - TSslope))
  TSintercept <- sort(y - x * TSslope)[medind2]
  return(list(intercept = TSintercept, slope = TSslope, MAD = TSMAD)) #MAD = median absolute deviation
}
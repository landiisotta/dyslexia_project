## measures
## TRUE on cols, PREDICTED on rows
## C[k,l] = number of elements of true class l predicted to be of class k

## Generalized MCC
MCC <- function(A){
  n <- dim(A)[1]
  mcc <-0
  N <- sum((A))
  T <- sum(diag(A))
  num <- 0
  den1 <- 0
  den2 <- 0
  
  for(k in seq(n)){
    for(l in seq(n)){
      for(m in seq(n)){
        num <- num + A[k,k]*A[l,m]-A[k,l]*A[m,k]
      }
    }
  }	
  
  for(k in seq(n)){
    t11 <- 0
    for(l in seq(n)){
      t11 <- t11 +A[k,l]
    }
    t12 <- 0
    for(l1 in seq(n)){
      for(k1 in seq(n)){
        if(k1!=k){
          t12 <- t12 + A[k1,l1]
        }
      }
    }
    den1 <- den1 +t11*t12
  }	
  
  for(k in seq(n)){
    t21 <- 0
    for(l in seq(n)){
      t21 <- t21 +A[l,k]
    }
    t22 <- 0
    for(l1 in seq(n)){
      for(k1 in seq(n)){
        if(k1!=k){
          t22 <- t22 + A[l1,k1]
        }
      }
    }
    den2 <- den2 +t21*t22
  }	
  den <- sqrt(den1*den2)
  if(den==0){
    den=1
  }
  mcc <- num/den
  
  
  return(mcc)
}

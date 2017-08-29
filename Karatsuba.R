K_in <- function(n){
  N <- as.character(n)
  N <- rev(strsplit(N,"")[[1]])
  if(any(N=="e")){
    stop("\nInput passed in scientific notation: ",n,
         "\nType input as a character string to avoid this.")}
  N <- as.integer(N)
  return(N)
}

K_out <- function(n){
  n <- rev(n)
  n <- as.character(n)
  n <- paste(n,collapse = "")
  return(n)
}


"%K%" <- function(A,B){
  
  # Pad shorter vector with leading zeros
  digits <- max(length(A),length(B))
  A <- pad_to(A,digits)
  B <- pad_to(B,digits)
  
  # Base case: product of two single-digit numbers
  if(digits==1){
    Y <- c(0,0)
    Y[1] <- A * B
    
    # Carry from units to tens
    while(Y[1] > 9){
      Y[2] <- Y[2]+1
      Y[1] <- Y[1]-10
    }
    return(unpad(Y))
  }

  # Split vectors at midpoint "m" (i.e. log10 of largest number).
  m <- digits %/% 2
  # Little ends
  A0 <- A[1:m]
  B0 <- B[1:m]
  # Big ends
  A1 <- A[-(1:m)]
  B1 <- B[-(1:m)]
  
 
  # Big end and little end products
  z2 <- A1 %K% B1 
  z0 <- A0 %K% B0 
  
  #Final Karatsuba operation
  z1 <- (A0 %+% A1) %K% (B0 %+% B1) %-% z2 %-% z0

  # Shift z2 and z1 by "00" * m
  z2 <- K_shift(z2,2*m)
  z1 <- K_shift(z1,m)
  
 
  
 

  #z1 <- pad_to(z1,digits*2)
  #z2 <- pad_to(z2,digits*2)
  #z0 <- pad_to(z0,digits*2)
  Y <- z2 %+% z1 %+% z0
  
 # Y <-K_add(K_add(z2,z1),z0)


  return(unpad(Y))
}


"%+%"<- function(A,B){
    digits <- max(length(A),length(B))
    A <- pad_to(A,digits)
    B <- pad_to(B,digits)
  
  Y <- A+B
  # Carry tens forward
  while(any(Y>9)){
    carry <- rep(0,length(Y))
    carry[Y>9] <- 1
    Y[Y>9] <- Y[Y>9] - 10
    Y <- c(Y,0)+c(0, carry)
  }
  # Carry negatives backward
  while(any(Y<0)){
    carry <- rep(0,length(Y))
    carry[Y<0] <- 1
    Y[Y<0] <- Y[Y<0] + 10
    Y <- c(Y,0)-c(0, carry)
  }
  
  return(Y)
}

"%-%" <- function(A,B){A %+% -B}

  


"%*%" <- function(a,b){
  a <- K_in(a)
  b <- K_in(b)
  digits <- max(length(a),length(b))
  a <- pad_to(a,digits)
  b <- pad_to(b,digits)
  #y <- K_mult(a,b)
  y <- a %K% b
  y <- K_out(y)
  return(y)
  
}
"%+%" <- function(a,b){
  a <- K_in(a)
  b <- K_in(b)
  
  y <- K_add(a,b)
  y <- K_out(y)
  return(y)
  
}
"%-%" <- function(a,b){
  a <- K_in(a)
  b <- K_in(b)
  
  y <- K_sub(a,b)
  y <- K_out(y)
  return(y)
  
}

pad_to <- function(M,max){
  pad <- max-length(M)
  if(pad<0){pad <- 0}
  M <- c(M,rep(0,pad))
  return(M)
}

unpad <- function(M){
  while(M[length(M)]==0 & length(M)>1){
    M <- M[-length(M)]
  }
  return(M)
}


K_shift <- function(N,magnitude){
  zeros <- rep(0,magnitude)
  N <- c(zeros,N)
}

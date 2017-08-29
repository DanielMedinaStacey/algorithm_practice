# Karatsuba algorithm in R, using big endian decimal vectors of integers for
# representing numbers internally. It seemed like a good idea at the time,
# but required reimplementing addition as well as converting input and output.
# There are probably many smarter ways to do this.
# DMS 2017

### %X% ###
## Wrapper for Karatsuba multiplication. Takes two whole numbers or character
## string representation and returns their product as a character string.
## Strings are used to avoid limitations in numeric representation.

"%X%" <- function(a,b){
  A <- K_in(a)
  B <- K_in(b)
  digits <- max(length(A),length(B))
  A <- pad_to(A,digits)
  B <- pad_to(B,digits)
  Y <- A %K% B
  y <- K_out(Y)
  return(y)
}

### %K% ###
## Karatsuba product of two big endian decimal vectors
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
    return(Y)
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
  Z2 <- A1 %K% B1 
  Z0 <- A0 %K% B0 
  
  #Final Karatsuba operation
  Z1 <- (A0 %+% A1) %K% (B0 %+% B1) %-% Z2 %-% Z0

  # Shift z2 and z1 by "00" * m
  Z2 <- K_shift(Z2,2*m)
  Z1 <- K_shift(Z1,m)
  
  #Result
  Y <- Z2 %+% Z1 %+% Z0
  return(Y)
}
### %+% ###
## Big endian decimal vector addition
"%+%"<- function(A,B){
  digits <- max(length(A),length(B))
  A <- pad_to(A,digits)
  B <- pad_to(B,digits)
  
  # Add each element
  Y <- A+B
  # Shift magnitudes (e.g. "12 , 3" ->  "2 , 4")
  while(any(Y>9)){
    carry <- rep(0,length(Y))
    carry[Y>9] <- 1
    Y[Y>9] <- Y[Y>9] - 10
    Y <- c(Y,0)+c(0, carry)
  }
  # Shift negatives (e.g. "-7, 3" -> "3, 2" )
  while(any(Y<0)){
    carry <- rep(0,length(Y))
    carry[Y<0] <- 1
    Y[Y<0] <- Y[Y<0] + 10
    Y <- c(Y,0)-c(0, carry)
  }
  
  return(Y)
}
### %-% ###
## Wrapper for big endian decimal vector subtraction
"%-%" <- function(A,B){A %+% -B}


### K_shift() ###
## Shift big endian vector "N" by "m" to the right - i.e. multiply by 10^m
K_shift <- function(N,m){
  zeros <- rep(0,m)
  N <- c(zeros,N)
}
### pad_to() ###
## Pad vector up to "max" digits with trailing zeros
pad_to <- function(M,max){
  pad <- max-length(M)
  if(pad<0){pad <- 0}
  M <- c(M,rep(0,pad))
  return(M)
}

### K_in(n) ###
## Convert input numbers to big endian vectors where each digit is an element
K_in <- function(n){
  
  # Coerce to character, split and reverse
  N <- rev(strsplit(as.character(n),"")[[1]])
  
  # Check for scientific notation in input
  if(any(N=="e")){
    stop("\nInput passed in scientific notation: ",n,
         "\nType input as a character string to avoid this.")}
  
  # Coerce to integer vector
  N <- as.integer(N)
  return(N)
}
### K_out(N) ###
## Convert big endian digit vector to character representation of number
K_out <- function(N){
  
  # Remove leading zeros (trailing zeros in big endian vector )
  while(N[length(N)]==0 & length(N)>1){
    N <- N[-length(N)]
  }
  # Reverse vector, convert to character, and collapse into string
  n <- paste(as.character(rev(N)),collapse = "")
  
  return(n)
}


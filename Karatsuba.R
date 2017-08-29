

K_in <- function(n){
  N <- as.character(n)
  N <- rev(strsplit(N,"")[[1]])
  if(any(N=="e")){
    stop("\nInput passed in scientific notation: ",n,
         "\nType input as a character string to avoid this.")
  }
  N <- as.integer(N)

  return(N)
  
}
K_out <- function(n){
  n <- rev(n)
  n <- as.character(n)
  n <- paste(n,collapse = "")
  
  return(n)
}


K_mult <- function(A,B){
  
  digits <- max(length(A),length(B))
  A <- pad_to(A,digits)
  B <- pad_to(B,digits)
  
  
  Y <- rep(0, digits*2)
  if(digits==1){
    Y[1] <- A * B
    while(Y[1] > 9){
      Y[2] <- Y[2]+1
      Y[1] <- Y[1]-10
    }
    return(unpad(Y))
  }

  
  splitter <- digits %/% 2
  A_0 <- A[1:splitter]
  A_1 <- A[-(1:splitter)]
  B_0 <- B[1:splitter]
  B_1 <- B[-(1:splitter)]
  
 

  z2 <- K_mult(A_1,B_1)
  z0 <- K_mult(A_0,B_0)
  zA <- K_add(A_0,A_1)
  zB <- K_add(B_0,B_1)
  
  zAxB <- K_mult(zA,zB)
  
  zSub <- K_add(z2,z0)
  
  
  z1 <- K_sub(zAxB,zSub)

  
  z2 <- c(rep(c(0,0),splitter),z2)
  z0 <- c(z0)
  z1 <- c(rep(c(0),splitter),z1)
  
 

  z1 <- pad_to(z1,digits*2)
  z2 <- pad_to(z2,digits*2)
  z0 <- pad_to(z0,digits*2)
  
  
  Y <-K_add(K_add(z2,z1),z0)


  return(unpad(Y))
}


K_add<- function(A,B){
    digits <- max(length(A),length(B))
    A <- pad_to(A,digits)
    B <- pad_to(B,digits)
  
  Y <- A+B
  while(any(Y>9)){
    carry <- rep(0,length(Y))
    carry[Y>9] <- 1
    Y[Y>9] <- Y[Y>9] - 10
    Y <- c(Y,0)+c(0, carry)
  }
  while(any(Y<0)){
    carry <- rep(0,length(Y))
    carry[Y<0] <- 1
    Y[Y<0] <- Y[Y<0] + 10
    Y <- c(Y,0)-c(0, carry)
  }
  
  return(Y)
}


K_sub <- function(A,B){
    digits <- max(length(A),length(B))
    A <- pad_to(A,digits)
    B <- pad_to(B,digits)
  
  polarity <- 1
  Y <- rep(0,digits)
  for (i in rev(seq_along(A))){
    if(A[i]==B[i]){
      next
    }
    if(A[i]>B[i]){
      Y <- A-B
      break
    }
    if(A[i]<B[i]){
      Y <- B-A
      polarity <- -1
      warning("AAAAAA")
      break
    }
    
  }
  
  while(any(Y<0)){
    carry <- rep(0,length(Y))
    carry[Y<0] <- 1
    Y[Y<0] <- Y[Y<0] + 10
    Y <- c(Y,0)-c(0, carry)
  }
  Y <- polarity*Y
  
  return(Y)
}

"%*%" <- function(a,b){
  a <- K_in(a)
  b <- K_in(b)
  digits <- max(length(a),length(b))
  a <- pad_to(a,digits)
  b <- pad_to(b,digits)
  y <- K_mult(a,b)
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

DOOT <- function(name,thing){
  cat(paste(name,K_out(thing),"\n"))
}




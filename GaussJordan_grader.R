source("GaussJordan.R")

T1 = matrix(c(10,10,20,20,30,10,30,50,20,40,70,30), nrow = 3)

same.matrix <- function(M1,M2) {
  if (nrow(M1) != nrow(M2) | ncol(M1) != ncol(M2))
    return(FALSE)
  return(all(abs(M1 - M2) <= 0.0001))
}

GaussJordan.karro <- function(M, I=1:nrow(M)) {
  n = nrow(M)
  for (i in 1:n) {
    for (j in 1:n) 
      if (i != j) 
        M[j,] = M[j,]*M[i,I[i]] - M[i,]*M[j,I[i]]
  }
    
  for (i in 1:n) 
    M[i,] = M[i,]/M[i,I[i]]
  
  return(M)
}


readMatrix.karro <- function(file.name) {
  L = scan(file.name)
  nrow = L[1]
  return(suppressWarnings(matrix(L[-1], nrow=nrow, byrow=TRUE)))
}

GJtest <- function(file, I) {
  M = readMatrix.karro(file)
  G1 = GaussJordan(M, I)
  G2 = GaussJordan.karro(M,I)
  return(same.matrix(G1,G2))
}

test1 <- function() {
  # Test that readMatrix works (either with the num-rows number, or without)
  s = suppressWarnings(same.matrix(readMatrix("test1a.txt"), readMatrix.karro("test1a.txt")) | same.matrix(readMatrix("test1b.txt"), readMatrix.karro("test1a.txt")))
  if (!is.na(s) & s == TRUE)
    return(1)
  return(0)
}    

test2 <- function() {
  # Basic test of Gauss-Jordan
  s = GJtest("test1a.txt", c(1,2,3))
  if (!is.na(s) & s == TRUE)
    return(1)
  return(0)
}

test3 <- function() {
  # Basic test of Gauss-Jordan with variant I
  s = GJtest("test1a.txt", c(1,3,4)) & GJtest("test1a.txt", c(2,3,4))
  if (!is.na(s) & s == TRUE)
    return(1)
  return(0)
}
  

test4 <- function(n=100, seed = 39483) {
  s = GJtest("test2.txt", 1:5)
  if (!is.na(s) & s == TRUE)
    return(1)
  return(0)
}


calc.score <- function() {
  return( ceiling(10*test1() + 5*test2() + 2.5*test3() + 2.5*test4()) )
}


readMatrix <- function(file.name) {
  L = scan(file.name)
  nrow = L[1]
  return(matrix(L[-1], nrow=nrow, byrow=TRUE))
}

GaussJordan <- function(M, I=1:nrow(M)) {
  n = nrow(M)
  for (i in 1:n)        
    for (j in 1:n) 
      if (i != j) 
        M[j,] = M[j,]*M[i,I[i]] - M[i,]*M[j,I[i]]

        for (i in 1:n) 
          M[i,] = M[i,]/M[i,I[i]]

  return(M)
}


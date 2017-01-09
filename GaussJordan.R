# Problem 1: function that takes a file in matrix format and returns an R Matrix object

readMatrix <- function(file.name) {
  return(matrix(scan(file.name), nrow=(length(scan(file.name, flush=TRUE))), byrow=TRUE))
}

# Problem 2: GuassJordan Implementation
GaussJordan <- function(M, I=1:nrow(M)) {
 I <- sort(I) #Sorted Vector I of length n containing a subset of numbers for 1 to m.
 columnumn <- 0
 
 #Bringing to the reduced form
 for (j in M[1:nrow(M)-1, 1]) {
  column <- column + 1
  row <- column - 1
  M[column, column:ncolumn(M)] <- M[column, column:ncolumn(M)] / M[column, I[column]]
  
  for (i in M[column,column:nrow(M)-column]){
   row <- row + 1;
   M[row+1,1:ncolumn(M)] <- M[column,1:ncolumn(M)] - (M[row+1,1:ncolumn(M)] / M[row+1,I[column]])
  }
  
 }
 
 column = column + 1;
 
 for (j in M[1:nrow(M)-1,1]) {
  row <- column - 1
 
  for (i in M[column,1:column-1]) {
    M[column,1:ncolumn(M)] <- M[column, 1:ncolumn(M)] / M[column, I[column]]
    M[row,1:ncolumn(M)] <- M[row, 1:ncolumn(M)] - (M[column, 1:ncolumn(M)] * M[row, I[column]])
    row = row -1; 
  }
  
  column <- column - 1
 }
 
 return(M) 
}

# Comment: Your code is not running.  You have a number of variables and functions that do not appear to be 
# defined, thus causing errors.
# SCORE: 10/30
readMatrix <- function(file.name) {
  return(matrix(scan(file.name),nrow=(length(scan(file.name,flush=TRUE))),byrow=TRUE))
}

GaussJordan <- function(M, I=1:nrow(M)) {
  I <- sort(I)
  column <-0
  for (j in M[1:nrow(M)-1,1]){
    column<- column+ 1
    row <- column - 1
    if(M[column,I[column]] != 0){
    M[column,column:ncol(M)] <- M[column,column:ncol(M)] / M[column,I[column]]
    }
    for (i in M[column,column:nrow(M)-column]){
      row <- row + 1;
      if(M[row+1,I[column]] != 0){
      M[row+1,1:ncol(M)] <- M[column,1:ncol(M)] - (M[row+1,1:ncol(M)] / M[row+1,I[column]])
      }
  }
  }
  column = column + 1;
  for (j in M[1:nrow(M)-1,1]){
    row <- column - 1
    for (i in M[column,1:column-1]){
      if(M[column,I[column]] != 0 && M[row,I[column]] != 0){
      M[column,1:ncol(M)] <- M[column,1:ncol(M)] / M[column,I[column]]
      M[row,1:ncol(M)] <- M[row,1:ncol(M)] - (M[column,1:ncol(M)] * M[row,I[column]])
      }
      row = row -1; 
    }
    column <- column - 1
  }
  return(M)
}

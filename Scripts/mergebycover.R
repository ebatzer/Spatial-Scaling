
# Writing function that merges columns by highest cover
merge_by_cover <- function(com.matrix1,
                         com.matrix2,
                         com.id1,
                         com.id2,
                         id.order){
  
  # Adding simple numeric vector to id datasets to make reordering easier
  com.id1$index <- c(1:nrow(com.id1))
  com.id2$index <- c(1:nrow(com.id2))
  
  # Converting to tibbles
  com.id1 <- tbl_df(com.id1); com.id2 <- tbl_df(com.id2)
  
  # Checking that matrices are the same row number
  expect_true(nrow(com.matrix1) == nrow(com.matrix2))
  expect_true(length(id.order) < min(c(ncol(com.id1),ncol(com.id2))))
  
  # Arrange id datasets by user-specified variables
  com.id1 <- arrange_at(com.id1, (vars(id.order)))
  com.id2 <- arrange_at(com.id2, (vars(id.order)))
  
  # Arrange community datasets by index variables
  com.matrix1 <- com.matrix1[com.id1$index,]
  com.matrix2 <- com.matrix2[com.id2$index,]
  
  # Determining shared columns
  all.cols <- unique(c(names(com.matrix1), names(com.matrix2)))
  
  # For each species within the shared columns
  for(colval in all.cols){
    
    # If the column exists in matrix 1, but not in matrix 2, print column name
    if(colval %in% names(com.matrix1) & !(colval %in% names(com.matrix2))){
        print(paste("Column", colval, "only found in matrix 1"), sep = "")
      
    }else if(!(colval %in% names(com.matrix1)) & colval %in% names(com.matrix2)){
     
     # If the column exists in matrix 2, but not in matrix 1, bind and print column name
        com.matrix1 <- cbind(com.matrix1, com.matrix2[,names(com.matrix2) == colval])
        names(com.matrix1)[length(names(com.matrix1))] = colval
        print(paste("Column", colval, "only found in matrix 2"), sep = "")
          
    }else{
      
      # If the column exists in both matrices
      # Find what values matrix 2 > matrix 1, and overwrite those values
      overlap.index <- com.matrix2[,names(com.matrix2) == colval] > com.matrix1[,names(com.matrix1) == colval]
      com.matrix1[overlap.index,names(com.matrix1) == colval] <- com.matrix2[overlap.index, names(com.matrix2) == colval]
      
      # Print the column name and the number of times it was changed
      print(paste("Column", colval, "found in both matrices"), sep = "")
      print(paste("..replacing a total of", sum(overlap.index), "values"), sep = "")
        
    }
  }
  
  return(data.frame(com.id1, com.matrix1))
}
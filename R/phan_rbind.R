# A new R bind function that will bind together a list of dataframes or matrices

rbind.phan <- function(mat_list, coln=NULL) {
  if (length(unique(lapply(mat_list, colnames))) != 1) {
    stop("The objects passed do not have the same number of columns")
  }
  
  if (!is.null(coln)) {
    if (length(coln) == length(mat_list[[1]])) {
      df <- do.call(rbind, mat_list)
      colnames(df) <- coln
      return(df)
    }
    else {
      stop("Column name vector passed is not the same size as the number of columns in the objects passed")
    }
  }
  
  else{
    coln <- colnames(mat_list[[1]])
    df <- do.call(rbind, mat_list)
    colnames(df) <- coln
    return(df)
  }
}

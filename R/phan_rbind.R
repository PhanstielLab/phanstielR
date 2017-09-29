#' Quick & Easy Rbind
#' 
#' A new R bind function that will bind together a list of dataframes or matrices
#'
#' @param mat_list A list of matrices or data.frames that you would lke to rbind
#' @param coln A vector of names that you will be used to name the columns of the resulting object
#' @export
#' @export
#' 
rbind.phan <- function(mat_list, coln=NULL) {
  #checks if the columns are the same length
  if (length(unique(lapply(lapply(mat_list, colnames),function(x) {length(x)}))) != 1) {
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
    if (length(unique(lapply(mat_list, colnames))) != 1) {
      print("The list's columns do not have the same name. Rbinding with colnames from 1st object")
      coln <- colnames(mat_list[[1]])
      mat_list <- lapply(mat_list,function(x) { colnames(x) <- coln; return(x) })
    }
    df <- do.call(rbind, mat_list)
    colnames(df) <- coln
    return(df)
  }
}


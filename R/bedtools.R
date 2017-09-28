#' performs bedtools intersections
#'
#' Bedtools needs to be in your system PATH
#'
#' @param action the bedtools function to be used (Default=intersect)
#' @param args any arguments to be added (e.g. -wo) (Default=-wo)
#' @param a a filename or data frame to be used a set A
#' @param b a filename or data frame to be used a set B
#' @param acols which columns from set A to use
#' @param bcols which columns from set B to use
#' @param ahead boolean whether set A has a header (only relevant for is a is a filename) (Default=F)
#' @param bhead boolean whether set B has a header (only relevant for is b is a filename) (Default=F)
#' @param count boolean whether to return the results of the intersection or just a line count of the result
#' @param tmpdir inset from the top of the blot as a fraction of the plot width
#' @param columnfilt The number of columns to return
#' @param test boolean, if TRUE the function will print out the bedtools command but not execute it
#' @export


bedtools <- function(action="intersect",args="-wo",a,b,acols=NA,bcols=NA,ahead=FALSE,bhead=FALSE,count=FALSE,tmpdir="~/scratch/",columnfilt = NA,test=FALSE)
{
  # interior function to delete temp files
  deletethefiles <- function()
  {
    # delete temp files
    if (file.exists(afile)){file.remove(afile)}
    if (file.exists(bfile)){file.remove(bfile)}
    if (file.exists(cfile)){file.remove(cfile)}
  }
  
   options(scipen=999) 
  
  # file names
  afile = paste(tmpdir,"tmpa.bed",sep="")
  bfile = paste(tmpdir,"tmpb.bed",sep="")
  cfile = paste(tmpdir,"tmpc.bed",sep="")
  
  # read file
  if (class(a)=="character")
  {
    a = read.table(a,header=ahead,sep = "\t")
  }  
  if (class(b)=="character")
  {
    b = read.table(b,header=bhead,sep = "\t")
  } 
  
  if (is.na(acols)==FALSE)
  {
    a = a[,acols]
  }
  if (is.na(bcols)==FALSE)
  {
    b = b[,bcols]
  }
  
  # write files
  if (class(a)=="data.frame")
  {
    write.table(a,file=afile,quote = F,row.names = FALSE,col.names = FALSE,sep = "\t")
  }
  if (class(b)=="data.frame")
  {
    write.table(b,file=bfile,quote = F,row.names = FALSE,col.names = FALSE,sep = "\t")
  }  
  
  if (is.na(columnfilt) == TRUE)
  {
    # perform intersection
    cmd = paste("bedtools",action,args," -a",afile,"-b",bfile,">",cfile)
  }
  
  if (is.na(columnfilt) == FALSE)
  {
    # perform intersection
    cmd = paste(" bedtools ",action," ",args," -a ",afile," -b ",bfile," | cut -f1-",columnfilt," > ",cfile,sep="")
  }
  
  if (test == TRUE)
  {
    print (cmd)
  }
  if (test == FALSE)
  {
    system(cmd)
    
    info = file.info(cfile)
    if (info$size == 0)
    {
      if (count == TRUE)
      {
        deletethefiles()
        return (0)
      }
      deletethefiles()
      return(c())
    }
    if (info$size > 0)
    {
      res = read.table(cfile,header=F,sep="\t")
      deletethefiles()
      if (count == TRUE)
      {
        return (nrow(res))
      }
      return (res)
    }
  }
}
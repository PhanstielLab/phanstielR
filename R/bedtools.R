

bedtools <- function(action="intersect",args="-wo",a,b,acols=NA,bcols=NA,ahead=FALSE,bhead=FALSE,count=FALSE,tmpdir="~/scratch/",columnfilt = NA,test=FALSE)
{
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
        return (0)
      }
      return(c())
    }
    if (info$size > 0)
    {
      res = read.table(cfile,header=F,sep="\t")
      
      if (count == TRUE)
      {
        return (nrow(res))
      }
      return (res)
    }
  }
}

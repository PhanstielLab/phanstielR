




bedtoolsR <- function()
{
  # is it help?
  
  
  # send to correct sub function
  
  
  # return results
  
  
  
  
}



# Define a function to clear out temp files
cleartempfiles <- function(tempfiles=c())
{
  # delete temp files
  for (tempfile in tempfiles)
  {
    if (file.exists(tempfile)){file.remove(tempfile)}
  }
}

# Define a function to run bedtools intersect
bedtoolsR.intersect <- function(options=NULL, a=NULL, b=NULL)
{
  # Check for help
  if (is.null(a)==TRUE & is.null(b)==TRUE)
  {
    system("bedtools intersect --help")
    return ()
  }
  
  # delete temp files
  
  
  # determine data type
  a.datatype = getdatatype(a)
  b.datatype = getdatatype(b)
  
  # check to see if files have headers
  
  # write temp files
  
  
  #
  
  
  # set scipen
  scipen.atstart = getOption("scipen")
  options(scipen=999) 
  
  
  
  
  
  
  
  
  # reset scipen to intial state
  options(scipen=scipen.atstart) 
  
  
  
  
}
  
  

  
#' Reads in a gtf file and builds a bed file with geneid and gene_name as columns 4 and 6 respectively
#' 
#' @param gtf path to a gtf file
#' @param filter filter to apply to column 3 of the gtf file
#' @export
#' @examples
#' readingtf = readgtf("/Users/dphansti/Tools/genomes/gencode.v19/gencode.v19.annotation.gtf_withproteinids")
#' head(readingtf)
#' 
#' 
readgtf <- function(gtf,filter="gene")
{
  ### Define a function that gets the gene name from a gtf column 9
  getgtfattributes <- function(column9,attribute)
  {
    
    getstart = substr(column9,start = regexpr(attribute, column9)+nchar(attribute)+2,stop = nchar(column9))
    gene_name = substr(getstart,start = 1,stop = regexpr("\"", getstart)-1)
    return(gene_name)
  }
  
  # read in data
  gtfdata = data.frame(read_tsv(gtf,comment="#",col_names = F))
  
  # get gene level info
  gtfdata_gene = gtfdata[which(gtfdata[,3] == filter),]

  # extract gene names
  gtfdata_gene$gene_name = unlist(lapply(gtfdata_gene[,9],  getgtfattributes,attribute = "gene_name" ))
  
  # extract gene id
  gtfdata_gene$gene_id = unlist(lapply(gtfdata_gene[,9],  getgtfattributes,attribute = "gene_id" ))
  
  # build bed like data frame
  genesbed = data.frame(chrom=gtfdata_gene[,1],
                        start=gtfdata_gene[,4],
                        end=gtfdata_gene[,5],
                        name=gtfdata_gene$gene_id,
                        signal=gtfdata_gene[,6],
                        strand=-1 ,
                        oldstrand=gtfdata_gene[,7],
                        genename=gtfdata_gene$gene_name)

  genesbed$strand[which(genesbed$oldstrand == "+")] = 1
  genesbed = genesbed[,c(1:6,8)]
  names(genesbed) = c("chrom","start","stop","gene","score","strand","genename")
  
  genesbed[,6] = as.integer(genesbed[,6])
  genesbed[,1] = as.character(genesbed[,1])
  genesbed[,4] = as.character(genesbed[,4])
  genesbed[,5] = as.character(genesbed[,5])
  genesbed[,6] = as.character(genesbed[,6])
  rownames(genesbed) = 1:nrow(genesbed)
  
  return (genesbed)
}
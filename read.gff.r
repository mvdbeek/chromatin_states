library(tools)
### functions
read.gff <- function (x,name="score") {
  fn.ext <- file_ext(x)
  
  if (grepl("gff",ignore.case=T,fn.ext)) {
	temp.data <- read.table(x,row.names=NULL)
	if (ncol(temp.data) > 5) {
	  # GFF
	  trim.data <- temp.data[,c(1,4,5,6)]
	} else {
		cat("Error: file does not appear to be in GFF format\n\n")
		quit("no",1)
	}
  } else if (grepl("bed",ignore.case=T,fn.ext)) {
	temp.data <- read.table(x,row.names=NULL,skip=1)
	if (ncol(temp.data) == 4) {
		# bedgraph
		trim.data <- temp.data
	} else {
		cat("Error: file does not appear to be in bedGraph format\n\n")
		quit("no",1)
	}
  } else {
	cat("Error: input file does not appear to be in bedGraph or GFF format ...\n\n")
	quit("no",1)
  }
  
  names(trim.data) <- c("chr","start","end",name)
  
  trim.data$chr <- gsub("^chr","",trim.data$chr,perl=T)
  
  return(trim.data)
}

## A script to preprocess the alignments file from IMGT/HLA:
## This should be updated whenever a new version is available 



require("stringr")
require("doParallel")



#####Detect cores#####
ncores=detectCores()-1
cl <-makeCluster(ncores,type = "FORK")
registerDoParallel(cl)

directory="/media/hoanganh/du.lieu/primer_design/alignments/"
setwd(directory)
alleletype="C"
files=list.files()

#check if database is presented:

process.reference=function(directory,alleletype) {
  #We will be working with the gen.txt data:
  data<-readLines(paste(directory,alleletype,"_nuc.txt",sep=""))
  #virtual space: 
  vir<-NULL
  coord<-NULL
  stripped<-NULL
  nuc.table<-NULL
  ########## relevant function #############
  
  #get allele names:
  
  alleles=substr(data,2,19)
  alleles=alleles[str_detect(alleles,pattern=paste("\\*",sep=""))]
  alleles=unique(str_trim(alleles,"right"))
  alleles=paste(alleles," ",sep="")
  alleles=str_replace(alleles,paste(alleletype,"\\*",sep=""),paste(alleletype,"\\\\*",sep=""))
  
  
  #Process alignment file: 
  #output: virtual space indices (variable: vir) and 
  #output: a list of sequence for the wild-type allele. There are singular base and also base with 
  #virtual space
  
  #input: directory of the HLA alignment file
  #Process wild-type:
  ref=data[str_detect(data,pattern=alleles[1])]
  ref=str_trim(ref,"both")
  ref2=str_sub(ref,19,-1)
  ref2=str_replace_all(ref2,pattern="\\s+", replacement = "")
  ref2=str_replace_all(ref2,pattern="\\|", replacement = "")
  ref2=paste(ref2,collapse ="")
  #detect virtual space indices:
  vir=str_locate_all(ref2,"\\.+")
  vir[[1]][,1]=vir[[1]][,1]-1
  vir[[1]][,2]=vir[[1]][,2]+1
  vir<<-vir[[1]]
  #combine:
  combined_strips=str_sub(ref2,vir)
  stripped=str_split(ref2,"")[[1]]
  #strip the bases:
  stripped_index=vir[,1]
  stripped[stripped_index]<-combined_strips
  #delete the remaining part:
  merged_region=apply(vir,1,function(row) (row[1]+1):row[2])
  merged_region=unlist(merged_region)
  stripped<<-stripped[-merged_region]
  #get coordinate of each snippet:
  coord<-str_length(str_replace(stripped,pattern = "\\.+",replacement = ""))
  coord<<-cumsum(coord)
}


#process each allele:
merge_nuc=function(allele) {
  #merge fragments:
  nucs=data[str_detect(data,allele)]
  nucs=str_trim(nucs,"both")
  nucs=str_sub(nucs,19,-1)
  nucs=str_replace_all(nucs,pattern=" ", replacement = "")
  nucs=str_replace_all(nucs,pattern="\\|", replacement = "")
  nucs=paste(nucs,collapse = "")
  stripped=str_sub(nucs,vir)
  nucs=str_split(nucs,"")[[1]]
  stripped=str_replace_all(stripped,"\\.+","")
  nucs[stripped_index]=stripped
  return(nucs[-merged_region])
}


#create an allele table 
#output: a table 
process.reference(directory,alleletype)
alleles.done=mclapply(alleles,merge_nuc,mc.cores = 10)
nuc.table=as.data.frame(alleles.done,stringsAsFactors = F)
colnames(nuc.table)=alleles
row.names(nuc.table)=coord
nuc.table=as.data.frame(nuc.table)
save.image(paste("/media/hoanganh/du.lieu/primer_design/precomputed allele snps/",allele,".db",sep=""))


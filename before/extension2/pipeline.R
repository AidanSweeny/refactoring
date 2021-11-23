
require(jsonlite)
require(openPrimeR)
require(stringi)
require(purrr)
require(stringr)
require(spgs)
source("")

allele="C"
template=read_templates(paste("../fasta/",allele,"_gen.fasta",sep=""))
#read fasta:
load(paste("../precomputed allele snps/",allele,"",sep=""))
alleles=str_replace(alleles,"\\\\","")
alleles4.trimmed=str_trim(unlist(lapply(alleles,function(i){
  a=str_split(i,":")[[1]]
  return(paste(a[1],":",a[2],sep=""))}
)))
#optimal.set:
alleles4=unique(alleles4.trimmed)
alleles4=alleles4[alleles4 %in% names(db)]
template$ID=sapply(template$ID, function(i) str_split(i," ")[[1]][2])
gene.ind.fasta= lapply(alleles4, function(i) which(stri_detect(template$ID,regex=str_replace(i,"\\*","\\\\*")))[1])
gene.ind.table= lapply(alleles4, function(i) which(stri_detect(alleles,regex=str_replace(i,"\\*","\\\\*")))[1])
allele.length= template$Sequence_Length[unlist(gene.ind.fasta)]


db=list()
for(i in 1649:length(alleles4)){
  if (i==1648) {db=append(db,list(NULL))}
  result=unique.SNPs.4digit(str_replace(alleles4[i],"\\*","\\\\*"),nuc.table,100,allele.length[i])
  names(result)=alleles4[i]
  db=append(db,list(result))
}



fastq.coord=lapply(1:(length(alleles4)-1),function(i) get_fasta_coord(db[[i]][[1]],nuc.table,gene.ind.table[[i+1]]))

optim=lapply(c(1:length(db)),function(i)   {if (i==1648) {return(NULL)}
             optimal.set(fastq.coord[[i]],allele.length[[i]])})
legit=unlist(lapply(1:length(optim), function(i) length(optim[[i]][["fastq"]])<=2 & tail(db[[i]][[2]],1)==1))
legit.allele=optim[legit]
setwd("/media/hoanganh/du.lieu/primer_design//precomputed.db/B.new")
candidates=template
for (i in which(legit)) {
  
  
  if (str_replace_all(paste(alleles4[i],".",1,".csv",sep=""),"[\\*|\\:]",".") %in% list.files()) {
    next
  }
  tryCatch( 
    {
      print(i)
      a=lapply(1:length(optim[[i]][[1]]),function(j) primer.design.both(optim[[i]][[2]][[j]],optim[[i]][[1]][[j]],nuc.table,gene.ind.fasta[[i]],user.constraint(),template))
      
      lapply(1:length(a),function(j) write.csv(a[[j]],str_replace_all(paste(alleles4[i],".",j,".csv",sep=""),"[\\*|\\:]",".")))
    }
  )
}



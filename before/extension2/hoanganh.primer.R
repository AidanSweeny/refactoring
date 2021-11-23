#!/usr/bin/env Rscript
curdir= system(command = "pwd",intern = T)
setwd(curdir)
require(argparse)
require(rje)
require("stringr")
require("openPrimeR")
require("doParallel")
require("purrr")
require(crayon)
require("stringi")
require(data.table)
###multi-threads processing setup:
ncores=detectCores()-1
cl <- makeCluster(ncores,type="FORK")  
registerDoParallel(cl)  


 

 
 #get SNP's frequency for a given target and coordinate:



unique.SNPs=function(target,table,nprimer_max=2) {
  get_base_freq = function(target,coord,dataset) {
    return(sum(dataset[coord,]==target[coord]))
  }
  
  output=c()
  allele.table=table
  target.allele=unlist(allele.table[,target])
  
  SNPs=which(sapply(target.allele,function(i) str_detect(i,"([ACGT\\.])+")))
  if (length(SNPs)==0) {
    SNPs=1:nrow(allele.table)
    names(SNPs)=target.allele
  }
  #coordinate of the SNPs:
  #frequency of these SNPs:
  SNPs.freq=sapply(SNPs,function(i) get_base_freq(target.allele,i,allele.table))
  SNPs.sorted=SNPs
  SNPs.freq.sorted=SNPs.freq 
  #sort SNPs coordinate by their freqs:
  if (length(SNPs.freq)>1){
    SNPs.sorted=SNPs[order(SNPs.freq)]
    SNPs.freq.sorted=sort(SNPs.freq)
  }
  output=c(output,SNPs.sorted[1])
  snp.cur=1
  print(SNPs.freq.sorted)
  remaining_candidates=c(SNPs.freq.sorted[1])
  while (SNPs.freq.sorted[1]!=1) {
    print(remaining_candidates)
    if (snp.cur>=nprimer_max) {
      break
    }
    sub_index=which(unlist(allele.table[SNPs.sorted[1],])==names(SNPs.freq.sorted)[1])
    allele.table=allele.table[,sub_index]
    SNPs=1:nrow(allele.table)
    names(SNPs)=unlist(table[,target])
    SNPs.freq=unlist(mclapply(SNPs,function(i) get_base_freq(target.allele,i,allele.table),mc.cores = 10))
    
    SNPs.sorted=SNPs
    SNPs.freq.sorted=SNPs.freq 
    #sort SNPs coordinate by their freqs:
    if (length(SNPs.freq)>1){
      SNPs.sorted=SNPs[order(SNPs.freq)]
      SNPs.freq.sorted=sort(SNPs.freq)
    }
    print(SNPs.freq.sorted)
    output=c(output,SNPs.sorted[1])
    snp.cur=snp.cur+1

    remaining_candidates=c(remaining_candidates,min(SNPs.freq.sorted))
    if (tail(remaining_candidates,1)==min(SNPs.freq)) {
      break
    }
  }
  print(remaining_candidates)
  return(list("snp_list"=output,"remaining_candidates"=remaining_candidates))
}


unique.SNPs.4digit=function(target,table,nprimer_max=4,margin=NULL,enrich=T,allele.length) {
  get_fasta_coord.i=function(SNPs,nuc.table,target){
    #a function to convert the coordinates of a SNP 
    get_coord_ind=function(i){
      #get the first halve:
      alignment_coord=nuc.table[1:i,target]
      length.strip=str_length(str_replace(alignment_coord,pattern = "\\.+",replacement = ""))
      length.strip=sum(length.strip)
      unidentified_region=which(nuc.table[,target]!="*")[1]
      out=length.strip-unidentified_region+1
      return(out)
    }
    if (length(SNPs)>1)
      return(unlist(lapply(SNPs,get_coord_ind)))
    else 
      return(get_coord_ind(SNPs))
  }
  output=c()
  allele.table=table
  #match all alleles that fall into the same 4-digit category:
  
  allele.inds=which(stri_detect(alleles4.trimmed,regex=paste(target,"$",sep="")))
  if (target %in% c("A\\*01:01","B\\*07:02")) allele.inds=allele.inds[-1]
  if (length(allele.inds)==1) {
    
    core.base.ind.fastq<<-get_fasta_coord.i(1:nrow(allele.table),table,allele.inds[1])
    last=which(core.base.ind.fastq<margin-16)
    last=tail(last,1)
    first=which(core.base.ind.fastq>16)[1]
    output=unique.SNPs(allele.inds,table[first:last,],nprimer_max=500)
    return(list(snp_list=c(first:last)[output$snp_list],remaining_candidates=output$remaining_candidates,core_base=c(first:last)))
  }
  target.allele.table=allele.table[,allele.inds]
  #find the core SNPs:
  all.base.freq=unlist(mclapply(1:nrow(allele.table),function(x) length(unique(unlist(target.allele.table[x,])))==1,mc.cores=12))
  core.base.ind=which(all.base.freq)
  if (!is.null(margin)) {
    core.base.ind.fastq=get_fasta_coord.i(core.base.ind,table,allele.inds[1])
    #prune out those in the margin:
    core.base.ind=core.base.ind[which(core.base.ind.fastq>16 & core.base.ind.fastq< margin-16)]
  }
  
  #heuristic search: we will first find the SNPs:
  core.base=unlist(allele.table[core.base.ind,allele.inds[1]])
  target.allele.table=allele.table[core.base.ind,-allele.inds[-1]]
  
  #The final process is similar to the one done above with 8 digit
  output=unique.SNPs(allele.inds[1],target.allele.table,nprimer_max=nprimer_max)
  return(list("snp_list"=core.base.ind[output$snp_list],"remaining_candidates"=output$remaining_candidates,"core_base"=core.base.ind))
}
 


is.unique=mclapply(1:length(db),function(i) {
 allele.inds=which(alleles4.trimmed==alleles4[[i]])
 if (length(allele.inds)==1) {
    bases=nuc.table2[db[[i]]$core_base,] 
 }else{
   bases=nuc.table2[db[[i]]$core_base,-allele.inds[-1]] 
 }
 exist=colnames(bases)[which(sapply(c(1:ncol(bases))[-allele.inds[1]],function(j) identical(bases[,j],bases[[allele.inds[1]]])))]
 return(exist)
})
 

is.unique_snp=mclapply(1:29,function(k) {
  allele.inds=which(alleles4.trimmed==alleles4[[i]])
  if (length(allele.inds)==1) {
    bases=nuc.table2[db[[i]]$snp_list[-k],] 
  }else{
    bases=nuc.table2[db[[i]]$snp_list[-k],-allele.inds[-1]] 
  }
  exist=colnames(bases)[which(sapply(c(1:ncol(bases))[-allele.inds[1]],function(j) identical(bases[,j],bases[[allele.inds[1]]])))]
  return(exist)
})
 



 

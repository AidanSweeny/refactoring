## A script to preprocess the alignments file from IMGT/HLA:
## This should be updated whenever a new version is available 



require("stringr")
require("doParallel")




process_HLA_MSA=setRefClass("process_HLA_MSA",
                            fields = list(allele="character",
                                          alleles4="character",
                                          alleles="character",
                                          directory="character",
                                          mc.cores="numeric",
                                          ref="character",
                                          ref2="list",
                                          vir="matrix",
                                          coord="numeric",
                                          template="data.frame",
                                          gene.ind.fasta="numeric",
                                          gene.ind.table="numeric",
                                          allele.length="numeric"
                                          ))
setMethod("read_MSA",signature("process_HLA_MSA"),function(object){
  data@object=readLines()
})
setMethod("read_template",signature("process_HLA_MSA"),function(object,dir){
  object@read_template=read_templates(dir)
})

setMethod("process.reference",signature("process_HLA_MSA"), function(object){
  directory=object@directory
  alleletype=object@alleletype
  data<-readLines(directory)
 
  alleles=substr(data,2,19)
  alleles=alleles[str_detect(alleles,pattern=paste("\\*",sep=""))]
  alleles=unique(str_trim(alleles,"right"))
  alleles=paste(alleles," ",sep="")
  object@alleles=str_replace(alleles,paste(alleletype,"\\*",sep=""),paste(alleletype,"\\\\*",sep=""))
  
  
  #Process alignment file: 
  #output: virtual space indices (variable: vir) and 
  #output: a list of sequence for the wild-type allele. There are singular base and also base with 
  #virtual space
  
  #input: directory of the HLA alignment file
  #Process wild-type:
  ref=data[str_detect(data,pattern=alleles[1])]
  object@ref=str_trim(ref,"both")
  ref2=str_sub(ref,19,-1)
  ref2=str_replace_all(ref2,pattern="\\s+", replacement = "")
  ref2=str_replace_all(ref2,pattern="\\|", replacement = "")
  object@ref2=paste(ref2,collapse ="")
  #detect virtual space indices:
  vir=str_locate_all(ref2,"\\.+")
  vir[[1]][,1]=vir[[1]][,1]-1
  vir[[1]][,2]=vir[[1]][,2]+1
  object@vir=vir[[1]]
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
  coord=str_length(str_replace(stripped,pattern = "\\.+",replacement = ""))
  object@coord=cumsum(coord)

})
setMethod("merge_nuc",signature("process_HLA_MSA"), function(object){
 allele=object@allele
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
})
setMethod("setup_parallel",signature("process_HLA_MSA"),function(object){
  ncores=detectCores()-1
  cl <-makeCluster(ncores,type = "FORK")
  registerDoParallel(cl)
})
setMethod("create_MSA_table", signature("process_HLA_MSA"),function(object){
  object@process.reference(object@directory,object@alleletype)
  alleles.done=mclapply(object@alleles,object@merge_nuc,object@mc.cores)
  nuc.table=as.data.frame(alleles.done,stringsAsFactors = F)
  colnames(nuc.table)=object@alleles
  row.names(nuc.table)=coord
  object@nuc.table=as.data.frame(nuc.table)
  process.reference(directory,alleletype)
  alleles.done=mclapply(alleles,merge_nuc,mc.cores = 10)
  nuc.table=as.data.frame(alleles.done,stringsAsFactors = F)
  colnames(nuc.table)=alleles
  row.names(nuc.table)=coord
  object@nuc.table=as.data.frame(nuc.table)
})
setMethod("get_fasta_coord",signature("design_primer_HLA"),function(object,SNPs,target) {
  #a function to convert the coordinates of a SNP 
  get_coord_ind=function(i){
    #get the first halve:
    alignment_coord=nuc.table[1:i,target]
    length.strip=str_length(str_replace(alignment_coord,pattern = "\\.+",replacement = ""))
    length.strip=sum(length.strip)
    unidentified_region=which(object@nuc.table[,target]!="*")[1]
    out=length.strip-unidentified_region+1
    return(list("table"=i,"fastq"=out))
  }
  return((lapply(SNPs,get_coord_ind)))
})
setMethod("optimal_set",signature("design_primer_HLA"),function(object,snps.list,allele.length.optimal,sanger.length=1000){
  sort.snps<-sort(unlist(lapply(snps.list,function(i) i[["fastq"]])))
  sort.snps.table<-sort(unlist(lapply(snps.list,function(i) i[["table"]])))    
  
  
  solution=list()
  solution.table=list()
  
  
  while (length(sort.snps)>0) {
    #get the next snps that is within 1000bps: 
    end.ind=max(which(sort.snps-sort.snps[1]<sanger.length))
    if (end.ind==1) {
      #if there is only one such snps, and it is the first snps: 
      #at least more than 200bps away from  the 3' end:
      if (sort.snps[1] > allele.length.optimal/2) {
        solution=append(solution,list(c("fw",sort.snps[1])))
        solution.table=append(solution.table,list(c("fw",sort.snps.table[1])))
      }
      else {
        solution=append(solution,list(c(sort.snps[1],"rev")))
        solution.table=append(solution.table,list(c(sort.snps.table[1],"rev")))
      }
      sort.snps=sort.snps[-1]
      sort.snps.table=sort.snps.table[-1]
    }
    else{
      solution=append(solution,list(sort.snps[1:end.ind]))
      solution.table=append(solution.table,list(sort.snps.table[1:end.ind]))
      
      sort.snps=sort.snps[-c(1:end.ind)]
      sort.snps.table=sort.snps.table[-c(1:end.ind)]
      
    }
  }
  solution=lapply(solution,function(i)c(i[1],i[length(i)]))
  solution.table=lapply(solution.table,function(i)c(i[1],i[length(i)]))
  
  return(list("fastq"=solution,"table"=solution.table))
}) 
setMethod("get_alleles_4",signature("design_primer_HLA"),function(object) {
  alleles=str_replace(alleles,"\\\\","")
  alleles4.trimmed=str_trim(unlist(lapply(alleles,function(i){
    a=str_split(i,":")[[1]]
    return(paste(a[1],":",a[2],sep=""))}
  )))
  #optimal.set:
  alleles4=unique(alleles4.trimmed)
  alleles4=alleles4[alleles4 %in% names(db)]
  object@alleles4=alleles4
  
  template$ID=sapply(template$ID, function(i) str_split(i," ")[[1]][2])
  object@gene.ind.fasta= lapply(alleles4, function(i) which(stri_detect(template$ID,regex=str_replace(i,"\\*","\\\\*")))[1])
  object@gene.ind.table= lapply(alleles4, function(i) which(stri_detect(alleles,regex=str_replace(i,"\\*","\\\\*")))[1])
  object@allele.length= template$Sequence_Length[unlist(gene.ind.fasta)]
})
setMethod("save_result", signature("process_HLA_MSA"),function(object,outdir){
  save(object,paste(outdir,object@alleletype,"_preprocessed.db",sep=""))
  
})
setMethod("read_MSA_db",signature("design_primer_HLA"),function(object,directory){
  object@processed_db=readRDS(directory)
})
setMethod("primer.design.snp.rev.relaxed",signature("design_primer_HLA"),function(object,gene.ind.fasta,snps) {
  
  length.primer=17
  
  #start with the reverse direction:
  ## have to check if it is still within range of the destination allele:  
  ##readjust the range of the reverse primer:
  
  
  rev=c(object@candidates[gene.ind.fasta]$Sequence_Length-(snps+300),object@candidates[gene.ind.fasta]$Sequence_Length-(snps+100))
  template.fw=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw = c(),rev=rev)
  candidates.primer=design_primers(template.fw,"rev",settings.fw,opti.algo = "ILP",init.algo = "naive")
  return(candidates.primer$filtered$data)
})
setMethod("primer.design.snp.fw.relaxed",signature("design_primer_HLA"),function(object,gene.ind.fasta,snps) {
  
  
  #start with the reverse direction:
  ## have to check if it is still within range of the destination allele:  
  ##readjust the range of the reverse primer:
  
  fw=c(snps-400,snps-200)
  
  
  template.fw=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw=fw)
  candidates.primer=design_primers(template.fw,"fw",settings.fw,opti.algo = "ILP",init.algo = "naive")
  return(candidates.primer$filtered$data)
})
setMethod("primer.design.snp.fw",signature("design_primer_HLA"),function(object,gene.ind.fasta,snps) {
  
  
  solution=list()
  discard=list()
  length.primer=16
  
  #start with the reverse direction:
  ## have to check if it is still within range of the destination allele:  
  ##readjust the range of the reverse primer:
  
  
  
  template.fw=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw=c(snp-length.primer+1,snp))
  primer.fw=get_initial_primers(paste("primer.1",16,sep=""),template.fw,c(16,16),"fw")[1,]
  print(primer.fw)
  base=c("a","t","g","c")
  mismatch=rep(primer.fw$Forward,16)
  tobeadd=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw=c(snp-length.primer-11,snp-length.primer))$Allowed_fw
  
  last.5.bps=tolower(lapply(nuc.table[(snps.table-4):snps.table,],function(i) paste(i,collapse = "")))
  print(last.5.bps)
  for(i in 1:4) {
    for (j in 1:4){
      if (base[[i]] %in% str_sub(last.5.bps,-i-1,-i-1)) {
        substr(mismatch[4*i+j-4],length.primer-j,length.primer-j)=base[[i]]
      }
    }
  }
  #and then check for qualifying mismatch: 
  print(mismatch)
  mismatch=unique(mismatch)
  
  #test each of them for secondary structure and self-dimerization:
  primer1.fw.set=do.call(rbind,rep(list(primer.fw),length(mismatch)))
  primer1.fw.set$Forward=mismatch
  max.i.add=min(snp-length.primer,10)
  if (max.i.add>0) max.i.add=1:max.i.add
  for(i in max.i.add) {
    
    
    #.1 First forward primer 
    
    template=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw=c(snp-length.primer+1,snp))
    primer1=get_initial_primers(paste("primer.1",length.primer,sep=""),template,c(length.primer,length.primer),"fw",allowed.region.definition = "within")[1]
    
    primer1.fw.set$Forward=paste(str_sub(tobeadd,-i,-i),primer1.fw.set$Forward,sep="")
    primer1.fw.set$primer_length_fw=primer1.fw.set$primer_length_fw+1
    constraint1=check_constraints(primer1.fw.set,template,settings.fw,active.constraints = "melting_temp_range")
    
    fw.pass.melt=which(constraint1$EVAL_melting_temp_range)
    if (length(fw.pass.melt)==0) {
      length.primer=length.primer+1
      discard=rbind(discard,constraint1[which.min(constraint1$melting_temp),])
      next 
    }
    else{
      cat("2. check the forward primer for  additional constraints:: \n")
      constraint.fw.1=check_constraints(primer1.fw.set[fw.pass.melt,],template,settings.fw,active.constraints = user.constraint)
      solution=rbind(solution,constraint.fw.1)
    }
  }
  #last resort: 
  print("solutio:")
  if (length(solution)==0) {
    print("help")
    constraint.fw.1=check_constraints(discard,template,settings.fw,active.constraints = user.constraint)
    solution=constraint.fw.1
  }
  solution$melting_temp=check_constraints(solution,template,settings.fw,"melting_temp_range")$melting_temp
  return(solution)
})

setMethod("primer.design.snp.rev",signature("design_primer_HLA"),function(object,gene.ind.fasta,snps) {
  traceback=list()
  #setting file:
  settings.xml = system.file("extdata", "settings","C_Taq_PCR_high_stringency.xml", package = "openPrimeR")
  settings.fw = read_settings(settings.xml)
  #no mismatch plis:
  conOptions(settings.fw)[["allowed_mismatches"]] <- 0
  settings.fw@Input_Constraints@settings[["melting_temp_range"]]=c("min"=50,"max"=65)
  #also specificity:
  conOptions(settings.fw)[["allowed_other_binding_ratio"]] <- 0
  discard=list()
  solution=list()
  length.primer=17
  
  #start with the reverse direction:
  ## have to check if it is still within range of the destination allele:  
  ##readjust the range of the reverse primer:
  
  
  
  template.fw=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw=c(snp,snp+length.primer-1))
  
  primer.fw=get_initial_primers(paste("primer.1",17,sep=""),template.fw,c(17,17),"fw")[1,]
  base=c("a","t","g","c")
  mismatch=rep(primer.fw$Forward,16)
  tobeadd=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw=c(snp+length.primer,snp+length.primer+10))$Allowed_fw
  
  
  for(i in 1:4) {
    for (j in 1:4){
      existing=tolower(unique(c(unlist(nuc.table[snps.table+j,])),nuc.table[snps.table+j,1]))
      if (! base[[i]] %in% str_sub(existing,1,1))
        
        substr(mismatch[4*i+j-4],1+j,1+j)=base[[i]]
    }
  }
  
  mismatch=unique(unique(mismatch),primer.fw$Forward)
  #test each of them for secondary structure and self-dimerization:
  primer1.fw.set=do.call(rbind,rep(list(primer.fw),length(mismatch)))
  primer1.fw.set$Forward=mismatch
  for(i in c(1:11)) {
    
    
    
    #.1 First forward primer 
    cat("1. design forward primer::")
    
    template=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw=c(snp,snp+length.primer-1))
    primer1=get_initial_primers(paste("primer.1",length.primer,sep=""),template,c(length.primer,length.primer),"fw",allowed.region.definition = "within")[1]
    
    primer1.fw.set$Forward=paste(primer1.fw.set$Forward,str_sub(tobeadd,i,i),sep="")
    primer1.fw.set$primer_length_fw=primer1.fw.set$primer_length_fw+1
    constraint1=check_constraints(primer1.fw.set,template,settings.fw,active.constraints = "melting_temp_range")
    
    #.2 second primer at most 200 bp away from the reverse primer 
    fw.pass.melt=which(constraint1$EVAL_melting_temp_range)
    if (length(fw.pass.melt)==0) {
      length.primer=length.primer+1
      discard=append(discard,list(constraint1[which.min(constraint1$melting_temp),]))
      next 
    }
    else{
      cat("2. check the forward primer for  additional constraints:: \n")
      constraint.fw.1=check_constraints(primer1.fw.set[fw.pass.melt,],template,settings.fw,active.constraints = user.constraint)
      solution=rbind(solution,constraint.fw.1)
    }
  }
  #last resort: 
  if (length(solution)==0) {
    solution=check_constraints(do.call(rbind,discard),template,settings.fw,active.constraints = user.constraint)
  }
  solution$melting_temp=check_constraints(solution,template,settings.fw,"melting_temp_range")$melting_temp
  return(solution)
  
})

setMethod("set_settings",signature("design_primer_HLA"),function(object,design_type){
  settings.fw = read_settings(settings.xml)
  
  #no mismatch plis:
  conOptions(settings.fw)[["allowed_mismatches"]] <- 0
  settings.fw@Input_Constraints@settings[["melting_temp_range"]]=c("min"=50,"max"=65)
  settings.fw@Input_Constraints@settings[["melting_temp_range"]]=c("min"=50,"max"=65)
  settings.fw@Input_Constraints@settings[["primer_length"]]=c("min"=18,"max"=28)
  
  #also specificity:
  conOptions(settings.fw)[["allowed_other_binding_ratio"]] <- 0
  conOptions(settings.fw)[["allowed_mismatches"]] <- 0
  object@settings.fw=settings.fw
  
})


setMethod("primer_design_both",signature("design_primer_HLA"),function(object,design_type){
  if (snp.list[1]=="fw") {
    fw.all<<-primer.design.snp.fw.relaxed(as.numeric(snp.list[2]),gene.ind.fasta,table,user.constraint[[1]],template)
  }
  else {
    print("constrained")
    fw.all<<-primer.design.snp.fw(as.numeric(snps.table[1]),as.numeric(snp.list[1]),gene.ind.fasta,table,user.constraint[[1]],template)
    print(dim(fw.all))
  }
  if (snp.list[2]=="rev") {
    rev.all<<-primer.design.snp.rev.relaxed(as.numeric(snp.list[1]),gene.ind.fasta,table,user.constraint[[1]],template)
    rev.all$Forward=rev.all$Reverse
    rev.all$primer_length_fw=rev.all$primer_length_rev
  }
  else {
    rev.all<<-primer.design.snp.rev(as.numeric(tail(snps.table,1)),as.numeric(tail(snp.list,1)),gene.ind.fasta,table,user.constraint[[2]],template)
  }
  pairs.id = cross2(.x=1:nrow(fw.all),.y=1:nrow(rev.all))
  
  temp.diff=unlist(lapply(pairs.id,function(x) abs(fw.all$melting_temp[x[[1]]]-rev.all$melting_temp[x[[2]]])))
  satisfy=which(temp.diff<=5)
  #in the case that no two pairs are 5 degree apart, select the best one;
  if (length(satisfy)==0) {
    satisfy=order(temp.diff)[1:20]
    print(satisfy)
  }
  satisfy.pair=pairs.id[satisfy]
  primer.pairs=function(id,fw,rv) {
    copy=fw[id[[1]],]
    copy$Reverse=rv$Forward[id[[2]]]
    copy$primer_length_rev=rv$primer_length_fw[id[[2]]]
    copy$melting_temp_rev=rv$melting_temp[id[[2]]]
    copy$EVAL_secondary_structure_rev=rv$EVAL_secondary_structure[id[[2]]]
    copy$EVAL_self_dimerization_rev=rv$EVAL_self_dimerization[id[[2]]]
    return(copy)
  }
  all.primer.pairs=lapply(satisfy.pair,function(x) primer.pairs(x,fw.all,rev.all))
  all.primer.pairs=do.call(rbind,all.primer.pairs[1:min(50,length(all.primer.pairs))])
  template=assign_binding_regions(template.df = candidates[gene.ind.fasta],fw=c(1,2))
  
  
  all.primer.pairs$EVAL_cross_dimerization=check_constraints(all.primer.pairs,template,settings.fw,c("cross_dimerization"))$EVAL_cross_dimerization
  #Final analyses: gc_clamp, gc_ratio, no_runs, no_repeats:
  all.primer.pairs$EVAL_gc_clamp=check_constraints(all.primer.pairs,template,settings.fw,c("gc_clamp"))$EVAL_cross_dimerization
  all.primer.pairs$EVAL_gc_ratio=check_constraints(all.primer.pairs,template,settings.fw,c("gc_ratio"))$EVAL_cross_dimerization
  all.primer.pairs$EVAL_no_runs=check_constraints(all.primer.pairs,template,settings.fw,c("no_runs"))$EVAL_cross_dimerization
  all.primer.pairs$EVAL_no_repeats=check_constraints(all.primer.pairs,template,settings.fw,c("no_repeats"))$EVAL_cross_dimerization
  
  return(all.primer.pairs)
  
})


  
setMethod("unique_SNPs", signature("design_primer_HLA"),function(object,target,table,nprimer_max=2) {
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
})


setMethod("unique.SNPs.4digit",signature("design_primer_HLA"), function(object,target,nprimer_max=4,margin=NULL,enrich=T) {
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
})

setMethod("get_unique_SNPs_4",signature("design_primer_HLA"),function(object){
  object@unique_SNPS4=object@unique.SNPs.4digit(str_replace(object@alleles4[i],"\\*","\\\\*"),object@nuc.table,100,object@allele.length[i])
})
setMethod("get_unique_SNPs",signature("design_primer_HLA"),function(object){
  object@unique_SNPS=object@unique.SNPs(str_replace(object@alleles4[i],"\\*","\\\\*"),object@nuc.table,100,object@allele.length[i])
})



setMethod("Design_primer",signature("design_primer_HLA"),function(object,p.allele4){
  #assume that unique SNPs already exist:
  
  
  fastq.coord=lapply(1:(length(p.allele4)-1),function(i) get_fasta_coord(db[[i]][[1]],nuc.table,gene.ind.table[[i+1]]))
  optim=lapply(c(1:length(db)),function(i)  optimal_set(fastq.coord[[i]],allele.length[[i]]))
  legit=unlist(lapply(1:length(optim), function(i) length(optim[[i]][["fastq"]])<=2 & tail(db[[i]][[2]],1)==1))
  legit.allele=optim[legit]
  setwd("/media/hoanganh/du.lieu/primer_design//precomputed.db/B.new")
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
  
})


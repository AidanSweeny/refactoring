setwd("/media/hoanganh/du.lieu/primer_design/")
require("openPrimeR")
require("stringr")
require('jsonlite')
#precomputed PRIMERS DATABASE:
fasta=read_templates("")

#get unique SNPs and determine if primer design is possible:
nprimer=mclapply(snps.list,)

#design primers: 



save(list=c('nuc.table','alleles'),file = "precomputed allele snps/A.gen.db")
require(openPrimeR)
require(jsonlite)
to.process=c("A","B","C","DQB1","DRB1")
 #read fasta:
allele="A"
for (allele in to.process) {
  template=read_templates(paste("fasta/",allele,"_gen.fasta",sep=""))
  #read fasta:
  db=read_json(paste("precomputed allele snps/",allele,"_gen.precomputed.json",sep=""))
  load(paste("precomputed allele snps/",allele,".gen.db",sep=""))
  #optimal.set:
  template$ID=sapply(template$ID, function(i) str_split(i," ")[[1]][2])
  gene.ind.fasta= lapply(alleles[-1], function(i) which(str_trim(str_replace(i,"\\\\","")) == template$ID))
  allele.length= template$Sequence_Length[unlist(gene.ind.fasta)]
  optim=lapply(1:length(db),function(i) optimal.set(db[i][[1]][[1]][[1]],allele.length[i]))
  legit=unlist(lapply(optim, function(i) length(i)<2))
  legit.allele=optim[legit]
  fastq.coord=lapply(which(legit),function(i) get_fasta_coord(optim[[i]][[1]],nuc.table,i))
  primers=lapply(primer.design.both()
}
a=primer.design.both(optim[[1]][[1]],fastq.coord[[1]],nuc.table,2,user.constraint(),template)
output.8=mclapply(1:length(db),function(i)  primer.design.both(optim[[i]][[1]],fastq.coord[[i]],nuc.table,i+1,user.constraint(),template),mc.cores=10)
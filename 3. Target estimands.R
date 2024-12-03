library("TraMineR")
library("resample")

set.seed(1)
load("dataset1.Rdata")
load("dataset2.Rdata")
load("dataset3.Rdata")
load("dataset4.Rdata")
load("dataset5.Rdata")
load("dataset6.Rdata")
source("Functions script.R")

list_datasets <- list()
list_datasets[[1]] <- DataWork
list_datasets[[2]] <- long_living_4cat
list_datasets[[3]] <- long_living_8cat
list_datasets[[4]] <- civstat
list_datasets[[5]] <- satis
list_datasets[[6]] <- mvad.seq

# duration ####
ndatasets <- 6
nperm <- 1000
original <- list()
for(n in 1:ndatasets){
  size_alphabet <- length(alphabet(seqdef(list_datasets[[n]])))
  duration_permuted <- matrix(0,nperm,size_alphabet)
  seqdata <- seqdef(list_datasets[[n]])
  for(m in 1:nperm){
    print(m)
    tt <- sample(1:nrow(list_datasets[[n]]),replace = T, size=nrow(list_datasets[[n]]))
    duration_permuted[m,] <- duration(seqdata[tt,])
  }
  original[[n]] <- colMeans(duration_permuted)
}

save(original,file="target_duration.Rdata")

# timing #####
set.seed(1)
ndatasets <- 6
nperm <- 1000
original <- list()
for(n in 1:ndatasets){
  size_alphabet <- length(alphabet(seqdef(list_datasets[[n]])))
  timing_permuted <- array(0,dim=c(nperm,size_alphabet,ncol(list_datasets[[n]])))
  original[[n]] <- array(0,dim=c(size_alphabet,ncol(list_datasets[[n]])))
  for(m in 1:nperm){
    tt <- sample(1:nrow(list_datasets[[n]]),replace = T, size=nrow(list_datasets[[n]]))
    timing_permuted[m,,] <- seqstatd(seqdef(list_datasets[[n]][tt,]))$Frequencies
  }
  for(a in 1:size_alphabet){
    for(b in 1:ncol(list_datasets[[n]])){
      original[[n]][a,b] <- mean(timing_permuted[,a,b])
    }
  }
}

save(original,file="target_timing.Rdata")



# sequencing #####
set.seed(1)
max_seq <- list()
for(n in 1:ndatasets){
  size_alphabet <- length(alphabet(seqdef(list_datasets[[n]])))
  sds_trans <- seqtrate(seqdss(seqdef(list_datasets[[n]])))
  max_seq[[n]] <- rep(NA,size_alphabet)
  for(s in 1:size_alphabet){
    max_seq[[n]][s] <- which(sds_trans[s,]==max(sds_trans[s,]))[1]
  }
}


nperm <- 1000
original <- list()
for(n in 1:ndatasets){
  size_alphabet <- length(alphabet(seqdef(list_datasets[[n]])))
  sequencing_permuted <- array(0,dim=c(nperm,size_alphabet,size_alphabet-2))
  original[[n]] <- matrix(0,size_alphabet,size_alphabet-2)
  for(m in 1:nperm){
    print(m)
    tt <- sample(1:nrow(list_datasets[[n]]),replace = T, size=nrow(list_datasets[[n]]))
    sequencing_permuted[m,,] <- sequencing(seqdef(list_datasets[[n]][tt,]),max_seq[[n]])
  }
  for(a in 1:size_alphabet){
    for(b in 1:(size_alphabet-2)){
      original[[n]][a,b] <- mean(sequencing_permuted[,a,b])
    }
  }
}


save(original,file="target_sequencing.Rdata")
save(max_seq,file="max_seq.Rdata")

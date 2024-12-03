library("TraMineR")
# MAR simulations ####
## 1. Simulations of MD ####
# First resampling of datasets, then simulation of MD
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

list_states_high <- list()
list_states_high[[1]] <- c("Education","Non Working")
list_states_high[[2]] <- c("Child","Other")
list_states_high[[3]] <- c("Alone","Child","One parent","Partner")
list_states_high[[4]] <- c("divorced","separated","single, never married")
list_states_high[[5]] <- c("low","average")
list_states_high[[6]] <- c("FE","HE","training")



alphabets <- list()
for(i in 1:6){
  alphabets[[i]] <- alphabet(seqdef(list_datasets[[i]]))
}
save(alphabets,file="alphabets.Rdata")

set.seed(1)
ndatasets <- 6
nsimdatasets <- 100
list_simdatasets <- list()
list_datasets_perm <- list()
for(n in 1:ndatasets){
  list_simdatasets[[n]] <- list()
  list_datasets_perm[[n]] <- list()
  for(i in 1:nsimdatasets){
    print(i)
    tt <- sample(1:nrow(list_datasets[[n]]),replace = T, size=nrow(list_datasets[[n]]))
    list_datasets_perm[[n]][[i]] <- list_datasets[[n]][tt,]
    list_simdatasets[[n]][[i]] <- make_missing_states(list_datasets[[n]][tt,],list_states_high[[n]])
  }
}

save(list_simdatasets,file="Datasets_permuted_miss.Rdata")
save(list_datasets_perm,file="Datasets_permuted.Rdata")


# Save also by dataset for computation on Baobab
list_simdatasets_tmp <- list_simdatasets
for(i in 1:ndatasets){
  list_simdatasets <- list_simdatasets_tmp[[i]]
  save(list_simdatasets,file=paste0("n",i,"_MAR.Rdata"))
}

## b. Attrition process ####

set.seed(2)
ndatasets <- 6
nsimdatasets <- 100
list_simdatasets <- list()
list_datasets_perm <- list()
for(n in 1:ndatasets){
  list_simdatasets[[n]] <- list()
  list_datasets_perm[[n]] <- list()
  for(i in 1:nsimdatasets){
    print(i)
    tt <- sample(1:nrow(list_datasets[[n]]),replace = T, size=nrow(list_datasets[[n]]))
    list_datasets_perm[[n]][[i]] <- list_datasets[[n]][tt,]
    list_simdatasets[[n]][[i]] <- make_missing_attrition(list_datasets[[n]][tt,],list_states_high[[n]])
  }
}

save(list_simdatasets,file="Datasets_permuted_miss_att.Rdata")
save(list_datasets_perm,file="Datasets_permuted_att.Rdata")


# Save the simulation by dataset for computation on Baobab
list_simdatasets_tmp <- list_simdatasets
for(i in 1:ndatasets){
  list_simdatasets <- list_simdatasets_tmp[[i]]
  save(list_simdatasets,file=paste0("n",i,"_att.Rdata"))
}


## c. Small samples ####

set.seed(3)
ndatasets <- 6
nsimdatasets <- 100
list_simdatasets <- list()
list_datasets_perm <- list()
for(n in 1:ndatasets){
  list_simdatasets[[n]] <- list()
  list_datasets_perm[[n]] <- list()
  for(i in 1:nsimdatasets){
    print(i)
    tt <- sample(1:nrow(list_datasets[[n]]),replace = T, size=200)
    list_datasets_perm[[n]][[i]] <- list_datasets[[n]][tt,]
    list_simdatasets[[n]][[i]] <- make_missing_states(list_datasets[[n]][tt,],list_states_high[[n]])
  }
}

save(list_simdatasets,file="Datasets_permuted_miss_small.Rdata")
save(list_datasets_perm,file="Datasets_permuted_small.Rdata")


# Save the simulation by dataset for computation on Baobab
list_simdatasets_tmp <- list_simdatasets
for(i in 1:ndatasets){
  list_simdatasets <- list_simdatasets_tmp[[i]]
  save(list_simdatasets,file=paste0("n",i,"_small.Rdata"))
}

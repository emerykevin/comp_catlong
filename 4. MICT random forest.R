library("seqimpute")

# MAR #####

for(n in 1:6){
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(l in 1:100){
    print(l)
    filename <- paste0("./results/seqimp_rf_P1_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P1_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=0,nfi=1,npt=1,ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_P1_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    filename <- paste0("./results/seqimp_rf_P5_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P5_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=0,nfi=1,npt=5,ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_P5_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}



for(n in 1:6){
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(l in 1:100){
    print(l)
    filename <- paste0("./results/seqimp_rf_P1F1_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P1F1_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=1,nfi=1,npt=1,ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_P1F1_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    filename <- paste0("./results/seqimp_rf_P5F5_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P5F5_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=5,nfi=5,npt=5,ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_P5F5_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

## attrition #####
for(n in 1:6){
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/seqimp_rf_P1_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P1_n",n,"_m",m,"_att.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[m]],regr="rf",np=1,nf=0,nfi=1,npt=1,ParExec=TRUE,mi=nimp,ncores=nimp,SetRNGSeed = m)
      save(seqimp,file=paste0("./results/seqimp_rf_P1_n",n,"_m",m,"_att.Rdata"))
      rm(seqimp)
      gc()
      
    }
    filename <- paste0("./results/seqimp_rf_P5_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P5_n",n,"_m",m,"_att.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[m]],regr="rf",np=5,nf=0,nfi=1,npt=5,ParExec=TRUE,mi=nimp,ncores=nimp,SetRNGSeed = m)
      save(seqimp,file=paste0("./results/seqimp_rf_P5_n",n,"_m",m,"_att.Rdata"))
      rm(seqimp)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

## small #####
for(n in 1:6){
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/seqimp_rf_P1_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P1_n",n,"_m",m,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[m]],regr="rf",np=1,nf=0,nfi=1,npt=1,ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = m)
      save(seqimp,file=paste0("./results/seqimp_rf_P1_n",n,"_m",m,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    filename <- paste0("./results/seqimp_rf_P5_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P5_n",n,"_m",m,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[m]],regr="rf",np=5,nf=0,nfi=1,npt=5,ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = m)
      save(seqimp,file=paste0("./results/seqimp_rf_P5_n",n,"_m",m,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}



for(n in 1:6){
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(l in 1:100){
    print(l)
    filename <- paste0("./results/seqimp_rf_P1F1_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P1F1_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=1,nfi=1,npt=1,ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_P1F1_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    filename <- paste0("./results/seqimp_rf_P5F5_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_P5F5_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=5,nfi=5,npt=5,ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_P5F5_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}



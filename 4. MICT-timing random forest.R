library("seqimpute")

# MAR #####
for(n in 1:6){
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(l in 1:nsimdatasets){
    print(l)
    filename <- paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=5,nfi=5,npt=5, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=1,nfi=1,npt=1, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t0_P5_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P5_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=0,nfi=1,npt=5, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P5_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t0_P1_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P1_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=0,nfi=1,npt=1, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P1_n",n,"_m",l,"_MAR.Rdata"))
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
  for(l in 1:nsimdatasets){
    print(l)
    filename <- paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=5,nfi=5,npt=5, timing=T, timeFrame=5, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=1,nfi=1,npt=1, timing=T, timeFrame=5, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t5_P5_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P5_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=0,nfi=1,npt=5, timing=T, timeFrame=5, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P5_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t5_P1_n",n,"_m",l,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P1_n",n,"_m",l,"_MAR.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=0,nfi=1,npt=1, timing=T, timeFrame=5, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P1_n",n,"_m",l,"_MAR.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
  }
  rm(list=ls())
  gc()
}

#att#####
for(n in 1:6){
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_att.Rdata"))
  for(l in 1:nsimdatasets){
    print(l)
    filename <- paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_att.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=5,nfi=5,npt=5, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_att.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_att.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=1,nfi=1,npt=1, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_att.Rdata"))
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
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",m,"_att.Rdata"))
      seqimp <- seqimpute_timing(list_simdatasets[[m]],regr="rf",np=1,nf=1,nfi=1,npt=1, timeFrame=5, ParExec=TRUE,mi=nimp,ncores=nimp,SetRNGSeed = m)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",m,"_att.Rdata"))
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
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",m,"_att.Rdata"))
      seqimp <- seqimpute_timing(list_simdatasets[[m]],regr="rf",np=5,nf=5,nfi=5,npt=5, timeFrame=5, ParExec=TRUE,mi=nimp,ncores=nimp,SetRNGSeed = m)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",m,"_att.Rdata"))
      rm(seqimp)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


#small#####

for(n in 1:6){
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(l in 1:nsimdatasets){
    print(l)
    filename <- paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=5,nfi=5,npt=5, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P5F5_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=1,nfi=1,npt=1, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P1F1_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t0_P5_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P5_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=0,nfi=1,npt=5, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P5_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t0_P1_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t0_P1_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=0,nfi=1,npt=1, timing=T, timeFrame=0, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t0_P1_n",n,"_m",l,"_small.Rdata"))
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
  for(l in 1:nsimdatasets){
    print(l)
    filename <- paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=5,nfi=5,npt=5, timing=T, timeFrame=5, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P5F5_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=1,nfi=1,npt=1, timing=T, timeFrame=5, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P1F1_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t5_P5_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P5_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=5,nf=0,nfi=1,npt=5, timing=T, timeFrame=5, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P5_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
    filename <- paste0("./results/seqimp_rf_t5_P1_n",n,"_m",l,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/seqimp_rf_t5_P1_n",n,"_m",l,"_small.Rdata"))
      seqimp <- seqimpute(list_simdatasets[[l]],regr="rf",np=1,nf=0,nfi=1,npt=1, timing=T, timeFrame=5, ParExec=TRUE,m=nimp,ncores=nimp,SetRNGSeed = l)
      save(seqimp,file=paste0("./results/seqimp_rf_t5_P1_n",n,"_m",l,"_small.Rdata"))
      rm(seqimp)
      gc()
      
    }
    
  }
  rm(list=ls())
  gc()
}


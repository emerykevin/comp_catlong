library("mice")

#PF1 ####
for(n in 1:6){
  
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom1 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          predictionMatrixmultinom1[i,i-1]<-1
        }
        if(i!=ncol(D)){
          predictionMatrixmultinom1[i,i+1]<-1
        }
      }
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom1,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_multinom)
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
    filename <- paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom1 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          predictionMatrixmultinom1[i,i-1]<-1
        }
        if(i!=ncol(D)){
          predictionMatrixmultinom1[i,i+1]<-1
        }
      }
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom1,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_att.Rdata"))
      rm(mice_multinom)
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
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom1 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          predictionMatrixmultinom1[i,i-1]<-1
        }
        if(i!=ncol(D)){
          predictionMatrixmultinom1[i,i+1]<-1
        }
      }
      mice_multinom <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom1,remove_collinear=FALSE))
      save(mice_multinom,file=paste0("./results/mice_multinom_PF1_n",n,"_m",m,"_small.Rdata"))
      rm(mice_multinom)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

# PF5 ####

for(n in 1:6){
  
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom5 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixmultinom5[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }
        }
        if(i!=ncol(D)){
          if(i>(ncol(D)-5)){
            for(j in (i+1):(ncol(D))){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }else{
            for(j in (i+1):(i+5)){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }
        }
      }
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom5,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_multinom)
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
    filename <- paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom5 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixmultinom5[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }
        }
        if(i!=ncol(D)){
          if(i>(ncol(D)-5)){
            for(j in (i+1):(ncol(D))){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }else{
            for(j in (i+1):(i+5)){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }
        }
      }
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom5,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_att.Rdata"))
      rm(mice_multinom)
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
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom5 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixmultinom5[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }
        }
        if(i!=ncol(D)){
          if(i>(ncol(D)-5)){
            for(j in (i+1):(ncol(D))){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }else{
            for(j in (i+1):(i+5)){
              predictionMatrixmultinom5[i,j] <- 1
            }
          }
        }
      }
      mice_multinom <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom5,remove_collinear=FALSE))
      save(mice_multinom,file=paste0("./results/mice_multinom_PF5_n",n,"_m",m,"_small.Rdata"))
      rm(mice_multinom)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


#P1 ####
for(n in 1:6){
  
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_P1_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_P1_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom1past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i==1){
          predictionMatrixmultinom1past[i,2]<-1
        }else{
          predictionMatrixmultinom1past[i,i-1]<-1
        }
      }
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom1past,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_P1_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_multinom)
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
    filename <- paste0("./results/mice_multinom_P1_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_P1_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom1past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i==1){
          predictionMatrixmultinom1past[i,2]<-1
        }else{
          predictionMatrixmultinom1past[i,i-1]<-1
        }
      }
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom1past,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_P1_n",n,"_m",m,"_att.Rdata"))
      rm(mice_multinom)
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
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_P1_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_P1_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom1past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i==1){
          predictionMatrixmultinom1past[i,2]<-1
        }else{
          predictionMatrixmultinom1past[i,i-1]<-1
        }
      }
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom1past,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_P1_n",n,"_m",m,"_small.Rdata"))
      rm(mice_multinom)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

#P5 ####
for(n in 1:6){
  
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_P5_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_P5_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom5past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixmultinom5past[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixmultinom5past[i,j] <- 1
            }
          }
        }
      }
      predictionMatrixmultinom5past[1,2]<-1
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom5past,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_P5_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_multinom)
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
    filename <- paste0("./results/mice_multinom_P5_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_P5_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom5past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixmultinom5past[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixmultinom5past[i,j] <- 1
            }
          }
        }
      }
      predictionMatrixmultinom5past[1,2]<-1
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom5past,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_P5_n",n,"_m",m,"_att.Rdata"))
      rm(mice_multinom)
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
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_P5_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_P5_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom5past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixmultinom5past[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixmultinom5past[i,j] <- 1
            }
          }
        }
      }
      predictionMatrixmultinom5past[1,2]<-1
      mice_multinom <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixmultinom5past,remove_collinear=FALSE))
      save(mice_multinom,file=paste0("./results/mice_multinom_P5_n",n,"_m",m,"_small.Rdata"))
      rm(mice_multinom)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

##past ###

for(n in 1:6){
  
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:100){
    print(m)
    filename <- paste0("./results/mice_multinom_past_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_past_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixmultinom[i,i] <- 0
      }
      predictionMatrixmultinompast <- predictionMatrixmultinom
      predictionMatrixmultinompast[upper.tri(predictionMatrixmultinompast)]<-0
      predictionMatrixmultinompast[1,2]<-1
      
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinompast,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_past_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_multinom)
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
  for(m in 1:100){
    print(m)
    filename <- paste0("./results/mice_multinom_past_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_past_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixmultinom[i,i] <- 0
      }
      predictionMatrixmultinompast <- predictionMatrixmultinom
      predictionMatrixmultinompast[upper.tri(predictionMatrixmultinompast)]<-0
      predictionMatrixmultinompast[1,2]<-1
      
      mice_multinom <- mice(D,m=nimp,predictorMatrix=predictionMatrixmultinompast,remove_collinear=FALSE)
      save(mice_multinom,file=paste0("./results/mice_multinom_past_n",n,"_m",m,"_att.Rdata"))
      rm(mice_multinom)
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
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_multinom_past_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_multinom_past_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixmultinom <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixmultinom[i,i] <- 0
      }
      predictionMatrixmultinompast <- predictionMatrixmultinom
      predictionMatrixmultinompast[upper.tri(predictionMatrixmultinompast)]<-0
      predictionMatrixmultinompast[1,2]<-1
      
      mice_multinom <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixmultinompast,remove_collinear=FALSE))
      save(mice_multinom,file=paste0("./results/mice_multinom_past_n",n,"_m",m,"_small.Rdata"))
      rm(mice_multinom)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

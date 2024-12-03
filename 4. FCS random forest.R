library("mice")
#PF1####

for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_PF1_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_PF1_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf1 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          predictionMatrixrf1[i,i-1]<-1
        }
        if(i!=ncol(D)){
          predictionMatrixrf1[i,i+1]<-1
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf1,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_PF1_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_PF1_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_PF1_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf1 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          predictionMatrixrf1[i,i-1]<-1
        }
        if(i!=ncol(D)){
          predictionMatrixrf1[i,i+1]<-1
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf1,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_PF1_n",n,"_m",m,"_att.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_PF1_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_PF1_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf1 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          predictionMatrixrf1[i,i-1]<-1
        }
        if(i!=ncol(D)){
          predictionMatrixrf1[i,i+1]<-1
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf1,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_PF1_n",n,"_m",m,"_small.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

#PF5####

for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_PF5_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_PF5_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf5 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixrf5[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixrf5[i,j] <- 1
            }
          }
        }
        if(i!=ncol(D)){
          if(i>(ncol(D)-5)){
            for(j in (i+1):(ncol(D))){
              predictionMatrixrf5[i,j] <- 1
            }
          }else{
            for(j in (i+1):(i+5)){
              predictionMatrixrf5[i,j] <- 1
            }
          }
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf5,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_PF5_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_PF5_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_PF5_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf5 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixrf5[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixrf5[i,j] <- 1
            }
          }
        }
        if(i!=ncol(D)){
          if(i>(ncol(D)-5)){
            for(j in (i+1):(ncol(D))){
              predictionMatrixrf5[i,j] <- 1
            }
          }else{
            for(j in (i+1):(i+5)){
              predictionMatrixrf5[i,j] <- 1
            }
          }
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf5,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_PF5_n",n,"_m",m,"_att.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_PF5_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_PF5_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf5 <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixrf5[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixrf5[i,j] <- 1
            }
          }
        }
        if(i!=ncol(D)){
          if(i>(ncol(D)-5)){
            for(j in (i+1):(ncol(D))){
              predictionMatrixrf5[i,j] <- 1
            }
          }else{
            for(j in (i+1):(i+5)){
              predictionMatrixrf5[i,j] <- 1
            }
          }
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf5,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_PF5_n",n,"_m",m,"_small.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

#all####

for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_all_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_all_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixrf[i,i] <- 0
      }
      mice_rf <- mice(D,m=nimp,predictorMatrix=predictionMatrixrf,remove_collinear=FALSE,method="rf")
      save(mice_rf,file=paste0("./results/mice_rf_all_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:100){
    print(m)
    filename <- paste0("./results/mice_rf_all_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_all_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixrf[i,i] <- 0
      }
      mice_rf <- mice(D,m=nimp,predictorMatrix=predictionMatrixrf,remove_collinear=FALSE,method="rf")
      save(mice_rf,file=paste0("./results/mice_rf_all_n",n,"_m",m,"_att.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_all_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_all_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixrf[i,i] <- 0
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_all_n",n,"_m",m,"_small.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

#P1 ####
for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_P1_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_P1_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf1past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i==1){
          predictionMatrixrf1past[i,2]<-1
        }else{
          predictionMatrixrf1past[i,i-1]<-1
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf1past,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_P1_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_P1_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_P1_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf1past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i==1){
          predictionMatrixrf1past[i,2]<-1
        }else{
          predictionMatrixrf1past[i,i-1]<-1
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf1past,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_P1_n",n,"_m",m,"_att.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}



for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_P1_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_P1_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf1past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i==1){
          predictionMatrixrf1past[i,2]<-1
        }else{
          predictionMatrixrf1past[i,i-1]<-1
        }
      }
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf1past,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_P1_n",n,"_m",m,"_small.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

#P5 #####


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_P5_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_P5_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf5past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixrf5past[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixrf5past[i,j] <- 1
            }
          }
        }
      }
      predictionMatrixrf5past[1,2]<-1
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf5past,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_P5_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}



for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_P5_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_P5_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf5past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixrf5past[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixrf5past[i,j] <- 1
            }
          }
        }
      }
      predictionMatrixrf5past[1,2]<-1
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf5past,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_P5_n",n,"_m",m,"_att.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}



for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_P5_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_P5_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf5past <- matrix(0,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        if(i>1){
          if(i<=6){
            for(j in 1:(i-1)){
              predictionMatrixrf5past[i,j]<-1
            }
          }else{
            for(j in (i-5):(i-1)){
              predictionMatrixrf5past[i,j] <- 1
            }
          }
        }
      }
      predictionMatrixrf5past[1,2]<-1
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrf5past,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_P5_n",n,"_m",m,"_small.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}

#past ####
for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_MAR.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_past_n",n,"_m",m,"_MAR.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_past_n",n,"_m",m,"_MAR.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixrf[i,i] <- 0
      }
      predictionMatrixrfpast <- predictionMatrixrf
      predictionMatrixrfpast[upper.tri(predictionMatrixrfpast)]<-0
      predictionMatrixrfpast[1,2]<-1
      
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrfpast,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_past_n",n,"_m",m,"_MAR.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_att.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_past_n",n,"_m",m,"_att.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_past_n",n,"_m",m,"_att.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixrf[i,i] <- 0
      }
      predictionMatrixrfpast <- predictionMatrixrf
      predictionMatrixrfpast[upper.tri(predictionMatrixrfpast)]<-0
      predictionMatrixrfpast[1,2]<-1
      
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrfpast,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_past_n",n,"_m",m,"_att.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


for(n in 1:6){
  set.seed(n)
  nsimdatasets <- 100
  nimp <- 10
  load(paste0("n",n,"_small.Rdata"))
  for(m in 1:nsimdatasets){
    print(m)
    filename <- paste0("./results/mice_rf_past_n",n,"_m",m,"_small.Rdata")
    if(!file.exists(filename)){
      file.create(paste0("./results/mice_rf_past_n",n,"_m",m,"_small.Rdata"))
      D <- list_simdatasets[[m]]
      for(j in 1:ncol(D)){
        D[,j] <- droplevels(D[,j])
      }
      predictionMatrixrf <- matrix(1,ncol(D),ncol(D))
      for(i in 1:ncol(D)){
        predictionMatrixrf[i,i] <- 0
      }
      predictionMatrixrfpast <- predictionMatrixrf
      predictionMatrixrfpast[upper.tri(predictionMatrixrfpast)]<-0
      predictionMatrixrfpast[1,2]<-1
      
      mice_rf <- try(mice(D,m=nimp,predictorMatrix=predictionMatrixrfpast,remove_collinear=FALSE,method="rf"))
      save(mice_rf,file=paste0("./results/mice_rf_past_n",n,"_m",m,"_small.Rdata"))
      rm(mice_rf)
      gc()
      
    }
  }
  rm(list=ls())
  gc()
}


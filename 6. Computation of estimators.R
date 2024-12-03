source("duration.R")
library("TraMineR")
library("mice")
predictors <- c("P1","P5","PF1","PF5","past","all")
mecha <- c("MAR","att","small")

# 1. duration #####
## mice random forest ####
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/mice_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/mice_rf_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_duration.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- duration_mi(mice_rf,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

##mice multinomial ####
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/mice_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/mice_multinom_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_duration.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- duration_mi(mice_multinom,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

## MICT random forest #####
mecha <- c("MAR","att","small")
predictors <- c("P1","P5","P1F1","P5F5")
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/seqimp_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/seqimpute_rf_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_duration.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- duration_mi(seqimp,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

## MICT multinomial #####
predictors <- c("P1","P5","P1F1","P5F5")
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/seqimp_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/seqimpute_multinom_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_duration.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- duration_mi(seqimp,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}


## MICT-timing random forest ####
mecha <- c("MAR","att","small")
predictors <- c("P1","P5","P1F1","P5F5")
timing <- c("t0","t5")


ndatasets <- 6
nsimdatasets <- 100
for(l in 1:length(timing)){
  for(k in 1:length(mecha)){
    for(i in 1:length(predictors)){
      for(n in 1:ndatasets){
        for(m in 1:nsimdatasets){
          filename1 <- paste0("./results/seqimp_rf_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
          if(file.exists(filename1)){
            filename2 <- paste0("./results/seqimp_rf_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_duration.Rdata")
            if(!file.exists(filename2)){
              file.create(filename2)
              load(filename1)
              results <- duration_mi(seqimp,nimp=10,nsamples=1000)
              save(results,file=filename2)
            }
          }
        }
      }
    }
  }
}

## MICT-timing multinomial ####

ndatasets <- 6
nsimdatasets <- 100
for(l in 1:length(timing)){
  for(k in 1:length(mecha)){
    for(i in 1:length(predictors)){
      for(n in 1:ndatasets){
        for(m in 1:nsimdatasets){
          filename1 <- paste0("./results/seqimp_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
          if(file.exists(filename1)){
            filename2 <- paste0("./results/seqimp_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_duration.Rdata")
            if(!file.exists(filename2)){
              file.create(filename2)
              load(filename1)
              results <- duration_mi(seqimp,nimp=10,nsamples=1000)
              save(results,file=filename2)
            }
          }
        }
      }
    }
  }
}

##vlmc#####
mecha <- c("MAR","att","small")
predictors <- c("G1","G2")
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_duration.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- duration_mi(vlmc,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

# 2. sequencding ####
## mice random forest####
source("sequencing.R")
predictors <- c("P1","P5","PF1","PF5","past","all")
mecha <- c("MAR","att","small")

load("max_seq.Rdata")

ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/mice_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/mice_rf_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_sequencing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- sequencing_mi(mice_rf,nimp=10,nsamples=1000,max_seq[[n]])
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

## mice multinom ####
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/mice_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/mice_multinom_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_sequencing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- sequencing_mi(mice_multinom,nimp=10,nsamples=1000,max_seq[[n]])
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

## MICT random forest #####
mecha <- c("MAR","att","small")
predictors <- c("P1","P5","P1F1","P5F5")

load("max_seq.Rdata")
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/seqimp_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/seqimpute_rf_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_sequencing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- sequencing_mi(seqimp,nimp=10,nsamples=1000,max_seq[[n]])
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

## MICT multinom ####
predictors <- c("P1","P5","P1F1","P5F5")
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/seqimp_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/seqimpute_multinom_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_sequencing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- sequencing_mi(seqimp,nimp=10,nsamples=1000,max_seq[[n]])
            save(results,file=filename2)
          }
        }
      }
    }
  }
}


##MICT-timing random forest ####
mecha <- c("MAR","att","small")
predictors <- c("P1","P5","P1F1","P5F5")
timing <- c("t0","t5")

load("max_seq.Rdata")



ndatasets <- 6
nsimdatasets <- 100
for(l in 1:length(timing)){
  for(k in 1:length(mecha)){
    for(i in 1:length(predictors)){
      for(n in 1:ndatasets){
        for(m in 1:nsimdatasets){
          filename1 <- paste0("./results/seqimp_rf_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
          if(file.exists(filename1)){
            filename2 <- paste0("./results/seqimp_rf_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_sequencing.Rdata")
            if(!file.exists(filename2)){
              file.create(filename2)
              load(filename1)
              results <- sequencing_mi(seqimp,nimp=10,nsamples=1000,max_seq[[n]])
              save(results,file=filename2)
            }
          }
        }
      }
    }
  }
}


## MICT-timing multinom ####
ndatasets <- 6
nsimdatasets <- 100
for(l in 1:length(timing)){
  for(k in 1:length(mecha)){
    for(i in 1:length(predictors)){
      for(n in 1:ndatasets){
        for(m in 1:nsimdatasets){
          filename1 <- paste0("./results/seqimp_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
          if(file.exists(filename1)){
            filename2 <- paste0("./results/seqimp_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_sequencing.Rdata")
            if(!file.exists(filename2)){
              file.create(filename2)
              load(filename1)
              results <- sequencing_mi(seqimp,nimp=10,nsamples=1000,max_seq[[n]])
              save(results,file=filename2)
            }
          }
        }
      }
    }
  }
}

## vlmc ####
mecha <- c("MAR","att","small")
predictors <- c("G1","G2")

load("max_seq.Rdata")

ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_sequencing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- sequencing_mi(vlmc,nimp=10,nsamples=1000,max_seq[[n]])
            save(results,file=filename2)
          }
        }
      }
    }
  }
}


# 3. timing ####
## mice random forest #####
source("timing.R")
library("TraMineR")
library("mice")
predictors <- c("P1","P5","PF1","PF5","past","all")
mecha <- c("MAR","att","small")

ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/mice_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/mice_rf_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_timing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- timing_mi(mice_rf,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

## mice multinom ####
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/mice_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/mice_multinom_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_timing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- timing_mi(mice_multinom,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}


## MICT random forest ####
mecha <- c("MAR","att","small")
predictors <- c("P1","P5","P1F1","P5F5")


ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/seqimp_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/seqimpute_rf_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_timing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- timing_mi(seqimp,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

## MICT multinom #####
predictors <- c("P1","P5","P1F1","P5F5")
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/seqimp_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/seqimpute_multinom_",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_timing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- timing_mi(seqimp,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}

## MICT.timing random forest ####
mecha <- c("MAR","att","small")
predictors <- c("P1","P5","P1F1","P5F5")
timing <- c("t0","t5")


ndatasets <- 6
nsimdatasets <- 100
for(l in 1:length(timing)){
  for(k in 1:length(mecha)){
    for(i in 1:length(predictors)){
      for(n in 1:ndatasets){
        for(m in 1:nsimdatasets){
          filename1 <- paste0("./results/seqimp_rf_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
          if(file.exists(filename1)){
            filename2 <- paste0("./results/seqimp_rf_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_timing.Rdata")
            if(!file.exists(filename2)){
              file.create(filename2)
              load(filename1)
              results <- timing_mi(seqimp,nimp=10,nsamples=1000)
              save(results,file=filename2)
            }
          }
        }
      }
    }
  }
}

## MICT-timing multinomial ####
ndatasets <- 6
nsimdatasets <- 100
for(l in 1:length(timing)){
  for(k in 1:length(mecha)){
    for(i in 1:length(predictors)){
      for(n in 1:ndatasets){
        for(m in 1:nsimdatasets){
          filename1 <- paste0("./results/seqimp_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
          if(file.exists(filename1)){
            filename2 <- paste0("./results/seqimp_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_timing.Rdata")
            if(!file.exists(filename2)){
              file.create(filename2)
              load(filename1)
              results <- timing_mi(seqimp,nimp=10,nsamples=1000)
              save(results,file=filename2)
            }
          }
        }
      }
    }
  }
}

## vlmc ####
mecha <- c("MAR","att","small")
predictors <- c("G1","G2")
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)){
          filename2 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",
                              m,"_",mecha[k],"_timing.Rdata")
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            results <- timing_mi(vlmc,nimp=10,nsamples=1000)
            save(results,file=filename2)
          }
        }
      }
    }
  }
}


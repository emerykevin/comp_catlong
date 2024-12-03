source("duration.R")
library("TraMineR")
library("mice")
load("alphabets.Rdata")
predictors <- c("P1","P5","PF1","PF5","past","all")
mecha <- c("MAR","att","small")

ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/mice_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],".Rdata")
        filename2 <- paste0("./results/mice_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)&!file.exists(filename2)){
          load(filename1)
          if(!inherits(mice_rf,"try-error")){
            mice_rf <- transform_mice(mice_rf,10,alphabets[[n]])
            save(mice_rf,file=filename2)
          }
        }
      }
    }
  }
}


ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/mice_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],".Rdata")
        filename2 <- paste0("./results/mice_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        if(file.exists(filename1)&!file.exists(filename2)){
            load(filename1)
            if(!inherits(mice_multinom,"try-error")){
              mice_multinom <- transform_mice(mice_multinom,10,alphabets[[n]])
              save(mice_multinom,file=filename2)
          }
        }
      }
    }
  }
}



mecha <- c("MAR","att","small")
predictors <- c("P1","P5","PF1","PF5","P1F1","P5F5")

ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/seqimp_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],".Rdata")
        filename2 <- paste0("./results/seqimp_rf_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        
        if(file.exists(filename1)){
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            seqimp <- transform_seqimpute(seqimp,10,alphabets[[n]])
            save(seqimp,file=filename2)
          }
        }
      }
    }
  }
}

ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/seqimp",predictors[i],"_n",n,"_m",m,"_",mecha[k],".Rdata")
        filename2 <- paste0("./results/seqimp_multinom_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        
        if(file.exists(filename1)){
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            seqimp <- transform_seqimpute(seqimp,10,alphabets[[n]])
            save(seqimp,file=filename2)
          }
        }
      }
    }
  }
}

mecha <- c("MAR","att","small")
predictors <- c("P1","P5","PF1","PF5","P1F1","P5F5")

timing <- c("t0","t5")


ndatasets <- 6
nsimdatasets <- 100
for(l in 1:length(timing)){
  for(k in 1:length(mecha)){
    for(i in 1:length(predictors)){
      for(n in 1:ndatasets){
        for(m in 1:nsimdatasets){
          filename1 <- paste0("./results/seqimp_rf_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],".Rdata")
          filename2 <- paste0("./results/seqimp_rf_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
          
          if(file.exists(filename1)){
            if(!file.exists(filename2)){
              file.create(filename2)
              load(filename1)
              seqimp <- transform_seqimpute(seqimp,10,alphabets[[n]])
              save(seqimp,file=filename2)
            }
          }
        }
      }
    }
  }
}


ndatasets <- 6
nsimdatasets <- 100
for(l in 1:length(timing)){
  for(k in 1:length(mecha)){
    for(i in 1:length(predictors)){
      for(n in 1:ndatasets){
        for(m in 1:nsimdatasets){
          filename1 <- paste0("./results/seqimp_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],".Rdata")
          filename2 <- paste0("./results/seqimp_",timing[l],"_",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
          
          if(file.exists(filename1)){
            if(!file.exists(filename2)){
              file.create(filename2)
              load(filename1)
              seqimp <- transform_seqimpute(seqimp,10,alphabets[[n]])
              save(seqimp,file=filename2)
            }
          }
        }
      }
    }
  }
}


mecha <- c("MAR","att","small")
predictors <- c("G1")


ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",m,"_",mecha[k],".Rdata")
        filename2 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        
        if(file.exists(filename1)){
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            vlmc <- transform_vlmc(vlmcG1,10,alphabets[[n]])
            save(vlmc,file=filename2)
          }
        }
      }
    }
  }
}


predictors <- c("G2")
ndatasets <- 6
nsimdatasets <- 100
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(n in 1:ndatasets){
      for(m in 1:nsimdatasets){
        filename1 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",m,"_",mecha[k],".Rdata")
        filename2 <- paste0("./results/vlmc",predictors[i],"_n",n,"_m",m,"_",mecha[k],"_seq.Rdata")
        
        if(file.exists(filename1)){
          if(!file.exists(filename2)){
            file.create(filename2)
            load(filename1)
            vlmc <- transform_vlmc(vlmcG2,10,alphabets[[n]])
            save(vlmc,file=filename2)
          }
        }
      }
    }
  }
}

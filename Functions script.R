# Conversion imputed dataset on the same format:
# First column imputation number i 
miceconvert <- function(micedata,ndatasets,nsimdatasets,nimp){
  dataSI<-micedata
  if(ndatasets==1){
      for(j in 1:nsimdatasets){
        nr <- nrow(complete(micedata[[1]],1))
        if(class(micedata[[j]])!="try-error"){
          for(k in 1:nimp){
            if(k==1){
              tmp <- cbind(replicate(nr,k),complete(micedata[[j]],k))
            }else{
              tmp <- rbind(tmp,cbind(replicate(nr,k),complete(micedata[[j]],k)))
            }
          }
          dataSI[[j]]<-tmp
          colnames(dataSI[[j]])[1]<-".imp"
        }
      }
    }else{
    for(i in 1:ndatasets){
      for(j in 1:nsimdatasets){
        nr <- nrow(complete(micedata[[i]][[1]],1))
        if(class(micedata[[i]][[j]])!="try-error"){
          for(k in 1:nimp){
            if(k==1){
              tmp <- cbind(replicate(nr,k),complete(micedata[[i]][[j]],k))
            }else{
              tmp <- rbind(tmp,cbind(replicate(nr,k),complete(micedata[[i]][[j]],k)))
            }
          }
          dataSI[[i]][[j]]<-tmp
          colnames(dataSI[[i]][[j]])[1]<-".imp"
        }
      }
    }
  }
  return(dataSI)
}


seqimpconvert <- function(seqimpdata,ndatasets,nsimdatasets,nimp){
  if(ndatasets>1){
    if(nimp>1){
      dataSI<-seqimpdata
      for(i in 1:ndatasets){
        for(j in 1:nsimdatasets){
          dataSI[[i]][[j]]<-dataSI[[i]][[j]][,-c(2)]
        }
      }
    }else{
      dataSI<-seqimpdata
      for(i in 1:ndatasets){
        for(j in 1:nsimdatasets){
          nr <- nrow(dataSI[[i]][[j]])
          dataSI[[i]][[j]]<-cbind(replicate(nr,1),dataSI[[i]][[j]])
          colnames(dataSI[[i]][[j]])[1]<-".imp"
          
        }
      }
    }
  }else{
    if(nimp>1){
      dataSI<-seqimpdata
      for(i in 1:ndatasets){
        for(j in 1:nsimdatasets){
          dataSI[[j]]<-dataSI[[j]][,-c(2)]
        }
      }
    }else{
      dataSI<-seqimpdata
      for(i in 1:ndatasets){
        for(j in 1:nsimdatasets){
          nr <- nrow(dataSI[[j]])
          dataSI[[j]]<-cbind(replicate(nr,1),dataSI[[j]])
          colnames(dataSI[[j]])[1]<-".imp"
          
        }
      }
    }
  }
  return(dataSI)
}


miceconvert_samples <- function(micedata,ndatasets,nsimdatasets,nimp,nsamples){
  dataSI<-micedata
  for(i in 1:ndatasets){
    for(o in 1:nsamples){
      for(j in 1:nsimdatasets){
        if(class(micedata[[i]][[o]][[j]])!="try-error"){
          nr <- nrow(complete(micedata[[i]][[o]][[j]],1))
          
          for(k in 1:nimp){
            if(k==1){
              tmp <- cbind(replicate(nr,k),complete(micedata[[i]][[o]][[j]],k))
            }else{
              tmp <- rbind(tmp,cbind(replicate(nr,k),complete(micedata[[i]][[o]][[j]],k)))
            }
          }
          dataSI[[i]][[o]][[j]]<-tmp
          colnames(dataSI[[i]][[o]][[j]])[1]<-".imp"
        }
      }
    }
  }
  return(dataSI)
}


seqimpconvert_samples <- function(seqimpdata,ndatasets,nsimdatasets,nimp,nsamples){
  if(nimp>1){
    dataSI<-seqimpdata
    for(i in 1:ndatasets){
      for(o in 1:nsamples){
        for(j in 1:nsimdatasets){
          if(class(dataSI[[i]][[o]][[j]])[1]!="try-error"){
            dataSI[[i]][[o]][[j]]<-dataSI[[i]][[o]][[j]][,-c(2)]
          }
        }
      }
    }
  }else{
    dataSI<-seqimpdata
    for(i in 1:ndatasets){
      for(o in 1:nsamples){
        for(j in 1:nsimdatasets){
          if(class(dataSI[[i]][[o]][[j]])[1]!="try-error"){
            
            nr <- nrow(dataSI[[i]][[o]][[j]])
            dataSI[[i]][[o]][[j]]<-cbind(replicate(nr,1),dataSI[[i]][[o]][[j]])
            colnames(dataSI[[i]][[o]][[j]])[1]<-".imp"
          }
          
        }
      }
    }
  }
  return(dataSI)
}

vlmcconvert <- function(micedata,ndatasets,nsimdatasets,nimp){
  dataSI<-micedata
  if(nimp>1){
    for(i in 1:ndatasets){
      nr <- nrow(dataSI[[i]][[1]][[1]])
      for(j in 1:nsimdatasets){
        for(k in 1:nimp){
          if(k==1){
            tmp <- cbind(replicate(nr,k),micedata[[i]][[j]][[k]])
          }else{
            tmp <- rbind(tmp,cbind(replicate(nr,k),micedata[[i]][[j]][[k]]))
          }
        }
        dataSI[[i]][[j]]<-tmp
        colnames(dataSI[[i]][[j]])[1]<-".imp"
      }
    }
  }else{
    for(i in 1:ndatasets){
      nr <- nrow(dataSI[[i]][[1]])
      for(j in 1:nsimdatasets){
        dataSI[[i]][[j]] <- cbind(replicate(nr,1),micedata[[i]][[j]])
        colnames(dataSI[[i]][[j]])[1]<-".imp"
      }
    }
  }
  return(dataSI)
}


invert_indices <- function(dataset, ndatasets, nsamples){
  inv_data <- list()
  for(o in 1:nsamples){
    inv_data[[o]] <- list()
    for(n in 1:ndatasets){
      inv_data[[o]][[n]] <- dataset[[n]][[o]]
    }
  }
  return(inv_data)
}

computeaccuracy <- function(impdata,odata,simdata,nimp){
  acc <- rep(0,nimp)
  for(j in 1:nimp){
    acc[j] <- sum(impdata[impdata$.imp==j,2:ncol(impdata)][is.na(simdata)]==odata[is.na(simdata)])/sum(is.na(simdata))
    
  }
  return(acc)
}

diffstatefreq <- function(imputed,original){
  return(sum(abs(seqstatd(original)$Frequencies-seqstatd(imputed)$Frequencies))/(ncol(original)))
  
}

difftransrate <- function(imputed,original){
  dist <- colSums(seqistatd(original))
  dist <- dist/sum(dist)
  abs_diff <- abs(seqtrate(original)-seqtrate(imputed))
  return(sum(abs_diff*dist))
}

difftimevartransrate <- function(imputed,original){
  dist <- seqstatd(original)$Frequencies
  abs_diff <-abs(seqtrate(original, time.varying=TRUE)-seqtrate(imputed, time.varying=TRUE))
  sum_dist <- 0
  for(i in 1:(ncol(original)-1)){
    sum_dist <- sum_dist+abs_diff[,,i]*dist[,i]
  }
  return(sum(sum_dist)/(ncol(original)-1))
}

difftransratedistinct <- function(imputed,original){
  dist <- colSums(seqistatd(seqdss(original)))
  dist <- dist/sum(dist)
  abs_diff <-abs(seqtrate(seqdss(original))-seqtrate(seqdss(imputed)))
  return(sum(abs_diff*dist))
}

diffmeantime <- function(imputed,original){
  return(sum(abs(colMeans(seqistatd(original))-colMeans(seqistatd(imputed)))))
}

diffntrans <- function(imputed,original){
  return(mean(seqtransn(original))-mean(seqtransn(imputed)))
}


difflentropy <- function(imputed,original){
  return(mean(seqient(imputed))-mean(seqient(original)))
}

accuracy_ordered_satisfaction <- function(dat,D,j,data_miss){
  tt <- table(factor(dat[dat$.imp==j,2:ncol(dat)][is.na(data_miss)],levels=c("low","average","high","very high")),factor(D[is.na(data_miss)],levels=c("low","average","high","very high")))
  sdist<-0
  for(u in 1:(nrow(tt)-1)){
    for(v in (u+1):nrow(tt)){
      sdist<-sdist+abs(u-v)*tt[u,v]
    }
  }
  return(sdist/sum(tt))
}

accuracy_ordered_satisfaction_null <- function(dat,D,j,data_miss){
  tt <- table(factor(dat[is.na(data_miss)],levels=c("low","average","high","very high")),factor(D[is.na(data_miss)],levels=c("low","average","high","very high")))
  sdist<-0
  for(u in 1:(nrow(tt)-1)){
    for(v in (u+1):nrow(tt)){
      sdist<-sdist+abs(u-v)*tt[u,v]
    }
  }
  return(sdist/sum(tt))
}




seqmspell <- function(original, imputed, with.missing=TRUE, FUN=mean){
  olength <- ncol(original)
  mspell <- function(seqdata){
    spells <- data.frame(dss =factor(as.vector(as.matrix(seqdss(seqdata, with.missing=with.missing)))), dur=as.vector(seqdur(seqdata, with.missing=with.missing)))
    sp <- droplevels(subset(spells, !is.na(dur)))
    vec <- by(sp$dur, list(sp$dss), FUN=FUN)
    return(vec)
  }
  print(original <- mspell(original))
  print(imputed <- mspell(imputed))
  
  allstates <- unique(names(original), names(imputed))
  getNamed <- function(data){
    data <- data[allstates]
    return(ifelse(is.na(data), 0, data))
  }
  return(sum(abs(getNamed(original)-getNamed(imputed)))/(length(allstates)*olength))
  
}

make_missing_halfdataset<- function(data){
  sizehalf <- round(0.6*nrow(data))
  rowsmiss <- sample(1:nrow(data),size=sizehalf,replace=FALSE)
  matrix_missing <- matrix(NA,nrow(data),ncol(data))
  for(i in 1:length(rowsmiss)){
    nmis <- ncol(data)
    while(nmis>floor(0.75*ncol(data))){
      for(j in 1:ncol(data)){
        if(j==1){
          matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(6,94))
        }else{
          if(matrix_missing[rowsmiss[i],j-1]==1){
            matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(6,94))
          }else{
            matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(66,34))
          }
        }
      }
      nmis <- sum(matrix_missing[rowsmiss[i],]==0)
    }
  }
  data[matrix_missing==0] <- NA
  return(data)
}


make_missing_states<- function(data,states_high){
  sizehalf <- round(0.6*nrow(data))
  rowsmiss <- sample(1:nrow(data),size=sizehalf,replace=FALSE)
  matrix_missing <- matrix(NA,nrow(data),ncol(data))
  for(i in 1:length(rowsmiss)){
    nmis <- ncol(data)
    while(nmis>floor(0.75*ncol(data))){
      for(j in 1:ncol(data)){
        if(j==1){
          matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(6,94))
        }else{
          if(matrix_missing[rowsmiss[i],j-1]==1){
            if(data[rowsmiss[i],j-1]%in%states_high){
              matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(20,80))
            }else{
              matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(3,97))
              
            }
          }else{
            matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(66,34))
          }
        }
      }
      nmis <- sum(matrix_missing[rowsmiss[i],]==0)
    }
  }
  data[matrix_missing==0] <- NA
  return(data)
}


make_missing_mixed<- function(data,states_high){
  sizehalf <- round(0.6*nrow(data))
  rowsmiss <- sample(1:nrow(data),size=sizehalf,replace=FALSE)
  matrix_missing <- matrix(NA,nrow(data),ncol(data))
  for(i in 1:length(rowsmiss)){
    nmis <- ncol(data)
    while(nmis>floor(0.75*ncol(data))){
      for(j in 1:ncol(data)){
        if(j==1){
          matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(7,93))
        }else{
          if(matrix_missing[rowsmiss[i],j-1]==1){
            if(data[rowsmiss[i],j-1]%in%states_high){
              matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(20,80))
            }else{
              matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(3,97))
              
            }
          }else{
            if(data[rowsmiss[i],j-1]%in%states_high){
              matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(76,24))
            }else{
              matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(56,44))
              
            }
            matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(66,34))
          }
        }
      }
      nmis <- sum(matrix_missing[rowsmiss[i],]==0)
    }
  }
  data[matrix_missing==0] <- NA
  return(data)
}



make_missing_trans<- function(data){
  sizehalf <- round(0.6*nrow(data))
  rowsmiss <- sample(1:nrow(data),size=sizehalf,replace=FALSE)
  matrix_missing <- matrix(NA,nrow(data),ncol(data))
  for(i in 1:length(rowsmiss)){
    nmis <- ncol(data)
    while(nmis>floor(0.75*ncol(data))){
      for(j in 1:ncol(data)){
        if(j==1){
          matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(6,94))
        }else{
          if(matrix_missing[rowsmiss[i],j-1]==1){
            if(data[rowsmiss[i],j]==data[rowsmiss[i],j-1]){
              matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(5,95))
            }else{
              matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(10,90))
              
            }
          }else{
            matrix_missing[rowsmiss[i],j] <- sample(x=c(0,1),size=1,p=c(66,34))
          }
        }
      }
      nmis <- sum(matrix_missing[rowsmiss[i],]==0)
    }
  }
  data[matrix_missing==0] <- NA
  return(data)
}


make_missing_attrition<- function(data,states_high){
  sizehalf <- round(0.6*nrow(data))
  rowsmiss <- sample(1:nrow(data),size=sizehalf,replace=FALSE)
  matrix_missing <- matrix(NA,nrow(data),ncol(data))
  for(i in 1:length(rowsmiss)){
    nmis <- ncol(data)
    for(j in floor(0.5*ncol(data)):ncol(data)){
      if(data[rowsmiss[i],j-1]%in%states_high){
        tt <- sample(x=c(0,1),size=1,p=c(10,80))
        matrix_missing[rowsmiss[i],j] <- tt
      }else{
        tt <- sample(x=c(0,1),size=1,p=c(1.5,97))
        matrix_missing[rowsmiss[i],j] <- tt
      }
      if(tt==0){
        matrix_missing[rowsmiss[i],j:ncol(data)] <- 0
        break
      }
    }
  }
  data[matrix_missing==0] <- NA
  return(data)
}


sequencing <- function(seqdata,which_maxseq){
  size_alphabet <- length(alphabet(seqdata))
  sds_trans <- seqtrate(seqdss(seqdata))
  odd_transitions <- matrix(0,size_alphabet,size_alphabet-2)
  for(j in 1:size_alphabet){
    count <- 1
    for(k in 1:size_alphabet){
      if(k!=which_maxseq[j] & k!=j){
        odd_transitions[j,count] <- sds_trans[j,k]/sds_trans[j,which_maxseq[j]]
        count <- count+1
      }
    }
  }
  return(odd_transitions)
}

sequencing_mi_seqimpute <- function(dataset, nsimdatasets, nimp, nsamples,which_maxseq){
  nobs <- nrow(dataset[[1]][[1]])
  nstates <- length(alphabet(seqdef(dataset[[1]][[1]])))
  sequencing_values <- array(0,dim=c(nsimdatasets,nimp,nsamples,nstates*(nstates-2)))
  var_sequencing <- array(0,dim=c(nsimdatasets,nimp,nstates*(nstates-2)))
  param_mi_sequencing <- array(0,dim=c(nsimdatasets,nimp,nstates*(nstates-2)))
  for(m in 1:nsimdatasets){
    for(n in 1:nimp){
      for(s in 1:nsamples){
        tt <- sample(1:nobs,replace = TRUE,size=nobs)
        sequencing_values[m,n,s,] <- sequencing(seqdef(dataset[[m]][[n]][tt,]),which_maxseq[m,])
      }
      for(i in 1:(nstates*(nstates-2))){
        var_sequencing[m,n,i] <- var(sequencing_values[m,n,,i])
        param_mi_sequencing[m,n,i] <- mean(sequencing_values[m,n,,i])
      }
    }
  }
  
  param_sequencing <- array(0,dim=c(nsimdatasets,nstates*(nstates-2)))
  var_param_sequencing <- array(0,dim=c(nsimdatasets,nstates*(nstates-2)))
  for(m in 1:nsimdatasets){
    for(i in 1:(nstates*(nstates-2))){
      var_param_sequencing[m,i] <- Rubin_rule(param_mi_sequencing[m,,i],var_sequencing[m,,i])
      param_sequencing[m,i] <- mean(param_mi_sequencing[m,,i])
    }
  }
  return(list(param_sequencing,var_param_sequencing))
}



Rubin_rule <- function(means,variances,nimp){
  m <- length(means)
  B <- 1/(nimp-1)*sum((means-mean(means))^2)
  W <- mean(variances)
  S <- W + (1+1/nimp)*B
  return(list(mean(means),S))
}

Rubin_rule_variance_details <- function(means,variances,nimp){
  m <- length(means)
  B <- 1/(nimp-1)*sum((means-mean(means))^2)
  W <- mean(variances)
  S <- W + (1+1/nimp)*B
  
  if(S>0&!is.na(S)&!is.nan(S)){
    return((B/(nimp*S)))
  }else{
    return(0)
  }
}



  
duration_mi_seqimpute <- function(dataset,nimp,nsamples){
  nstates <- length(alphabet(seqdef(dataset[[1]])))
  nobs <- nrow(dataset[[1]])
  duration_values <- array(0,dim=c(nimp,nsamples,nstates))
  var_duration <- array(0,dim=c(nimp,nstates))
  param_duration <- array(0,dim=c(nimp,nstates))
  for(n in 1:nimp){
    for(s in 1:nsamples){
      tt <- sample(1:nobs,replace = TRUE,size=nobs)
      duration_values[n,s,] <- duration(seqdef(dataset[[n]][tt,]))
      for(i in 1:nstates){
        var_duration[n,i] <- var(duration_values[n,,i])
        param_duration[n,i] <- mean(duration_values[n,,i])
      }
    }
  }
  return(list(param_duration,var_duration))
}
  

mspell <- function(seqdata){
    spells <- data.frame(dss =factor(as.vector(as.matrix(seqdss(seqdata, with.missing=TRUE)))), dur=as.vector(seqdur(seqdata, with.missing=TRUE)))
    sp <- droplevels(subset(spells, !is.na(dur)))
    vec <- by(sp$dur, list(sp$dss), FUN=mean)
    return(vec)
}

duration <- function(seqdata){
  nalphabet <- length(alphabet(seqdata))
  olength <- ncol(seqdata)
  original <- mspell(seqdata)

  allstates <- names(original)
  getNamed <- function(data){
    data <- data[allstates]
    return(ifelse(is.na(data), 0, data))
  }
  return(getNamed(original))
  
}

chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))


timing <- function(dataset,nsubseq,list_alphabet){
  size_alphabet <- length(list_alphabet)
  mat_rate <- array(0,dim=c(nsubseq,size_alphabet,size_alphabet-1))
  tt <- chunk(1:ncol(dataset),nsubseq)
  for(i in 1:nsubseq){
    mat_trate <- seqtrate(seqdef(dataset[,tt[[i]]],alphabet=list_alphabet))
    for(j in 1:size_alphabet){
      if(mat_trate[j,j]>0){
        count <- 1
        for(k in 1:size_alphabet){
          if(k!=j){
            mat_rate[i,j,count] <- mat_trate[j,k]/mat_trate[j,j]
            count <- count +1
          }
        }
      }
    }
  }
  return(mat_rate)
}

timing_mi_seqimpute <- function(dataset, nsimdatasets, nimp, nsubseq,list_alphabet){
  nobs <- nrow(dataset[[1]][[1]])
  numcol <- ncol(dataset[[1]][[1]])
  nstates <- length(alphabet(seqdef(dataset[[1]][[1]])))
  timing_values <- array(0,dim=c(nsimdatasets,nimp,nsamples,nsubseq,nstates,nstates-1))
  var_timing <- array(0,dim=c(nsimdatasets,nimp,nsubseq,nstates,nstates-1))
  param_mi_timing <- array(0,dim=c(nsimdatasets,nimp,nsubseq,nstates,nstates-1))
  for(m in 1:nsimdatasets){
    for(n in 1:nimp){
      for(s in 1:nsamples){
        tt <- sample(1:nobs,replace = TRUE,size=nobs)
        timing_values[m,n,s,,,] <- timing(seqdef(dataset[[m]][[n]][tt,]),nsubseq,list_alphabet)
      }
      for(i in 1:nsubseq){
        for(j in 1:nstates){
          for(k in 1:(nstates-1)){
            var_timing[m,n,i,j,k] <- var(timing_values[m,n,,i,j,k])
            param_mi_timing[m,n,i,j,k] <- mean(timing_values[m,n,,i,j,k])
          }
        }
      }
    }
  }
  
  param_timing <- array(0,dim=c(nsimdatasets,nsubseq,nstates,nstates-1))
  var_param_timing <- array(0,dim=c(nsimdatasets,nsubseq,nstates,nstates-1))
  for(m in 1:nsimdatasets){
    for(i in 1:nsubseq){
      for(j in 1:nstates){
        for(k in 1:(nstates-1)){
          var_param_timing[m,i,j,k] <- Rubin_rule(param_mi_timing[m,,i,j,k],var_timing[m,,i,j,k])
          param_timing[m,i,j,k] <- mean(param_mi_timing[m,,i,j,k])
        }
      }
    }
  }
  return(list(param_timing,var_param_timing))
}

transform_seqimpute <- function(data,mi){
  tmp <- list()
  for(i in 1:mi){
    tmp[[i]] <- data[data$.imp==i,1:ncol(data)]
  }
  return(tmp)
}
transform_seqimpute <- function(data,mi,alphabets){
  tmp <- list()
  for(i in 1:mi){
    tmp[[i]] <- seqdef(data[data$.imp==i,3:ncol(data)],alphabet=alphabets)
  }
  return(tmp)
}

transform_mice <- function(data,mi,alphabets){
  tmp <- list()
  for(i in 1:mi){
    tt <- complete(data,mi)
    if(sum(is.na(tt))>0){
      for(j in 1:ncol(tt)){
        if(sum(is.na(tt[,j]))>0 & length(levels(tt[,j]))==1){
          tt[is.na(tt[,j]),j] <- levels(tt[,j])
        }
      }
    }
    tmp[[i]] <- seqdef(tt,alphabet=alphabets)
  }
  return(tmp)
}

transform_vlmc <- function(data,mi,alphabets){
  tmp <- list()
  for(i in 1:mi){
    tmp[[i]] <- seqdef(data[[i]],alphabet=alphabets)
  }
  return(tmp)
}


duration_mi <- function(dataset,nimp,nsamples){
  nstates <- length(alphabet(dataset[[1]]))
  nobs <- nrow(dataset[[1]])
  duration_values <- array(0,dim=c(nimp,nsamples,nstates))
  var_duration <- array(0,dim=c(nimp,nstates))
  param_duration <- array(0,dim=c(nimp,nstates))
  for(n in 1:nimp){
    seq_dss <- seqdss(dataset[[n]], with.missing=TRUE)
    seq_dur <- seqdur(dataset[[n]], with.missing=TRUE)
    for(s in 1:nsamples){
      tt <- sample(1:nobs,replace = TRUE,size=nobs)
      tmp <- duration(dataset[[n]][tt,],seq_dss[tt,],seq_dur[tt,])
      if(length(tmp)==nstates){
        duration_values[n,s,] <- tmp
      }else{
        for(i in 1:length(alphabet(dataset[[1]]))){
          if(alphabet(dataset[[1]])[i]%in%names(tmp)){
            duration_values[n,s,i] <- tmp[names(tmp)==alphabet(dataset[[1]])[i]]
          }else{
            duration_values[n,s,i] <- 0
          }
        }
      }
    }
    for(i in 1:nstates){
      param_duration[n,i]<-mean(duration_values[n,,i])
      var_duration[n,i]<-var(duration_values[n,,i])
    }
  }
  return(list(param_duration,var_duration))
}

# duration_mi <- function(dataset,nimp,nsamples){
#   nstates <- length(alphabet(dataset[[1]]))
#   nobs <- nrow(dataset[[1]])
#   duration_values <- array(0,dim=c(nimp,nstates))
#   var_duration <- array(0,dim=c(nimp,nstates))
#   param_duration <- array(0,dim=c(nimp,nstates))
#   for(n in 1:nimp){
#     tt <- duration(dataset[[n]])
#     var_duration[n,] <- tt[[2]]
#     param_duration[n,] <- tt[[1]]
#   }
#   return(list(param_duration,var_duration))
# }


# mspell <- function(seqdata){
#   spells <- data.frame(dss =factor(as.vector(as.matrix(seqdss(seqdata, with.missing=TRUE)))), dur=as.vector(seqdur(seqdata, with.missing=TRUE)))
#   sp <- droplevels(subset(spells, !is.na(dur)))
#   vec <- by(sp$dur, list(sp$dss), FUN=mean)
#   #vec_var <- by(sp$dur, list(sp$dss), FUN=var)/by(sp$dur, list(sp$dss), FUN=length)
#   #return(list(vec,vec_var))
#   return(vec)
# }

mspell <- function(seqdata,seq_dss,seq_dur){
  spells <- data.frame(dss =factor(as.vector(as.matrix(seq_dss))), dur=as.vector(seq_dur))
  sp <- droplevels(subset(spells, !is.na(dur)))
  vec <- by(sp$dur, list(sp$dss), FUN=mean)
  #vec_var <- by(sp$dur, list(sp$dss), FUN=var)/by(sp$dur, list(sp$dss), FUN=length)
  #return(list(vec,vec_var))
  return(vec)
}


getNamed <- function(data,allstates){
  data <- data[allstates]
  return(ifelse(is.na(data), 0, data))
}

duration <- function(seqdata,seq_dss,seq_dur){
  nalphabet <- length(alphabet(seqdata))
  olength <- ncol(seqdata)
  results <- mspell(seqdata,seq_dss,seq_dur)
  
  # allstates <- names(results[[1]])
  # return(list(getNamed(results[[1]],allstates),getNamed(results[[2]],allstates)))
  # 
  allstates <- names(results)
  tmp <- getNamed(results,allstates)
  if(length(tmp)!=length(alphabet(seqdata))){
    
  }
  return(getNamed(results,allstates))

}

chunk <- function(x,n) split(x, factor(sort(rank(x)%%n)))

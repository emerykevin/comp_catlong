
sequencing <- function(seqdata_dss,which_maxseq,size_alphabet){
  sds_trans <- seqtrate(seqdata_dss)
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


sequencing_mi <- function(dataset,nimp,nsamples,which_maxseq){
  nstates <- length(alphabet(dataset[[1]]))
  nobs <- nrow(dataset[[1]])
  sequencing_values <- array(0,dim=c(nimp,nsamples,nstates,nstates-2))
  var_sequencing <- array(0,dim=c(nimp,nstates,nstates-2))
  param_sequencing <- array(0,dim=c(nimp,nstates,nstates-2))
  for(n in 1:nimp){
    tmp <- seqdss(dataset[[n]])
    for(s in 1:nsamples){
      tt <- sample(1:nobs,replace = TRUE,size=nobs)
      sequencing_values[n,s,,] <- sequencing(tmp[tt,],which_maxseq,nstates)
      for(i in 1:nstates){
        for(j in 1:(nstates-2)){
          var_sequencing[n,i,j] <- var(sequencing_values[n,,i,j])
          param_sequencing[n,i,j] <- mean(sequencing_values[n,,i,j])
        }
      }
    }
  }
  return(list(param_sequencing,var_sequencing))
}


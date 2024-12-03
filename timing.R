

timing_mi <- function(dataset,nimp,nsamples){
  nstates <- length(alphabet(dataset[[1]]))
  nobs <- nrow(dataset[[1]])
  ncol <- ncol(dataset[[1]])
  timing_values <- array(0,dim=c(nimp,nsamples,nstates,ncol))
  var_timing <- array(0,dim=c(nimp,nstates,ncol))
  param_timing <- array(0,dim=c(nimp,nstates,ncol))
  for(n in 1:nimp){
    for(s in 1:nsamples){
      tt <- sample(1:nobs,replace = TRUE,size=nobs)
      timing_values[n,s,,] <- seqstatd(dataset[[n]][tt,])$Frequencies
      for(i in 1:nstates){
        for(j in 1:ncol){
          var_timing[n,i,j] <- var(timing_values[n,,i,j])
          param_timing[n,i,j] <- mean(timing_values[n,,i,j])
        }
      }
    }
  }
  return(list(param_timing,var_timing))
}

source("Functions script.R")
library("TraMineR")
library("mice")

predictors <- c("P1","P5","PF1","PF5","past","all","P1F1","P5F5")
mecha <- c("MAR","att","small")
model <- c("mice_rf","mice_multinom","seqimpute_rf","seqimpute_multinom","seqimp_t0","seqimp_t5","seqimp_rf_t0","seqimp_rf_t5")
criteria <- c("timing","duration","sequencing")

# Compute estimators and their estimated variance
ndatasets <- 6
nsimdatasets <- 100
nimp <- 10
for(k in 1:length(mecha)){
  print(k)
  for(i in 1:length(predictors)){
    print(i)
    for(l in 1:length(model)){
      print(l)
      for(o in 1:length(criteria)){
        for(n in 1:ndatasets){
          filename1 <- paste0("./results/",model[l],"_",predictors[i],"_n",n,"_m",
                              100,"_",mecha[k],"_",criteria[o],".Rdata")
          filename3 <- paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_valeur.Rdata")
          if(file.exists(filename1)&!file.exists(filename3)){
            load(filename1)
            param <- list()
            param_var <- list()
            details_var <- list()
            for(m in 1:nsimdatasets){
              param[[m]] <- array(NA,dim=dim(results[[1]])[-c(1)])
              param_var[[m]] <- array(NA,dim=dim(results[[1]])[-c(1)])
              details_var[[m]] <- array(NA,dim=c(dim(results[[1]])[-c(1)]))
              filename2 <- paste0("./results/",model[l],"_",predictors[i],"_n",n,"_m",
                                  m,"_",mecha[k],"_",criteria[o],".Rdata")
              if(file.exists(filename2)){
                load(filename2)
                if(length(dim(results[[1]]))==3){
                  for(a in 1:dim(results[[1]])[2]){
                    for(b in 1:dim(results[[1]])[3]){
                      tt <- Rubin_rule(results[[1]][,a,b],results[[2]][,a,b],nimp)
                      param[[m]][a,b] <- tt[[1]]
                      param_var[[m]][a,b] <- tt[[2]]
                      details_var[[m]][a,b] <- Rubin_rule_variance_details(results[[1]][,a,b],results[[2]][,a,b],nimp)
                      
                    }
                  }
                  
                }else if(length(dim(results[[1]]))==4){
                  for(a in 1:dim(results[[1]])[2]){
                    for(b in 1:dim(results[[1]])[3]){
                      for(c in 1:dim(results[[1]])[4]){
                        tt <- Rubin_rule(results[[1]][,a,b,c],results[[2]][,a,b,c],nimp)
                        param[[m]][a,b,c] <- tt[[1]]
                        param_var[[m]][a,b,c] <- tt[[2]]
                        details_var[[m]][a,b,c] <- Rubin_rule_variance_details(results[[1]][,a,b,c],results[[2]][,a,b,c],nimp)
                        
                        
                      }
                      
                    }
                  }
                  
                }else{
                  for(a in 1:dim(results[[1]])[2]){
                    
                    tt <- Rubin_rule(results[[1]][,a],results[[2]][,a],nimp)
                    param[[m]][a] <- tt[[1]]
                    param_var[[m]][a] <- tt[[2]]
                    details_var[[m]][a] <- Rubin_rule_variance_details(results[[1]][,a],results[[2]][,a],nimp)
                  }
                }
                
                
              }
            }
            
            save(param,file=paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_valeur.Rdata"))
            save(param_var,file=paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_variance.Rdata"))
            save(details_var,file=paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_details_variance.Rdata"))
            
          }
        }
      }
    }
  }
}

# Bias+Coverage ####
ndatasets <- 6
nsimdatasets <- 100
nimp <- 10
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(l in 1:length(model)){
      for(o in 1:length(criteria)){
        load(paste0("target_",criteria[o],".Rdata"))
        for(n in 1:ndatasets){
          filename1 <- paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_valeur.Rdata")
          if(file.exists(filename1)){
            filename10 <- paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_variance.Rdata")
            load(filename1)
            load(filename10)
            if(is.vector(param[[1]])){
              results <- array(NA,dim=c(nsimdatasets,4,length(param[[1]])))
              results
            }else{
              results <- array(NA,dim=c(nsimdatasets,4,dim(param[[1]])))
            }
            for(m in 1:nsimdatasets){
              if(is.vector(param[[m]])|length(dim(param[[m]]))==1){
                results[m,1,] <- m
                results[m,2,] <- param[[m]]
                results[m,3,] <- param_var[[m]]
                results[m,4,] <- paste0(model[l],"_",predictors[i])
              }else if(length(dim(param[[m]]))==2){
                results[m,1,,] <- m
                results[m,2,,] <- param[[m]]
                results[m,3,,] <- param_var[[m]]
                results[m,4,,] <- paste0(model[l],"_",predictors[i])
              }else{
                results[m,1,,,] <- m
                results[m,2,,,] <- param[[m]]
                results[m,3,,,] <- param_var[[m]]
                results[m,4,,,] <- paste0(model[l],"_",predictors[i])
              }
            }
            
            save(results,file=paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_results.Rdata"))

          }
        }
      }
    }
  }
}


# vlmc #####
predictors <- c("G1","G2")
mecha <- c("MAR","att","small")
model <- c("vlmc")
criteria <- c("timing","duration","sequencing")

# Calculs parameters and their variance
ndatasets <- 6
nsimdatasets <- 100
nimp <- 10
for(k in 1:length(mecha)){
  print(k)
  for(i in 1:length(predictors)){
    print(i)
    for(l in 1:length(model)){
      print(l)
      for(o in 1:length(criteria)){
        for(n in 1:ndatasets){
          filename1 <- paste0("./results/",model[l],predictors[i],"_n",n,"_m",
                              100,"_",mecha[k],"_",criteria[o],".Rdata")
          filename3 <- paste0("./results/",model[l],predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_valeur.Rdata")
          if(file.exists(filename1)&!file.exists(filename3)){
            load(filename1)
            param <- list()
            param_var <- list()
            details_var <- list()
            for(m in 1:nsimdatasets){
              param[[m]] <- array(NA,dim=dim(results[[1]])[-c(1)])
              param_var[[m]] <- array(NA,dim=dim(results[[1]])[-c(1)])
              details_var[[m]] <- array(NA,dim=c(dim(results[[1]])[-c(1)]))
              filename2 <- paste0("./results/",model[l],predictors[i],"_n",n,"_m",
                                  m,"_",mecha[k],"_",criteria[o],".Rdata")
              if(file.exists(filename2)){
                load(filename2)
                if(length(dim(results[[1]]))==3){
                  for(a in 1:dim(results[[1]])[2]){
                    for(b in 1:dim(results[[1]])[3]){
                      tt <- Rubin_rule(results[[1]][,a,b],results[[2]][,a,b],nimp)
                      param[[m]][a,b] <- tt[[1]]
                      param_var[[m]][a,b] <- tt[[2]]
                      details_var[[m]][a,b] <- Rubin_rule_variance_details(results[[1]][,a,b],results[[2]][,a,b],nimp)
                      
                    }
                  }
                  
                }else if(length(dim(results[[1]]))==4){
                  for(a in 1:dim(results[[1]])[2]){
                    for(b in 1:dim(results[[1]])[3]){
                      for(c in 1:dim(results[[1]])[4]){
                        tt <- Rubin_rule(results[[1]][,a,b,c],results[[2]][,a,b,c],nimp)
                        param[[m]][a,b,c] <- tt[[1]]
                        param_var[[m]][a,b,c] <- tt[[2]]
                        details_var[[m]][a,b,c] <- Rubin_rule_variance_details(results[[1]][,a,b,c],results[[2]][,a,b,c],nimp)
                        
                        
                      }
                      
                    }
                  }
                  
                }else{
                  for(a in 1:dim(results[[1]])[2]){
                    
                    tt <- Rubin_rule(results[[1]][,a],results[[2]][,a],nimp)
                    param[[m]][a] <- tt[[1]]
                    param_var[[m]][a] <- tt[[2]]
                    details_var[[m]][a] <- Rubin_rule_variance_details(results[[1]][,a],results[[2]][,a],nimp)
                  }
                }
                
                
              }
            }
            
            save(param,file=paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_valeur.Rdata"))
            save(param_var,file=paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_variance.Rdata"))
            save(details_var,file=paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_details_variance.Rdata"))
            
          }
        }
      }
    }
  }
}

# Bias+Coverage ####
ndatasets <- 6
nsimdatasets <- 100
nimp <- 10
for(k in 1:length(mecha)){
  for(i in 1:length(predictors)){
    for(l in 1:length(model)){
      for(o in 1:length(criteria)){
        load(paste0("target_",criteria[o],".Rdata"))
        for(n in 1:ndatasets){
          filename1 <- paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_valeur.Rdata")
          if(file.exists(filename1)){
            filename10 <- paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_variance.Rdata")
            load(filename1)
            load(filename10)
            if(is.vector(param[[1]])){
              results <- array(NA,dim=c(nsimdatasets,4,length(param[[1]])))
              results
            }else{
              results <- array(NA,dim=c(nsimdatasets,4,dim(param[[1]])))
            }
            for(m in 1:nsimdatasets){
              if(is.vector(param[[m]])|length(dim(param[[m]]))==1){
                results[m,1,] <- m
                results[m,2,] <- param[[m]]
                results[m,3,] <- param_var[[m]]
                results[m,4,] <- paste0(model[l],"_",predictors[i])
              }else if(length(dim(param[[m]]))==2){
                results[m,1,,] <- m
                results[m,2,,] <- param[[m]]
                results[m,3,,] <- param_var[[m]]
                results[m,4,,] <- paste0(model[l],"_",predictors[i])
              }else{
                results[m,1,,,] <- m
                results[m,2,,,] <- param[[m]]
                results[m,3,,,] <- param_var[[m]]
                results[m,4,,,] <- paste0(model[l],"_",predictors[i])
              }
            }
            
            save(results,file=paste0("./results/",model[l],"_",predictors[i],"_n",n,"_",mecha[k],"_",criteria[o],"_results.Rdata"))
            
          }
        }
      }
    }
  }
}
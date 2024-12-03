library("PST")
library("TraMineR")

ndatasets <-6
nimp <- 10
nsimdatasets <- 100
vlmcG1 <- list()
vlmcG2 <- list()
for(n in 1:6){
  
  load(paste0("n",n,"_MAR.Rdata"))
  k <- length(attr(seqdef(list_simdatasets[[n]][[1]]),"alphabet"))
  for(m in 1:nsimdatasets){
    tmpseq <- seqdef(list_simdatasets[[m]])
    SRH.pst.L10 <- pstree(tmpseq, nmin = 2, ymin = 0.001)
    alpha <- c(0.05, 0.04, 0.03, 0.02, 0.01, 0.001)
    C.list2 <- qchisq(1 - alpha, df = k - 1) / 2
    C.tuned2 <- tune(SRH.pst.L10, gain = "G2", C = C.list2, output = "PST",criterion = "AICc")
    
    C.list <- c(1,1.05,1.10,1.2,1.5,2)
    C.tuned <- tune(SRH.pst.L10, gain = "G1", C = C.list, output = "PST",criterion = "AICc")
    
    D <- list_simdatasets[[m]]
    D[,ncol(D)+1]<-D[,ncol(D)]
    D[,ncol(D)]<-levels(D[,1])[1]
    tmpseq <- seqdef(D)
    vlmcG1 <- list()
    vlmcG2 <- list()
    for(mi in 1:nimp){
      vlmcG2[[mi]] <- impute(C.tuned2, tmpseq, method = "prob")
      vlmcG1[[mi]] <- impute(C.tuned, tmpseq, method = "prob")
      
      vlmcG1[[mi]] <- vlmcG1[[mi]][,1:(ncol(vlmcG1[[mi]])-1)]
      vlmcG2[[mi]] <- vlmcG2[[mi]][,1:(ncol(vlmcG2[[mi]])-1)]
    }
    save(vlmcG1,file=paste0("./results/vlmcG1_n",n,"_m",m,"_MAR.Rdata"))
    save(vlmcG2,file=paste0("./results/vlmcG2_n",n,"_m",m,"_MAR.Rdata"))
  }
}


ndatasets <-6
nimp <- 10
nsimdatasets <- 100
vlmcG1 <- list()
vlmcG2 <- list()
for(n in 1:ndatasets){
  
  load(paste0("n",n,"_att.Rdata"))
  k <- length(attr(seqdef(list_simdatasets[[n]][[1]]),"alphabet"))
  for(m in 1:nsimdatasets){
    tmpseq <- seqdef(list_simdatasets[[m]])
    SRH.pst.L10 <- pstree(tmpseq, nmin = 2, ymin = 0.001)
    alpha <- c(0.05, 0.04, 0.03, 0.02, 0.01, 0.001)
    C.list2 <- qchisq(1 - alpha, df = k - 1) / 2
    C.tuned2 <- tune(SRH.pst.L10, gain = "G2", C = C.list2, output = "PST",criterion = "AICc")
    
    C.list <- c(1,1.05,1.10,1.2,1.5,2)
    C.tuned <- tune(SRH.pst.L10, gain = "G1", C = C.list, output = "PST",criterion = "AICc")
    
    D <- list_simdatasets[[m]]
    D[,ncol(D)+1]<-D[,ncol(D)]
    D[,ncol(D)]<-levels(D[,1])[1]
    tmpseq <- seqdef(D)
    vlmcG1 <- list()
    vlmcG2 <- list()
    for(mi in 1:nimp){
      vlmcG2[[mi]] <- impute(C.tuned2, tmpseq, method = "prob")
      vlmcG1[[mi]] <- impute(C.tuned, tmpseq, method = "prob")
      
      vlmcG1[[mi]] <- vlmcG1[[mi]][,1:(ncol(vlmcG1[[mi]])-1)]
      vlmcG2[[mi]] <- vlmcG2[[mi]][,1:(ncol(vlmcG2[[mi]])-1)]
    }
    save(vlmcG1,file=paste0("./results/vlmcG1_n",n,"_m",m,"_att.Rdata"))
    save(vlmcG2,file=paste0("./results/vlmcG2_n",n,"_m",m,"_att.Rdata"))
  }
}



ndatasets <-6
nimp <- 10
nsimdatasets <- 100
vlmcG1 <- list()
vlmcG2 <- list()
for(n in 1:ndatasets){
  
  load(paste0("n",n,"_small.Rdata"))
  k <- length(attr(seqdef(list_simdatasets[[n]][[1]]),"alphabet"))
  for(m in 1:nsimdatasets){
    tmpseq <- seqdef(list_simdatasets[[m]])
    SRH.pst.L10 <- pstree(tmpseq, nmin = 2, ymin = 0.001)
    alpha <- c(0.05, 0.04, 0.03, 0.02, 0.01, 0.001)
    C.list2 <- qchisq(1 - alpha, df = k - 1) / 2
    C.tuned2 <- tune(SRH.pst.L10, gain = "G2", C = C.list2, output = "PST",criterion = "AICc")
    
    C.list <- c(1,1.05,1.10,1.2,1.5,2)
    C.tuned <- tune(SRH.pst.L10, gain = "G1", C = C.list, output = "PST",criterion = "AICc")
    
    D <- list_simdatasets[[m]]
    D[,ncol(D)+1]<-D[,ncol(D)]
    D[,ncol(D)]<-levels(D[,1])[1]
    tmpseq <- seqdef(D)
    vlmcG1 <- list()
    vlmcG2 <- list()
    for(mi in 1:nimp){
      vlmcG2[[mi]] <- impute(C.tuned2, tmpseq, method = "prob")
      vlmcG1[[mi]] <- impute(C.tuned, tmpseq, method = "prob")
      
      vlmcG1[[mi]] <- vlmcG1[[mi]][,1:(ncol(vlmcG1[[mi]])-1)]
      vlmcG2[[mi]] <- vlmcG2[[mi]][,1:(ncol(vlmcG2[[mi]])-1)]
    }
    save(vlmcG1,file=paste0("./results/vlmcG1_n",n,"_m",m,"_small.Rdata"))
    save(vlmcG2,file=paste0("./results/vlmcG2_n",n,"_m",m,"_small.Rdata"))
  }
}
library("rsimsum")
library("ggplot2")
library("TraMineR")
library("ggh4x")

load("dataset1.Rdata")
load("dataset2.Rdata")
load("dataset3.Rdata")
load("dataset4.Rdata")
load("dataset5.Rdata")
load("dataset6.Rdata")
load("alphabets.Rdata")
alphabets[[4]][5] <- "widow"
alphabets[[4]][4] <- "single"
alphabets[[2]][3] <- "parents"
alphabets[[3]][6] <- "Partner, no child"
alphabets[[5]][2] <- "higher"

datasets <- c("Professional","Cohabitational (4 states)", "Cohabitational (8 states)","Civil status","Satisfaction with health status","mvad")

# I) Chronograms #####
colnames(DataWork) <- 15:40
seqdata <- seqdef(DataWork,alphabet=c("Full time","Part time","Non working","Education"),
                  labels=c("full time","part time","non working","education"),cpal=grey.colors(4))

pdf(file = "./Plots/prof.pdf",  width = 10, height =6)
seqdplot(seqdata,with.legend="right",main="Professional status",xlab="age",border=NA,cex.legend=1.2,cex.lab=1.2,cex.main=1.2)
dev.off()

colnames(long_living_4cat) <- 15:40
seqdata <- seqdef(long_living_4cat,alphabet=c("Parent(s)","Partner","Child","Other"),labels=c("parent(s)","partner","child","other"),cpal=grey.colors(4))

pdf(file = "./Plots/cohab4.pdf",  width = 10, height =6)
seqdplot(seqdata,with.legend="right",main="Cohabitational status (4 states)",xlab="age",border=NA,cex.legend=1.2,cex.lab=1.2,cex.main=1.2)
dev.off()

colnames(long_living_8cat) <- 15:40
seqdata <- seqdef(long_living_8cat,alphabet=c("Alone","Both parents","One parent","Partner","Partner and child"
                                              ,"Child","Relatives","Other"),labels=c("alone","both parents","one parent","partner","partner and child",
                                                                                     "child","relatives","other"),cpal=grey.colors(8))

pdf(file = "./Plots/cohab8.pdf",  width = 10, height =6)
seqdplot(seqdata,with.legend="right",main="Cohabitational status (8 states)",xlab="age",border=NA,cex.legend=1.2,cex.lab=1.2,cex.main=1.2)
dev.off()


colnames(civstat) <- 1999:2019
seqdata <- seqdef(civstat,alphabet=c("single, never married","married","separated","divorced","widower/widow"),labels=c("single","married","separated","divorced","widower/widow"),cpal=grey.colors(5))

pdf(file = "./Plots/civstat.pdf",  width = 10, height =6)
seqdplot(seqdata,with.legend="right",main="Civil status",xlab="year",border=NA,cex.legend=1.2,cex.lab=1.2,cex.main=1.2)
dev.off()

colnames(satis) <- 1999:2019
seqdata <- seqdef(satis,alphabet=c("low","average","high","very high"),labels=c("low","average","high","very high"),cpal=grey.colors(4))

pdf(file = "./Plots/satis.pdf",  width = 10, height =6)
seqdplot(seqdata,with.legend="right",main="Satisfaction with health status",xlab="year",border=NA,cex.legend=1.2,cex.lab=1.2,cex.main=1.2)
dev.off()


seqdata <- seqdef(mvad.seq,alphabet=c("school","FE","HE","training","employment","joblessness"),labels=c("school","FE","HE","training","employment","joblessness"),cpal=grey.colors(6))

pdf(file = "./Plots/mvad.pdf",  width = 10, height =6)
seqdplot(seqdata,with.legend="right",main="School-to-work transition",border=NA,cex.legend=1.2,cex.lab=1.2,cex.main=1.2)
dev.off()




# II) Results by algorithm #####
# - The plots, which are built separately by criteria and dataset,
# are built. These plots are not included in the article.
# - Summary tables, which are built separately by criteria and dataset,
# are built. These tables will be used to construct the summary plot
# included in the article

## 1. Mice rf ####
### a. duration ####
#### I) MAR
settings <- c("P1","P5","past","PF1","PF5","all")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/mice_rf_",settings[k],"_n",num_dataset,"_MAR_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-micerf-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-micerf-coverage-MAR-table.Rdata"))
  
  
  name_file <- paste0("./Plots/mice-rf-duration - n",num_dataset,"-bias-MAR.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/mice-rf-duration - n",num_dataset,"-coverage-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


settings <- c("P1","P5","past","PF1","PF5","all")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/mice_rf_",settings[k],"_n",num_dataset,"_att_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-micerf-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-micerf-coverage-att-table.Rdata"))
  
  name_file <- paste0("./Plots/mice-rf-duration - n",num_dataset,"-bias-att.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/mice-rf-duration - n",num_dataset,"-coverage-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}

settings <- c("P1","P5","past","PF1","PF5","all")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/mice_rf_",settings[k],"_n",num_dataset,"_small_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-micerf-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-micerf-coverage-small-table.Rdata"))
  
  name_file <- paste0("./Plots/mice-rf-duration - n",num_dataset,"-bias-small.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/mice-rf-duration - n",num_dataset,"-coverage-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


### b. timing #####
# MAR
ndatasets <- 6
methods <- c("mice_rf_P1","mice_rf_P5","mice_rf_past","mice_rf_PF1","mice_rf_PF5","mice_rf_all")
methods_short <- c("P1","P5","past","PF1","PF5","all")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-micerf-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-micerf-coverage-MAR-table.Rdata"))
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-rf-timing-n",num_dataset,"-bias-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-rf-timing-n",num_dataset,"-cover-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}



# att
ndatasets <- 6
methods <- c("mice_rf_P1","mice_rf_P5","mice_rf_past","mice_rf_PF1","mice_rf_PF5","mice_rf_all")
methods_short <- c("P1","P5","past","PF1","PF5","all")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==1|n==2|n==3){
    results_summary <- results_summary[results_summary$timepoint>12,]
  }else if(n==4|n==5){
    results_summary <- results_summary[results_summary$timepoint>9,]
  }else{
    results_summary <- results_summary[results_summary$timepoint>35,]
    
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-micerf-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-micerf-coverage-att-table.Rdata"))
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-rf-timing-n",num_dataset,"-bias-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-rf-timing-n",num_dataset,"-cover-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


# small
ndatasets <- 6
methods <- c("mice_rf_P1","mice_rf_P5","mice_rf_past","mice_rf_PF1","mice_rf_PF5","mice_rf_all")
methods_short <- c("P1","P5","past","PF1","PF5","all")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-micerf-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-micerf-coverage-small-table.Rdata"))
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-rf-timing-n",num_dataset,"-bias-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-rf-timing-n",num_dataset,"-cover-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


### c. sequencing #####
## MAR
ndatasets <- 6
load("max_seq.Rdata")

methods <- c("mice_rf_P1","mice_rf_P5","mice_rf_past","mice_rf_PF1","mice_rf_PF5","mice_rf_all")
methods_short <- c("P1","P5","past","PF1","PF5","all")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-micerf-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-micerf-coverage-MAR-table.Rdata"))
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## att
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("mice_rf_P1","mice_rf_P5","mice_rf_past","mice_rf_PF1","mice_rf_PF5","mice_rf_all")
methods_short <- c("P1","P5","past","PF1","PF5","all")


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-micerf-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-micerf-coverage-att-table.Rdata"))
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}

## small
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("mice_rf_P1","mice_rf_P5","mice_rf_past","mice_rf_PF1","mice_rf_PF5","mice_rf_all")
methods_short <- c("P1","P5","past","PF1","PF5","all")


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-micerf-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-micerf-coverage-small-table.Rdata"))
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-rf-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}

## 2. Mice multinom ####
### a. duration ####
#### I) MAR
settings <- c("P1","P5","past","PF1","PF5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/mice_multinom_",settings[k],"_n",num_dataset,"_MAR_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-micemulti-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-micemulti-coverage-MAR-table.Rdata"))
  
  
  name_file <- paste0("./Plots/mice-multinom-duration - n",num_dataset,"-bias-MAR.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/mice-multinom-duration - n",num_dataset,"-coverage-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


settings <- c("P1","P5","past","PF1","PF5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/mice_multinom_",settings[k],"_n",num_dataset,"_att_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-micemulti-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-micemulti-coverage-att-table.Rdata"))
  
  
  name_file <- paste0("./Plots/mice-multinom-duration - n",num_dataset,"-bias-att.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/mice-multinom-duration - n",num_dataset,"-coverage-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}

settings <- c("P1","P5","past","PF1","PF5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/mice_multinom_",settings[k],"_n",num_dataset,"_small_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-micemulti-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-micemulti-coverage-small-table.Rdata"))
  
  
  name_file <- paste0("./Plots/mice-multinom-duration - n",num_dataset,"-bias-small.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/mice-multinom-duration - n",num_dataset,"-coverage-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


### b. timing #####
# MAR
ndatasets <- 6
methods <- c("mice_multinom_P1","mice_multinom_P5","mice_multinom_past","mice_multinom_PF1","mice_multinom_PF5")
methods_short <- c("P1","P5","past","PF1","PF5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-micemulti-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-micemulti-coverage-MAR-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-multinom-timing-n",num_dataset,"-bias-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-multinom-timing-n",num_dataset,"-cover-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}



# att
ndatasets <- 6
methods <- c("mice_multinom_P1","mice_multinom_P5","mice_multinom_past","mice_multinom_PF1","mice_multinom_PF5")
methods_short <- c("P1","P5","past","PF1","PF5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==1|n==2|n==3){
    results_summary <- results_summary[results_summary$timepoint>12,]
  }else if(n==4|n==5){
    results_summary <- results_summary[results_summary$timepoint>9,]
  }else{
    results_summary <- results_summary[results_summary$timepoint>35,]
    
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-micemulti-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-micemulti-coverage-att-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-multinom-timing-n",num_dataset,"-bias-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-multinom-timing-n",num_dataset,"-cover-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


# small
ndatasets <- 6
methods <- c("mice_multinom_P1","mice_multinom_P5","mice_multinom_past","mice_multinom_PF1","mice_multinom_PF5")
methods_short <- c("P1","P5","past","PF1","PF5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-micemulti-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-micemulti-coverage-small-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-multinom-timing-n",num_dataset,"-bias-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/mice-multinom-timing-n",num_dataset,"-cover-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


### c. sequencing #####
## MAR
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("mice_multinom_P1","mice_multinom_P5","mice_multinom_past","mice_multinom_PF1","mice_multinom_PF5")
methods_short <- c("P1","P5","past","PF1","PF5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-micemulti-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-micemulti-coverage-MAR-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## att
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("mice_multinom_P1","mice_multinom_P5","mice_multinom_past","mice_multinom_PF1","mice_multinom_PF5")
methods_short <- c("P1","P5","past","PF1","PF5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-micemulti-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-micemulti-coverage-att-table.Rdata"))
  
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}

## small
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("mice_multinom_P1","mice_multinom_P5","mice_multinom_past","mice_multinom_PF1","mice_multinom_PF5")
methods_short <- c("P1","P5","past","PF1","PF5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-micemulti-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-micemulti-coverage-small-table.Rdata"))
  
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/mice-multinom-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## 3. seqimpute timing multinom ####
### a. duration #####
settings <- c("seqimp_t0_P1","seqimp_t0_P5","seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1","seqimp_t5_P5","seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/",settings[k],"_n",num_dataset,"_MAR_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-simultit-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-simultit-coverage-MAR-table.Rdata"))
  
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-duration - n",num_dataset,"-bias-MAR.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimp-t-multinom-duration - n",num_dataset,"-coverage-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}

settings <- c("seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/",settings[k],"_n",num_dataset,"_att_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-simultit-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-simultit-coverage-att-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-duration - n",num_dataset,"-bias-att.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimp-t-multinom-duration - n",num_dataset,"-coverage-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}



load("target_duration.Rdata")
settings <- c("seqimp_t0_P1","seqimp_t0_P5","seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1","seqimp_t5_P5","seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/",settings[k],"_n",num_dataset,"_small_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-simultit-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-simultit-coverage-small-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-duration - n",num_dataset,"-bias-small.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimp-t-multinom-duration - n",num_dataset,"-coverage-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


### b. timing #####
# MAR
ndatasets <- 6
settings <- c("seqimp_t0_P1","seqimp_t0_P5","seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1","seqimp_t5_P5","seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-simultit-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-simultit-coverage-MAR-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-timing-n",num_dataset,"-bias-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-timing-n",num_dataset,"-cover-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}



# att
ndatasets <- 6
settings <- c("seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==1|n==2|n==3){
    results_summary <- results_summary[results_summary$timepoint>12,]
  }else if(n==4|n==5){
    results_summary <- results_summary[results_summary$timepoint>9,]
  }else{
    results_summary <- results_summary[results_summary$timepoint>35,]
    
  }
  
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-simultit-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-simultit-coverage-att-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-timing-n",num_dataset,"-bias-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-timing-n",num_dataset,"-cover-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


# small
ndatasets <- 6
settings <- c("seqimp_t0_P1","seqimp_t0_P5","seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1","seqimp_t5_P5","seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-simultit-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-simultit-coverage-small-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-timing-n",num_dataset,"-bias-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-multinom-timing-n",num_dataset,"-cover-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


### c. sequencing #####
## MAR
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")
settings <- c("seqimp_t0_P1","seqimp_t0_P5","seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1","seqimp_t5_P5","seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-simultit-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-simultit-coverage-MAR-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## att
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

settings <- c("seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5")
methods <- settings


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-simultit-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-simultit-coverage-att-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## small
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

settings <- c("seqimp_t0_P1","seqimp_t0_P5","seqimp_t0_P1F1","seqimp_t0_P5F5",
              "seqimp_t5_P1","seqimp_t5_P5","seqimp_t5_P1F1","seqimp_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-simultit-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-simultit-coverage-small-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-multinom-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}
## 4. seqimpute timing rf ####
### a. duration #####
settings <- c("seqimp_rf_t0_P1","seqimp_rf_t0_P5","seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1","seqimp_rf_t5_P5","seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/",settings[k],"_n",num_dataset,"_MAR_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-sirft-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-sirft-coverage-MAR-table.Rdata"))
  
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-duration - n",num_dataset,"-bias-MAR.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimp-t-rf-duration - n",num_dataset,"-coverage-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}

settings <- c("seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short <- c("t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/",settings[k],"_n",num_dataset,"_att_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-sirft-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-sirft-coverage-att-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-duration - n",num_dataset,"-bias-att.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimp-t-rf-duration - n",num_dataset,"-coverage-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}

load("target_duration.Rdata")
settings <- c("seqimp_rf_t0_P1","seqimp_rf_t0_P5","seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1","seqimp_rf_t5_P5","seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/",settings[k],"_n",num_dataset,"_small_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-sirft-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-sirft-coverage-small-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-duration - n",num_dataset,"-bias-small.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimp-t-rf-duration - n",num_dataset,"-coverage-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


### b. timing #####
# MAR
ndatasets <- 6
settings <- c("seqimp_rf_t0_P1","seqimp_rf_t0_P5","seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1","seqimp_rf_t5_P5","seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short<- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-sirft-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-sirft-coverage-MAR-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-timing-n",num_dataset,"-bias-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-timing-n",num_dataset,"-cover-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}



# att
ndatasets <- 6
settings <- c("seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short <- c("t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==1|n==2|n==3){
    results_summary <- results_summary[results_summary$timepoint>12,]
  }else if(n==4|n==5){
    results_summary <- results_summary[results_summary$timepoint>9,]
  }else{
    results_summary <- results_summary[results_summary$timepoint>35,]
    
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-sirft-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-sirft-coverage-att-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-timing-n",num_dataset,"-bias-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-timing-n",num_dataset,"-cover-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


# small
ndatasets <- 6
settings <- c("seqimp_rf_t0_P1","seqimp_rf_t0_P5","seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1","seqimp_rf_t5_P5","seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-sirft-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-sirft-coverage-small-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-timing-n",num_dataset,"-bias-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimp-t-rf-timing-n",num_dataset,"-cover-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


### c. sequencing #####
## MAR
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")
settings <- c("seqimp_rf_t0_P1","seqimp_rf_t0_P5","seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1","seqimp_rf_t5_P5","seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-sirft-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-sirft-coverage-MAR-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## att
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

settings <- c("seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short <- c("t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5")
methods <- settings


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-sirft-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-sirft-coverage-att-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## small
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

settings <- c("seqimp_rf_t0_P1","seqimp_rf_t0_P5","seqimp_rf_t0_P1F1","seqimp_rf_t0_P5F5",
              "seqimp_rf_t5_P1","seqimp_rf_t5_P5","seqimp_rf_t5_P1F1","seqimp_rf_t5_P5F5")
methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")
methods <- settings

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-sirft-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-sirft-coverage-small-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimp-t-rf-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}

## 5. seqimpute rf ####
### a. duration ####
#### I) MAR
settings <- c("P1","P5","P1F1","P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/seqimpute_rf_",settings[k],"_n",num_dataset,"_MAR_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-sirf-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-sirf-coverage-MAR-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimpute-rf-duration - n",num_dataset,"-bias-MAR.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimpute-rf-duration - n",num_dataset,"-coverage-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


settings <- c("P1F1","P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/seqimpute_rf_",settings[k],"_n",num_dataset,"_att_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-sirf-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-sirf-coverage-att-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimpute-rf-duration - n",num_dataset,"-bias-att.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimpute-rf-duration - n",num_dataset,"-coverage-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}

settings <- c("P1","P5","P1F1","P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/seqimpute_rf_",settings[k],"_n",num_dataset,"_small_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-sirf-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-sirf-coverage-small-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimpute-rf-duration - n",num_dataset,"-bias-small.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimpute-rf-duration - n",num_dataset,"-coverage-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


### b. timing #####
# MAR
ndatasets <- 6
methods <- c("seqimpute_rf_P1","seqimpute_rf_P5","seqimpute_rf_P1F1","seqimpute_rf_P5F5")
methods_short <- c("P1","P5","P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-sirf-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-sirf-coverage-MAR-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-rf-timing-n",num_dataset,"-bias-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-rf-timing-n",num_dataset,"-cover-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}



# att
ndatasets <- 6
methods <- c("seqimpute_rf_P1F1","seqimpute_rf_P5F5")
methods_short <- c("P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==1|n==2|n==3){
    results_summary <- results_summary[results_summary$timepoint>12,]
  }else if(n==4|n==5){
    results_summary <- results_summary[results_summary$timepoint>9,]
  }else{
    results_summary <- results_summary[results_summary$timepoint>35,]
    
  }
  
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-sirf-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-sirf-coverage-att-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-rf-timing-n",num_dataset,"-bias-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-rf-timing-n",num_dataset,"-cover-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


# small
ndatasets <- 6
methods <- c("seqimpute_rf_P1","seqimpute_rf_P5","seqimpute_rf_P1F1","seqimpute_rf_P5F5")
methods_short <- c("P1","P5","P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-sirf-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-sirf-coverage-small-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-rf-timing-n",num_dataset,"-bias-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-rf-timing-n",num_dataset,"-cover-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


### c. sequencing #####
## MAR
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("seqimpute_rf_P1","seqimpute_rf_P5","seqimpute_rf_P1F1","seqimpute_rf_P5F5")
methods_short <- c("P1","P5","P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-sirf-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-sirf-coverage-MAR-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## att
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("seqimpute_rf_P1F1","seqimpute_rf_P5F5")
methods_short <- c("P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-sirf-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-sirf-coverage-att-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}

## small
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("seqimpute_rf_P1","seqimpute_rf_P5","seqimpute_rf_P1F1","seqimpute_rf_P5F5")
methods_short <- c("P1","P5","P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-sirf-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-sirf-coverage-small-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-rf-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}

## 6. seqimpute multinom ####
### a. duration ####
#### I) MAR
settings <- c("P1","P5","P1F1","P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/seqimpute_multinom_",settings[k],"_n",num_dataset,"_MAR_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-simulti-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-simulti-coverage-MAR-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-duration - n",num_dataset,"-bias-MAR.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimpute-multinom-duration - n",num_dataset,"-coverage-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


settings <- c("P1F1","P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/seqimpute_multinom_",settings[k],"_n",num_dataset,"_att_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-simulti-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-simulti-coverage-att-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-duration - n",num_dataset,"-bias-att.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimpute-multinom-duration - n",num_dataset,"-coverage-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}

settings <- c("P1","P5","P1F1","P5F5")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/seqimpute_multinom_",settings[k],"_n",num_dataset,"_small_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-simulti-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-simulti-coverage-small-table.Rdata"))
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-duration - n",num_dataset,"-bias-small.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/seqimpute-multinom-duration - n",num_dataset,"-coverage-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


### b. timing #####
# MAR
ndatasets <- 6
methods <- c("seqimpute_multinom_P1","seqimpute_multinom_P5","seqimpute_multinom_P1F1","seqimpute_multinom_P5F5")
methods_short <- c("P1","P5","P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-simulti-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-simulti-coverage-MAR-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-timing-n",num_dataset,"-bias-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-timing-n",num_dataset,"-cover-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}



# att
ndatasets <- 6
methods <- c("seqimpute_multinom_P1F1","seqimpute_multinom_P5F5")
methods_short <- c("P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==1|n==2|n==3){
    results_summary <- results_summary[results_summary$timepoint>12,]
  }else if(n==4|n==5){
    results_summary <- results_summary[results_summary$timepoint>9,]
  }else{
    results_summary <- results_summary[results_summary$timepoint>35,]
    
  }
  
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-simulti-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-simulti-coverage-att-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-timing-n",num_dataset,"-bias-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-timing-n",num_dataset,"-cover-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


# small
ndatasets <- 6
methods <- c("seqimpute_multinom_P1","seqimpute_multinom_P5","seqimpute_multinom_P1F1","seqimpute_multinom_P5F5")
methods_short <- c("P1","P5","P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-simulti-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-simulti-coverage-small-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-timing-n",num_dataset,"-bias-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/seqimpute-multinom-timing-n",num_dataset,"-cover-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


### c. sequencing #####
## MAR
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("seqimpute_multinom_P1","seqimpute_multinom_P5","seqimpute_multinom_P1F1","seqimpute_multinom_P5F5")
methods_short <- c("P1","P5","P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-simulti-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-simulti-coverage-MAR-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## att
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("seqimpute_multinom_P1F1","seqimpute_multinom_P5F5")
methods_short <- c("P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-simulti-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-simulti-coverage-att-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## small
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("seqimpute_multinom_P1","seqimpute_multinom_P5","seqimpute_multinom_P1F1","seqimpute_multinom_P5F5")
methods_short <- c("P1","P5","P1F1","P5F5")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-simulti-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-simulti-coverage-small-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/seqimpute-multinom-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## 7. vlmc ####
### a. duration ####
#### I) MAR
settings <- c("G1","G2")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/vlmc_",settings[k],"_n",num_dataset,"_MAR_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-vlmc-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-vlmc-coverage-MAR-table.Rdata"))
  
  
  name_file <- paste0("./Plots/vlmc-duration - n",num_dataset,"-bias-MAR.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/vlmc-duration - n",num_dataset,"-coverage-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


settings <- c("G1","G2")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/vlmc_",settings[k],"_n",num_dataset,"_att_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-vlmc-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-vlmc-coverage-att-table.Rdata"))
  
  
  name_file <- paste0("./Plots/vlmc-duration - n",num_dataset,"-bias-att.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/vlmc-duration - n",num_dataset,"-coverage-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}

settings <- c("G1","G2")
load("target_duration.Rdata")
ndatasets <- 6
mean_bias <- matrix(NA,length( settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)

for(n in 1:ndatasets){
  num_dataset <- n
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(settings)){
    
    load(paste0("./results/vlmc_",settings[k],"_n",num_dataset,"_small_duration_results.Rdata"))
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- settings[k]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(k==1&i==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-vlmc-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-vlmc-coverage-small-table.Rdata"))
  
  
  name_file <- paste0("./Plots/vlmc-duration - n",num_dataset,"-bias-small.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/vlmc-duration - n",num_dataset,"-coverage-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


### b. timing #####
# MAR
ndatasets <- 6
methods <- c("vlmc_G1","vlmc_G2")
methods_short <- c("G1","G2")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-vlmc-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-vlmc-coverage-MAR-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/vlmc-timing-n",num_dataset,"-bias-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/vlmc-timing-n",num_dataset,"-cover-MAR.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}



# att
ndatasets <- 6
methods <- c("vlmc_G1","vlmc_G2")
methods_short <- c("G1","G2")


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==1|n==2|n==3){
    results_summary <- results_summary[results_summary$timepoint>12,]
  }else if(n==4|n==5){
    results_summary <- results_summary[results_summary$timepoint>9,]
  }else{
    results_summary <- results_summary[results_summary$timepoint>35,]
    
  }
  
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-vlmc-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-vlmc-coverage-att-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/vlmc-timing-n",num_dataset,"-bias-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/vlmc-timing-n",num_dataset,"-cover-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


# small
ndatasets <- 6
methods <- c("vlmc_G1","vlmc_G2")
methods_short <- c("G1","G2")


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-vlmc-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-vlmc-coverage-small-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/vlmc-timing-n",num_dataset,"-bias-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/vlmc-timing-n",num_dataset,"-cover-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
}


### c. sequencing #####
## MAR
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("vlmc_G1","vlmc_G2")
methods_short <- c("G1","G2")


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-vlmc-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-vlmc-coverage-MAR-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## att
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("vlmc_G1","vlmc_G2")
methods_short <- c("G1","G2")


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-vlmc-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-vlmc-coverage-att-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}


## small
ndatasets <- 6
mean_bias <- matrix(NA,length(settings),6)
frac_int_bias <- matrix(NA,length(settings),6)
frac_int_cover <- matrix(NA,length(settings),6)
mean_bias <- as.data.frame(mean_bias)
frac_int_bias <- as.data.frame(frac_int_bias)
frac_int_cover <- as.data.frame(frac_int_cover)
rownames(mean_bias) <- settings
rownames(frac_int_bias) <- settings
rownames(frac_int_cover) <- settings
load("max_seq.Rdata")

methods <- c("vlmc_G1","vlmc_G2")
methods_short <- c("G1","G2")


for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"RR",j)
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-vlmc-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-vlmc-coverage-small-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/vlmc-sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
}

## Summary plots #####
### i) Proportion of bias Monte Carlo confidence intervals that contain #######
###  the value 0 for each scenario #####
#### micerf - bias ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","past","PF1","PF5","all")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-micerf-bias-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_bias[table_bias$method==methods_short[j],]
        table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  ylim(0,1)+
  theme_bw() +
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario")
#print(tt)

pdf(file = "./Plots/mice_rf_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### mice multinom####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","past","PF1","PF5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-micemulti-bias-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_bias[table_bias$method==methods_short[j],]
        table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario")
#print(tt)

pdf(file = "./Plots/mice_multi_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### seqimp rf ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","P1F1","P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-sirf-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("P1F1","P5F5","P1F1","P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario")
#print(tt)

pdf(file = "./Plots/si_rf_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()



#### seqimp multi ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","P1F1","P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-simulti-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("P1F1","P5F5","P1F1","P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario")
#print(tt)

pdf(file = "./Plots/si_multi_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### seqimp rf timing####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-sirft-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("t0 P1F1","t0 P5F5","t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5","t5 P1F1","t5 P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario")
#print(tt)

pdf(file = "./Plots/si_rf_t_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()

#### seqimp multi timing ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-simultit-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("t0 P1F1","t0 P5F5","t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5","t5 P1F1","t5 P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario")
#print(tt)

pdf(file = "./Plots/si_multi_t_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### vlmc####
table_summary <- matrix(NA,6*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("G1","G2")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-vlmc-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("G1","G2")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario")
#print(tt)

pdf(file = "./Plots/vlmc_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()


### ii) Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario#####
#### mice random forest ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","past","PF1","PF5","all")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-micerf-coverage-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_cover[table_cover$method==methods_short[j],]
        table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario")
#print(tt)

pdf(file = "./Plots/mice_rf_prop_coverage.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### mice multinom ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","past","PF1","PF5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-micemulti-coverage-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_cover[table_cover$method==methods_short[j],]
        table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario")
#print(tt)

pdf(file = "./Plots/mice_multi_prop_coverage.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### MICT random forest ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","P1F1","P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-sirf-coverage-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("P1F1","P5F5","P1F1","P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario")
#print(tt)

pdf(file = "./Plots/si_rf_prop_coverage.pdf",  width = 16, height = 10)
print(tt)
dev.off()



#### MICT multinomial ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","P1F1","P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-simulti-coverage-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("P1F1","P5F5","P1F1","P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario")
#print(tt)

pdf(file = "./Plots/si_multi_prop_coverage.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### MICT-timing random forest ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-sirft-coverage-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("t0 P1F1","t0 P5F5","t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5","t5 P1F1","t5 P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario")
#print(tt)

pdf(file = "./Plots/si_rf_t_prop_coverage.pdf",  width = 16, height = 10)
print(tt)
dev.off()

#### MICT-timing multinomial####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-simultit-coverage-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("t0 P1F1","t0 P5F5","t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5","t5 P1F1","t5 P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario")
#print(tt)

pdf(file = "./Plots/si_multi_t_prop_coverage.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### vlmc ####
table_summary <- matrix(NA,6*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("G1","G2")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-vlmc-coverage-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("G1","G2")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short_att[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_cover[table_cover$method==methods_short[j],]
          table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() + ylim(0,1)+
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario")
#print(tt)

pdf(file = "./Plots/vlmc_prop_coverage.pdf",  width = 16, height = 10)
print(tt)
dev.off()



### iii) mean bias #####
#### mice random forest ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","past","PF1","PF5","all")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-micerf-bias-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_bias[table_bias$method==methods_short[j],]
        table_summary[row,1] <- mean(abs(tt$est))
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)

set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset),scales="free")+
  theme_bw() +
  labs(x = "dataset", y = "mean bias")+
  ggtitle("Mean bias")+
  facetted_pos_scales(y=list(
    crit=="duration"~scale_y_continuous(limits=c(0,10)),
    crit=="timing"~scale_y_continuous(limits=c(0,0.05)),
    crit=="sequencing"~scale_y_continuous(limits=c(0,1))
  ))
#print(tt)

pdf(file = "./Plots/mice_rf_mean.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### mice multinomial####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","past","PF1","PF5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-micemulti-bias-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_bias[table_bias$method==methods_short[j],]
        table_summary[row,1] <- mean(abs(tt$est))
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset),scales="free")+
  facetted_pos_scales(y=list(
    crit=="duration"~scale_y_continuous(limits=c(0,10)),
    crit=="timing"~scale_y_continuous(limits=c(0,0.05)),
    crit=="sequencing"~scale_y_continuous(limits=c(0,1))
  ))+
  theme_bw() +
  labs(x = "dataset", y = "mean bias")+
  ggtitle("Mean bias")

pdf(file = "./Plots/mice_multi_mean.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### MICT random forest ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","P1F1","P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-sirf-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("P1F1","P5F5","P1F1","P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset),scales="free")+
  theme_bw() +
  labs(x = "dataset", y = "mean bias")+
  ggtitle("Mean bias")+
  facetted_pos_scales(y=list(
    crit=="duration"~scale_y_continuous(limits=c(0,10)),
    crit=="timing"~scale_y_continuous(limits=c(0,0.05)),
    crit=="sequencing"~scale_y_continuous(limits=c(0,1))
  ))

pdf(file = "./Plots/si_rf_mean.pdf",  width = 16, height = 10)
print(tt)
dev.off()



#### MICT multinomial ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("P1","P5","P1F1","P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-simulti-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("P1F1","P5F5","P1F1","P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset),scales="free")+
  theme_bw() +
  labs(x = "dataset", y = "mean bias")+
  ggtitle("Mean bias")+
  facetted_pos_scales(y=list(
    crit=="duration"~scale_y_continuous(limits=c(0,10)),
    crit=="timing"~scale_y_continuous(limits=c(0,0.05)),
    crit=="sequencing"~scale_y_continuous(limits=c(0,1))
  ))

pdf(file = "./Plots/si_multi_mean.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### MICT-timing random forest ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-sirft-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("t0 P1F1","t0 P5F5","t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5","t5 P1F1","t5 P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset),scales="free")+
  theme_bw() +
  labs(x = "dataset", y = "mean bias")+
  ggtitle("Mean bias")+
  facetted_pos_scales(y=list(
    crit=="duration"~scale_y_continuous(limits=c(0,10)),
    crit=="timing"~scale_y_continuous(limits=c(0,0.05)),
    crit=="sequencing"~scale_y_continuous(limits=c(0,1))
  ))

pdf(file = "./Plots/si_rf_t_mean.pdf",  width = 16, height = 10)
print(tt)
dev.off()

#### MICT-timing multinomial ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("t0 P1","t0 P5","t0 P1F1","t0 P5F5","t5 P1","t5 P5","t5 P1F1","t5 P5F5")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-simultit-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("t0 P1F1","t0 P5F5","t0 P1F1","t0 P5F5","t5 P1F1","t5 P5F5","t5 P1F1","t5 P5F5")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:8) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset),scales="free")+
  theme_bw() +
  labs(x = "dataset", y = "mean bias")+
  ggtitle("Mean bias")+
  facetted_pos_scales(y=list(
    crit=="duration"~scale_y_continuous(limits=c(0,10)),
    crit=="timing"~scale_y_continuous(limits=c(0,0.05)),
    crit=="sequencing"~scale_y_continuous(limits=c(0,1))
  ))
pdf(file = "./Plots/si_multi_t_mean.pdf",  width = 16, height = 10)
print(tt)
dev.off()


#### vlmc ####
table_summary <- matrix(NA,6*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("G1","G2")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-vlmc-bias-",mech[k],"-table.Rdata"))
      if(k==2){
        methods_short_att <- c("G1","G2")
        for(j in 1:length(methods_short_att)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short_att[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }else{
        for(j in 1:length(methods_short)){
          row <- row+1
          tt <- table_bias[table_bias$method==methods_short[j],]
          table_summary[row,1] <- mean(abs(tt$est))
          table_summary[row,2]<-methods_short[j]
          table_summary[row,3]<-mech[k]
          table_summary[row,4]<-crit[l]
          table_summary[row,5]<-datasets[num_dataset]
        }
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset),scales="free")+
  theme_bw() +
  labs(x = "dataset", y = "mean bias")+
  ggtitle("Mean bias")+
  facetted_pos_scales(y=list(
    crit=="duration"~scale_y_continuous(limits=c(0,10)),
    crit=="timing"~scale_y_continuous(limits=c(0,0.05)),
    crit=="sequencing"~scale_y_continuous(limits=c(0,1))
  ))

pdf(file = "./Plots/vlmc_mean.pdf",  width = 16, height = 10)
print(tt)
dev.off()




# III) Comparison between methods####
# - The plots, which are built separately by criteria and dataset,
# are built. These plots are included in the article (A22 to A78)
# - Summary tables, which are built separately by criteria and dataset,
# are built. These tables will be used to construct the summary plot
# included in the article (Figure 5 to 7)
## a. duration ########
methods <- c("mice_multinom_PF5","mice_rf_all","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P1F1","seqimp_rf_t0_P1F1","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")
ndatasets <- 6
for(n in 1:ndatasets){
  num_dataset <- n
  load("target_duration.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(meth in 1:length(methods)){
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    colnames(results_table) <- c("m","valeur","sd","method","state","true")
    load(paste0("./results/",methods[meth],"_n",num_dataset,"_MAR_duration_results.Rdata"))
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[meth]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(i==1&meth==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  colnames(results_summary) <- c("m","valeur","sd","method","state","true")
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-coverage-MAR-table.Rdata"))
  
  
  name_file <- paste0("./Plots/duration - n",num_dataset,"-bias-MAR.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/duration - n",num_dataset,"-coverage-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="empse")
  name_file <- paste0("./Plots/duration - n",num_dataset,"-empse-MAR.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = " empirical standard error")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


methods <- c("mice_multinom_P5","mice_rf_past","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P5F5","seqimp_rf_t0_P5F5","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")
ndatasets <- 6
for(n in 1:ndatasets){
  num_dataset <- n
  load("target_duration.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  colnames(results_table) <- c("m","valeur","sd","method","state","true")
  for(meth in 1:length(methods)){
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    load(paste0("./results/",methods[meth],"_n",num_dataset,"_att_duration_results.Rdata"))
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[meth]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(i==1&meth==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  colnames(results_summary) <- c("m","valeur","sd","method","state","true")
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-coverage-att-table.Rdata"))
  
  
  name_file <- paste0("./Plots/duration - n",num_dataset,"-bias-att.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/duration - n",num_dataset,"-coverage-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="empse")
  name_file <- paste0("./Plots/duration - n",num_dataset,"-empse-att.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = " empirical standard error")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


methods <- c("mice_multinom_PF5","mice_rf_all","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P1F1","seqimp_rf_t0_P1F1","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")
ndatasets <- 6
for(n in 1:ndatasets){
  num_dataset <- n
  load("target_duration.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  colnames(results_table) <- c("m","valeur","sd","method","state","true")
  for(meth in 1:length(methods)){
    results_table <- matrix(NA,100,6)
    results_table <- as.data.frame(results_table)
    load(paste0("./results/",methods[meth],"_n",num_dataset,"_small_duration_results.Rdata"))
    for(i in 1:dim(results)[3]){
      results_table[,1] <- results[,1,i]
      results_table[,2] <- results[,2,i]
      results_table[,3] <- results[,3,i]
      results_table[,4] <- methods_short[meth]
      results_table[,5] <- alphabets[[num_dataset]][i]
      results_table[,6] <- original[[num_dataset]][i]
      if(i==1&meth==1){
        results_summary <- results_table
        
      }else{
        results_summary <- rbind(results_summary,results_table)
      }
    }
  }
  colnames(results_summary) <- c("m","valeur","sd","method","state","true")
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="state", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/duration - n",num_dataset,"-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/duration - n",num_dataset,"-coverage-small-table.Rdata"))
  
  
  name_file <- paste0("./Plots/duration - n",num_dataset,"-bias-small.pdf")
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "bias")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="cover")
  name_file <- paste0("./Plots/duration - n",num_dataset,"-coverage-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = "coverage")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
  test <- tidy(ss1,stats="empse")
  name_file <- paste0("./Plots/duration - n",num_dataset,"-empse-small.pdf")
  tt <- ggplot(test, aes(x = method, y = est, ymin = lower, ymax = upper,colour=state)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    theme_bw() +
    labs(x = "Method", y = " empirical standard error")+
    ggtitle(datasets[n])
  
  pdf(file = name_file,  width = 10, height =10)
  print(tt)
  dev.off()
  
}


##b. timing ####
methods <- c("mice_multinom_PF5","mice_rf_all","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P1F1","seqimp_rf_t0_P1F1","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")
for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-coverage-MAR-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(paste0("Bias of each parameter related to timing along with their Monte Carlo intervals for the school-to-work transitions"))
  
  
    name_file <- paste0("./Plots/timing-n",num_dataset,"-bias-MAR.pdf")

    pdf(file = name_file,
        width = 20,
        height = 30)
    print(tt)

    dev.off()
    
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      facet_grid(rows=vars(method))+
      theme_bw() +
      labs(x = "time-point", y = "coverage")+
      ggtitle(datasets[num_dataset])
    
    
    name_file <- paste0("./Plots/timing-n",num_dataset,"-cover-MAR.pdf")
    
    pdf(file = name_file,
        width = 20,
        height = 30)
    print(tt)
    
    dev.off()
    
  tt <- ggplot(tidy(ss1,stats="empse"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      facet_grid(rows=vars(method))+
      theme_bw() +
      labs(x = "time-point", y = "empirical standard error")+
      ggtitle(datasets[num_dataset])
    
    
    name_file <- paste0("./Plots/timing-n",num_dataset,"-empse-MAR.pdf")
    
    pdf(file = name_file,
        width = 20,
        height = 30)
    print(tt)
    
    dev.off()
 
}

methods <- c("mice_multinom_P1","mice_rf_past","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P1F1","seqimp_rf_t0_P5F5","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))

  if(n==1|n==2|n==3){
    results_summary <- results_summary[results_summary$timepoint>12,]
  }else if(n==4|n==5){
    results_summary <- results_summary[results_summary$timepoint>9,]
  }else{
    results_summary <- results_summary[results_summary$timepoint>35,]
    
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-coverage-att-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/timing-n",num_dataset,"-bias-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/timing-n",num_dataset,"-cover-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="empse"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "empirical standard error")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/timing-n",num_dataset,"-empse-att.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  
}



methods <- c("mice_multinom_PF5","mice_rf_all","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P1F1","seqimp_rf_t0_P1F1","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","param","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","param"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/timing - n",num_dataset,"-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/timing - n",num_dataset,"-coverage-small-table.Rdata"))
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "bias")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/timing-n",num_dataset,"-bias-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
  tt <- ggplot(tidy(ss1,stats="cover"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=param)) +
    geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
    geom_point() +
    geom_errorbar(width = 1 / 3) +
    facet_grid(rows=vars(method))+
    theme_bw() +
    labs(x = "time-point", y = "coverage")+
    ggtitle(datasets[num_dataset])
  
  
  name_file <- paste0("./Plots/timing-n",num_dataset,"-cover-small.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 30)
  print(tt)
  
  dev.off()
  
 
}



##c. sequencing ####
load("max_seq.Rdata")

methods <- c("mice_multinom_PF5","mice_rf_all","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P1F1","seqimp_rf_t0_P1F1","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-bias-MAR-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-coverage-MAR-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-empse-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="empse"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "empirical standard error")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    

  }else{
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-bias-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-coverage-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-empse-MAR.pdf")
    tt <- ggplot(tidy(ss1,stats="empse"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "empirical standard error")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
  
 
}

load("max_seq.Rdata")

methods <- c("mice_multinom_P5","mice_rf_past","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P5F5","seqimp_rf_t0_P5F5","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_att_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-bias-att-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-coverage-att-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-empse-att.pdf")
    tt <- ggplot(tidy(ss1,stats="empse"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "empirical standard error")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-bias-att.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-coverage-att.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-empse-att.pdf")
    tt <- ggplot(tidy(ss1,stats="empse"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "empirical standard error")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
  
  
}


load("max_seq.Rdata")

methods <- c("mice_multinom_PF5","mice_rf_all","seqimpute_multinom_P5F5","seqimpute_rf_P5F5","seqimp_t0_P1F1","seqimp_rf_t0_P1F1","vlmc_G2")
methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")

for(n in 1:ndatasets){
  num_dataset <-n
  load("target_sequencing.Rdata")
  results_summary <- matrix(NA,100,6)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_small_sequencing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,6)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","param","true","method")
      for(i in 1:dim(results)[3]){
        mm <- max_seq[[num_dataset]][i]
        
        test <- 1:length(alphabets[[num_dataset]])
        test <- test[-c(i,mm)]
        
        
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- paste0(alphabets[[num_dataset]][i],"-",alphabets[[num_dataset]][test[j]]," / ",alphabets[[num_dataset]][mm])
        results_table[,5] <- original[[num_dataset]][i,j]
        results_table[,6] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  if(n==4){
    results_summary <- results_summary[!grepl("widow",results_summary[,4]),]
  }
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by="param", se = "sd", methodvar = "method")
  ss1 <- summary(s1)
  
  table_bias <- tidy(ss1,stats="bias")
  table_cover <- tidy(ss1,stats="cover")
  
  save(table_bias,file=paste0("./Plots/sequencing - n",num_dataset,"-bias-small-table.Rdata"))
  save(table_cover,file=paste0("./Plots/sequencing - n",num_dataset,"-coverage-small-table.Rdata"))
  
  
  if(n %in% c(3,6)){
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-empse-small.pdf")
    tt <- ggplot(tidy(ss1,stats="empse"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "empirical standard error")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 20, height =10)
    print(tt)
    dev.off()
  }else{
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-bias-small.pdf")
    tt <- ggplot(tidy(ss1,stats="bias"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "bias")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-coverage-small.pdf")
    tt <- ggplot(tidy(ss1,stats="cover"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0.95, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "coverage")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
    
    name_file <- paste0("./Plots/sequencing - n",num_dataset,"-empse-small.pdf")
    tt <- ggplot(tidy(ss1,stats="empse"), aes(x = method, y = est, ymin = lower, ymax = upper,colour=param)) +
      geom_hline(yintercept = 0, color = "red", lty = "dashed") +
      geom_point() +
      geom_errorbar(width = 1 / 3) +
      theme_bw() +
      labs(x = "Method", y = "empirical standard error")+
      ggtitle(datasets[n])+
      theme(legend.position="bottom")
    
    pdf(file = name_file,  width = 10, height =8)
    print(tt)
    dev.off()
  }
  
  
  
}

## Summary plots ####
### i) Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-bias-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_bias[table_bias$method==methods_short[j],]
        table_summary[row,1] <- sum(!is.nan(tt$lower)&tt$lower<=0&tt$upper>=0)/nrow(tt)
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() +
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of bias Monte Carlo confidence intervals that contain the value 0 for each scenario")
print(tt)

pdf(file = "./Plots/bias_comparison_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()

### ii) Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-coverage-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_cover[table_cover$method==methods_short[j],]
        table_summary[row,1] <- sum(tt$lower<=0.95&tt$upper>=0.95)/nrow(tt)
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)


set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset))+
  theme_bw() +
  labs(x = "dataset", y = "proportion")+
  ggtitle("Proportion of coverage Monte Carlo confidence intervals that contain the value 0.95 for each scenario")
print(tt)


pdf(file = "./Plots/coverage_comparison_prop.pdf",  width = 16, height = 10)
print(tt)
dev.off()


### iii) Mean bias ####
table_summary <- matrix(NA,6*3*3*3,4)
table_summary <- as.data.frame(table_summary)
mech <- c("MAR","att","small")
crit <- c("duration","timing","sequencing")
datasets <- c("Professional","Cohabitational (4 states)","Cohabitational (8 states)","Civil Status","Health Satisfaction","School-to-work transition")
ndatasets <- 6

methods_short <- c("mice multinom","mice rf","MICT multinom","MICT rf","MICT-t multinom","MICT-t rf","vlmc G2")

row <- 0
for(num_dataset in 1:ndatasets){
  for(k in 1:length(mech)){
    for(l in 1:length(crit)){
      load(paste0("./Plots/",crit[l]," - n",num_dataset,"-bias-",mech[k],"-table.Rdata"))
      for(j in 1:length(methods_short)){
        row <- row+1
        tt <- table_bias[table_bias$method==methods_short[j],]
        table_summary[row,1] <- mean(abs(tt$est))
        table_summary[row,2]<-methods_short[j]
        table_summary[row,3]<-mech[k]
        table_summary[row,4]<-crit[l]
        table_summary[row,5]<-datasets[num_dataset]
      }
    }
  }
}
colnames(table_summary) <- c("valeur","method","mech","crit","dataset")
table_summary[,2] <- factor(table_summary[,2],levels=methods_short)
table_summary[,3] <- factor(table_summary[,3],levels=c("MAR","att","small"))
levels(table_summary[,3])[2] <- "attrition"
table_summary[,4] <- factor(table_summary[,4],levels=crit)
table_summary[,5] <- factor(table_summary[,5],levels=datasets)



set.seed(2)
tt <- ggplot(table_summary, aes(x = mech, y = valeur, colour=method,shape=method)) +
  scale_shape_manual(values=1:7) +    
  scale_color_brewer(palette="Dark2")+
  geom_point(position = position_dodge(width=0.3)) +
  facet_grid(rows=vars(crit),cols=vars(dataset),scales="free")+
  theme_bw() +
  labs(x = "dataset", y = "mean bias")+
  ggtitle("Mean bias")+
  facetted_pos_scales(y=list(
    crit=="duration"~scale_y_continuous(limits=c(0,8)),
    crit=="timing"~scale_y_continuous(limits=c(0,0.03)),
    crit=="sequencing"~scale_y_continuous(limits=c(0,0.05))
  ))

pdf(file = "./Plots/bias_comparison_mean.pdf",  width = 16, height = 10)
print(tt)
dev.off()

# IV) Figure 8 mvad MICT- MICT-timing article ####
methods <- c("seqimpute_multinom_P5F5","seqimp_t0_P1F1")
methods_short <- c("MICT multinom","MICT-timing multinom")
for(n in c(6)){
  num_dataset <-n
  load("target_timing.Rdata")
  results_summary <- matrix(NA,100,7)
  results_summary <- as.data.frame(results_summary)
  for(k in 1:length(methods)){
    load(paste0("./results/",methods[k],"_n",num_dataset,"_MAR_timing_results.Rdata"))
    for(j in 1:dim(results)[4]){
      results_table <- matrix(NA,100,7)
      results_table <- as.data.frame(results_table)
      colnames(results_table) <- c("m","valeur","sd","timepoint","state","true","method")
      for(i in 1:dim(results)[3]){
        results_table[,1] <- results[,1,i,j]
        results_table[,2] <- results[,2,i,j]
        results_table[,3] <- results[,3,i,j]
        results_table[,4] <- j
        results_table[,5] <- alphabets[[num_dataset]][i]
        results_table[,6] <- original[[num_dataset]][i,j]
        results_table[,7] <- methods_short[k]
        if(i==1&j==1&k==1){
          results_summary <- results_table
          
        }else{
          results_summary <- rbind(results_summary,results_table)
        }
      }
    }
  }
  results_summary[,1] <- as.numeric(results_summary[,1])
  results_summary[,2] <- as.numeric(results_summary[,2])
  results_summary[,3] <- sqrt(as.numeric(results_summary[,3]))
  
  results_summary <- results_summary[results_summary$state=="employment",]
  
  s1 <- simsum(data = results_summary, estvarname = "valeur", true = "true", by=c("method","state"), se = "sd", methodvar = "timepoint")
  ss1 <- summary(s1)
  
  
  
  tt <- ggplot(tidy(ss1,stats="bias"), aes(x = timepoint, y = est, ymin = lower, ymax = upper, colour=state)) +
    geom_hline(yintercept = 0, color = "black", lty = "dashed") +
    geom_point(color="black") +
    geom_errorbar(width = 1 / 3,color="black") +
    facet_grid(rows=vars(method))+
    theme_bw() +
    theme(strip.text= element_text(size = 20),axis.text.y=element_text(size=15),axis.title=element_text(size=20))+
    labs(x = "time point", y = "bias")+
    ggtitle(paste0("Bias of the probability to be in employment at each state along with the 95% Monte Carlo intervals"))
  
  
  name_file <- paste0("./Plots/mvad-article.pdf")
  
  pdf(file = name_file,
      width = 20,
      height = 20)
  print(tt)
  dev.off()
  
  
}


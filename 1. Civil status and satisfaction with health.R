source("extractSeqFromWaves.R")

# 0.loadAllSHP() function to load all the SHP data #####
# (including all waves and time-constant databases
wavedir <- "./Data_SPSS/SHP-Data-W1-W21-SPSS/"
datadir <- "./Data_SPSS/SHP-Data-WA-SPSS/"
loadAllSHP(wavedir, datadir, maxwave=21)

# 1. Civil status trajectories ####
civstat<- extractSeqFromWaves(pvarseq="CIVSTA$$",
                                  use.value.labels=TRUE, maxMissing=21)

civstat <- civstat[rowSums(is.na(civstat))==0,]
civstat[civstat=="registered partnership"] <- "married"
for(j in 4:ncol(civstat)){
  civstat[,j] <- droplevels(civstat[,j])
}
civstat <- civstat[,4:24]

save(civstat,file="dataset4.Rdata")

# 2. Satisfaction with health trajectories ####

satis<- extractSeqFromWaves(pvarseq="P$$C02",
                          use.value.labels=TRUE, maxMissing=21)

satis <- satis[rowSums(is.na(satis))==0,]

for(j in 4:ncol(satis)){
  levels(satis[,j]) <- c(rep("low",5),rep("average",2),rep("high",2),rep("very high",2))
}
satis<- satis[,4:24]


save(satis,file="dataset5.Rdata")



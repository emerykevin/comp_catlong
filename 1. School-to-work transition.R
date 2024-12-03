library("TraMineR")
data(mvad)
mvad.seq <- mvad[,15:86]
save(mvad.seq,file="dataset6.Rdata")
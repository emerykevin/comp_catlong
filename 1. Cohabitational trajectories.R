library("foreign")
library("readstata13")
library("tidyr")

######################################################
# 1. Data preparation##############################
###################################################
MasterFile <- read.dta13("./Data_STATA/SHP-Data-WA-STATA/shp_mp.dta",generate.factors=T)
living <- read.dta("./Data_STATA/SHP-Data-SHP-3-W1-STATA/SHP-Data-SHP-3-W1-STATA/shpiii_la_user.dta") 

MasterFile <- MasterFile[MasterFile$idpers%in%living$idpers,]

# Remove persons with no birthyear 
No_birth <- which(MasterFile[,"birthy"]=="inapplicable",)
MasterFile <- MasterFile[-No_birth,]
living <- living[living$idpers%in%MasterFile$idpers,]


# Transform birthyear from factor to numeric
MasterFile$birthy <- as.numeric(levels(MasterFile$birthy))[MasterFile$birthy]

MasterFileLiving <- MasterFile[MasterFile$idpers%in%living$idpers,]
LivingFirstExp <- living[living$ep_livingarr==1,]

# Correct cohabitation observations that differ by one year from birthyear
for(i in 1:dim(LivingFirstExp)[1]){
  if(LivingFirstExp[i,"livingarr_a"]!=MasterFileLiving[i,"birthy"]){
    if(abs(LivingFirstExp[i,"livingarr_a"]-MasterFileLiving[i,"birthy"])<=1){
      living[living$idpers==LivingFirstExp[i,"idpers"]&living$ep_livingarr==1,"livingarr_a"]<-MasterFileLiving[i,"birthy"]
    }
  }
}


# Correct obvious errors in the datasets
MasterFile[MasterFile$idpers=="62222101","birthy"]<-1988
MasterFile[MasterFile$idpers=="62222103","birthy"]<-1960
MasterFile[MasterFile$idpers=="63457102","birthy"]<-1990
MasterFile[MasterFile$idpers=="63457103","birthy"]<-1987
MasterFile[MasterFile$idpers=="67035102","birthy"] <- 1963
MasterFile[MasterFile$idpers=="62609101","birthy"] <- 1952
MasterFile[MasterFile$idpers=="63457101","birthy"] <- 1954
MasterFile[MasterFile$idpers=="67394103","birthy"]<-1979
living[living$idpers=="61133102" & living$ep_livingarr==1,"livingarr_a"] <- 1948
living[living$idpers=="62296102" & living$ep_livingarr==1,"livingarr_a"] <- 1977
living[living$idpers=="68703101" & living$ep_livingarr==1,"livingarr_a"]<-1927
living[living$idpers=="61900101" & living$ep_livingarr==1,"livingarr_b"] <- 1966


#Remove individuals which have birthyear and first cohab that
#differ more than one year
LivingFirstExp <- living[living$ep_livingarr==1,]
MasterFileLiving <- MasterFile[MasterFile$idpers%in%living$idpers,]
IdProblematic <- MasterFileLiving[MasterFileLiving$birthy!=LivingFirstExp$livingarr_a,"idpers"]

idpers <- unique(living$idpers)
probCases <- which(idpers%in%IdProblematic)
idpers <- idpers[-probCases]
living <- living[living$idpers%in%idpers,]

###################################################
# 2. Transform from event data to longitudinal#####
###################################################
Mastertokeep <- MasterFile[,"idpers"]%in%idpers
Birthday <- MasterFile[Mastertokeep,"birthy"]

# Recode to 8 states
levels(living$livingarr_typ) <- c("Alone",rep("Both parents",13),rep("One parent",20),rep("Partner",22),rep("Partner and child",21),rep("Child",5),
                                  rep("Relatives",6),"Other(s)","All missing")


num_cases_living <- length(unique(living$idpers))
long_living <- matrix(NA,num_cases_living,100)
long_living <- as.data.frame(long_living)
num_events_living <- dim(living)[1]
j <- 0
col <- 1
for(i in 1:num_events_living){
  print(i)
  if(living[i,"ep_livingarr"]==1){
    print(j)
    j <- j+1
    col <- 1
  }
  if(living[i,"livingarr_b"]==living[i,"livingarr_a"]){
    long_living[j,col] <- levels(living$livingarr_typ)[living[i,"livingarr_typ"]]
    col <- col+1
  }else{
    num_obs <- living[i,"livingarr_b"]-living[i,"livingarr_a"]+1
    long_living[j,col:(col+num_obs-1)] <- rep(living[i,"livingarr_typ"],num_obs)
    col <- col+num_obs
  }
}

# Recode NA 
long_living[!is.na(long_living)&long_living=="All missing"] <- NA

# Keep trajectories between 15 and 40
long_living <- long_living[,15:40]

# Remove trajectories with NA
long_living <- long_living[rowSums(is.na(long_living))==0,]

# Recode Other(s) state
long_living[long_living=="Other(s)"] <- "Other"

long_living_8cat <- long_living

for(j in 1:ncol(long_living_8cat)){
  long_living_8cat[,j] <- factor(long_living_8cat[,j],levels=c("Alone","One parent","Both parents","Partner","Partner and child","Child",
                                                               "Relatives","Other"))
}
save(long_living_8cat,file="dataset2.Rdata")


###############################
# 3. Recode as 4 states #######
###############################
long_living_4cat <- long_living
long_living_4cat[long_living_4cat=="Alone"|long_living_4cat=="Relatives"|long_living_4cat=="Other"] <- "Other"
long_living_4cat[long_living_4cat=="Both parents"|long_living_4cat=="One parent"] <- "Parent(s)"
long_living_4cat[long_living_4cat=="Partner and child"|long_living_4cat=="Child"] <- "Child"

for(j in 1:ncol(long_living_4cat)){
  long_living_4cat[,j] <- factor(long_living_4cat[,j],levels=c("Parent(s)","Partner","Child","Other"))
}

save(long_living_4cat,file="dataset3.Rdata")


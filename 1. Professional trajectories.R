library("foreign")
library("readstata13")
library("tidyr")

# 0. Data cleaning #####
MasterFile <- read.dta13("./Data_Stata/SHP-Data-WA-STATA/shp_mp.dta",generate.factors=T)
#Keep only people of the third dataset (the one with the life calendar)
MasterFile <- MasterFile[MasterFile$filter19=="SHP_III (sample 2013)",]

#Load the datasets
work <- read.dta("./Data_Stata/SHP-Data-SHP-3-W1-STATA/SHP-Data-SHP-3-W1-STATA/shpiii_prof_act_user.dta")    

#Remove persons with no birthyear 
No_birth <- which(MasterFile[,"birthy"]=="inapplicable")
MasterFile <- MasterFile[-No_birth,]

#Transform birthyear from factor to numerical
MasterFile$birthy <- as.numeric(levels(MasterFile$birthy))[MasterFile$birthy]

#Remove cases with errors
MasterFile <- MasterFile[MasterFile$idpers!="62222101",]
MasterFile <- MasterFile[MasterFile$idpers!="62222103",]
MasterFile <- MasterFile[MasterFile$idpers!="67394103",]

# 1. From professional events to trajectories #####
work <- work[work$idpers%in%MasterFile$idpers,]
idpers <- unique(work$idpers)
Mastertokeep <- MasterFile[,"idpers"]%in%idpers
Birthday <- MasterFile[Mastertokeep,"birthy"]


long_work <- matrix(0,length(idpers),100)
long_work <- as.data.frame(long_work)
num_events_work <- dim(work)[1]


j <- 0
col <- 1
t <- 0
vec <- c(0)
for(i in 1:num_events_work){
  print(i)
  if(work[i,"ep_activity"]==1){
    j <- j+1
    col <- 1
    print(j)
    long_work[j,col:(col+15-1)] <- rep("Education",15)
    col <- col+work[i,"activity_a"]-Birthday[j]
  }else{
    #Test if the next spell really begins the year after the last spell
    if((work[i,"activity_a"]-work[i-1,"activity_b"]-1)!=0){
      t<-t+1
      vec <- c(vec,i)
    }
  }
  num_obs <- work[i,"activity_b"]-work[i,"activity_a"]+1
  if(num_obs==1){
    long_work[j,col] <- levels(work$activity_mj)[work[i,"activity_mj"]]
  }else{
    long_work[j,col:(col+num_obs-1)] <- rep(work[i,"activity_mj"],num_obs)
  }
  col <- col+num_obs
}

long_work <- cbind(idpers,Birthday,long_work)

# 2. Inclusion of education #####
###Load the wave containing the educational periods
vague2 <- read.dta13("./Data_Stata/SHP-Data-W1-W21-STATA/W16_2014/shp14_p_user.dta",generate.factors = T)
education <- vague2[vague2$filter14=="SHP_III (sample 2013)",c(1,4,120:132)]
idpers2 <- education$idpers

# Common observations in professional events and education periods
idcommon <- Reduce(intersect,list(long_work$idpers,idpers2))
education <- education[education$idpers%in%idcommon,]

BirthyEd <- MasterFile[MasterFile[,"idpers"]%in%idcommon,"birthy"]

#Store persons that do not have education spells
'%!in%' <- function(x,y)!('%in%'(x,y))

#Remove observations with missing data for the first educational experience and missing data for the year
education <- education[education$p14e01!="inapplicable",]
#Toutes les observations qui ont "does not know" pour le type ont "inapplicable" pour tout le reste. On les supprime donc.
education <- education[education$p14e01!="does not know",]
#Among the observations that have "inapplicable" for the year, they all have "incomplete compulsory school" or "only completed compulsory school"
education <-education[education$p14e32!="inapplicable",]


#Recode to further use data that have issues with first experience but good second and/or third experience.
levels(education$p14e32)[levels(education$p14e32)=="does not know"] <- "0"
levels(education$p14e32)[levels(education$p14e32)=="no answer"] <- "0"
levels(education$p14e33)[levels(education$p14e33)=="does not know"] <- "0"
levels(education$p14e33)[levels(education$p14e33)=="no answer"] <- "0"

#Recode to further use data that have issues with second experience but good third experience.
levels(education$p14e34)[levels(education$p14e34)=="inapplicable"] <- "0"
levels(education$p14e34)[levels(education$p14e34)=="does not know"] <- "0"
levels(education$p14e35)[levels(education$p14e35)=="does not know"] <- "0"
levels(education$p14e35)[levels(education$p14e35)=="no answer"] <- "0"
levels(education$p14e35)[levels(education$p14e35)=="inapplicable"] <- "0"

#Third Experience issues:
levels(education$p14e36)[levels(education$p14e36)=="inapplicable"] <- "0"
levels(education$p14e36)[levels(education$p14e36)=="does not know"] <- "0"
levels(education$p14e37)[levels(education$p14e37)=="inapplicable"] <- "0"
levels(education$p14e37)[levels(education$p14e37)=="does not know"] <- "0"

#Remove data that have finishing year before beginning year.
education$p14e32 <- as.numeric(levels(education$p14e32))[education$p14e32]
education$p14e33 <- as.numeric(levels(education$p14e33))[education$p14e33]
education <- education[!(education$p14e32>education$p14e33),]

education$p14e34bis<-rep(0,dim(education)[1])
education$p14e34bis[education$p14e02=="yes"] <- as.numeric(levels(education[education$p14e02=="yes","p14e34"]))[education[education$p14e02=="yes","p14e34"]]
education$p14e35bis<-rep(0,dim(education)[1])
education$p14e35bis[education$p14e02=="yes"] <- as.numeric(levels(education[education$p14e02=="yes","p14e35"]))[education[education$p14e02=="yes","p14e35"]]
education$p14e36bis<-rep(0,dim(education)[1])
education$p14e36bis[education$p14e06=="yes"] <- as.numeric(levels(education[education$p14e06=="yes","p14e36"]))[education[education$p14e06=="yes","p14e36"]]
education$p14e37bis<-rep(0,dim(education)[1])
education$p14e37bis[education$p14e06=="yes"] <- as.numeric(levels(education[education$p14e06=="yes","p14e37"]))[education[education$p14e06=="yes","p14e37"]]


education <- education[education$p14e02=="no"|(education$p14e02=="yes" & education$p14e35bis>=education$p14e34bis),]
education <- education[education$p14e06=="no"|education$p14e06=="inapplicable"|(education$p14e06=="yes" & education$p14e37bis>=education$p14e36bis),]

# Add education to professional trajectories #####
idworked <- education$idpers
Birthday <- long_work[long_work$idpers%in%idworked,"Birthday"]
long_work_ed <- long_work[,-c(1,2)]
for(i in 1:nrow(education)){
  print(i)
  rowC <- which(long_work$idpers==idworked[i])
  
  if(education[i,"p14e32"]!="0" & education[i,"p14e33"]!="0"){
    col <- 1+education$p14e32[i]-Birthday[i]
    num_obs <- education$p14e33[i]-education$p14e32[i]+1
    long_work_ed[rowC,col:(col+num_obs-1)] <- rep("Education",num_obs)
    
  }
  
  #Second education experience
  if(education[i,"p14e02"]=="yes"){
    if(education[i,"p14e34"]!=0 & education[i,"p14e35"]!=0){
      col <- 1+education$p14e34bis[i]-Birthday[i]
      num_obs <- education$p14e35bis[i]-education$p14e34bis[i]+1
      long_work_ed[rowC,col:(col+num_obs-1)] <- rep("Education",num_obs)
      
    }
    
    #Third education experience
    if(education[i,"p14e06"]=="yes"){
      if(education[i,"p14e36"]!=0 & education[i,"p14e37"]!=0){
        col <- 1+education$p14e36bis[i]-Birthday[i]
        num_obs <- education$p14e37bis[i]-education$p14e36bis[i]+1
        long_work_ed[rowC,col:(col+num_obs-1)] <- rep("Education",num_obs)
        
      }
    }
  }
}

# Recode professional status
for(i in 1:dim(long_work_ed)[1]){
  for(j in 1:dim(long_work_ed)[2]){
    if(long_work_ed[i,j]=="inapplicable"){
      long_work_ed[i,j] <- "Non working"
    }
    if(long_work_ed[i,j]==0){
      long_work_ed[i,j] <- NA
    }
    if(long_work_ed[i,j]%in%levels(work$activity_mj)[c(7,10)]){
      long_work_ed[i,j]<- "Full time"
    }
    if(long_work_ed[i,j]%in%levels(work$activity_mj)[c(8,9,11,12)]){
      long_work_ed[i,j]<- "Part time"
    }
    if(long_work_ed[i,j]%in%levels(work$activity_mj)[c(13,14)]){
      long_work_ed[i,j] <- "Full time"
    }
  }
}

# Fill gaps of NA surrounded by the same work state
for(i in 1:nrow(long_work_ed)){
  for(j in 16:(ncol(long_work_ed)-1)){
    obs.state <- "not"
    if(is.na(long_work_ed[i,j])){
      for(k in (j+1):ncol(long_work_ed)){
        if(!is.na(long_work_ed[i,k])){
          if(long_work_ed[i,k]==obs.state&obs.state!="Education"){
            long_work_ed[i,j:(k-1)] <- obs.state
          }
          break
        }
      }
    }else{
      obs.state <- long_work_ed[i,j]
    }
  }
}

# Missing in age 16:20 as education
for(i in 1:nrow(long_work_ed)){
  if(is.na(long_work_ed[i,16])){
    long_work_ed[i,16] <- "Education"
  }
  if(is.na(long_work_ed[i,17])){
    long_work_ed[i,17] <- "Education"
  }
  if(is.na(long_work_ed[i,18])){
    long_work_ed[i,18] <- "Education"
  }
  if(is.na(long_work_ed[i,19])){
    long_work_ed[i,19] <- "Education"
  }
  if(is.na(long_work_ed[i,20])){
    long_work_ed[i,20] <- "Education"
  }
}
long_work_ed <- long_work_ed[,15:40]
long_work_ed <- long_work_ed[rowSums(is.na(long_work_ed))==0,]

# Posteriori correction
long_work_ed[2483,4:7] <- "Education"

for(j in 1:ncol(long_work_ed)){
  long_work_ed[,j] <- factor(long_work_ed[,j],levels=c("Full time","Part time","Non working","Education"))
}
# Save data
DataWork <- long_work_ed
save(DataWork,file="dataset1.Rdata")

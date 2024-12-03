library(foreign)
loadAllSHP <- function(wavedir, datadir, maxwave){

	wavedir <- paste(wavedir, "/", sep="")
	datadir <- paste(datadir, "/", sep="")
	
	loadFile <- function(path, varname){
		suppressWarnings(shp <- read.spss(path, to.data.frame = TRUE, use.value.labels=TRUE))
		shp$IDPERS <- as.character(shp$IDPERS)
		assign(paste0(varname, "_label"), shp, envir = .GlobalEnv)
		suppressWarnings(shp <- read.spss(path, to.data.frame = TRUE, use.value.labels=FALSE))
		shp$IDPERS <- as.character(shp$IDPERS)
		assign(paste0(varname, "_code"), shp, envir = .GlobalEnv)
	}
	## Master individual file
	for(file in c("so", "lj", "ca", "mp")){
		loadFile(paste(datadir, "shp_", file, ".sav", sep=""), paste0("SHP_",toupper(file)))
	}
	for(w in 1:maxwave){
		twoDigitYear <- formatC(w - 2, width=2, flag="0")
		if(w==1){
			twoDigitYear <- "99"
		}
		loadWave <- function(use.value.labels){
			suppressWarnings(WW <- read.spss(paste(wavedir,"/W",w, "_", (w+1998), "/shp", twoDigitYear, "_p_user.sav",sep=""),  to.data.frame = TRUE, use.value.labels=use.value.labels))
			WW$IDPERS <- as.character(WW$IDPERS)
			WW[,paste("IDHOUS", twoDigitYear, sep="")] <- as.character(WW[,paste("IDHOUS", twoDigitYear, sep="")])
			## select variables
			suppressWarnings(WWH <- read.spss(paste(wavedir,"/W",w, "_", (w+1998), "/shp", twoDigitYear, "_h_user.sav",sep=""),  to.data.frame = TRUE, use.value.labels=use.value.labels))
			WWH[,paste("IDHOUS", twoDigitYear, sep="")] <- as.character(WWH[,paste("IDHOUS", twoDigitYear, sep="")])
			WW <- merge(WW, WWH, all.x=TRUE, by=paste("IDHOUS", twoDigitYear, sep=""))
			assign(paste0("SHP_W", twoDigitYear, ifelse(use.value.labels, "_label", "_code")), WW, envir = .GlobalEnv)
		}
		loadWave(FALSE)
		loadWave(TRUE)
	}	
	return(NULL)
}
#wavedir <- "../SHP-Data-W1-W21-SPSS/"
#datadir <- "../SHP-Data-WA-SPSS/"
#loadAllSHP(wavedir, datadir, maxwave=21)

saveAllSHPdata <- function(filename){

	save(list=c("SHP_CA_code", "SHP_CA_label", "SHP_LJ_code", "SHP_LJ_label", 
	"SHP_MP_code", "SHP_MP_label", "SHP_SO_code", "SHP_SO_label", 
	"SHP_W00_code", "SHP_W00_label", "SHP_W01_code", "SHP_W01_label", 
	"SHP_W02_code", "SHP_W02_label", "SHP_W03_code", "SHP_W03_label", 
	"SHP_W04_code", "SHP_W04_label", "SHP_W05_code", "SHP_W05_label", 
	"SHP_W06_code", "SHP_W06_label", "SHP_W07_code", "SHP_W07_label", 
	"SHP_W08_code", "SHP_W08_label", "SHP_W09_code", "SHP_W09_label", 
	"SHP_W10_code", "SHP_W10_label", "SHP_W11_code", "SHP_W11_label", 
	"SHP_W12_code", "SHP_W12_label", "SHP_W13_code", "SHP_W13_label", 
	"SHP_W14_code", "SHP_W14_label", "SHP_W15_code", "SHP_W15_label", 
	"SHP_W16_code", "SHP_W16_label", "SHP_W17_code", "SHP_W17_label", 
	"SHP_W18_code", "SHP_W18_label", "SHP_W19_code", "SHP_W19_label", 
	"SHP_W20_code", "SHP_W20_label",
	"SHP_W21_code", "SHP_W21_label",
	"SHP_W22_code", "SHP_W22_label",
	"SHP_W99_code", "SHP_W99_label"), file=filename)
}
## extractSeqFromWaves() function for extracting sequence data from the SHP
##          SHP waves
##
## usage:
##  extractSeqFromWaves(pvarseq=NULL, hvarseq=NULL,
##        use.value.labels=TRUE, maxMissing=12, MPvar = c("SEX", "BIRTHY"),
##		  SOvar=NULL, LJvar=NULL, CAvar=NULL, PLWvar=NULL, HLWvar=NULL)
##
##  pvarseq : vector with wanted personal variable names using protonames ($$ for waves)
##  hvarseq : vector with wanted household variable names using protonames ($$ for waves)
##  MPvar   : vector with names of variables to be extracted from the MP file
##  SOvar   : vector with names of variables to be extracted from the SO file
##  CAvar   : vector with names of variables to be extracted from the CA file
##  PLWvar  : vector with names of variables to be extracted from the last wave personal file
##  HLWvar  : vector with names of variables to be extracted from the last wave household file
##  use.value.labels : logical. Should we retrieve the labels? If FALSE,
##          the numerical code is retrieved. Set to FALSE for scale variables.
##  maxMissing: Maximum allowed missing states in one of the sequence specified by pvarseq or hvarseq
##

extractSeqFromWaves <- function (pvarseq=NULL, hvarseq=NULL,
          use.value.labels=TRUE, maxMissing=-1, MPvar = c("SEX", "BIRTHY"),
		  SOvar=NULL, LJvar=NULL, CAvar=NULL, PLWvar=NULL, HLWvar=NULL, maxwave=21){
	
	wavedir <- paste(wavedir, "/", sep="")
	datadir <- paste(datadir, "/", sep="")
	## Master individual file
	getData <- function(name){
		return(get(paste0("SHP_", name, ifelse(use.value.labels, "_label", "_code")) , envir = .GlobalEnv))
	}
	MP <- getData("MP")
	shp <- subset(MP, select=c("IDPERS", MPvar))
	
	## ==============
	## For each waves
	## ==============
	pvarseq <- c(pvarseq, hvarseq)
	allvars <- NULL
	for(w in 1:maxwave){
		twoDigitYear <- formatC(w - 2, width=2, flag="0")
		if(w==1){
			twoDigitYear <- "99"
		}
		WW <- getData(paste0("W", twoDigitYear))
		varsubsetYear <- sub("\\$\\$", twoDigitYear, pvarseq)
		allvars <- c(allvars, varsubsetYear)
		if(w==maxwave){
			varsubsetYear <- c(varsubsetYear,  sub("\\$\\$", twoDigitYear, c(HLWvar, PLWvar)))
		}
		varsubsetYearPos <- match(varsubsetYear, colnames(WW))
		## Some variable were not found in this wave
		if(any(is.na(varsubsetYearPos))){
			nomatch <- varsubsetYear[is.na(varsubsetYearPos)]
			warning("[!] Variable(s): ", paste(nomatch, collapse=", "), " were not found in wave W", w, ". Skipping.\n")
			varsubsetYear <- varsubsetYear[!is.na(varsubsetYearPos)]
		}
		WW <- subset(WW, select=c("IDPERS", varsubsetYear))
		## merge data frame with previous.
		shp <- merge(shp, WW, by="IDPERS", all=TRUE)
	}
	for(file in c("so", "lj", "ca")){
		var <- get(paste(toupper(file), "var", sep=""))
		if(!is.null(var)){
			var <- sub("\\$\\$", "..", var)
			WW <- getData(toupper(file))
			WW <- subset(WW, select=c("IDPERS", var))
			shp <- merge(shp, WW, all.x=TRUE, by="IDPERS")
		}
	}
	if(maxMissing>0){
		for(var in pvarseq){
			shp <- shp[rowSums(is.na(shp[, getColumnIndex(shp, var)])) < maxMissing, ]
		}
		
	}
	return(shp)
}


## extractSeqFromWavesUnloaded() function for extracting sequence data from the SHP
##          SHP waves
##
## usage:
##  extractSeqFromWavesUnloaded(wavedir, datadir, pvarseq=NULL, hvarseq=NULL,
##        use.value.labels=TRUE, maxMissing=12, MPvar = c("SEX", "BIRTHY"),
##		  SOvar=NULL, LJvar=NULL, CAvar=NULL, PLWvar=NULL, HLWvar=NULL)
##
##  wavedir : path to the folder where we find the SPSS SHP wave data
##  datadir : path to the folder where we find the WA (All Waves) data
##  pvarseq : vector with wanted personal variable names using protonames ($$ for waves)
##  hvarseq : vector with wanted household variable names using protonames ($$ for waves)
##  MPvar   : vector with names of variables to be extracted from the MP file
##  SOvar   : vector with names of variables to be extracted from the SO file
##  CAvar   : vector with names of variables to be extracted from the CA file
##  PLWvar  : vector with names of variables to be extracted from the last wave personal file
##  HLWvar  : vector with names of variables to be extracted from the last wave household file
##  use.value.labels : logical. Should we retrieve the labels? If FALSE,
##          the numerical code is retrieved. Set to FALSE for scale variables.
##  maxMissing: Maximum allowed missing states in one of the sequence specified by pvarseq or hvarseq
##

extractSeqFromWavesUnloaded <- function (wavedir, datadir, pvarseq=NULL, hvarseq=NULL,
          use.value.labels=TRUE, maxMissing=-1, MPvar = c("SEX", "BIRTHY"),
		  SOvar=NULL, LJvar=NULL, CAvar=NULL, PLWvar=NULL, HLWvar=NULL, maxwave=21){
	
	wavedir <- paste(wavedir, "/", sep="")
	datadir <- paste(datadir, "/", sep="")
	## Master individual file
	suppressWarnings(MP <- read.spss(paste(datadir,"shp_mp.sav",sep=""),  to.data.frame = TRUE))
	shp <- subset(MP, select=c("IDPERS", MPvar))
	shp$IDPERS <- as.character(shp$IDPERS)
	## ==============
	## For each waves
	## ==============
	pvarseq <- c(pvarseq, hvarseq)
	allvars <- NULL
	for(w in 1:maxwave){
		twoDigitYear <- formatC(w - 2, width=2, flag="0")
		if(w==1){
			twoDigitYear <- "99"
		}
		suppressWarnings(WW <- read.spss(paste(wavedir,"/W",w, "_", (w+1998), "/shp", twoDigitYear, "_p_user.sav",sep=""),  to.data.frame = TRUE, use.value.labels=use.value.labels))
		WW$IDPERS <- as.character(WW$IDPERS)
		WW[,paste("IDHOUS", twoDigitYear, sep="")] <- as.character(WW[,paste("IDHOUS", twoDigitYear, sep="")])
		## select variables
		if((!is.null(hvarseq)) || (w==maxwave&& !is.null(HLWvar))){
			suppressWarnings(WWH <- read.spss(paste(wavedir,"/W",w, "_", (w+1998), "/shp", twoDigitYear, "_h_user.sav",sep=""),  to.data.frame = TRUE, use.value.labels=use.value.labels))
			WWH[,paste("IDHOUS", twoDigitYear, sep="")] <- as.character(WWH[,paste("IDHOUS", twoDigitYear, sep="")])
			WW <- merge(WW, WWH, all.x=TRUE, by=paste("IDHOUS", twoDigitYear, sep=""))
			
		}
		varsubsetYear <- sub("\\$\\$", twoDigitYear, pvarseq)
		allvars <- c(allvars, varsubsetYear)
		if(w==maxwave){
			varsubsetYear <- c(varsubsetYear,  sub("\\$\\$", twoDigitYear, c(HLWvar, PLWvar)))
		}
		varsubsetYearPos <- match(varsubsetYear, colnames(WW))
		## Some variable were not found in this wave
		if(any(is.na(varsubsetYearPos))){
			nomatch <- varsubsetYear[is.na(varsubsetYearPos)]
			warning("[!] Variable(s): ", paste(nomatch, collapse=", "), " were not found in wave W", w, ". Skipping.\n")
			varsubsetYear <- varsubsetYear[!is.na(varsubsetYearPos)]
		}
		WW <- subset(WW, select=c("IDPERS", varsubsetYear))
		## merge data frame with previous.
		shp <- merge(shp, WW, by="IDPERS", all=TRUE)
	}
	for(file in c("so", "lj", "ca")){
		var <- get(paste(toupper(file), "var", sep=""))
		if(!is.null(var)){
			var <- sub("\\$\\$", "..", var)
			suppressWarnings(WW <- read.spss(paste(datadir, "shp_", file, ".sav", sep=""), to.data.frame = TRUE, use.value.labels=use.value.labels))
			WW$IDPERS <- as.character(WW$IDPERS)
			WW <- subset(WW, select=c("IDPERS", var))
			shp <- merge(shp, WW, all.x=TRUE, by="IDPERS")
		}
	}
	if(maxMissing>0){
		for(var in pvarseq){
			shp <- shp[rowSums(is.na(shp[, getColumnIndex(shp, var)])) < maxMissing, ]
		}
		
	}
	return(shp)
}


## getColumnIndex() function to retrieve the column index corresponding to
## the sequences of a given variable
##
## usage:
##  getColumnIndex(data, protoname)
##
##  data : a data.frame as produced by extractSeqFromWaves()
##  protoname : the name of a the variable using protoname ($$ for waves)
##

getColumnIndex <- function(data, protoname){
	index <- grep(paste("^", sub("\\$\\$", "[0-9][0-9]", protoname), "$", sep=""), names(data))
	if(length(index)==0){
		stop("[!] Variable ", protoname, " not found.")
	}
	return(index)
}

########################
## Variable Recodes   ##
## Created 6/22/2015  ##
## A Ben Rogers Joint ##
########################


######################################################
## Preamble: Recoded New Variables for the analysis ##
######################################################

############################
## read in final data set ##
############################

dat <- read.csv("/Users/bjr/Desktop/School/WSFDat/mergeRec.csv")
colnames(dat)

homdat <- read.csv("/Users/bjr/Desktop/School/WSFDat/homRec.csv")
colnames(homdat)

femdat <- read.csv("/Users/bjr/Desktop/School/WSFDat/femRec.csv")


## A couple of analytic recodes
## Create Oppose Yes/No Var from WhyOpposeFemaleService

dat$OpposeBin <- dat$WhyOpposeFemaleService
levels(dat$OpposeBin) <- c(levels(dat$OpposeBin), "Oppose")
dat$OpposeBin[!dat$OpposeBin %in% "DontOppose"] <- "Oppose"
dat$OpposeBin <- factor(dat$OpposeBin)

dat$OpposeNum <- dat$OpposeBin
levels(dat$OpposeNum) <- c(levels(dat$OpposeNum), "0", "1")
dat$OpposeNum[ dat$OpposeNum == "Oppose"] <- "1"
dat$OpposeNum[ dat$OpposeNum == "DontOppose"] <- "1"

homdat$OpposeBin <- homdat$WhyOpposeFemaleService
levels(homdat$OpposeBin) <- c(levels(homdat$OpposeBin), "Oppose")
homdat$OpposeBin[!homdat$OpposeBin %in% "DontOppose"] <- "Oppose"
homdat$OpposeBin <- factor(homdat$OpposeBin)

homdat$OpposeNum <- homdat$OpposeBin
levels(homdat$OpposeNum) <- c(levels(homdat$OpposeNum), "0", "1")
homdat$OpposeNum[ homdat$OpposeNum == "Oppose"] <- "1"
homdat$OpposeNum[ homdat$OpposeNum == "DontOppose"] <- "1"


femdat$OpposeBin <- femdat$WhyOpposeFemaleService
levels(femdat$OpposeBin) <- c(levels(femdat$OpposeBin), "Oppose")
femdat$OpposeBin[!femdat$OpposeBin %in% "DontOppose"] <- "Oppose"
femdat$OpposeBin <- factor(femdat$OpposeBin)

femdat$OpposeNum <- femdat$OpposeBin
levels(femdat$OpposeNum) <- c(levels(femdat$OpposeNum), "0", "1")
femdat$OpposeNum[ femdat$OpposeNum == "Oppose"] <- "1"
femdat$OpposeNum[ femdat$OpposeNum == "DontOppose"] <- "1"









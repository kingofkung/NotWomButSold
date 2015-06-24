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
dat$OpposeNum[ dat$OpposeNum == "DontOppose"] <- "0"
dat$OpposeNum <- as.numeric(as.character(dat$OpposeNum))

homdat$OpposeBin <- homdat$WhyOpposeFemaleService
levels(homdat$OpposeBin) <- c(levels(homdat$OpposeBin), "Oppose")
homdat$OpposeBin[!homdat$OpposeBin %in% "DontOppose"] <- "Oppose"
homdat$OpposeBin <- factor(homdat$OpposeBin)

homdat$OpposeNum <- homdat$OpposeBin
levels(homdat$OpposeNum) <- c(levels(homdat$OpposeNum), "0", "1")
homdat$OpposeNum[ homdat$OpposeNum == "Oppose"] <- "1"
homdat$OpposeNum[ homdat$OpposeNum == "DontOppose"] <- "0"
homdat$OpposeNum <- as.numeric(as.character(homdat$OpposeNum))



femdat$OpposeBin <- femdat$WhyOpposeFemaleService
levels(femdat$OpposeBin) <- c(levels(femdat$OpposeBin), "Oppose")
femdat$OpposeBin[!femdat$OpposeBin %in% "DontOppose"] <- "Oppose"
femdat$OpposeBin <- factor(femdat$OpposeBin)

femdat$OpposeNum <- femdat$OpposeBin
levels(femdat$OpposeNum) <- c(levels(femdat$OpposeNum), "0", "1")
femdat$OpposeNum[ femdat$OpposeNum == "Oppose"] <- "1"
femdat$OpposeNum[ femdat$OpposeNum == "DontOppose"] <- "0"
femdat$OpposeNum <- as.numeric(as.character(femdat$OpposeNum))



####################################################################
## ## Recode Racial Categories for easy regressions               ##
##                                                                ##
## ##' A function to add a second level to a categorical variable ##
## ##' with only one level                                        ##
## ##' @title nonmaker                                            ##
## ##' @param var A variable with 1 level, and NAs                ##
## ##' @return the variable with nonVar in place of NAs           ##
## ##' @author Benjamin Rogers                                    ##
## nonmaker <- function(var){                                     ##
##     nonlvl <- paste0("non", levels(var))                       ##
##     levels(var) <- c(levels(var), nonlvl)                      ##
##     var[is.na(var)] <- nonlvl                                  ##
##     var <- relevel(var, nonlvl)                                ##
##     return(var)                                                ##
## }                                                              ##
## dat$RaceWhite <- nonmaker(dat$RaceWhite)                       ##
## dat$RaceBlack <- nonmaker(dat$RaceBlack)                       ##
## dat$RaceHispanic <- nonmaker(dat$RaceHispanic)                 ##
## dat$RaceAsian <- nonmaker(dat$RaceAsian)                       ##
## dat$RaceOther <- nonmaker(dat$RaceOther)                       ##
####################################################################

dat$RaceComp <- gsub('NA', '', paste0(as.character(dat$RaceWhite),
                      as.character(dat$RaceBlack),
                      as.character(dat$RaceHispanic),
                      as.character(dat$RaceAsian),
                      as.character(dat$RaceOther)
                      )
                     )
dat$RaceComp[dat$RaceComp %in% ""] <- NA
dat$RaceComp <- factor(dat$RaceComp)
dat$RaceComp <- relevel(dat$RaceComp, ref = "White")

length(dat$RaceComp[dat$RaceComp == "Hispanic" & is.na(dat$RaceComp) == F])

dat$sharp <- gsub('NA', '',
                  paste0(dat$HarrassTrainingDoneRight,
                         dat$HarrassTrainingConfusing,
                         dat$HarrassTrainingInsufficient,
                         dat$HarrassTrainingCausesFear)
                  )
dat$sharp[dat$sharp %in% ""] <- NA


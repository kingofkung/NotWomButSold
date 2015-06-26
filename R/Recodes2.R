##################################
## Recodes 2: Electric Boogaloo ##
## Created 6/25/2015            ##
## A Ben Rogers Joint           ##
##################################

##################################################
## Preamble: Here are recodes s.t. analysis can ##
## Procead as needed                            ##
##################################################

library(plyr)

femdat <- read.csv("/Users/bjr/Desktop/School/WSFDat/femdat.csv")
homdat <- read.csv("/Users/bjr/Desktop/School/WSFDat/homdat.csv")

## Fix a problem w/Homdat$BirthYear

as.character(homdat$BirthYear)
homdat$BirthYear[!homdat$BirthYear %in% as.character(1900:1999)] <- NA
homdat$BirthYear <- as.numeric(as.character(homdat$BirthYear))

colnames(homdat)

levels(homdat$WhySupportFemaleService)
homdat$WhySupportFemaleService[homdat$WhySupportFemaleService == ""] <- NA


## Combine the two datasets,

homdat$female <- 0
femdat$female <- 1

datcomb <- rbind.fill(homdat, femdat)



##' Test 1 2 3... Description goes here
##'
##' A function to make a single kind of response into NAs Details
##' @param vec A single dimensional vector of data
##' @param toNA The response we'd like to be an NA
##' @return the vector in vec with toNA responses = NA
##' @author Benjamin Rogers
namaker <- function(vec, toNA = "" ){
    vec[vec == toNA] <- NA
    if( is.factor(vec) == T) vec <- factor(vec)
    return(vec)
}

namaker(homdat$FemalesAreWhiny)

homdatimp <- lapply(homdat, namaker)

homdatimp <- lapply(homdatimp, namaker, toNA = "-")

homdatimp$HarrassTrainingInsufficient

table(datcomb$ServedCST)
table(femdat$ServedCST)

table(datcomb$HarrassTrainingInsufficient)
table(femdat$HarrassTrainingInsufficient)
table(homdat$HarrassTrainingInsufficient)



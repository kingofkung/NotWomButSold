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

datimp <- lapply(datcomb, namaker)

datimp <- lapply(datimp, namaker, toNA = "-")
datcomb <- datimp

##' Get it so I've only got the first word from categories
##' @title firstword
##' @param dat a vector of factor or character data
##' @return the first word from the dat vector
##' @author Benjamin Rogers
firstword <- function(dat){
    testracial <- strsplit(as.character(dat), split = "\\s")
    output <- sapply(testracial, FUN = function(col) col[[1]])
    return(output)
}

datcomb$RaceHispanic <- firstword(datcomb$RaceHispanic)
datcomb$RaceBlack <- firstword(datcomb$RaceBlack)
datcomb$RaceWhite <- firstword(datcomb$RaceWhite)
datcomb$RaceAsian <- firstword(datcomb$RaceAsian)

datcomb$RaceComb <- paste(datcomb$RaceWhite, datcomb$RaceBlack, datcomb$RaceHispanic, datcomb$RaceAsian)

datcomb$RaceComb <- gsub("[,]", "", datcomb$RaceComb)
## Get rid of NAs. Note the brackets. It'll get rid of any N and any A unless they're bracketed separately. Then it knows they're supposed to be in that order
datcomb$RaceComb <- gsub("[N][A]", "", datcomb$RaceComb)

## Get blacks, whites, and hispanics on their own.
datcomb$RaceComb[ grep("Black", datcomb$RaceComb)] <- "Black"
datcomb$RaceComb[ grep("Hispanic", datcomb$RaceComb)] <- "Hispanic"
datcomb$RaceComb[ grep("White", datcomb$RaceComb)] <- "White"

## Eliminate unwanted spaces
datcomb$RaceComb <- gsub("\\s", "", datcomb$RaceComb)
datcomb$RaceComb[datcomb$RaceComb == ""] <- NA
datcomb$RaceComb <- factor(datcomb$RaceComb)


## Begin working on marital status
## Recall, getting involved with a civilian vs. getting involved w/a service member is the key distinction
datcomb$MaritalSimp <- as.character(datcomb$Marital)


datcomb$MaritalSimp[grep("civilian", datcomb$MaritalSimp)] <- "Civilian"
datcomb$MaritalSimp[grep("service", datcomb$MaritalSimp)] <- "Service Member"



## Get rid of levels that don't consider civilians or service members
levels(factor(datcomb$MaritalSimp))
rmlvls <- c("No Comment", "Never Married", "Widowed")
datcomb$MaritalSimp[datcomb$MaritalSimp %in% rmlvls] <- NA
datcomb$MaritalSimp <- factor(datcomb$MaritalSimp)


## Eliminate question marks in ideology question

datcomb$Ideology <- as.character(datcomb$Ideology)
datcomb$Ideology <- gsub("[?]", "", datcomb$Ideology)
datcomb$Ideology <- factor(datcomb$Ideology)

## Make a numeric opposition variable


levels( datcomb$WhySupportFemaleService)

datcomb$WhySupportFemaleService ==  "I oppose females serving in combat units"

datcomb$NumSupport <- NA
datcomb$NumSupport[datcomb$WhySupportFemaleService ==  "I oppose females serving in combat units"] <- 0
datcomb$NumSupport[!datcomb$WhySupportFemaleService ==  "I oppose females serving in combat units"] <- 1

table(datcomb$NumSupport, datcomb$WhySupportFemaleService)

## Relevel some stuff
levels(datcomb$BathroomQuestion) <- levels(datcomb$BathroomQuestion)[c(1:2, 4, 3)]
levels(datcomb$ComfortFemales) <- levels(datcomb$ComfortFemales)[c(4,3,1,2)]

##Make this a data.frame

datcomb <- as.data.frame(datcomb)
str(datcomb)

datcomb[sample(1:nrow(datcomb), size = 5),]

##Make a variable that is 3 ranks: Warrant, Commissioned, or Enlisted

datcomb$GradeRankSimp <- as.character(datcomb$GradeRank)
datcomb$GradeRankSimp <- substr(datcomb$GradeRankSimp, 1, 1)

datcomb$GradeRankSimp[datcomb$GradeRankSimp %in%  "E" ] <- "Warrant"
datcomb$GradeRankSimp[datcomb$GradeRankSimp %in%  "C" ] <- "Commissioned"
datcomb$GradeRankSimp[datcomb$GradeRankSimp %in%  "W" ] <- "Enlisted"
## Wait, what is O
datcomb$GradeRankSimp <- factor(datcomb$GradeRankSimp)

##################################################################################################
## ## For Dr. Haider-Markel                                                                     ##
## library(foreign)                                                                             ##
## write.dta(datcomb, file = "/Users/bjr/Desktop/School/WSFDat/CombData.dta")                   ##
##                                                                                              ##
## tst <- read.dta(file = "/Users/bjr/Desktop/School/WSFDat/CombData.dta", convert.factors = F) ##
##################################################################################################

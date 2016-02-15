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

## levels(homdat$WhySupportFemaleService)
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
datcomb$RaceComb <- factor(datcomb$RaceComb, levels = c("White", "Asian", "Black", "Hispanic"))



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
## str(datcomb)

datcomb[sample(1:nrow(datcomb), size = 5),]


## Number of Deployments Ndep
## Create Number of Deployments variable that isn't hard to work with.
data.frame(levels(datcomb$TimesDeployed))
datcomb$nDep <- as.character(datcomb$TimesDeployed)
datcomb$nDep <- gsub("deployment", "", datcomb$nDep)
datcomb$nDep <- gsub("\\s[s]", "", datcomb$nDep)
datcomb$nDep <- gsub("\\s", "", datcomb$nDep) ## Since 1s have an extra space, we need this to keep them in the data

datcomb$nDep[datcomb$nDep %in% "Don't know"] <- NA
datcomb$nDep <- factor(datcomb$nDep, levels = c("0", "1", "2", "3", "4", "5", "6", '7', '8', '9', '10', '11-15', '16-20', '20+'))

## Keep 0 separate, 1-5, 6-10

levels(datcomb$nDep) <- c(levels(datcomb$nDep), '1-5', '6-10')
datcomb$nDep[datcomb$nDep %in% c("1", '2', '3', '4', '5')] <- '1-5'
datcomb$nDep[datcomb$nDep %in% c("6", '7', '8', '9', '10')] <- '6-10'

datcomb$nDep <- factor(datcomb$nDep, levels = c("0", '1-5', '6-10', '11-15', '16-20', '20+'))

table(datcomb$TimesDeployed, datcomb$nDep) ##Looks in Order


##Make a variable that is 3 ranks: Warrant, Commissioned, or Enlisted
datcomb$GradeRankSimp <- as.character(datcomb$GradeRank)
datcomb$GradeRankSimp <- substr(datcomb$GradeRankSimp, 1, 1)

datcomb$GradeRankSimp[datcomb$GradeRankSimp %in%  "E" ] <- "Enlisted"
datcomb$GradeRankSimp[datcomb$GradeRankSimp %in%  "O" ] <- "Commissioned"

datcomb$GradeRankSimp[datcomb$GradeRankSimp %in%  "W" ] <- "Warrant"
## Wait, what is O
datcomb$GradeRankSimp <- factor(datcomb$GradeRankSimp)
table(datcomb$GradeRank, datcomb$GradeRankSimp)




## Simplify Education variable so that we can read the chart on 1 page
edLvls <- data.frame("lvls" = levels(datcomb$Education),
                     "nulvls" = c("Assoc", "Bachelors", "GED", "HSGrad", "PostGrad","SomeCollege","TechOrTradeSchool")) ##

datcomb$Ed <- datcomb$Education
levels(datcomb$Ed) <- c(levels(datcomb$Ed), as.character(edLvls$nulvls))

for(i in 1:nrow(edLvls)) datcomb$Ed[datcomb$Education %in% edLvls$lvls[i]] <- edLvls$nulvls[i]
datcomb$Ed <- factor(datcomb$Ed)
## Make an education index
data.frame(summary(datcomb$Ed))

datcomb$EdInd <- as.character(datcomb$Ed)

edVals <- c("GED" = "1", "HSGrad" = "1", "SomeCollege" = "2", "TechOrTradeSchool" = NA, "Assoc" = "3", "Bachelors" = "4", "PostGrad" = "5")

for(i in 1:length(edVals)) datcomb$EdInd[datcomb$EdInd %in% labels(edVals)[i]] <- edVals[i]
datcomb$EdInd <- as.numeric(as.character(datcomb$EdInd))
table(datcomb$Ed, datcomb$EdInd)


data.frame(datcomb$Ed)


## TechOrTradeSchool should be a separate Dummy Variable
datcomb$TechOrTS <- NA
datcomb$TechOrTS[datcomb$Ed == "TechOrTradeSchool"] <- 1
datcomb$TechOrTS[datcomb$Ed != "TechOrTradeSchool"] <- 0
table(datcomb$Ed, datcomb$TechOrTS)


## Relevel Ideology so we can easily read it

prefOrd <- levels(datcomb$Ideology)[c(6,1,3,2,5,4)]
datcomb$Ideo <- factor(datcomb$Ideology, levels = prefOrd)
datcomb$Ideo[datcomb$Ideo %in% "No Comment"] <- NA
datcomb$Ideo <- factor(datcomb$Ideo)

##Index for Ideology
levels(datcomb$Ideo)
datcomb$IdeoInd <- as.character(datcomb$Ideo)

IdVals <- c("Very conservative" = "5", "Conservative" = "4", "Moderate" = "3", "Liberal" = "2", "Very Liberal" = "1")

for(j in 1:length(IdVals)) datcomb$IdeoInd[datcomb$IdeoInd %in% labels(IdVals)[j]] <- IdVals[j]

datcomb$IdeoInd <- as.numeric(as.character(datcomb$IdeoInd))

#######################################################
## table(datcomb$Ideo, datcomb$IdeoInd) ##Looks Good ##
#######################################################

substr(datcomb$FemalesAreCompetent, 1, 1)


## Reorder MISO Influence for our chart

datcomb$MISOInfluence <- factor(datcomb$MISOInfluence, levels(datcomb$MISOInfluence)[c(3, 6, 1, 5, 2, 4)])


## Begin working on DVs for wave II
##First Dv to look at: ComfortFemales

levels(datcomb$ComfortFemales) ## All appears to be in order

## Look at q25: BossGenderPref
levels(datcomb$BossGenderPref) ## Too wordy. Let's cut it down

datcomb$BossPrefRec <- as.character(datcomb$BossGenderPref)
datcomb$BossPrefRec[datcomb$BossPrefRec %in% "A male boss?"] <- "Male"
datcomb$BossPrefRec[datcomb$BossPrefRec %in% "A female boss?"] <- "Female"
datcomb$BossPrefRec[datcomb$BossPrefRec %in% "No preference - No difference"] <- "No Difference"

table(datcomb$BossGenderPref, datcomb$BossPrefRec)
## Issue to talk with Dr. Haider-Markel about: We have only 8 people saying they'd like a female boss.

## Look at Q31 AKA the Cohesion Questions
## levels()

table(datcomb$CohesionIntegrationTensions )
##CohesionMaleDom

datcomb$CohesionMaleDom

##' a function to get the recoded Cohesion variables taken care of
##' @title cohesionRec
##' @param dat # A single factor variable with the levels seen in the factor command
##' @return dat, but with the levels reordered properly
##' @author Benjamin Rogers
cohesionRec <- function(dat){
    dat <- factor(dat, levels = c("Greatly Hurt Unit Cohesiveness",  "Somewhat Hurt Unit Cohesiveness",  "No Effect on Unit Cohesiveness"  ))
    return(dat)
}

##CohesionIntegrationTensions
datcomb$CohIntegrateTenseRec <- cohesionRec(datcomb$CohesionIntegrationTensions)

##CohesionMaleDom
datcomb$CohMaleDomRec <- cohesionRec( datcomb$CohesionMaleDom)
##CohesionNonMilitary

datcomb$CohNonMilitaryRec <- cohesionRec(datcomb$CohesionNonMilitary)

##CohesionLanguageBan

datcomb$CohLanguageBanRec <- cohesionRec(datcomb$CohesionLanguageBan)

##CohesionPromoInequitable

datcomb$CohPromoInequitableRec <- cohesionRec(datcomb$CohesionPromoInequitable)


##CohesionCivInterloping

datcomb$CohCivInterlopingRec <- cohesionRec(datcomb$CohesionCivInterloping)


##CohesionSexHarrass

datcomb$CohSexHarrassRec <-  cohesionRec(datcomb$CohesionSexHarrass)


## Rearrange MaleValues so that the order is correct

levels(datcomb$MaleValues)
datcomb$MaleValues <- factor(datcomb$MaleValues, levels = c("Strongly agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree"))

## datcomb[, c("MaleValues", "female")]

#############################################################
## index on how females are considered in American Society ##
## These are under Q42 FemalesAre... in the keys
## Note: The stereotypes are positive and negative
## Watch for negative codings (FemalesAreWarm is Positive ... for now)

## femalesarecols <- grep("FemalesAre", colnames(datcomb))
## datcomb[ , femalesarecols]

posaffect <- c("Competent", "Likeable", "Warm", "Independent", "Intelligent")
posaffectcols <- unlist(lapply(posaffect, function(x) grep(x, colnames(datcomb))))

## colnames(datcomb[,posaffectcols])

negaffect <- c("Emotional", "DifficultCoworkers", "Irritable", "Hostile", "Whiny", "Complaining")
negaffectcols <- unlist(lapply(negaffect, function(x) grep(x, colnames(datcomb))))

## colnames(datcomb[,negaffectcols])

goodaffectcols <- apply(datcomb[, posaffectcols], 2 , FUN = function(x) as.numeric(substr(as.character(x), 1,1)))

badaffectcols <- apply(datcomb[, negaffectcols], 2 , FUN = function(x) as.numeric(substr(as.character(x), 1,1)))


datcomb$affectIndex <- rowSums(goodaffectcols) -  rowSums(badaffectcols)

## hist(datcomb$affectIndex, breaks = length(unique(datcomb$affectIndex)))

############################################################
## MasculineFeminine/FeminineMasculine as numeric indices ##
############################################################

table(datcomb$FeminineMasculine, datcomb$female)

datcomb$FMindex <- as.numeric(substr(datcomb$FeminineMasculine, 1,2))
table(datcomb$FeminineMasculine, datcomb$FMindex)


table(datcomb$MasculineFeminine, datcomb$female)

datcomb$MFindex <- as.numeric(substr(datcomb$MasculineFeminine, 1,2))
table(datcomb$MasculineFeminine, datcomb$MFindex)


##Write the latest version of the code for Dr. Hader Markel
############################################################################################################################################
library(foreign)                                                                                                                       ##
write.dta(datcomb, file = "/Users/bjr/Dropbox/Women in Special Forces Project Folder/Ben Rogers & Carolina Costa Candal/CombData.dta") ##
############################################################################################################################################

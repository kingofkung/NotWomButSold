########################
## Data Analysis      ##
## Created 6/22/2015  ##
## A Ben Rogers Joint ##
########################

########################################
## Preamble: Data Analysis Goes here. ##
########################################
library(rockchalk)
library(gmodels)

CrossTable(dat$OpposeBin, dat$Woman, prop.chisq = F, prop.r = F, prop.t = F)

## Overall Ranking
CrossTable(dat$OpposeBin, dat$GradeRank, prop.chisq = F, prop.r = F, prop.t = F)

##Racial Categories(Done Separately due to lack of exclusivity)
pctable(dat$OpposeBin, dat$RaceWhite)
pctable(dat$OpposeBin, dat$RaceBlack)
pctable(dat$OpposeBin, dat$RaceHispanic)
pctable(dat$OpposeBin, dat$RaceAsian)
pctable(dat$OpposeBin, dat$RaceOther)

## Marital Status
pctable(dat$OpposeBin, dat$Marital)

## 18Xray
pctable(homdat$OpposeBin, homdat$XRay18)

###########################
## Local men, women, etc ##
###########################

pctable(femdat$OpposeBin, femdat$TreatmentVMaleSoldiers)
pctable(femdat$OpposeBin, femdat$TreatmentVLocalFemales)

pctable(femdat$OpposeBin, femdat$GenderAndLocalFemales)
pctable(femdat$OpposeBin, femdat$GenderAndLocalMales)

pctable(femdat$OpposeBin, femdat$GenderAndLocalChildren)
pctable(femdat$OpposeBin, femdat$GenderAndLocalYouth)



## PG 8 Units should remain Masculine
pctable(dat$OpposeBin, dat$MaleValues)

#######################################
## PG 6: Civillian affairs personnel ##
#######################################

pctable(homdat$OpposeBin, homdat$CAPersonnel)

#############################################
## Pg 12: Unit Cohesiveness qustions (Q31) ##
#############################################

pctable(dat$OpposeBin, dat$CohesionIntegrationTensions)
pctable(dat$OpposeBin, dat$CohesionMaleDom)
pctable(dat$OpposeBin, dat$CohesionNonMilitary)
pctable(dat$OpposeBin, dat$CohesionLanguageBan)
pctable(dat$OpposeBin, dat$CohesionPromoInequitable)
pctable(dat$OpposeBin, dat$CohesionCivInterloping)
pctable(dat$OpposeBin, dat$CohesionSexHarrass)

## Harrassment Training Assessments

pctable(dat$OpposeBin, dat$HarrassTrainingDoneRight)
pctable(dat$OpposeBin, dat$HarrassTrainingConfusing)
pctable(dat$OpposeBin, dat$HarrassTrainingInsufficient)
pctable(dat$OpposeBin, dat$HarrassTrainingCausesFear)
## ^Point this out^






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

#############################################################################
## Remember: Negative == Integrate, Positive == Segregate for coefficients ##
#############################################################################

CrossTable(dat$OpposeBin, dat$Woman, prop.chisq = F, prop.r = F, prop.t = F)

m1 <- glm(OpposeNum ~ Woman, data = dat, family = binomial)
summary(m1)

## Overall Ranking
CrossTable(dat$OpposeBin, dat$GradeRank, prop.chisq = F, prop.r = F, prop.t = F)
m2 <- update(m1, .~. - Woman + GradeRank)
summary(m2)
outreg(m2, type = "html")

##Racial Categories(Done Separately due to lack of exclusivity)
pctable(dat$OpposeBin, dat$RaceWhite)
pctable(dat$OpposeBin, dat$RaceBlack)
pctable(dat$OpposeBin, dat$RaceHispanic)
pctable(dat$OpposeBin, dat$RaceAsian)
pctable(dat$OpposeBin, dat$RaceOther)

m3 <- glm(OpposeNum ~ RaceComp, data = dat) ## This one's problematic. Since the Reference Category is just non-everything, these aren't really being compared to anything... Need advice!
summary(m3)

## Marital Status
pctable(dat$OpposeBin, dat$Marital)

m4 <- update(m2, . ~ . - GradeRank + Marital)
summary(m4)

## 18Xray
pctable(homdat$OpposeBin, homdat$XRay18)

m5 <- glm(OpposeNum ~ XRay18, data = homdat, family = binomial)
summary(m5)

###########################
## Local men, women, etc ##
###########################

pctable(femdat$OpposeBin, femdat$TreatmentVMaleSoldiers)
pctable(femdat$OpposeBin, femdat$TreatmentVLocalFemales)

m6 <- glm(OpposeNum ~ TreatmentVMaleSoldiers, data = femdat, family = binomial)
summary(m6)
m7 <- update(m6, . ~ TreatmentVLocalFemales)
summary(m7)
## Relevel to worse as reference

pctable(femdat$OpposeBin, femdat$GenderAndLocalFemales)
pctable(femdat$OpposeBin, femdat$GenderAndLocalMales)

m8 <- update(m7, . ~ GenderAndLocalFemales)
summary(m8)
m9 <- update(m7, . ~ GenderAndLocalMales)
summary(m9)

pctable(femdat$OpposeBin, femdat$GenderAndLocalChildren)
pctable(femdat$OpposeBin, femdat$GenderAndLocalYouth)

m10 <- update(m6, . ~ GenderAndLocalChildren)
summary(m10)

m11 <- update(m6, . ~ GenderAndLocalYouth)
summary(m11)


## PG 8 Units should remain Masculine
pctable(dat$OpposeBin, dat$MaleValues)

m12 <- glm(OpposeNum ~ MaleValues, data = dat)
summary(m12)

#######################################
## PG 6: Civillian affairs personnel ##
#######################################

pctable(homdat$OpposeBin, homdat$CAPersonnelInfluence)

sort(colnames(homdat))

m13 <- glm(OpposeNum ~ CAPersonnelInfluence, data = homdat, family = binomial)
summary(m13) ## Fascinating. This one is entirely in appropos order

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

pctable(dat$OpposeBin, dat$sharp)

msharp <- glm(OpposeNum ~ sharp, data = dat)
summary(msharp)
outreg(msharp, type = 'html')
## ^Point this out^

## Education data ## NOTE: Some of the data appears to be missing:
##Specifically some women who had pre-high school experience
pctable(dat$OpposeBin, dat$Education)

medu <- glm(OpposeNum ~ Education, data = dat)
summary(medu)


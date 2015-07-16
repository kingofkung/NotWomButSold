##################################
## Data Analysis 2: The Streets ##
## Created 6/25/2015            ##
## A Ben Rogers JOint           ##
##################################

#####################################################
## Preamble: In addition to restarting my recodes, ##
## I have restarted my analysis as well.           ##
#####################################################

## Quick note: WHile we can look at the new analysis, any specific comments need to be considered with a grain of salt.
##In order to make factor categories less terrible, I removed any / symbols, and replaced them with "or". I can fix this, but won't unless specifically asked. It's a bad idea to keep special characters that closely resemble the escape in reg expressions.

library(rockchalk)

head(femdat)

str(femdat)

dat <- homdat
varname <- "BirthYear"



## Display ideas
#########################
## Mosaic plot is cool ##
#########################

##################################################
## rgraph gallery for ideas on displaying stuff ##
##################################################

##########################################
## alt q breaks up giant blocks of text ##
##########################################

#############################################################################################################################################################################################
## ## Weird Hypotheses                                                                                                                                                                     ##
##                                                                                                                                                                                         ##
## ## Boys Club Hypothesis: Male soldiers who consider themselves more masculine will be less likely to support female integration                                                         ##
## levels(datcomb$MasculineFeminine) ## NOte that 10 is the reference category                                                                                                             ##
##                                                                                                                                                                                         ##
## m1 <- glm(NumSupport ~ MasculineFeminine, data = datcomb, family = binomial)                                                                                                            ##
## summary(m1)                                                                                                                                                                             ##
## exp(coef(m1))                                                                                                                                                                           ##
## plot(datcomb$NumSupport ~ datcomb$MasculineFeminine)                                                                                                                                    ##
## ## Racial Hypothesis: All minorities will be more likely to support integration                                                                                                         ##
## datcomb$RaceComb <- relevel(datcomb$RaceComb, "White")                                                                                                                                  ##
##                                                                                                                                                                                         ##
## m2 <- update(m1, . ~. - MasculineFeminine + RaceComb)                                                                                                                                   ##
## summary(m2)                                                                                                                                                                             ##
##                                                                                                                                                                                         ##
## ## Called it!                                                                                                                                                                           ##
##                                                                                                                                                                                         ##
## ## Divorce Hypothesis                                                                                                                                                                   ##
##                                                                                                                                                                                         ##
## m3 <- update(m1, . ~. - MasculineFeminine + MaritalSimp)                                                                                                                                ##
## summary(m3)                                                                                                                                                                             ##
##                                                                                                                                                                                         ##
##                                                                                                                                                                                         ##
## ## Pro Dev Hypothesis: Military professional development makes you less likely to support integration, but Female pro development makes you more likely to be supportive of integration ##
##                                                                                                                                                                                         ##
##                                                                                                                                                                                         ##
## m4 <- update(m1, . ~. - MasculineFeminine + ProfDevFemale + ProfDevMilitary +ProfDevCivilian)                                                                                           ##
## summary(m4)                                                                                                                                                                             ##
##                                                                                                                                                                                         ##
## ## Obvious Education question                                                                                                                                                           ##
## m5 <- update(m1, . ~. - MasculineFeminine + Education)                                                                                                                                  ##
## summary(m5)                                                                                                                                                                             ##
## mosaicplot(formula(m5), data = datcomb, color = T)                                                                                                                                      ##
## outreg(m5, type = 'html')                                                                                                                                                               ##
##                                                                                                                                                                                         ##
## ## ## See what the average what society thinks females are question looks like                                                                                                          ##
## m6 <- update(m1, . ~ . - MasculineFeminine + FemalesAreEmotional)                                                                                                                       ##
## summary(m6)                                                                                                                                                                             ##
##                                                                                                                                                                                         ##
## ## Ideology question                                                                                                                                                                    ##
## levels(datcomb$Ideology)                                                                                                                                                                ##
## table(datcomb$Ideology) ## Note the low number of very liberal people                                                                                                                   ##
## m7 <- update(m1, . ~ . - MasculineFeminine + Ideology)                                                                                                                                  ##
## summary(m7)                                                                                                                                                                             ##
##                                                                                                                                                                                         ##
## mosaicplot(formula(m7), data = datcomb)                                                                                                                                                 ##
##                                                                                                                                                                                         ##
## ##                                                                                                                                                                                      ##
## m8 <-  update(m1, . ~ . - MasculineFeminine + BirthYear)                                                                                                                                ##
## summary(m8)                                                                                                                                                                             ##
## plot(formula(m8), data = datcomb)                                                                                                                                                       ##
##                                                                                                                                                                                         ##
##                                                                                                                                                                                         ##
## ## THose who don't care about bathrooms are more likely to support integration                                                                                                          ##
##                                                                                                                                                                                         ##
## levels(datcomb$BathroomQuestion)                                                                                                                                                        ##
## m9 <- update(m1, .~. -MasculineFeminine + BathroomQuestion)                                                                                                                             ##
## summary(m9)                                                                                                                                                                             ##
##                                                                                                                                                                                         ##
## ## The more comfortable you are working with women, the more likely you are to support integration                                                                                      ##
##                                                                                                                                                                                         ##
## m10 <- update(m1, . ~ . - MasculineFeminine + ComfortFemales)                                                                                                                           ##
## summary(m10)                                                                                                                                                                            ##
#############################################################################################################################################################################################


## Wave 1.

## Note: Do men, and do women separately.
##########
## Race ##
##########

nrow(datcomb[datcomb$female == 1,])

m1m <- glm(NumSupport ~ RaceComb, data = datcomb[datcomb$female == 0,], family = binomial)
summary(m1m)

m1f <- glm(NumSupport ~ RaceComb, data = datcomb[datcomb$female == 1,], family = binomial)
summary(m1f)


##Age

m2m <- update(m1m, .~. - RaceComb + BirthYear, data = datcomb[datcomb$female == 0,])
summary(m2m)

m2f <- update(m1f, .~. - RaceComb + BirthYear, data = datcomb[datcomb$female == 1,])
summary(m2f)
##Marital Status

m3m <- update(m2m, .~. - BirthYear + MaritalSimp, data = datcomb[datcomb$female == 0,])
summary(m3m)

m3f <- update(m2f, .~. - BirthYear + MaritalSimp, data = datcomb[datcomb$female == 1,])
summary(m3f)


## Times Deployed
plot(datcomb$nDep)
table(datcomb$nDep)
m4m <- update(m3m, .~. - MaritalSimp + nDep, data = datcomb[datcomb$female == 0,])
summary(m4m)

m4f <- update(m3f, .~. - MaritalSimp + nDep, data = datcomb[datcomb$female == 1,])
summary(m4f)


## Education

m5m <- update(m4m, .~. - nDep + EdInd, data = datcomb[datcomb$female == 0,])
summary(m5m)

m5f <- update(m4f, .~. - nDep + EdInd, data = datcomb[datcomb$female == 1,])
summary(m5f)

## Ideology
levels(datcomb$Ideology)
m6m <- update(m5m, .~. - EdInd + IdeoInd, data = datcomb[datcomb$female == 0,])
summary(m6m)

m6f <- update(m5f, .~. - EdInd + IdeoInd, data = datcomb[datcomb$female == 1,]) ## Right here, tiny, tiny number of respondents in liberal and esp. very liberal category. ## Talk to profs about combining liberals together into 1 (still small) category
summary(m6f)

## Seen Combat

m7m <- update(m6m, .~. - IdeoInd + ServedInCombat, data = datcomb[datcomb$female == 0,])
summary(m7m)

m7f <- update(m6f, .~. - Ideo + ServedInCombat, data = datcomb[datcomb$female == 1,])
summary(m7f)

mgm <- glm(NumSupport ~ RaceComb + BirthYear + MaritalSimp + nDep + EdInd + IdeoInd + ServedInCombat + Reserves, data = datcomb[datcomb$female == 0,])


summary(mgm)

mgf <- update(mgm, .~. - Reserves, data = datcomb[datcomb$female == 1,])
summary(mgf)

## Do some outreg tables

outreg(list(m1m, m2m, m3m, m4m, m5m, m6m, m7m),
       type = 'latex', title = "Male Data Results")

outreg(list(m1f, m2f, m3f, m4f, m5f, m6f, m7f),
       type = 'latex', title = "Female Data Results")



## Next Phase: New DVs to indicate support
## Consider doing Ordered Logit, ordered Probit
## Supporting integration is higher, not supporting is lower
########################################
## Comfort working with females ##Q24 ##
########################################

############################
## preference of boss Q25 ##
############################
##############################
## Cohesion questions ##Q31 ##
##############################
#####################
## Q32  Male Values##
#####################

##############
## Q38, Q39 ##
##############

## Non-Control Independent variables
## and continue separating out the men and women
############################################################
## MasculineFeminine/FeminineMasculine as numeric indices ##
############################################################

#############################################################
## index on how females are considered in American Society ##
#############################################################
## These are under Q42 FemalesAre... in the keys
## Note: The stereotypes are positive and negative
## Watch for negative codings (FemalesAreWarm is Positive ... for now)

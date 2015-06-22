########################
## Data Analysis      ##
## Created 6/22/2015  ##
## A Ben Rogers Joint ##
########################

########################################
## Preamble: Data Analysis Goes here. ##
########################################

library(rockchalk)

pctable(dat$OpposeBin, dat$Woman)
pctable(dat$OpposeBin, dat$GradeRank)

##Racial Categories(Done Separately due to lack of exclusivity)
pctable(dat$OpposeBin, dat$RaceWhite)
pctable(dat$OpposeBin, dat$RaceBlack)


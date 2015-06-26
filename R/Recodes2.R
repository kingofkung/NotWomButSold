##################################
## Recodes 2: Electric Boogaloo ##
## Created 6/25/2015            ##
## A Ben Rogers Joint           ##
##################################

##################################################
## Preamble: Here are recodes s.t. analysis can ##
## Procead as needed                            ##
##################################################

femdat <- read.csv("/Users/bjr/Desktop/School/WSFDat/femdat.csv")
homdat <- read.csv("/Users/bjr/Desktop/School/WSFDat/homdat.csv")

## Fix a problem w/Homdat$BirthYear

as.character(homdat$BirthYear)
homdat$BirthYear[!homdat$BirthYear %in% as.character(1900:1999)] <- NA
homdat$BirthYear <- as.numeric(as.character(homdat$BirthYear))

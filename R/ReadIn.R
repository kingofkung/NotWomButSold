########################
## Data Read In       ##
## Created 6/11/15    ##
## A Ben Rogers Joint ##
########################

#########################################################
## Preamble: This file will allow for the read in      ##
## and clean up of the two data files for the project. ##
#########################################################


getwd()

library(foreign)

fem <- read.dta("/Users/bjr/Desktop/School/WSFDat/females15.dta")
hom <- read.dta("/Users/bjr/Desktop/School/WSFDat/males15.dta")


## Begin cleaning up the data

head(females15)
data.frame("fem" = colnames(fem)[1:114], "hom" = colnames(hom))


################
## vari <- 5  ##
## fem[,vari] ##
## hom[,vari] ##
################

########################
## Data Read In       ##
## Created 6/11/15    ##
## A Ben Rogers Joint ##
########################

####################################################
## Preamble: This file will allow for the read in ##
## of the two data files for the project.         ##
####################################################


getwd()

library(foreign)

females15 <- read.dta("/Users/bjr/Desktop/School/WSFDat/females15.dta")
males15 <- read.dta("/Users/bjr/Desktop/School/WSFDat/males15.dta")

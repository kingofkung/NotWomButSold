########################
## Data Recode        ##
## Created 6/16/15    ##
## A Ben Rogers Joint ##
########################

###############################################
## Preamble: This progeram will serve as the ##
## recoder for the male and female data      ##
###############################################

library(foreign)

fem <- read.dta("/Users/bjr/Desktop/School/WSFDat/females15.dta")
hom <- read.dta("/Users/bjr/Desktop/School/WSFDat/males15.dta")

femkey <- read.csv("/Users/bjr/Desktop/School/WSFDat/femkey.csv", header = TRUE)
homkey <- read.csv("/Users/bjr/Desktop/School/WSFDat/homkeytitled.csv", header = TRUE)

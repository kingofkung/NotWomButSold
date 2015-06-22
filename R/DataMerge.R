########################
## Data Merger        ##
## Created 6/22/2015  ##
## A Ben Rogers Joint ##
########################

#############################################################################
## Preamble: Now that I have taken the data, it is time to merge           ##
## the questions as well as the files so that an analysis can be conducted ##
#############################################################################

rm(list = ls())

## Read in files

getwd()
femdat <- read.csv("/Users/bjr/Desktop/School/WSFDat/femRec.csv")
homdat <- read.csv("/Users/bjr/Desktop/School/WSFDat/homRec.csv")

##Add Gender ID Variable

femdat$Woman <- 1
homdat$Woman <- 0

##Merge all variables of the same name into one big data frame
bigdat <- femdat[, colnames(femdat)[ colnames(femdat) %in% colnames(homdat)]]
bigdat <- rbind(bigdat, homdat[, colnames(homdat)[ colnames(homdat) %in% colnames(femdat)] ] )



write.csv(bigdat, "/Users/bjr/Desktop/School/WSFDat/mergeRec.csv")

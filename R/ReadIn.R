########################
## Data Read In       ##
## Created 6/11/15    ##
## A Ben Rogers Joint ##
########################


########################################################
## Preamble: This file will create  the read in       ##
## files for the two data files for the project.      ##
########################################################



getwd()

library(foreign)

fem <- read.dta("/Users/bjr/Desktop/School/WSFDat/females15.dta")
hom <- read.dta("/Users/bjr/Desktop/School/WSFDat/males15.dta")


## Begin cleaning up the data

 nrow(hom)

head(fem)
data.frame("fem" = colnames(fem)[1:114], "hom" = colnames(hom))
###############
## View(hom) ##
## View(fem) ##
###############

questno <- 4
summary(factor(fem[,"A6_Q4"]))
length(na.omit(hom[,questno + 1]))
hom[, "A2_Q17SPECIFIED_8"]

#######################################################################
## write.table(data.frame("varOrig" =  colnames(fem)),               ##
##             file = "/Users/bjr/Desktop/School/WSFDat/femkey.csv", ##
##             row.names = FALSE, col.names = TRUE,                  ##
##             sep = ",")                                            ##
##                                                                   ##
## write.table(data.frame("varOrig" = colnames(hom)),                ##
##             file = "/Users/bjr/Desktop/School/WSFDat/homkey.csv", ##
##             row.names = FALSE, col.names = TRUE,                  ##
##             sep = ",")                                            ##
#######################################################################

## read in keys
femkey <- read.csv("/Users/bjr/Desktop/School/WSFDat/femkey.csv", header = TRUE)
homkey <- read.csv("/Users/bjr/Desktop/School/WSFDat/homkey.csv", header = TRUE)

###################################################################
## remove AN_ designation at beginning of var orig in female key ##
###################################################################



femkey$VarToMatch <- gsub("A\\d+\\_", "", femkey$varOrig)
homkey$VarToMatch <- gsub("A\\d+\\_", "", homkey$varOrig)
## We'll need to create a coding to make this work
femkey$sampleWord <- gsub(" ","",femkey$Question)
homkey$sampleWord <- gsub(" ","",homkey$Question)

femkey$sampleWord <- gsub("\\d\\d\\D","",femkey$sampleWord)
homkey$sampleWord <- gsub("\\d\\d\\D","",homkey$sampleWord)

femkey$sampleWord <- gsub("\\d\\D","",femkey$sampleWord)
homkey$sampleWord <- gsub("\\d\\D","",homkey$sampleWord)


femkey$sampleWord <- substr(femkey$sampleWord, 1, 55)
homkey$sampleWord <- substr(homkey$sampleWord, 1, 55)

femkey$VarToMatch <- paste0(femkey$VarToMatch, femkey$sampleWord)
homkey$VarToMatch <- paste0(homkey$VarToMatch, homkey$sampleWord)


homkey$varNew <- femkey[match(homkey$VarToMatch, femkey$VarToMatch), "varNew"]

homkey$VarToMatch <- NULL
homkey$sampleWord <- NULL

##Eventually we'll have to do some manual matching on the homkey, but for now, this is better

write.csv(homkey, "/Users/bjr/Desktop/School/WSFDat/homkeytitled.csv")



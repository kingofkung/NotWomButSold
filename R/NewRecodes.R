########################
## New set of Recodes ##
## Created 6/24/2015  ##
## A Ben Rogers Joint ##
########################

#############################################################################
## Preamble: This code will attempt to reconsider how the data is read in, ##
## and hopefully make it possible to continue using the same analysis file ##
#############################################################################

library(xlsx)
rm(list = ls())

## Read in the data from its Excel Files
homdat <- read.xlsx2("/Users/bjr/Desktop/School/WSFDat/Active Duty SF Males - Expanded (WIP 11_07_14).xlsx", sheetIndex = "Raw")

homdatNG <- read.xlsx2("/Users/bjr/Desktop/School/WSFDat/National Guard SF Males - Expanded (WIP 11_07_14).xlsx", sheetIndex = "Raw")

femdat <- read.xlsx2("/Users/bjr/Desktop/School/WSFDat/Active Duty USASOC Females - Expanded (WIP 11_07_14).xlsx", sheetIndex = "Raw")


##Combine all male data
homdatcols <- ncol(homdat) - 3 ## The last 3 raw columns appear to be
homdat <- rbind(homdat[,1:homdatcols], homdatNG)


## Read in the old keys
femkey <- read.csv("/Users/bjr/Desktop/School/WSFDat/femkey.csv", header = TRUE, stringsAsFactors = F)
homkey <- read.csv("/Users/bjr/Desktop/School/WSFDat/homkey.csv", header = TRUE, stringsAsFactors = F)


## So we have a problem. The read in .xlsx file has had its spaces and parens in the title replaced by periods, while the excel files have had an X added to just about every title name except the first
data.frame(
    "Raw" =  head(colnames(femdat)),
    "Key" =  head(femkey$varOrig)
)



femdatnames <- colnames(femdat)
femdatnames <- gsub("[X]\\d{1,2}", "", femdatnames) ## Remove an X followed by 1 or 2 numbers in sequence.
femdatnames <- gsub("[.*]", "", femdatnames) ## Remove any number of periods and replace them with spaces


femkeynames <- femkey$varOrig
femkeynames <- gsub("[)]", "", femkeynames)
femkeynames <- gsub("\\d{1,2}\\s[(]", "", femkeynames)
femkeynames <- gsub("\\s", "", femkeynames)

colnames(femdat) <- femdatnames
femkey$varOrig <- femkeynames

data.frame( femkey$varOrig,  femkey$varNew)

##Guarantee that even if the data's out of order, we still get the right label to the right data


colnames(femdat) <- femkey$varNew[ match(colnames(femdat), femkey$varOrig)]


homdatnames <- colnames(homdat)
homdatnames <- gsub("[X]\\d{1,2}", "", homdatnames) ## Remove an X followed by 1 or 2 numbers in sequence.
homdatnames <- gsub("[.*]", "", homdatnames) ## Remove any number of periods and replace them with spaces


homkeynames <- homkey$varOrig
homkeynames <- gsub("[)]", "", homkeynames)
homkeynames <- gsub("\\d{1,2}\\s[(]", "", homkeynames)
homkeynames <- gsub("\\s", "", homkeynames)

colnames(homdat) <- homdatnames
homkey$varOrig <- homkeynames

colnames(homdat) <- homkey$varNew[ match(colnames(homkey), homkey$varOrig)]

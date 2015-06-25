########################
## New data cleanup   ##
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
##########################################
## data.frame(                          ##
##     "Raw" =  head(colnames(femdat)), ##
##     "Key" =  head(femkey$varOrig)    ##
## )                                    ##
##########################################



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

colnames(homdat) <- homkey$varNew[ match(colnames(homdat), homkey$varOrig)]


## Take from factors with dodgy levels to factors with less dodgy levels
homdatchar <- lapply(homdat, as.character)

## The "\\x{00a0} below indicates a unicode character as seen below
## recode st <U+00A0> or  \u00a0 is no longer anywhere.
homdatchar  <- lapply(homdatchar, gsub, pattern = "\\x{00a0}", replacement = "\\ ")

##Get rid of a couple of other patterns I dislike.
homdatchar <- lapply(homdatchar, gsub, pattern = "\"", replacement = "")
homdatchar  <- lapply(homdatchar, gsub, pattern = "[/]", replacement = "or")

str(homdatchar)
homdat <- as.data.frame(lapply(homdatchar, factor)) ## Factor everything and turn it into a data frame.
str(homdat)


femdatchar <- lapply(femdat, as.character)
femdatchar  <- lapply(femdatchar, gsub, pattern = "\\x{00a0}", replacement = "\\ ")
femdatchar  <- lapply(femdatchar, gsub, pattern = "\"", replacement = "")
femdatchar  <- lapply(femdatchar, gsub, pattern = "[/]", replacement = "or")


str(femdatchar)
femdat <- as.data.frame(lapply(femdatchar, factor))
str(femdat)

str(femdat$GenderAndLocalFemales)

head(as.data.frame(femdat))





## Write each of these to a .csv file so I don't have to deal with this code a lot more

write.csv(homdat,"/Users/bjr/Desktop/School/WSFDat/homdat.csv", row.names = FALSE)
write.csv(femdat,"/Users/bjr/Desktop/School/WSFDat/femdat.csv", row.names = FALSE)




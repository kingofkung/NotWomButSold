########################
## Data Recode        ##
## Created 6/16/15    ##
## A Ben Rogers Joint ##
########################

###############################################
## Preamble: This progeram will serve as the ##
## recoder for the male and female data      ##
###############################################
rm(list = ls())

library(foreign)

fem <- read.dta("/Users/bjr/Desktop/School/WSFDat/females15.dta")
hom <- read.dta("/Users/bjr/Desktop/School/WSFDat/males15.dta")

femkey <- read.csv("/Users/bjr/Desktop/School/WSFDat/femkey.csv", header = TRUE, stringsAsFactors = F)
homkey <- read.csv("/Users/bjr/Desktop/School/WSFDat/homkeytitled.csv", header = TRUE)

####################################################
## Figure out a way to create codings in our      ##
## key file, then enact those recodes in our data.##
####################################################

colnames(fem) <- femkey$varNew

colnames(femkey)
##Figure out way to easily recode using the format below




##' Import recodes
##'
##' take our recodes from the .csv  variable key and turn them into usable lists
##' @title recimp
##' @param recodeimp the imported text from the data file
##' @return a list with the original level in the name and the recoded name in the file
##' @author Ben Rogers
recimp <- function(recodeimp){
    recodr <- recodeimp
    recodr <- gsub("\"", "\"", recodr) ##Transform imported recode from having \" symbols to having just " symbols
    recodr <- gsub("\'", "\'", recodr) ## Do the same thing, but using ' instead of "
    recodr <- eval(parse(text = recodr)) ## Evaluate the imported recode as though it had been written in the R file rather than a .csv
}

##' Recode Just one factor variable level
##'
##' @title recimp
##' @param col a name of a variable level we would like to import in the format "name" = "newlevel"
##' @param data the data we would like to recode
##' @return the data with a recoded level
##' @author Benjamin Rogers
recodefunc <- function(col, data = fem$WhyJoin){
    levels(data) <- c(levels(data), col) ## Add data from col to the factor levels
    data[data %in% names(col)] <- col ## Change
    data <- factor(data) ## Remove the old level
    return(data)
}


## Temporarily moved vartitle up for ease of testing
vartitle <- "OfficerStatus"


colnames(femkey)
femkey$varNew[!is.na(femkey$RespOpts)]

## All below should be placed into a loop.
for(j in femkey$varNew[!is.na(femkey$RespOpts)]){

    vartitle <- j

    davar <- fem[, vartitle] ## Put our variable somewhere we can change it
    ##summary(factor(davar))

    ##savvar <- davar ## Save the original for comparison (can delete once verified)
    davar <- factor(davar) ##make our variable a factor
    dataRecode <- recimp(recodeimp = femkey[femkey$varNew %in% vartitle, "RespOpts"]) ## import our recode using new function
    for(i in 1:length(dataRecode)) davar <- recodefunc(col = dataRecode[i], data = davar) ##Using now evaluated recode list, recode davar
    fem[,vartitle] <- davar ## and return transformed variable to its original name

}


write.csv(fem, '/Users/bjr/Desktop/School/WSFDat/femRec.csv')

fem[,vartitle]
print(vartitle)
data.frame(summary(davar))

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



##Take our recoding variable from

recimp <- function(recodeimp ){
    recodr <- recodeimp
    recodr <- gsub("\"", "\"", recodr) ##Transform imported recode from having \" symbols to having just " symbols
    recodr <- eval(parse(text = recodr)) ## Evaluate the imported recode as though it had been written in the R file rather than a .csv
}


recodefunc <- function(col, data = fem$WhyJoin){
    levels(data) <- c(levels(data), col) ## Add data from col to the factor  levels
    data[data %in% names(col)] <- col ## Change
    return(data)
}


fem$WhyJoin <- factor(fem$WhyJoin) ##make our variable a factor

dataRecode <- recimp(recodeimp = femkey[3, "RespOpts"])
for(i in 1:length(dataRecode)) fem$WhyJoin <- recodefunc(col = dataRecode[i], data = fem$WhyJoin)


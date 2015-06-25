##################################
## Data Analysis 2: The Streets ##
## Created 6/25/2015            ##
## A Ben Rogers JOint           ##
##################################

#####################################################
## Preamble: In addition to restarting my recodes, ##
## I have restarted my analysis as well.           ##
#####################################################

## Quick note: WHile we can look at the new analysis, any specific comments need to be considered with a grain of salt.
##In order to make factor categories less terrible, I removed any / symbols, and replaced them with "or". I can fix this, but won't unless specifically asked. It's a bad idea to keep special characters that closely resemble the escape in reg expressions.

library(rockchalk)
library(ggplot2)

head(femdat)

str(femdat)

dat <- homdat
varname <- "BirthYear"


data.frame(
    'freqs' =
        sort(
            summary(2015 - dat[, varname]),
            decreasing = TRUE
            )
)

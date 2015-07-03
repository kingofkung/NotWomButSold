  ########################
## Plots!             ##
## Created 6/30/2015  ##
## A Ben Rogers Joint ##
########################

######################
## Preamble: Plots! ##
######################

library(ggplot2)
library(rockchalk)
library(lattice)
library(Hmisc)
library(reshape)

## Make it easier to see where the plots are going:
fileLoc <- "/Users/bjr/GitHub/NotWomButSOld/Output/"

## Plot 1: Percent breakdown of opposition reasons
levels(datcomb$WhyOpposeFemaleService)

labeling <- c("DeathsDemoralizing", "Pregnancy", "PrisonerPotential", "DoNotOppose", "MalesDiminishedEffectiveness", "PhysicalQualifications", "Other", "UnitCohesion", "LowPrivacy")


jpeg(paste0(fileLoc, "Opposition.jpg"))
plot(table(datcomb$WhyOpposeFemaleService), xaxt = "n")
axis(1, 1:9, labels = F)
text(x = 1:9 - .25, y = par("usr")[3]-35, labels = labeling, xpd = T, srt = 15)
dev.off()



## Racial breakdown in R, and a chance
jpeg(paste0(fileLoc, "RacialBreakdown.jpg"))
mp <- melt(table(datcomb$RaceComb))

ggplot(data = mp, aes(x = Var.1, y = value)) + geom_bar(stat = "identity")
dev.off()

## Racial breakdown v. Bathroom question, attempt 1
jpeg(paste0(fileLoc, "RaceAndBathroom.jpg"))
P <- table(datcomb$RaceComb, datcomb$BathroomQuestion)
Pm <- melt(P)

categories <- c("Asian", "Black", "Hispanic", "White")
catTots <- lapply(categories, function(u, dat = Pm) sum(dat$value[dat$Var.1 == u]))

catdf <- as.data.frame( catTots)
colnames(catdf) <- categories

Pm$colPercs <- NA
for(i in colnames(catdf)) Pm$colPercs[Pm$Var.1 == i] <- round(Pm[Pm$Var.1 == i,'value']/catdf[,i],2) *100

Pm$colPercslab <- paste0(Pm$colPercs, "%")

Pm$valuepercs <- paste0(round(Pm$value/sum(Pm$value), 2)*100, "%")

##Why people don't like using ggplot2
plotr <- ggplot(data = Pm, aes(x = Var.1, y = Var.2))

plotr <- plotr + geom_tile(aes(fill = colPercs), color = "white") ##, size = 35, shape = 15) ##


## plotr <- plotr + geom_point(aes(color = colPercs, size = colPercs), shape = 15) ##, size = 35, shape = 15) ##
################################################################################################################


plotr <- plotr + labs(x = "Race", y = "Comfort Using Unisex Bathroom")

plotr <- plotr + geom_text(aes(label = colPercslab)) ##

plotr <- plotr + scale_color_gradient(high = "blue", low = "yellow")
plotr <- plotr + theme(legend.position = 'none')
print(plotr)
dev.off()




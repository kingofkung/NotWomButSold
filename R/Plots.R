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

melty <- function(rowdat, coldat){

    P <- table(rowdat, coldat)
    Pm <- melt(P)

    Pm$rowdat <- factor(Pm$rowdat, levels = rev(levels(rowdat))) ## rev() reverses the order of whatever you put in there.
    Pm$coldat <- factor(Pm$coldat, levels = rev(levels(coldat)))

    categories <- levels(rowdat)
    catTots <- lapply(categories, function(u, dat = Pm) sum(dat$value[dat$rowdat == u]))

    catdf <- as.data.frame( catTots)
    colnames(catdf) <- categories

    Pm$colPercs <- NA
    for(i in colnames(catdf)) Pm$colPercs[Pm$rowdat == i] <- round(Pm[Pm$rowdat == i,'value']/catdf[,i],3) *100

    Pm$colPercslab <- paste0(Pm$colPercs, "%")

    Pm$valuepercs <- paste0(round(Pm$value/sum(Pm$value), 2)*100, "%")
    Pm
}



##Why people don't like using ggplot2

heatmappr <- function(dat = Pm,
                      rowdata = "rowdat",
                      coldata = "coldat",
                      filler = "colPercs",
                      labeler = "colPercslab",
                      xlabr = "thisIstheXLab",
                      ylabr = "ThisIsTheYLab",
                      percLabs= FALSE){

    plotr <- ggplot(data = dat, aes_string(x = rowdata, y = coldata))
    plotr <- plotr + geom_raster(aes_string(fill = filler), color = "white") ##, size = 35, shape = 15) ##

    plotr <- plotr + labs(x = xlabr, y = ylabr) ##

    if(percLabs == TRUE) plotr <- plotr + geom_text(aes_string(label = labeler )) ## ##


    plotr <- plotr + scale_fill_gradient(high = "red", low = "yellow") ##
    plotr <- plotr + theme_minimal()

    plotr <- plotr + theme(legend.position = 'none')
    print(plotr)
}

jpeg(paste0(fileLoc,  "RaceAndBathroom.jpg"), quality = 100) ## Save a jpeg to where the file should be located
meltedDat <- melty(rowdat = datcomb$RaceComb, coldat = datcomb$BathroomQuestion)
heatmappr(dat = meltedDat, xlabr = "Race", ylabr = "Willingness to Use Unisex Bathroom")
dev.off()

pctable(cv = datcomb$RaceComb,rv =  datcomb$BathroomQuestion)


edBR <- melty(rowdat = datcomb$Ed, coldat = datcomb$BathroomQuestion)
jpeg(paste0(fileLoc, "EducationandBathroom.jpg"), width = 480*2, height = 480, quality = 100)
heatmappr(dat = edBR, xlabr = "Level of Education", ylabr = "Unisex Bathroom Usage", percLabs = TRUE)
dev.off()
  pctable(datcomb$BathroomQuestion, datcomb$Ed)




IdeoVMISO <- melty(rowdat = datcomb$Ideo, coldat = datcomb$MISOInfluence)
jpeg(paste0(fileLoc, "Ideo v MISOInfluence.jpg"), width = 480*2, height = 480, quality = 100)
heatmappr(dat = IdeoVMISO, xlabr = "Ideology", ylabr = "MISO influence", percLabs = TRUE)
dev.off()

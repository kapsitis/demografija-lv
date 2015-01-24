#if (!"data.table" %in% installed.packages()) install.packages("data.table")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"grid" %in% installed.packages()) install.packages("grid")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"shiny" %in% installed.packages()) install.packages("shiny")
if (!"stats" %in% installed.packages()) install.packages("stats")
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")

#library(data.table)
library(ggplot2)
library(grid)
library(Hmisc)
library(plyr)
library(shiny) 
library(stats)

dataPath <- "/home/kalvis/demografija-lv/visc/sampledata"

setwd(dataPath)
source("../R/getAllData.R")

renamings <- getRenamings(dataPath)
nonCentralizedDF <- getNonCentralizedDF(dataPath)

socialIndicators <- getSocialIndicators(dataPath)

## Create the school demography table; 
## and attach municipality to every school there
schoolPop <- getSchoolPopulations(dataPath)
skolaPasvaldiba <- getSkolaPasvaldiba(dataPath, "skola_pasvaldiba.csv")
skolaPasvaldibaAlt <- getSkolaPasvaldiba(dataPath, "skola_pasvaldiba2.csv")
viisDati <- getViisDati(dataPath)

myMun <- sapply(as.vector(schoolPop$SchoolName), findMunBySch)
schoolPop$Municipality <- myMun
#schoolPop$Used <- rep(0,times=nrow(schoolPop))
renNumber <- as.vector(nonCentralizedDF$renamed)
nonCentralizedDF$studentNum <- sapply(renNumber,getStudentNum)
nonCentralizedDF$weightedResult <- nonCentralizedDF$result * nonCentralizedDF$studentNum


theSubject <- "MAT9"

factor <- "FemaleRatio"
# factorNames <- c("UnemploymentRate",
#             "DependencyRatio",
#             "PopulationChangePerYear",
#             "PerCapitaIncomeTax",
#             "ExpenditurePerStudent",
#             "PerCapitaRealEstateTax",
#             "FemaleRatio")

#for (gg in 2010:2014) {
gg <- 2011

imgName <- sprintf("temp/%s-%03d.png",factor,gg)

# fSchoolData <- nonCentralizedAndSocialDF[nonCentralizedAndSocialDF$year == gg & 
#                                            nonCentralizedAndSocialDF$subject == theSubject,]
fSchoolData <- subset(nonCentralizedDF, year == gg & subject == theSubject)

aggSchoolData <- aggregate(studentNum ~ municipality, fSchoolData, sum)
aggSchoolResult <- aggregate(weightedResult ~ municipality, fSchoolData, sum)

aggSchoolFull <- merge(aggSchoolData, aggSchoolResult, 
                       by.x = "municipality", by.y = "municipality")
aggSchoolFull$avg <- aggSchoolFull$weightedResult/aggSchoolFull$studentNum
siCurrent <- socialIndicators[socialIndicators$year==gg,]

aggSchoolWithSocial <- merge(aggSchoolFull, siCurrent, 
                             by.x = "municipality", by.y="Municipality")
aggSchoolWithSocial$pointSize <- (aggSchoolWithSocial$studentNum)^0.8
regionData <- getRegionData(dataPath)
aggSchoolUltimate <- merge(aggSchoolWithSocial, regionData, 
                           by.x="municipality", by.y="municipality")

theColors <- list("Riga city" = "#FF2F2F", 
                  "Latgale region" = "#3F4FFF", 
                  "Vidzeme region" = "#FF982F", 
                  "Kurzeme region" = "#2FBFD5", 
                  "Riga region" = "#68FF5F",
                  "Zemgale region" = "#D5FF2F"                    
)

myColors <- c("#2FBFD5","#3F4FFF", "#FF2F2F", "#68FF5F","#FF982F", "#D5FF2F" )

aggSchoolUltimate$theColor <- sapply(as.vector(aggSchoolUltimate$region), function(idx) { theColors[[idx]]})

aggSchoolUltimate <- aggSchoolUltimate[with(aggSchoolUltimate,order(-studentNum)),]



ourTitle <- sprintf("Vidējais %s un %s pašvaldībās",theSubject,indicatorsShort[[factor]])
ourSubtitle <- sprintf("%s.g., %s",gg,examTypes[[theSubject]])


my_grob <- grobTree(textGrob(gg, x=0.01,  y=0.95, hjust=0,
                            gp=gpar(col="#cccccc", 
                                    fontsize=25, 
                                    fontfamily="Helvetica", 
                                    fontface="bold")))


#  png(filename=imgName,  width = 800, height = 600)  
g4 <- ggplot(aggSchoolUltimate, aes_string(x=factor, y="avg", fill="region")) +
  annotation_custom(my_grob) +
  geom_point(shape=21, aes(size = pointSize)) +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0))) +
  scale_size_area(max_size = 16, 
                  breaks=c(8,40,189,910), 
                  labels=c("15","100","700","5000"), 
                  name="Eks\u0101menu\nskaits") +
  
  xScales[[factor]] +
  scale_y_continuous(
    name=sprintf("Vidējais %s rezultāts pašvaldībā, %%",theSubject), 
    minor_breaks=seq(40,80,by=5), breaks=seq(40,80,by=10)) +
  ggtitle(bquote(atop(.(ourTitle), atop(italic(.(ourSubtitle)), "")))) +
  theme(
    legend.title=element_text(size=12),  
    panel.background = element_rect(fill = 'white', colour = 'darkgreen'),
    plot.title = element_text(size = 20, face = "bold", colour = "black", vjust = -1),
    panel.grid.minor = element_line(colour="lightgray", size=0.5, linetype="dotted"),
    panel.grid.major = element_line(colour="black", size=0.5, linetype="dotted")
  ) +
  scale_fill_manual(values=myColors, name="Re\u0123ioni",
                    labels=c("Kurzeme", 
                             "Latgale", 
                             "R\u012Bga",
                             "Pier\u012Bga",
                             "Vidzeme",
                             "Zemgale")) +  
  geom_smooth(method = "loess", size = 0.6, fill=NA)  
  
g4

  

#  dev.off()
#}

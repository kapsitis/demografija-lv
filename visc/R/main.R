if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"shiny" %in% installed.packages()) install.packages("shiny")
if (!"stats" %in% installed.packages()) install.packages("stats")
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")

library(ggplot2)
library(Hmisc)
library(plyr)
library(shiny) 
library(stats)

dataPath <- "/home/kalvis/demografija-lv/visc/sampledata"

setwd(dataPath)
source("../R/getAllData.R")


## Non-centralized exam results
renamings <- getRenamings(dataPath)
nonCentralizedDF <- getNonCentralizedDF(dataPath)

socialIndicators <- getSocialIndicators(dataPath)
# nonCentralizedAndSocialDF <- merge(nonCentralizedDF, socialIndicators, 
#                      by.x = c("municipality","year"),
#                      by.y = c("Municipality","year"))



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


# df1 <- aggregate(totalExaminees ~ municipality + year + subject, nonCentralizedAndSocialDF, sum)
# 
# df1 <- aggregate(weightedResult ~ municipality + year + subject, nonCentralizedAndSocialDF, sum)


# neDF <- data.frame(municipality=neMunicipalities,school=neSchools)
# write.table(neDF, 
#             file="renamings.new.csv", 
#             sep=",",
#             qmethod="double", 
#             row.names=FALSE, 
#             fileEncoding="UTF-8")





### For loop by year
gg <- "2014"
theSubject <- "MAT9"

# fSchoolData <- nonCentralizedAndSocialDF[nonCentralizedAndSocialDF$year == gg & 
#                                            nonCentralizedAndSocialDF$subject == theSubject,]
fSchoolData <- subset(nonCentralizedDF, year == gg & subject == theSubject)

aggSchoolData <- aggregate(studentNum ~ municipality, fSchoolData, sum)
aggSchoolResult <- aggregate(weightedResult ~ municipality, fSchoolData, sum)

aggSchoolFull <- merge(aggSchoolData, aggSchoolResult, 
                       by.x = "municipality", by.y = "municipality")
aggSchoolFull$avg <- aggSchoolFull$weightedResult/aggSchoolFull$studentNum
siCurrent <- socialIndicators[socialIndicators$year=="2014",]

aggSchoolWithSocial <- merge(aggSchoolFull, siCurrent, 
                             by.x = "municipality", by.y="Municipality")
aggSchoolWithSocial$pointSize <- 2*sqrt(aggSchoolWithSocial$studentNum)
regionData <- getRegionData(dataPath)
aggSchoolUltimate <- merge(aggSchoolWithSocial, regionData, 
                           by.x="municipality", by.y="municipality")

# 
# aggSchoolData <- aggregate(result ~ municipality, fSchoolData, mean)
# sbr <- getSocialByRegion(dataPath)
# fullSchools <- merge(aggSchoolData, sbr, 
#                      by.x = "municipality",
#                      by.y = "municipality")

theColors <- list("Riga city" = "#FF2F2F", 
                  "Latgale region" = "#3F4FFF", 
                  "Vidzeme region" = "#FF982F", 
                  "Kurzeme region" = "#2FBFD5", 
                  "Riga region" = "#68FF5F",
                  "Zemgale region" = "#D5FF2F"                    
)

myColors <- c("#2FBFD5","#3F4FFF", "#FF2F2F", "#68FF5F","#FF982F", "#D5FF2F" )

aggSchoolUltimate$theColor <- sapply(as.vector(aggSchoolUltimate$region), function(idx) { theColors[[idx]]})

plot(aggSchoolUltimate$FemaleRatio, 
     aggSchoolUltimate$avg, 
     col="black",
     bg=aggSchoolUltimate$theColor, 
     lwd=1,
     pch=21,
     xlab="Darba meklētāji %", 
     ylab="MAT9 rezultāti %")
title("2014.g. 9.kl. matemātika un bezdarbs pašvaldībās")
lines(lowess(aggSchoolUltimate$FemaleRatio,aggSchoolUltimate$avg), col="blue")







ourTitle <- "Necentralizētie eksāmeni un bezdarbs pašvaldībās"
ourSubtitle <- "2014.g., 9.kl. matemātika"


ggplot(aggSchoolUltimate, aes(x=UnemploymentRate, y=avg, fill=region)) +
  geom_point(shape=21, aes(size = pointSize)) +
  #  geom_point() +
  
  scale_x_continuous(name="Darba meklētāji %",
                     minor_breaks=seq(0, 27.5, by=2.5), breaks=seq(0,25,by=5)) +
  scale_y_continuous(name="MAT9 rezultāts% (svērts vidējais skolām pašvaldībā)", 
                     minor_breaks=seq(40,80,by=5), breaks=seq(40,80,by=10)) +
  ggtitle(bquote(atop(.(ourTitle), atop(italic(.(ourSubtitle)), "")))) +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'darkgreen'),
    #    legend.position="none",
    plot.title = element_text(size = 20, face = "bold", colour = "black", vjust = -1),
    panel.grid.minor = element_line(colour="lightgray", size=0.5, linetype="dotted"),
    panel.grid.major = element_line(colour="black", size=0.5, linetype="dotted")
  ) +
  scale_fill_manual(values=myColors) 
#  geom_smooth(method=lm,  se=FALSE) 



















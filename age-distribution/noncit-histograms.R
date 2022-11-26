# Set this path to the one that you are using

# setwd("/home/st/java-eim/java-eim-parent/src/site/resources/R/dataproc/demography/")
# source("noncit-load.R")

library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



#source("noncit-load.R")
dsets <- c("2007-01-01", "2007-07-01", "2008-01-01", "2008-07-01", 
           "2009-01-01", "2009-07-01", "2010-01-01", "2010-07-01", 
           "2011-01-01", "2011-07-01", "2012-01-01", "2012-07-01", 
           "2013-01-01", "2013-07-01")

getYear <- function(x) {
  ddd <- as.numeric(as.Date(x) - as.Date("2007-01-01"))
  round(2007+ddd/365.25, digits=2)
}

df <- read.table("DzGads-VPd-2007_2013.csv", head=TRUE, sep=",")



nonCitAges <- df[df$ReportDate=="2013-01-01","LvNonCitizen"]

##################################################
### 1st histogram - age groups 0-4, 5-9, etc.
##################################################
breaks <- seq(0,100,by=5)
interv <- findInterval(0:111,breaks)
slices <- vector()
for (i in 1:length(breaks)) {
  slices[i] <- sum(nonCitAges*(interv==i))
}
breakLabels <- breaks
breakLabels[21] <- "100+"

png("noncit-histogram1.png", width=600, height=450)
barplot(slices, names.arg=breakLabels, col="pink",
  xlab="Vecuma grupas pa 5 gadiem (0-4, 5-9, utt.)", 
  ylab="Skaits")
title("Nepilso\u0146u vecumu histogramma (1)")
dev.off()


##################################################
### 2nd histogram - every year (0-99 and also 100+)
##################################################
png("noncit-histogram2.png", width=600, height=450)
newNames <- rep("",101)
newVar <- nonCitAges[1:100]
newVar[101] <- sum(nonCitAges[101:length(nonCitAges)])
barplot(newVar, names.arg = newNames, 
        col="pink",space=0, ylim=c(0,7500),
        xlab="Vecums", ylab="Skaits 2013-01-01")
for (m in 0:10) {
  if (m %% 2 == 0) {
    polygon(c(m*10,m*10,(m+1)*10,(m+1)*10),
            c(0,7500,7500,0),col='lightskyblue1', border=NA)
  }
  else {
    polygon(c(m*10,m*10,(m+1)*10,(m+1)*10),
            c(0,7500,7500,0),col='white', border=NA)
  }
}

# Try transparent colors
pinkRGB <- as.vector(col2rgb("pink"))/255
newPink <- rgb(pinkRGB[1],pinkRGB[2],pinkRGB[3],0.5)


barplot(newVar, names.arg = newNames, 
        col=newPink,space=0, ylim=c(0,7500), add=TRUE)
box()
abline(col="lightgray",lty="dotted",h=(1000*(1:7)))
midpts <- seq(0,100,by=5)
midptsShifted <- midpts + 0.5
midpts <- as.character(midpts)
midpts[21] <- "100+"
axis(1, at = midptsShifted, labels=midpts, cex.axis=0.9)
title("Nepilso\u0146u vecumstrukt\u016Bra")
dev.off()

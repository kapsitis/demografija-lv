# Set this path to the one that you are using

#setwd("/home/student/java-eim/java-eim-parent/src/site/resources/R/dataproc/demography/")
#source("noncit-load.R")



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



peopleCategories <- c("LvCitizen", "LvNonCitizen", "Foreign",
                      "LvStateless", "LvAlternative", "LvRefugee")
categoryColors <- c("LvCitizen" = "chartreuse3",
                    "LvNonCitizen" = "red", 
                    "Foreign" = "blue",
                    "LvStateless"="gray", 
                    "LvAlternative" = "magenta", 
                    "LvRefugee" = "orange")

totCount <- function(dd, colName) {
  sum(df[df$ReportDate == dd, colName]) 
}

totCountList <- list()
for (cc in peopleCategories) {
  totCountList[[cc]] <- as.vector(sapply(X=dsets,FUN=totCount,colName=cc))
}

png("population-categories.png", width=700, height=450)
x=getYear(dsets)
par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)

plot(x, totCountList[["LvCitizen"]], 
     log="y", yaxt="n", xlab="P\u0101rskata gads", 
     ylab="Skaits kategorij\u0101",
     ylim=c(1, 2000000), type="n", 
     pch=16, col=categoryColors[["LvCitizen"]])

for (pcat in peopleCategories) {
  points(x, totCountList[[pcat]], type="o", cex=1.5,
    pch=16, col=categoryColors[[pcat]])
}

axis(2,at=c(1000000, 100000, 10000, 1000, 100, 10, 1),
     labels=c(expression(10^6), 
              expression(10^5),
              expression(10^4), 
              1000, 100, 10, 1))
title("Iedz\u012Bvot\u0101ju kategorijas Latvij\u0101")
legend("topright", inset=c(-0.45,0), 
       legend=c("Pilso\u0146i",
                "Nepilso\u0146i",
                "\u0100rzemnieki",
                "Bezvalstnieki",
                "Alternat\u012Bvais st.",
                "B\u0113g\u013ci"), 
       pch=16, lty = 1, cex = 1.5, text.font=0,
       col=as.vector(categoryColors))
par(xpd=FALSE)
abline(col = "lightgray", lty = "dotted", v=2007:2013)
abline(col = "lightgray", lty = "dotted", 
       h=c(1000000, 100000, 10000, 1000, 100, 10, 1))
dev.off()

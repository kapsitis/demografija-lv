# Set this path to the one that you are using


#setwd("/home/student/java-eim/java-eim-parent/src/site/resources/R/dataproc/demography/")

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


# gap.plot() needs "plotrix" package
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")
library(plotrix)

avgAge <- function(dd,colName) {
  dFrame <- df[df$ReportDate == dd, ]
  
  
  v1 <- as.vector(dFrame[[colName]])
  v2 <- as.vector(getYear(dd) - (dFrame$BirthYear + 0.5))
  (rr <- sum(v1*v2)/sum(v1))
}


peopleCategories <- c("LvCitizen", "LvNonCitizen", "Foreign")
categoryColors <- c("LvCitizen" = "chartreuse3",
                    "LvNonCitizen" = "red", 
                    "Foreign" = "blue")

avgAgesList <- list()
for (cc in peopleCategories) {
  avgAgesList[[cc]] <- as.vector(sapply(X=dsets,FUN=avgAge,colName=cc))
}



##################################################
### Draw a plain chart with colored dots
##################################################
years <- as.vector(sapply(dsets, getYear))
twogrp <- c(avgAgesList[["LvCitizen"]], 
            avgAgesList[["LvNonCitizen"]],
            avgAgesList[["Foreign"]])
gpcol<-rep(c("chartreuse3","red","blue"),each=length(years))
yr <- rep(years,times=3)
png("noncit-aging-2007_2013.png", width = 600, height = 450)
plot(yr, twogrp, col=gpcol, pch=16, cex=1.5, 
     xlab="P\u0101rskata datums", ylab="Vid\u0113jais vecums")
grid()
title("Noveco\u0161anas \u0101trums pa kategorij\u0101m")
dev.off()


##################################################
### Draw the same chart with a gap and a legend
##################################################
png("noncit-aging-full-2007_2013.png", width = 600, height = 450)
# allow copying between output devices
dev.control(displaylist='enable') 

gap.plot(yr,twogrp,gap=c(39.5,49.5),
         main="Pilso\u0146u, nepilso\u0146u un \u0101rzemnieku\n noveco\u0161anas \u0101trumi",
         col=gpcol, pch=16, cex=1.2,
         ytics=c(38,39,40,50,51,52,53,54,55),
         xlab="PMLP p\u0101rskata datums", 
         ylab="Vid\u0113jais aritm\u0113tiskais vecums")
axis.break(axis=2,breakpos=39.5,pos=NA,bgcol="white",breakcol="black",
           style="slash",brw=0.04)

LehmanBrothers <- as.Date(c("2008-09-15", "2008-01-11"))
xx <- 2008+(LehmanBrothers[1]-LehmanBrothers[2])/365
abline(v = xx, lty=2, lwd=2, col="black")

grid()
temp <- legend("topleft", legend = c(" "," "," "),
               text.width = strwidth("\u0100rzemnieki"), 
               col=c("chartreuse3","red","blue"),
               lty = 1, xjust = 1, yjust = 1, lwd=c(1,1,1), 
               pch=16, cex=1.5, text.font=0)
text(temp$rect$left + temp$rect$w, temp$text$y,
     c("Pilso\u0146i", "Nepilso\u0146i", 
       "\u0100rzemnieki"), pos = 2)
abline(v = 2010.5, lty=2, lwd=2, col="magenta")

# Save intermediate plot - gap-plot (without regression lines)
dev.copy(png,file="noncit-aging-gap-2007_2013.png", width=600, height=450)
dev.off()


##################################################
### Simple Linear Regression 
##################################################

for (cc in peopleCategories) {
  # Linearas regresijas taisne alpha+beta*t. 
  beta <- (mean(years[5:14]*avgAgesList[[cc]][5:14]) - 
             mean(years[5:14])*mean(avgAgesList[[cc]][5:14]))/
    (mean(years[5:14]*years[5:14]) - (mean(years[5:14]))^2)
  alpha <- mean(avgAgesList[[cc]][5:14]) - beta*mean(years[5:14])
  print(paste("For category '", cc, "' the aging speed is ", beta, sep=""))
  if (cc == "LvCitizen") {
    vv <- alpha + beta*years
    points(years[5:14],vv[5:14], type='l', 
           col=categoryColors[[cc]])
    points(years[1:5],vv[1:5], type='l', 
           lty=3, lwd=2,  col=categoryColors[[cc]])    
  }
  if (cc == "LvNonCitizen") {
    vv <- alpha + beta*years - 10
    points(years[5:14], vv[5:14], type="l", 
           lty=1, col=categoryColors[[cc]])
    points(years[1:5],vv[1:5], type='l', 
           lty=3, lwd=2,  col=categoryColors[[cc]])    
  }  
}
dev.off()




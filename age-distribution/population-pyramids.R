# Set this path to the one that you are using
setwd("/home/student/java-eim/java-eim-parent/src/site/resources/R/dataproc/demography/")
source("noncit-load.R")

if (!"pyramid" %in% installed.packages()) install.packages("pyramid")
library(pyramid)

dset = "2013-07-01"
startInt <- seq(0,99,by=1)

# initialize pyramid slices - 101 slices in total. 
# citsData[1] - 0-years old; 
# citsData[2] - 1-year old
# ... 
# citsData[101] - 100+ years old
nonCitsData <- rep(0,times=length(startInt)+1)
citsData <- rep(0,times=length(startInt)+1)

endInt <- seq(1,100,by=1)  
ages <- paste(startInt,"-",endInt,sep="")
ages <- c(ages,"100+")  
dFrame <- df[df$ReportDate == dset,]

for (kk in 1:nrow(dFrame)) {
  # for January 1 of each year simply add the
  # birthYear groups to respective pyramid slices
  if (round(getYear(dset)) == getYear(dset)) {
    incrAge <- round(getYear(dset)) - dFrame[kk,"BirthYear"]
    # should not assign for indices > 101
    # they all should be in "100+" group
    idx <- min(incrAge, length(startInt)+1)
    nonCitsData[idx] <- nonCitsData[idx] + dFrame[kk,"LvNonCitizen"]
    citsData[idx] <- citsData[idx] + dFrame[kk,"LvCitizen"]
  }
  # for July 1 of each year add one half of the 
  # birthYear group to one pyramid slice;
  # another half - to the next one
  else {
    incrAge <- floor(getYear(dset))- dFrame[kk,"BirthYear"]
    # guys born in this year are of age 0
    if (incrAge == 0) {
      nonCitsData[1] <- nonCitsData[1] + dFrame[kk,"LvNonCitizen"]
      citsData[1] <- citsData[1] + dFrame[kk,"LvCitizen"]
    }
    else {
      idx1 <- min(incrAge, length(startInt)+1)
      idx2 <- min(incrAge + 1, length(startInt)+1)
      nonCitsData[idx1] <- nonCitsData[idx1] + dFrame[kk,"LvNonCitizen"]/2
      citsData[idx1] <- citsData[idx1] + dFrame[kk,"LvCitizen"]/2
      nonCitsData[idx2] <- nonCitsData[idx2] + dFrame[kk,"LvNonCitizen"]/2
      citsData[idx2] <- citsData[idx2] + dFrame[kk,"LvCitizen"]/2
    }
  }    
}



png("noncit-pyramid-2013-07-01.png", width=700, height=700)

# Create a dataframe - 
# (1) 101 noncitizen annual slices, 
# (2) 101 citizen annual slices
# (3) numbers from 0 to 99 appended by "100+"
data <- data.frame(nonCitsData,citsData,c(startInt,"100+"))
pyramid(data,Lcol="Pink",Rcol="#009900",
        Llab="                                    Nepilso\u0146i",
        Rlab="Pilso\u0146i",Clab="Vecums", 
        AxisFM="d", AxisBM="",
        Laxis=seq(0,40000,length.out=5), 
        Csize=0.8, Cstep =5, Cadj=-0.034)
title("\n\n\n\nPilso\u0146u-nepilso\u0146u vecumpiram\u012Bda")
dev.off()


##################################################
### Adding horizontal lines
##################################################
png("noncit-pyramid-full-2013-07-01.png", width=700, height=700)
pyramid(data,Lcol="Pink",Rcol="#009900",
        Llab="                                    Nepilso\u0146i",
        Rlab="Pilso\u0146i",Clab="Vecums", 
        AxisFM="d", AxisBM="",
        Laxis=seq(0,40000,length.out=5), 
        Csize=0.8, Cstep =5, Cadj=-0.034)

k <- 100/101
segments(c(-0.5,-0.5), c(0.67*k,0.72*k), 
         c(-0.1,-0.1), c(0.67*k,0.72*k), col="red")

segments(c(0.1,0.1), c(0.65*k,0.70*k), 
         c(0.75,0.75), c(0.65*k,0.70*k), col="red")
title("\n\n\n\nPilso\u0146u-nepilso\u0146u vecumpiram\u012Bda")
dev.off()

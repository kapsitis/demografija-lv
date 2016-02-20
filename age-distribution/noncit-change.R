# Set this path to the one that you are using
setwd("/home/student/java-eim/java-eim-parent/src/site/resources/R/dataproc/demography/")
source("noncit-load.R")

# draw.circle() needs "plotrix" package
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")
library(plotrix)

# all the ages - starting from birthyear 2013, until birthyear 1914
years <- 2013:1913

# for each birthyear compute the min and max of the noncitizen counts
all_ranges <- sapply(years, function(yyyy) { 
  vv <- df[df$BirthYear == yyyy, "LvNonCitizen"]
  if (yyyy < 2007) { range(vv) }
  else { range(tail(vv,n=max(length(vv)-1,1))) }
})

# compute absolute and relative decreases
decr_abs <- all_ranges[2,] - all_ranges[1,]
decr_rel <- (all_ranges[2,] - all_ranges[1,])/all_ranges[2,]
  
# Absolute decrease of noncitizens (depending on their age)
png("noncit-change-absolute.png", width = 600, height = 450)
plot(0:100, decr_abs, type="h",
  ylab="Absol\u016btais samazin\u0101jums", 
  xlab="Vecums (veselos gados) 2013.gada beig\u0101s", 
  col="red")
title("Nepilso\u0146u skaita absol\u016btais samazin\u0101jums")
dev.off()

# Relative decrease of noncitizens (depending on their age)
png("noncit-change-relative.png", width = 600, height = 450)
plot(0:100, decr_rel, type="h",
  ylab="Samazin\u0101jums kop\u0161 2007.g. vai dzim\u0161anas br\u012B\u017Ea", 
  xlab="Vecums (veselos gados) 2013.gada beig\u0101s",
  axes=FALSE, col="red")
box()
axis(side=1)
axis(side=2, at=pretty(decr_rel), labels=paste0(100*pretty(decr_rel),"%"))
title("Relat\u012Bvais samazin\u0101jums atkar\u012Bb\u0101 no vecuma")
draw.circle(x=60, y=0.23, radius=7, border="blue", lwd=1)
dev.off()

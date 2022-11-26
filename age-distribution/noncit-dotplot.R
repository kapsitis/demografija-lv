# Set this path to the one that you are using
#setwd("/home/student/java-eim/java-eim-parent/src/site/resources/R/dataproc/demography/")

if (!"Unicode" %in% installed.packages()) install.packages("Unicode")
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
library("Unicode")
library("RCurl")

library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# urlPrefix = "https://docs.google.com/spreadsheet/pub?key=0Asrw_"
# urlKey = "eAzHG5ydGp5X014eG5LbWVyVnBocW5jUU1yZ0E"
# urlSuffix = "&single=true&gid=0&output=csv"
# 
# csvUrl <- paste0(urlPrefix,urlKey,urlSuffix)  
# print(paste0("Downloading from URL ", csvUrl))
# csvFile <- getURL(csvUrl, ssl.verifypeer=FALSE)
# df <- read.csv(textConnection(csvFile), encoding="UTF-8")
# print(paste0("Total rows read: ",nrow(df)))


df <- read.table("Taut-VPd-2013-07-01.csv", header=TRUE, sep=",")



zdf <- df[ order(-df[,3]/(df[,2] + df[,3]),df[,1]), ]
zzdf <- zdf[ zdf[2] + zdf[3] >= 100, ]

firstData <- zzdf[,3]/(zzdf[,2] + zzdf[,3])
firstLabels <- u_to_lower_case(as.vector(zzdf[,1]))

png("noncit-dotplot-2013-07-01.png", width=600, height=450)
dotchart(firstData,labels=firstLabels,cex=0.8,
         main="Nepilso\u0146u \u012Bpatsvari taut\u012Bb\u0101s", 
         xlab="Nepilso\u0146u \u012Bpatsvars")
grid()
dev.off()
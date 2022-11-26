# Set this path to the one that you are using
#setwd("/home/st/java-eim/java-eim-parent/src/site/resources/R/dataproc/demography/")


library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# needed for getURL()
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
library(RCurl)

urlPrefix = "https://docs.google.com/spreadsheet/pub?key=0Asrw_"
urlSuffix = "&single=true&gid=0&output=csv"
urlKey = "eAzHG5ydEFOVTJDTG8zLUc4dDgySnpKQVJkanc"


dsets <- c("2007-01-01", "2007-07-01", "2008-01-01", "2008-07-01", 
           "2009-01-01", "2009-07-01", "2010-01-01", "2010-07-01", 
           "2011-01-01", "2011-07-01", "2012-01-01", "2012-07-01", 
          "2013-01-01", "2013-07-01")

# Transform date from the YYYY-MM-DD notation to real number 
# E.g. for January 1 - getYear("2013-01-01") is 2013, and
# for July 1 - getYear("2013-07-01") is 2013.5
getYear <- function(x) {
  ddd <- as.numeric(as.Date(x) - as.Date("2007-01-01"))
  round(2007+ddd/365.25, digits=2)
}


csvUrl <- paste0(urlPrefix,urlKey,urlSuffix)  
print(paste0("Downloading from URL ", csvUrl))

##### If internet connection does not work, 
##### read the CSV file from a local copy:
#csvFile <- getURL(theUrl, ssl.verifypeer=FALSE)
#df <- read.csv(textConnection(csvFile))
df <- read.csv("DzGads-VPd-2007_2013.csv")

# These commands may be used to test that you 
# have indeed received the necessary data in the
# dataframe 'df'
print(paste0("Total rows read: ",nrow(df)))
write.table(df, "DzGads-VPd-2007_2013-local-copy.csv", sep=",",row.names=FALSE)


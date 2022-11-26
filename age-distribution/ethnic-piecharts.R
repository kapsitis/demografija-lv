# Set this path to the one that you are using
# setwd("/home/student/java-eim/java-eim-parent/src/site/resources/R/dataproc/demography/")

if (!"plotrix" %in% installed.packages()) install.packages("plotrix")
if (!"Unicode" %in% installed.packages()) install.packages("Unicode")
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
library("plotrix")
library("Unicode")
library("RCurl")
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#urlPrefix = "https://docs.google.com/spreadsheet/pub?key=0Asrw_"
#urlKey = "eAzHG5ydGp5X014eG5LbWVyVnBocW5jUU1yZ0E"
#urlSuffix = "&single=true&gid=0&output=csv"
#csvUrl <- paste0(urlPrefix,urlKey,urlSuffix)  
#print(paste0("Downloading from URL ", csvUrl))
#csvFile <- getURL(csvUrl, ssl.verifypeer=FALSE)




#df <- read.csv(textConnection(csvFile), encoding="UTF-8")
df <- read.table("Taut-VPd-2013-07-01.csv", header=TRUE, sep=",")


print(paste0("Total rows read: ",nrow(df)))

# Order in decreasing order by # of noncitizens
xdf <- df[ order(-df[,3], df[,1]), ]
allNoncits <- sum(xdf[,"LvNonCitizens"])

##################################################
### First piechart - major ethnic groups
##################################################
firstData <- c(xdf[1:5,3], allNoncits - sum(xdf[1:5,3]))
firstLabels <- u_to_lower_case(c(as.vector(xdf[1:5,1]), "citi-sk.blakus"))
firstColors <- rainbow(6)
firstColors[6] <- "gray"
png("simple-pie1-2013-07-01.png", width=600, height=450)
pie(firstData, firstLabels, col=firstColors, clockwise=FALSE,
    main="Nepilso\u0146u sektoru diagramma - 1")
dev.off()

##################################################
### Second piechart - smaller ethnic groups
##################################################
secondData <- c(xdf[6:21,3], allNoncits - sum(xdf[1:21,3]))
secondLabels <- u_to_lower_case(c(as.vector(xdf[6:21,1]), "visi citi"))
secondColors <- c(topo.colors(16), "gray")
png("simple-pie2-2013-07-01.png", width=600, height=450)
pie(secondData, secondLabels, col=secondColors, clockwise=TRUE,
    main="Nepilso\u0146u sektoru diagramma - 2")
dev.off()


##################################################
### Combine both piecharts in a single plot
##################################################
png("ethnic-piecharts-2013-07-01.png", width=600, height=450)
# Draw an empty coordinate plane, without any graph, axes, grids or labels
plot(x=c(1,5), y=c(1,3), axes=FALSE, type="n", bty="n", xlab="", ylab="")

# x, y and r of both circles (floating.pies)
c1x <- 1.8
c1y <- 2.3
c1r <- 0.9
c2x <- 4.0
c2y <- 1.5
c2r <- 0.7

floating.pie(c1x, c1y, firstData, col=firstColors, radius=c1r)
floating.pie(c2x, c2y, secondData, col = secondColors, radius=c2r)

title("Nepilso\u0146u etniskais sast\u0101vs")

legend(
  x=1, y=1.5, box.lty=0,
  legend=firstLabels,
  pch=21,
  pt.bg=firstColors,
  pt.cex=3,
  y.intersp=1.2,
  ncol=2
)

legend(
  x=3.1, y=3.1, box.lty=0,
  legend=secondLabels,
  pch=21,
  pt.bg=secondColors,
  text.width=strwidth("azerbaidzanis"),
  pt.cex=2,
  y.intersp=0.9,
  ncol=2 # one-column legend (=default)
)

# 1st segment
x00 <- c1x + c1r
y00 <- c1y
x01 <- c2x
y01 <- c2y + c2r
# 2nd segment
x10 <- c1x + c1r*cos(2*pi*firstData[6]/sum(firstData))
y10 <- c1y - c1r*sin(2*pi*firstData[6]/sum(firstData))
x11 <- c2x - c2r*cos(pi/4)
y11 <- c2y - c2r*sin(pi/4)

segments(c(x00,x10), c(y00,y10), c(x01,x11), c(y01,y11), 
         col = "black", lty = "dotted")

dev.off()



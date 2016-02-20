if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"fmsb" %in% installed.packages()) install.packages("fmsb")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"stringr" %in% installed.packages()) install.packages("stringr")

library(dplyr)
library(fmsb)
library(plyr)
library(stringr)
library(RColorBrewer)

thePath <- "/home/st/demografija-lv/visc/sampledata/"
setwd(thePath)

preiluSkolas <- c("Preiļu 1. pamatskola",
                  "Preiļu 2. vidusskola",
                  "Preiļu novada Vakara (maiņu) un neklātienes vidusskola")
preiluNovSkolas <- c("Pelēču pamatskola",
                     "Priekuļu pamatskola",
                     "Salas pamatskola")

varkavasSkolas <- c("Vārkavas pamatskola",
                    "Vārkavas vidusskola")

riebinuSkolas <- c("Dravnieku pamatskola",                   
                   "Riebiņu vidusskola",
                   "Rušonas pamatskola")



skolas <- c(preiluSkolas, preiluNovSkolas, varkavasSkolas, riebinuSkolas)
skolasLab <- skolas
skolasLab[3] <- "Preiļu nekl. vsk."

novads <- c(rep("Preiļu",6), rep("Vārkavas",2), rep("Riebiņu",3))

files <- c("2014-necentralizetie-MAT9.csv")







getExams <- function() {
  
  inputFiles <- list.files(
    path=thePath,
    pattern="201[0-9]-necentralizetie-(ANG9|MAT9|VES9|VLL9).csv"
  )
  filenames <- paste0(thePath,inputFiles)
  allFiles = plyr::ldply(filenames, function(filename) {    
    dum = read.table(
      file=filename, 
      header=TRUE,
      sep=",",
      row.names=NULL,  
      fileEncoding="UTF-8"
    )
    yyyy <- str_extract(filename, "([0-9]{4})")
    subject <- str_extract(filename, "(ANG9|MAT9|VES9|VLL9)")
    dum$year <- yyyy
    dum$subject <- subject
    dum$Kopvertejums <- as.numeric(sub("%","",dum$Kopvertejums))
    return(dum)
  })
  return(allFiles)
}

df <- getExams()
dfMat <- df[(df$Skola %in% skolas | 
               str_detect(df$Skola, "Rušonas pamatskola")) ,]


presence <- matrix(c(c(1,1,1,1,1,1,1,1,1,1,1), 
                     c(1,1,1,1,1,1,1,1,1,1,1),
                     c(1,1,1,1,1,0,1,1,1,1,1),
                     c(1,1,1,1,1,1,1,1,1,1,1),
                     c(1,0,1,1,1,1,1,1,0,1,1)), byrow = FALSE, ncol = 5)

averaged <- dfMat %>% 
  group_by(Skola, subject) %>%
  dplyr::summarise(Kopvertejums = mean(Kopvertejums), numYears = n())

newDf <- as.data.frame(averaged)




getColumn <- function(col, theSchools) {  
  res <- numeric(0)
  for (skola in theSchools) {
    r <- newDf$Kopvertejums[newDf$subject==col &
                              (newDf$Skola == skola | 
                                 str_detect(newDf$Skola, skola))]
    if (length(r) == 0) {
      res <- c(res,NA)
    } else {
      res <- c(res,r[1])    
    }
  } 
  return(res)
}



theSchools <- skolas
NN <- length(theSchools)
dd<-data.frame(
  clust = 1:NN, 
  ANG9 = getColumn("ANG9", theSchools),
  MAT9 = getColumn("MAT9", theSchools),
  VES9 = getColumn("VES9", theSchools),
  VLL9 = getColumn("VLL9", theSchools)
)





# preiluColors <- brewer.pal(8,"Set1")[1:3]
# preiluNovColors <- brewer.pal(8,"Dark2")[1:3]
# varkavasColors <- brewer.pal(8,"Oranges")[7:8]
# riebinuColors <- brewer.pal(8,"Set2")[1:3]
cc1 <- brewer.pal(8,"Set1")[1:6]
cc2 <- brewer.pal(8,"Dark2")[1:5]
# allColors <- c(preiluColors, preiluNovColors, 
#                varkavasColors, riebinuColors)

allColors <- c(cc1,cc2)


titles <- c("Prei\u013Cu skolas\n(pils\u0113ta)", 
            "Prei\u013Cu skolas\n(lauki)", 
            "V\u0101rkavas skolas",
            "Riebi\u0146u skolas\n(novada R da\u013Ca)")

starts <- c(1,4,7,9)
ends <- c(3,6,8,11)



png(filename="barcharts.png", width=1000,height=1000)
old.par <- par(mar=c(5, 4, 4, 2) + 0.1)
barplot(dd$MAT9, 
        main="MAT9 rezult\u0101ti", 
        ylab="Procenti",
        space=1,
        xaxt="n", 
        col=allColors,
        ylim=c(30,75),
        xpd = FALSE)
labs <- skolasLab
# text(cex=1, x=x-.25, y=-1.25, labs, xpd=TRUE, srt=45, pos=1)
# grid()

end_point = 0.5 + 2*nrow(dd) -1

text(seq(1.5,end_point,by=2), par("usr")[3]-0.35, 
     srt = 30, adj= 1, xpd = TRUE,
     labels = labs, cex=0.65)
abline(h=c(30,40,50,60,70),col = "lightgray", lty = "dotted")
avg <- mean(df$Kopvertejums[df$subject=="MAT9"])
abline(h=avg, col="darkred",lwd=2,lty="dashed")

par(old.par)
dev.off()



dds <- dd
dds$lab <- skolasLab
dds <- dds[order(-dds[,3]),]
barplot(dds$MAT9, 
        main="MAT9 rezult\u0101ti", 
        ylab="Procenti",
        space=1,
        xaxt="n", 
        col="darkgreen",
        ylim=c(30,75),
        xpd = FALSE)
labs <- dds$lab
# text(cex=1, x=x-.25, y=-1.25, labs, xpd=TRUE, srt=45, pos=1)
# grid()

end_point = 0.5 + 2*nrow(dds) -1

text(seq(1.5,end_point,by=2), par("usr")[3]-0.35, 
     srt = 30, adj= 1, xpd = TRUE,
     labels = labs, cex=0.65)
abline(h=c(30,40,50,60,70),col = "lightgray", lty = "dotted")
avg <- mean(df$Kopvertejums[df$subject=="MAT9"])
abline(h=avg, col="darkred",lwd=2,lty="dashed")








png(filename="radarcharts.png", width=1000,height=1000)
old.par <- par(mar=c(1, 2, 2, 1)) #decrease default margin

layout(matrix(1:4, ncol=2))
lapply(1:4, function(i) { 
radarchart(
  rbind(rep(75,4), rep(35,4), dd[starts[i]:ends[i],-1]), 
  plty=1, 
  plwd=2,
  pcol=allColors[starts[i]:ends[i]], 
  caxislabels=1:5,
  title=titles[i])
})
par(old.par)
dev.off()


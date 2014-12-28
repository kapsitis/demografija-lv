setwd("/home/kalvis/demografija-lv/shiny/visc-exams/")

if (!"plyr" %in% installed.packages()) install.packages("plyr")
library(plyr)

inputFiles <- c("visi_dati_2012.csv",
                "visi_dati_2013.csv",
                "visi_dati_2014.csv")

filenames <- paste0(getwd(),"/",inputFiles)

visc_exams = ldply(filenames, function(filename) {
  dum = read.table(
    file=filename, 
    header=FALSE,
    sep=",",
    row.names=NULL,  
    col.names=c("prieksmets", "nosaukums", "skolnkods", 
                "gK","gL","gR","gV","gX","kop",
                "pK","pL","pR","pV","pX","koppro",
                "limenis", "ValstsValodasPakape",
                "dzimums","valdes_kod","klase",
                "urban","tips", "valoda",
                "LauksVisiVieni","gads"),
    skip=1
  )   
  return(dum)
})

sbr <- read.table(
    file="social-indicators-by-region.csv", 
    header=FALSE,
    sep=",",
    row.names=NULL,  
    col.names=c("Num","municipality","region",
                "jobless","income",
                "schools","visc_code"),
    skip=1)


visi_dati <- merge(visc_exams, sbr, 
               by.x = "valdes_kod",
               by.y = "visc_code")


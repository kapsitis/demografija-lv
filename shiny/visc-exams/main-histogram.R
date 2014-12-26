setwd("/home/st/demografija-lv/shiny/visc-exams/")
source("read-data.R")

if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
library(ggplot2)

##############################
### Parameters to the script
##############################

filters <- list(
  gads=2014,
  tips=c(6),
  prieksmets="MAT")

currentFacet <- "dzimums"
# either median/IQR ("m-iqr"), 
# or mean/st.dev. ("mu-sigma")
annoStyle <- "m-iqr"
histStep=2.5

#######################################
### Metadata and helper functions
#######################################

if (currentFacet=="dzimums") {
  facetFilters <- list(
    list(facet="dzimums",val="2"),
    list(facet="dzimums",val="1")
    )
  names(facetFilters) <- 1:2
} else if (currentFacet=="valoda") {
  facetFilters <- list(
    list(facet="valoda",val="1"),
    list(facet="valoda",val="2"),
    list(facet="valoda",val="3"),
    list(facet="valoda",val="4")
    )
  names(facetFilters) <- 1:4
} else if (currentFacet == "region") {
  facetFilters <- list(
    list(facet="region",val="Kurzeme region"),
    list(facet="region",val="Latgale region"),
    list(facet="region",val="Riga city"),
    list(facet="region",val="Riga region"),
    list(facet="region",val="Vidzeme region"),
    list(facet="region",val="Zemgale region")
    )
  names(facetFilters) <- 1:6
} else if (currentFacet == "urban") {
  facetFilters <- list(
    list(facet="urban",val="1"),
    list(facet="urban",val="2"),
    list(facet="urban",val="3"),
    list(facet="urban",val="4")
    )
  names(facetFilters) <- 1:4
} else if (currentFacet == "tips") {
  facetFilters <- list(
    list(facet="tips",val="3"),
    list(facet="tips",val="4"),
    list(facet="tips",val="5"),
    list(facet="tips",val="6"),
    list(facet="tips",val="7"),
    list(facet="tips",val="8"),
    list(facet="tips",val="9") 
    )
  names(facetFilters) <- 1:7
} else if (currentFacet == "joblessCat") {
  facetFilters <- list(
    list(facet="joblessCat",val="1"),
    list(facet="joblessCat",val="2"),
    list(facet="joblessCat",val="3"),
    list(facet="joblessCat",val="4"),
    list(facet="joblessCat",val="5"),
    list(facet="joblessCat",val="6")
  )
  names(facetFilters) <- 1:6
}


subtitles <- list(
  dzimums = "P\u0113c dzimuma",
  valoda = "P\u0113c valodas",
  region = "P\u0113c re\u01E7iona",
  urban = "P\u0113c urbaniz\u0101cijas",
  tips = "P\u0113c skolas tipa",
  joblessCat = "P\u0113c bezdarba l\u012Bme\u0146a"
  )

prieksmeti <- list(
  MAT="Matem\u0101tika",
  ANG="Ang\u013Cu valoda",
  VLL="Latvie\u0161u valoda",
  BIO="Biolo\u01E7ija",
  FIZ="Fizika",
  FRA="Fran\u010Du valoda",
  KIM="\u0136\u012Bmija",
  KRV="Krievu valoda",
  VAC="V\u0101cu valoda",
  VES="V\u0113sture",
  LV9="Latv.val. 9-kl.")


joblessBreaks <- c(2.5,5,7.5,10,12.5,15,30)

regionNames <- list(
  K = "Kurzeme",
  L = "Latgale",
  C = "R\u012Bga",
  R = "Pier\u012Bga",
  V = "Vidzeme",
  Z = "Zemgale"
  )

regionValues <- list(
  K = "Kurzeme region",
  L = "Latgale region",
  C = "Riga city",
  R = "Riga region",
  V = "Vidzeme region",
  Z = "Zemgale region"
  )

urbanNames <- list(
  "R\u012Bga",
  "lielas pils.",
  "mazas pils.",
  "lauki"
  )

valodaNames <- list(
  "latvie\u0161u",
  "krievu",
  "jauktas",
  "citas"
  )

friendlyTitle <- function() {
  result <- ""
  if ("prieksmets" %in% names(filters)) {
    result <- 
      paste0(result, 
             prieksmeti[[filters[["prieksmets"]]]])
  }
  if ("gads" %in% names(filters)) {
      result <- 
        paste0(result,"; ",
               filters[["gads"]])
  }
  if ("tips" %in% names(filters)) {
    result <- 
      paste0(result,
             "; skolas: ",
             paste(filters[["tips"]],collapse=","))
  }
  if ("valoda" %in% names(filters)) {
    result <- 
      paste0(result,
             "(", valodaNames[[filters[["valoda"]]]],")")
  }
  if ("jobless" %in% names(filters)) {
    JLidx <- min(which(filters[["jobless"]] == letters))
    JLLeft <- joblessBreaks[JLidx]
    JLRight <- joblessBreaks[JLidx+1]    
    result <- 
      paste0(result,
             "; Bdarbs (",JLLeft,",",JLRight,"]")
  }
  if ("region" %in% names(filters)) {
    result <- 
      paste0(result,
             "; ",
             regionNames[[filters[["region"]]]])
  }
  if ("urban" %in% names(filters)) {
    result <- 
      paste0(result,
             "; urb.:",
             urbanNames[[filters[["urban"]]]])
  }
  return(result)  
}


applyFilters <- function(prieksmets,
                         gads,
                         tips,
                         jobless,
                         region,
                         urban,
                         valoda) {
  result <- TRUE
  if ("prieksmets" %in% names(filters)) {
    result <- result & 
      (prieksmets == filters[["prieksmets"]])
  }
  if ("gads" %in% names(filters)) {
    result <- result & 
      (gads == filters[["gads"]])
  }
  if ("tips" %in% names(filters)) {
    result <- result & 
      (tips %in% filters[["tips"]])
    
  } 
  if ("valoda" %in% names(filters)) {
    result <- result & 
      (valoda == filters[["valoda"]])
  }
  if ("jobless" %in% names(filters)) {
    JLidx <- min(which(filters[["jobless"]] == letters))
    JLLeft <- joblessBreaks[JLidx]
    JLRight <- joblessBreaks[JLidx+1]
    result <- result & 
      (jobless > JLLeft) &
      (jobless <= JLRight)
  }
  if ("region" %in% names(filters)) {
    result <- result & 
      (region == regionValues[[filters[["region"]]]])
  }
  if ("urban" %in% names(urban)) {
    result <- result &
      (urban == filters[["urban"]])
  }
  return(result)
}

facetDzimums <- 
  list(column = "dzimums",
       discreteValues = c(2,1),
       guiNames = c("meitenes","z\u0113ni"),
       colors = c("#E41A1C", "#377EB8"))
facetValoda <- 
  list(column = "valoda",
       discreteValues = c(1,2,3,4),
       guiNames = c("latvie\u0161u","krievu","jauktas","citas"),
       colors = c("chartreuse4", "red","darkorange2","cornflowerblue"))
facetRegion <- 
  list(column = "region",
       discreteValues = c("Kurzeme region",
                          "Latgale region",
                          "Riga city",
                          "Riga region",
                          "Vidzeme region",
                          "Zemgale region"),
       guiNames = c("Kurzeme","Latgale","R\u012Bga",
                    "Pier\u012Bga","Vidzeme","Zemgale"),
       colors = rainbow(6))
facetUrban <- 
  list(column = "urban",
       discreteValues = c(1,2,3,4),
       guiNames = c("R\u012Bga","Lielpils\u0113tas","Pils\u0113tas","Lauki"),
       colors = c("#E41A1C","#377EB8","#4DAF4A","#984EA3"))
facetTips <- 
  list(column = "tips",
       discreteValues = c(3,4,5,6,7,8,9),
       guiNames = c("Vidussk.","Vakarsk.",
                    "\u0122imn.","V-\u0122imn.",
                    "Prof.&M.","Specsk.","Augstsk."),
       colors = c("#66C2A5","#FC8D62",
                  "#8DA0CB","#E78AC3",
                  "#A6D854","#FFD92F","#E5C494"))
facetJoblessCat <- 
  list(column = "joblessCat",
       discreteValues = c(1,2,3,4,5,6),
       guiNames = c("BD (02.5;5]","BD (05;07.5]",
                    "BD (07.5;10]","BD (10;12.5]",
                    "BD (12.5;15]","BD (15;26]"),
       colors = c("#FFFFCC","#C7E9B4",
                  "#7FCDBB","#41B6C4",
                  "#2C7FB8","#253494"))


facets <- list(
  dzimums = facetDzimums,
  valoda = facetValoda,
  region = facetRegion,
  urban = facetUrban,
  tips = facetTips,
  joblessCat = facetJoblessCat)


myJoblessCat <- 
  sapply(as.vector(visi_dati$jobless), function(x) {
    xx <- as.numeric(sub("%","",x))
    sum(xx > joblessBreaks)
    }
    )
visi_dati$joblessCat <- myJoblessCat

theData <- 
  subset(visi_dati, applyFilters(prieksmets,
                                 gads,
                                 tips,
                                 jobless,
                                 region,
                                 urban,
                                 valoda))

annoData <- vector(mode="list", length=length(facetFilters))
for (ffilter in names(facetFilters)) {
  ffacet <- facetFilters[[ffilter]]$facet
  fval <- facetFilters[[ffilter]]$val
  colNum <- min(which(ffacet==colnames(theData)))
  subData <- theData[theData[,colNum]==fval,]
  fM <- median(subData$koppro)
  fIQR <- fivenum(subData$koppro)[4] - fivenum(subData$koppro)[2]
  fN <- nrow(subData)
  fMU <- mean(subData$koppro)
  fSD <- sqrt(sum((subData$koppro - fMU)^2)/fN)
  annoData[[as.numeric(ffilter)]] <- 
    list(fM = fM, fIQR = fIQR, 
         fN = fN, fMU = fMU, fSD = fSD)
}

# How many annotation lines per 1 plot
numAnno <- 2

# Compute string that is displayed
# for the $idx-th plot, located on line num=$row
# Typically each plot has 2 lines of text,
# i.e. row is 1 or 2. But idx corresponds to 
# the number of plots.
getAnnotation <- function (row, idx) {
  r <- "Undef"
  if (row == 1) {
    if (annoStyle=="m-iqr") {
      r <- sprintf("m=%2.2f; IQR=%2.2f",
                   annoData[[idx]]$fM,
                   annoData[[idx]]$fIQR)
    }
    if (annoStyle=="mu-sigma") {
      r <- sprintf("\u03BC=%2.2f; \u03C3=%2.2f",
                   annoData[[idx]]$fMU,
                   annoData[[idx]]$fSD)
    }
  } else if (row == 2) {
    r <- sprintf("N=%i",annoData[[idx]]$fN)
  }
  return(r)
}

# Get all text annotation strings as 
# one big vector
getAllAnnotations <- function() {
  numPlots <- length(facetFilters)
  rows <- rep(1:numAnno, each=numPlots)
  idxs <- rep(1:numPlots, times=numAnno)
  return(sapply(1:(numAnno*numPlots), 
                function(i) {
                  getAnnotation(rows[i],idxs[i])
                }))
}

cFacetNum <- min(which(currentFacet==colnames(theData)))

extraCol <- sapply(
  theData[,cFacetNum],
  function(x) {
    theFacet <- facets[[currentFacet]]
    theResult <- "NA"
    if ("discreteValues" %in% names(theFacet)) {
      theDV <- theFacet$discreteValues
      theNames <- theFacet$guiNames
      theIdx <- min(which(x == theDV))
      theResult <- theNames[theIdx]
    }
    return(theResult)
  }
  )
theData$extra_col <- extraCol


p <- ggplot(theData, aes(x=koppro, fill=extra_col)) +
  geom_histogram(colour="#E5E5E5", breaks=seq(0,100,by=2.5))
g <- ggplot_build(p)
yRange <- g$panel$ranges[[1]]$y.range
yMax <- max(g$data[[1]]$ymax - g$data[[1]]$ymin)
yMaxMin <- yRange[2] - yRange[1]

## Adding text annotations
theAnnotations <- 
  data.frame(x=rep(c(60,70), each=length(facetFilters)),
             y=rep(c(yMax,yMax-yMaxMin/15), each=length(facetFilters)),   
             lab=getAllAnnotations(),
             extra_col=rep(
               facets[[currentFacet]]$guiNames,
               times=numAnno)) 

ourTitle <- friendlyTitle()
ourSubtitle <- subtitles[[currentFacet]]
ggplot(theData, aes(x=koppro, fill=extra_col)) +
  geom_histogram(colour="#E5E5E5", breaks=seq(0,100,by=histStep)) +
  facet_grid(extra_col ~ .) +
  scale_x_continuous(name="Kopv\u0113rt\u0113juma %",
                     breaks=c(0, 20, 40, 60, 80,100)) +
  scale_y_continuous(name="Skaits") +
  geom_text(aes(x, y, label=lab),
            data=theAnnotations, vjust=1, hjust=0) +
  scale_fill_manual(values=facets[[currentFacet]]$colors) +
  ggtitle(bquote(atop(.(ourTitle), atop(italic(.(ourSubtitle)), "")))) +
  theme(
    legend.position="none",
    plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = -1)
    )




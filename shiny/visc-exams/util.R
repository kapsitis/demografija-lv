getFacetFilters <- function(pCurrentFacet) {
  
  if (pCurrentFacet=="dzimums") {
    facetFilters <- list(
      list(facet="dzimums",val="2"),
      list(facet="dzimums",val="1")
    )
    names(facetFilters) <- 1:2
  } else if (pCurrentFacet=="valoda") {
    facetFilters <- list(
      list(facet="valoda",val="1"),
      list(facet="valoda",val="2"),
      list(facet="valoda",val="3"),
      list(facet="valoda",val="4")
    )
    names(facetFilters) <- 1:4
  } else if (pCurrentFacet == "region") {
    facetFilters <- list(
      list(facet="region",val="Kurzeme region"),
      list(facet="region",val="Latgale region"),
      list(facet="region",val="Riga city"),
      list(facet="region",val="Riga region"),
      list(facet="region",val="Vidzeme region"),
      list(facet="region",val="Zemgale region")
    )
    names(facetFilters) <- 1:6
  } else if (pCurrentFacet == "urban") {
    facetFilters <- list(
      list(facet="urban",val="1"),
      list(facet="urban",val="2"),
      list(facet="urban",val="3"),
      list(facet="urban",val="4")
    )
    names(facetFilters) <- 1:4
  } else if (pCurrentFacet == "tips") {
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
  } else if (pCurrentFacet == "joblessCat") {
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
  return(facetFilters)
}


subtitles <- list(
  dzimums = "P\u0113c dzimuma",
  valoda = "P\u0113c valodas",
  region = "P\u0113c re\u01E7iona",
  urban = "P\u0113c urbaniz\u0101cijas",
  tips = "P\u0113c skolas tipa",
  joblessCat = "P\u0113c bezdarba"
)


joblessBreaks <- c(2.5,5,7.5,10,12.5,15,30)

# Find the bin number for jobless rate "x" written with "%" sign
findJoblessCat <- function(x) {
  xx <- as.numeric(sub("%","",x))
  sum(xx > joblessBreaks)
}


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

urbanNames <- c(
  "R\u012Bga",
  "Lielpils\u0113tas",
  "Mazpils\u0113tas",
  "Lauki"
)

getUrbanNames <- function(arg) {
  result <- "("
  for (i in 1:length(arg)) {
    prefix <- ","
    if (i == 1) { prefix <- ""}
    result <- paste0(result,prefix,urbanNames[arg[i]])
  }
  result <- paste0(result,")")
  return(result)
}

valodaNames <- list(
  "1" = "latvie\u0161u",
  "2" = "krievu",
  "3" = "jauktas",
  "4" = "citas"
)

regionNames <- list(
  K = "Kurzeme",
  L = "Latgale",
  C = "R\u012Bga",
  R = "Pier\u012Bga",
  V = "Vidzeme",
  Z = "Zemgale"
)


friendlyTitle <- function(pFilters) {
  result <- ""  
  if ("prieksmets" %in% names(pFilters)) {
    result <- paste0(result, prieksmeti[[pFilters[["prieksmets"]]]])
  }
  if ("gads" %in% names(pFilters)) {
    result <- paste0(result, "; ", pFilters[["gads"]])
  }
  if ("tips" %in% names(pFilters)) {
    result <- 
      paste0(result,
             "; skolas: ",
             paste(pFilters[["tips"]],collapse=","))
  }
  if ("valoda" %in% names(pFilters)) {
    result <- 
      paste0(result,
             "(", valodaNames[[pFilters[["valoda"]]]],")")
  }
  if ("joblessCat" %in% names(pFilters)) {
    JLLeft <- joblessBreaks[as.numeric(pFilters[["joblessCat"]])]
    JLRight <- joblessBreaks[as.numeric(pFilters[["joblessCat"]])+1]
    result <- 
      paste0(result,
             "; Bdarbs (",JLLeft,";",JLRight,"]")
  }
  if ("region" %in% names(pFilters)) {
    result <- 
      paste0(result,
             "; ",
             regionNames[[pFilters[["region"]]]])
  }
  if ("urban" %in% names(pFilters)) {
    result <- 
      paste0(result,
             "; urb.=", getUrbanNames(pFilters[["urban"]]))
  }
  return(result)  
}


joblessBreaks <- c(2.5,5,7.5,10,12.5,15,30)
regionValues <- list(
  K = "Kurzeme region",
  L = "Latgale region",
  C = "Riga city",
  R = "Riga region",
  V = "Vidzeme region",
  Z = "Zemgale region"
)


applyFilters <- function(prieksmets,
                         gads,
                         tips,
                         jobless,
                         joblessCat,
                         region,
                         urban,
                         valoda, 
                         pFilters) {
  result <- TRUE
  if ("prieksmets" %in% names(pFilters)) {
    result <- result & 
      (prieksmets == pFilters[["prieksmets"]])
  }
  if ("gads" %in% names(pFilters)) {
    result <- result & 
      (gads == pFilters[["gads"]])
  }
  if ("tips" %in% names(pFilters)) {
    result <- result & 
      (tips %in% pFilters[["tips"]])
    
  } 
  if ("valoda" %in% names(pFilters)) {
    result <- result & 
      (valoda == pFilters[["valoda"]])
  }
  if ("joblessCat" %in% names(pFilters)) {
    result <- result & 
      (joblessCat == pFilters[["joblessCat"]])
  }
  if ("region" %in% names(pFilters)) {
    result <- result & 
      (region == regionValues[[pFilters[["region"]]]])
  }
  if ("urban" %in% names(pFilters)) {
    result <- result &
      (urban %in% pFilters[["urban"]])
  }
  return(result)
}



# Compute string that is displayed
# for the $idx-th plot, located on line num=$row
# Typically each plot has 2 lines of text,
# i.e. row is 1 or 2. But idx corresponds to 
# the number of plots.
getAnnotation <- function (row, idx, pAnnoStyle, pAnnoData) {
  r <- "Undef"
  if (row == 1) {
    if (pAnnoStyle=="m-iqr") {
      r <- sprintf("m=%2.2f; IQR=%2.2f",
                   pAnnoData[[idx]]$fM,
                   pAnnoData[[idx]]$fIQR)
    }
    if (pAnnoStyle=="mu-sigma") {
      r <- sprintf("\u03BC=%2.2f; \u03C3=%2.2f",
                   pAnnoData[[idx]]$fMU,
                   pAnnoData[[idx]]$fSD)
    }
  } else if (row == 2) {
    r <- sprintf("N=%i",pAnnoData[[idx]]$fN)
  }
  return(r)
}

# Get all text annotation strings as 
# one big vector
getAllAnnotations <- function(facetFilters,numAnno,pAnnoStyle,pAnnoData) {
  numPlots <- length(facetFilters)
  rows <- rep(1:numAnno, each=numPlots)
  idxs <- rep(1:numPlots, times=numAnno)
  return(
    sapply(1:(numAnno*numPlots), 
           function(i) {
             getAnnotation(rows[i],idxs[i],pAnnoStyle,pAnnoData)
           }))
}




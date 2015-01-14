if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"shiny" %in% installed.packages()) install.packages("shiny")

library(ggplot2)
library(plyr)
library(shiny) 

source("util.R")

visi_dati <- getAllData()


###########################################################################



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


myJoblessCat <- sapply(as.vector(visi_dati$jobless), findJoblessCat)
visi_dati$joblessCat <- myJoblessCat


shinyServer(function(input, output) { # server is defined within these parentheses
  filters <- reactive({
    rFilters <- list(
      gads=input$gads,
      tips=as.numeric(input$schoolType),
      prieksmets=input$prieksmets
    )
    if (input$region != "A") {
      rFilters[["region"]] <- input$region
    }
    if (input$valoda != "0") {
      rFilters[["valoda"]] <- input$valoda
    }
    if (input$joblessCat != "0") {
      rFilters[["joblessCat"]] <- as.character(input$joblessCat)
    }
    if (length(input$urban) != 4) {
      rFilters[["urban"]] <- as.numeric(input$urban)
    }
    rFilters
  })
  
  
  currentFacet <- reactive({
    input$groupBy
  })
  
  
  facetFilters <- reactive({
    getFacetFilters(currentFacet())
  }) 
  
  
  theData <- reactive({
    rTheData <- 
      subset(visi_dati, applyFilters(prieksmets,
                                     gads,
                                     tips,
                                     jobless,
                                     joblessCat,
                                     region,
                                     urban,
                                     valoda, 
                                     filters()))
    
    cFacetNum <- min(which((currentFacet()==colnames(rTheData))))
    
    extraCol <- sapply(
      rTheData[,cFacetNum],
      function(x) {
        theFacet <- facets[[currentFacet()]]
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
    rTheData$extra_col <- extraCol    
    rTheData
  }) 
  
  annoData <- reactive({
    rAnnoData <- vector(mode="list", length=length((facetFilters())))
    for (ffilter in names(facetFilters())) {
      ffacet <- facetFilters()[[ffilter]]$facet
      fval <- facetFilters()[[ffilter]]$val
      colNum <- min(which(ffacet==colnames(theData())))
      subData <- theData()[theData()[,colNum]==fval,]
      fM <- median(subData$koppro)
      fIQR <- fivenum(subData$koppro)[4] - fivenum(subData$koppro)[2]
      fN <- nrow(subData)
      fMU <- mean(subData$koppro)
      fSD <- sqrt(sum((subData$koppro - fMU)^2)/fN)
      rAnnoData[[as.numeric(ffilter)]] <- 
        list(fM = fM, fIQR = fIQR, 
             fN = fN, fMU = fMU, fSD = fSD)
    }
    rAnnoData
  })
  
  theAnnotations <- reactive({
    
    # How many annotation lines per 1 plot
    numAnno <- 2
    
    p <- ggplot(theData(), aes(x=koppro, fill=extra_col)) +
      geom_histogram(colour="#E5E5E5", breaks=seq(0,100,by=100/(input$histBars)))
    g <- ggplot_build(p)
    yRange <- g$panel$ranges[[1]]$y.range
    yMax <- max(g$data[[1]]$ymax - g$data[[1]]$ymin)
    yMaxMin <- yRange[2] - yRange[1]
    
    
    data.frame(x=rep(c(60,70), each=length((facetFilters()))),
               y=rep(c(yMax,yMax-yMaxMin/15), each=length(facetFilters())),   
               lab=getAllAnnotations(facetFilters(),numAnno,input$annoStyle,annoData()),
               extra_col=rep(
                 facets[[currentFacet()]]$guiNames,
                 times=numAnno)) 
  })

  
#   theSingleAnnotations <- reactive({    
#     numAnno <- 2
#     
#     p <- ggplot(theData(), aes(x=koppro, fill=extra_col)) +
#       geom_histogram(colour="#E5E5E5", breaks=seq(0,100,by=2.5))
#     g <- ggplot_build(p)
#     yRange <- g$panel$ranges[[1]]$y.range
#     yMax <- max(g$data[[1]]$ymax - g$data[[1]]$ymin)
#     yMaxMin <- yRange[2] - yRange[1]
#     
#     
#     data.frame(x=c(60,70),
#                y=c(yMax,yMax-yMaxMin/15),   
#                lab=getAllAnnotations(facetFilters(),numAnno,input$annoStyle,annoData()),
#                extra_col=rep(
#                  facets[[currentFacet()]]$guiNames,
#                  times=numAnno)) 
#   })
  
  
  
  output$comparisonGraph <- renderPlot({
    ourTitle <- friendlyTitle(filters())
    ourSubtitle <- subtitles[[currentFacet()]]
    theGraph <- ggplot(theData(), aes(x=koppro, fill=extra_col)) +
      geom_histogram(colour="#E5E5E5", breaks=seq(0,100,by=100/(input$histBars))) +
      facet_grid(extra_col ~ .) +
      scale_x_continuous(name="Kopv\u0113rt\u0113juma %",
                         breaks=c(0, 20, 40, 60, 80,100)) +
      scale_y_continuous(name="Skaits") +
      geom_text(aes(x, y, label=lab),
                data=theAnnotations(), vjust=1, hjust=0) +
      scale_fill_manual(values=facets[[currentFacet()]]$colors) +
      ggtitle(bquote(atop(.(ourTitle), atop(italic(.(ourSubtitle)), "")))) +
      theme(
        legend.position="none",
        plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = -1)
      ) 
    print(theGraph)    
  }, width=600, height=800)
  
  output$totalGraph <- renderPlot({
    ourTitle <- friendlyTitle(filters())
    ourSubtitle <- subtitles[[currentFacet()]]
    theGraph <- ggplot(theData(), aes(x=koppro, fill=extra_col)) +
      geom_histogram(colour="#E5E5E5", breaks=seq(0,100,by=100/(input$histBars))) +
      scale_x_continuous(name="Kopv\u0113rt\u0113juma %",
                         breaks=c(0, 20, 40, 60, 80,100)) +
      scale_y_continuous(name="Skaits") +
#       geom_text(aes(x, y, label=lab),
#                data=theSingleAnnotations(), vjust=1, hjust=0) +
      scale_fill_manual(values=facets[[currentFacet()]]$colors) +
      ggtitle(bquote(atop(.(ourTitle), atop(italic(.(ourSubtitle)), "")))) +
      theme(
         plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = -1)
      ) 
    print(theGraph)        
  }, width=700, height=800)
  
    
  output$textDisplay <- renderUI({
    str1 <- sprintf("Histogrammas solis ir %3.2f  procenti vien\u0101 stabi\u0146\u0101.",100/input$histBars)
    if (input$annoStyle == "m-iqr") {
      str2 <- "<b>Anot\u0101cija:</b> m - medi\u0101na, IQR - starpkvarti\u013Cu interv\u0101ls, N - skol\u0113nu skaits"
    } else {  
      str2 <- "<b>Anot\u0101cija:</b> \u03BC - vid\u0113j\u0101 v\u0113rt\u012Bba, \u03C3 - standartnovirze, N - skol\u0113nu skaits"
    }
    str3 <- sprintf("<b>Virsraksts:</b> %s",friendlyTitle(filters()))
    HTML(paste(c(str1, str2, str3), collapse = '<br/>'))
  })
  
  
})
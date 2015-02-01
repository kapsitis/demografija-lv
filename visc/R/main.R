if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"grid" %in% installed.packages()) install.packages("grid")
if (!"gridSVG" %in% installed.packages()) install.packages("gridSVG")
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"proto" %in% installed.packages()) install.packages("proto")
if (!"shiny" %in% installed.packages()) install.packages("shiny")
if (!"stats" %in% installed.packages()) install.packages("stats")


library(ggplot2)
library(grid)
library(gridSVG)
library(Hmisc)
library(plyr)
library(proto)
library(shiny) 
library(stats)


dataPath <- "/home/kalvis/demografija-lv/visc/sampledata"

setwd(dataPath)
source("../R/getAllData.R")




geom_tooltip <- function (mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", real.geom = NULL, ...) {
  rg <- real.geom(mapping = mapping, data = data, stat = stat, ## (ref:init)
                  position = position, ...)
  
  rg$geom <- proto(rg$geom, { ## (ref:proto)
    draw <- function(., data, ...) {
      grobs <- list()
      for (i in 1:nrow(data)) {
        grob <- .super$draw(., data[i,], ...) ## (ref:each)
        grobs[[i]] <- garnishGrob(grob,  ## (ref:garnish)
                                  `data-tooltip`=data[i,]$tooltip)
      }
      ggplot2:::ggname("geom_tooltip", gTree(
        children = do.call("gList", grobs)
      ))
    }
    required_aes <- c("tooltip", .super$required_aes)
  })
  
  rg ## (ref:return)
}



jscript <- '
function showTooltip(evt, label) {
// Getting rid of any existing tooltips
hideTooltip();

var svgNS = "http://www.w3.org/2000/svg",
target = evt.currentTarget,
wrappingGroup = document.getElementsByTagName("g")[0];

// Create a span node to hold the tooltip HTML
var content = document.createElementNS("http://www.w3.org/1999/xhtml", "span");
content.innerHTML = label;

var text = document.createElementNS(svgNS, "foreignObject");
text.setAttribute("id", "tooltipText");
// foreignObject nodes must have a width and height
// explicitly set; they do not auto-size.  Thus, well
// initially set the width and height to a large value, then
// measure how much space is actually used by the span node
// created above.  Then, we set the width and height to
// exactly those values.
text.setAttribute("width", "1000");
text.setAttribute("height", "1000");
text.appendChild(content);
wrappingGroup.appendChild(text);
var r = content.getBoundingClientRect();
wrappingGroup.removeChild(text);
var width = r.width, height = r.height;
if (/Chrome/.test(navigator.userAgent)) {
// Chrome gives us a zoomed rect; Firefox a natural one.
width = width / document.documentElement.currentScale;
height = height / document.documentElement.currentScale;
}
text.setAttribute("width", width);
text.setAttribute("height", height);

// By rights we should set this, but it makes Chrome barf.
// text.setAttribute("requiredExtensions",
//                   "http://www.w3.org/1999/xhtml");

var rect = document.createElementNS(svgNS, "rect");
rect.setAttribute("id", "tooltipRect");


// Add rect and span to the bottom of the document.  This is
// because SVG has a rendering order.  We want the tooltip to
// be on top, therefore inserting last.
wrappingGroup.appendChild(rect);
wrappingGroup.appendChild(text);

// Transforming the mouse location to the SVG coordinate system
// Snippet lifted from:
// http://tech.groups.yahoo.com/group/svg-developers/message/52701
var m = target.getScreenCTM();
var p = document.documentElement.createSVGPoint();
p.x = evt.clientX;
p.y = evt.clientY;
p = p.matrixTransform(m.inverse());

// Determine position for tooltip based on location of
// element that mouse is over
// AND size of text label
// Currently the tooltip is offset by (3, 3)
var tooltipx = p.x + 3;
var tooltiplabx = tooltipx + 5;
var tooltipy = p.y + 3;
var tooltiplaby = tooltipy + 5;

// Position tooltip rect and text
text.setAttribute("transform",
"translate(" + tooltiplabx + ", " +
(tooltiplaby + height - 3) + ") " +
"scale(1, -1)");

rect.setAttribute("x", tooltipx);
rect.setAttribute("y", tooltipy);
rect.setAttribute("width", width + 10);
rect.setAttribute("height", height + 5);
rect.setAttribute("stroke", "black");
rect.setAttribute("fill", "#ffffcc");
}

function hideTooltip() {
// Remove tooltip text and rect
var text = document.getElementById("tooltipText");
var rect = document.getElementById("tooltipRect");

if (text !== null && rect !== null) {
text.parentNode.removeChild(text);
rect.parentNode.removeChild(rect);
}
}
function mouseoverHandler(e) {
showTooltip(e, this.getAttribute("data-tooltip"));
}

function mouseoutHandler () {
hideTooltip();
}

var points = document.getElementsByClassName("points");
var i;
for (i = 0; i < points.length; i++) {
points[i].onmouseover = mouseoverHandler;
points[i].onmouseout = mouseoutHandler;
}
'






renamings <- getRenamings(dataPath)
nonCentralizedDF <- getNonCentralizedDF(dataPath)

socialIndicators <- getSocialIndicators(dataPath)

## Create the school demography table; 
## and attach municipality to every school there
schoolPop <- getSchoolPopulations(dataPath)
skolaPasvaldiba <- getSkolaPasvaldiba(dataPath, "skola_pasvaldiba.csv")
skolaPasvaldibaAlt <- getSkolaPasvaldiba(dataPath, "skola_pasvaldiba2.csv")
viisDati <- getViisDati(dataPath)

myMun <- sapply(as.vector(schoolPop$SchoolName), findMunBySch)
schoolPop$Municipality <- myMun
#schoolPop$Used <- rep(0,times=nrow(schoolPop))
renNumber <- as.vector(nonCentralizedDF$renamed)
nonCentralizedDF$studentNum <- sapply(renNumber,getStudentNum)
nonCentralizedDF$weightedResult <- nonCentralizedDF$result * nonCentralizedDF$studentNum


#for (theSubject in c("ANG9","MAT9","VES9","VLL9")) {
#for (theSubject in c("MAT9")) {  
theSubject <- "MAT9"
  
  theRho <- data.frame(factor=factorNames,
                       Y2010=rep("N",7),
                       Y2011=rep("N",7),
                       Y2012=rep("N",7),
                       Y2013=rep("N",7),
                       Y2014=rep("N",7),
                       stringsAsFactors=FALSE)
  
  
#  for (factor in factorNames) {  
#  for (factor in "UnemploymentRate") {
factor <- "FemaleRatio"
    
    initYear <- list(UnemploymentRate = 2010,
                     DependencyRatio = 2010, 
                     PopulationChangePerYear = 2011,
                     PerCapitaIncomeTax = 2010,
                     ExpenditurePerStudent = 2012,
                     PerCapitaRealEstateTax = 2010,
                     FemaleRatio = 2011)

    measurementUnit <- list(UnemploymentRate = "%",
                            DependencyRatio = "", 
                            PopulationChangePerYear = "",
                            PerCapitaIncomeTax = "EUR",
                            ExpenditurePerStudent = "EUR",
                            PerCapitaRealEstateTax = "EUR",
                            FemaleRatio = "%")

#    for (gg in initYear[[factor]]:2014) {            
#    for (gg in 2014) {

gg <- 2014

#      imgName <- sprintf("temp2/%s-%s-%03d.png",theSubject,factor,gg)
      imgName <- sprintf("temp2/%s-%s-%03d.svg",theSubject,factor,gg)
      print(sprintf("Processing image %s",imgName))
      fSchoolData <- subset(nonCentralizedDF, year == gg & subject == theSubject)      
      aggSchoolData <- aggregate(studentNum ~ municipality, fSchoolData, sum)
      aggSchoolResult <- aggregate(weightedResult ~ municipality, fSchoolData, sum)
      
      aggSchoolFull <- merge(aggSchoolData, aggSchoolResult, 
                             by.x = "municipality", by.y = "municipality")
      aggSchoolFull$avg <- aggSchoolFull$weightedResult/aggSchoolFull$studentNum
      siCurrent <- socialIndicators[socialIndicators$year==gg,]
      
      aggSchoolWithSocial <- merge(aggSchoolFull, siCurrent, 
                                   by.x = "municipality", by.y="Municipality")
      aggSchoolWithSocial$pointSize <- (aggSchoolWithSocial$studentNum)^0.8
      regionData <- getRegionData(dataPath)
      aggSchoolUltimate <- merge(aggSchoolWithSocial, regionData, 
                                 by.x="municipality", by.y="municipality")
      
      
      myColors <- c("#2FBFD5","#3F4FFF", 
                    "#68FF5F", "#FF2F2F", 
                    "#FF982F", "#D5FF2F" )
      
      aggSchoolUltimate <- aggSchoolUltimate[with(aggSchoolUltimate,order(-studentNum)),]
      
      
      
      ourTitle <- sprintf("Vid\u0113jais %s un %s pa\u0161vald\u012Bb\u0101s",theSubject,indicatorsShort[[factor]])
      ourSubtitle <- sprintf("%s.g., %s",gg,examTypes[[theSubject]])
      
      
      rho <- unname(cor.test(aggSchoolUltimate[,factor],aggSchoolUltimate$avg)$estimate)
      theRho[which(factorNames==factor),paste0("Y",gg)] <- sprintf("%.3f",rho)
      
      
       my_grob <- grobTree(textGrob(sprintf("%d, \u03C1=%.3f",gg,rho), 
                                    x=0.01,  y=0.95, hjust=0,
                                    gp=gpar(col="#cccccc", 
                                            fontsize=25, 
                                            fontfamily="Helvetica", 
                                            fontface="bold")))
       
       
#        ggplot(aggSchoolUltimate) +
#          aes_string(x=factor, y="avg", fill="region", size = "pointSize") + 
#          annotation_custom(my_grob) +
#          geom_point(shape=21) +
#          scale_size_area(max_size = 16, 
#                          breaks=c(8,40,189,910), 
#                          labels=c("15","100","700","5000"), 
#                          name="Eks\u0101menu\nskaits") +
#          
#          xScales[[factor]] +
#          scale_y_continuous(
#            name=sprintf("Vidējais %s rezultāts pašvaldībā, %%",theSubject), 
#            minor_breaks=seq(40,80,by=5), breaks=seq(40,80,by=10)) +
#          ggtitle(bquote(atop(.(ourTitle), atop(italic(.(ourSubtitle)), "")))) +
#          theme(
#            legend.title=element_text(size=12),  
#            panel.background = element_rect(fill = 'white', colour = 'darkgreen'),
#            plot.title = element_text(size = 20, face = "bold", colour = "black", vjust = -1),
#            panel.grid.minor = element_line(colour="lightgray", size=0.5, linetype="dotted"),
#            panel.grid.major = element_line(colour="black", size=0.5, linetype="dotted")
#          ) +
#          scale_fill_manual(values=myColors, name="Re\u0123ioni",
#                            labels=c("Kurzeme", 
#                                     "Latgale", 
#                                     "Pier\u012Bga",
#                                     "R\u012Bga",
#                                     "Vidzeme",
#                                     "Zemgale"),
#                            guide = guide_legend(override.aes = list(size = 7))) +  
#          geom_smooth(method = "loess", size = 0.6, fill=NA)  
# 
#       
#        ggsave(file=imgName,  width = 8, height = 6, dpi=300, units="in")     


# aggSchoolUltimate$shortAvg <- 
#   sapply(as.vector(aggSchoolUltimate$shortAvg),
#          function(idx) { })

aggSchoolUltimate$note <- 
#   sprintf("Bezdarbs:&#xA0;%.2f%%,<br />MAT9:&#xA0;%.2f%%&#xA0;&#x0D;&#x0A;",
#          aggSchoolUltimate[,factor],
#          aggSchoolUltimate$avg)
  paste0(indicatorsShort[[factor]],": ",
         aggSchoolUltimate[,factor],
         measurementUnit[[factor]],
         "<br />",
         theSubject,": ", 
         round(aggSchoolUltimate$avg,digits=2),
         "&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;&#xA0;")

      regionNames <- list(        
        "Kurzeme region" = "Kurzeme", 
        "Latgale region" = "Latgale", 
        "Riga region" = "Pier\u012Bga",
        "Riga city" = "R\u012Bga",
        "Vidzeme region" = "Vidzeme", 
        "Zemgale region" = "Zemgale")


aggSchoolUltimate$region <- 
  sapply(as.vector(aggSchoolUltimate$region), function(idx) { regionNames[[idx]]})
  

gridsvg(imgName,exportJS="inline",
        addClasses=TRUE,width=10,height=7.5,
        xmldecl='<?xml version="1.0" encoding="UTF-8"?>')
ggplot(aggSchoolUltimate) + 
  aes_string(x = factor, y = "avg", size="pointSize") +
  annotation_custom(my_grob) +
  geom_tooltip(aes(tooltip = paste0(
    "<b>", municipality,
    ":</b><br />",
    note),
    color=region, size = pointSize),
    real.geom = geom_point) +  
  geom_point(shape=21) +  
  ggtitle(ourTitle) + 
#  ggtitle(bquote(atop(.(ourTitle), atop(italic(.(ourSubtitle)), "")))) +
  theme(
    legend.title=element_text(size=12),  
    panel.background = element_rect(fill = 'white', colour = 'darkgreen'),
    panel.grid.minor = element_line(colour="lightgray", size=0.5, linetype="dotted"),
    panel.grid.major = element_line(colour="black", size=0.5, linetype="dotted"),
    plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "in"),
#    plot.title = element_text(size = 20, face = "bold", colour = "black"),
    plot.title = element_text(size = 16, vjust = 2, face = "bold"),
    plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "in")
  ) +
  scale_colour_manual(values=myColors, name="Re\u0123ioni",
                      labels=c("Kurzeme", 
                               "Latgale", 
                               "Pier\u012Bga",
                               "R\u012Bga",
                               "Vidzeme",
                               "Zemgale"),
                      guide = guide_legend(override.aes = list(size = 7))) +  
  
  scale_size_area(max_size=16, 
                  breaks=c(8,40,189,910),
                  labels=c("15","100","700","5000"), 
                  name="Eks\u0101menu\nskaits",
                  guide = 
                    guide_legend(override.aes = list(shape=21))
                  ) + 
  xScales[[factor]] +
  scale_y_continuous(
                name=sprintf("Vid\u0113jais %s rezult\u0101ts pa\u0161vald\u012Bb\u0101, %%",theSubject), 
                minor_breaks=seq(40,80,by=5), breaks=seq(40,80,by=10)) +
  geom_smooth(method = "loess", size = 0.6, fill=NA)
grid.script(jscript)
dev.off()



#    }

#     animName <- sprintf("temp/%s-%s.gif",theSubject,factor,gg)
#     if (Sys.info()['sysname'] == "Windows") {
#       cmdPrefix <- "cmd /c "
#     } else { cmdPrefix <- "" } 
#     system(paste0(
#       cmdPrefix,
#       sprintf("convert -delay 200 -loop 0 temp/%s-%s-*.png %s",theSubject,factor,animName)
#     ))   
    
#  }

#   write.table(theRho, file=paste0(theSubject,"-rho.csv"), quote=TRUE, sep=",", 
#               row.names=FALSE, qmethod="double")

#}


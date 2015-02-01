## http://aaronecay.com/blog/2014/02/tooltips-in-ggplot/

if (!"dplyr" %in% installed.packages()) install.packages("dplyr")

library(ggplot2)
library(proto)
library(gridSVG)
library(grid)

setwd("/home/st/demografija-lv/visc/r-maps/")
u.data <- read.csv("newer-house.csv")
u.data[50:55,]

summaryString <- function(label1,value1) {
    r <- ""
    r <- paste0(r, "b",label1,":", value1)
    return (r)
}


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

rukruk <- read.table(
  file="tmp.csv", 
  header=FALSE,
  sep=",",
  row.names=NULL,  
  col.names=c("Town",
              "Latitude",
              "Longitude",
              "region",
              "NUnLowered",
              "M",
              "UnLoweredWds",
              "N",
              "unemployment"),
  skip=1)


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



ourTitle <- "Aaa"
ourSubtitle <- "Bbb"
myColors <- c("#2FBFD5",
              "#3F4FFF", 
              "#68FF5F",
              "#FF2F2F", 
              "#FF982F", 
              "#D5FF2F" )


rukruk$note <- paste0("Bezdarbs: ",rukruk$unemployment)
#rukruk$theColor <- sample(myColors,nrow(rukruk), replace=TRUE)

svgDest <- "u.svg"
gridsvg(svgDest,exportJS="inline",
        addClasses=TRUE,width=10,height=7.5,
        xmldecl='<?xml version="1.0" encoding="UTF-8"?>')
ggplot(rukruk) + 
  aes_string(x = "Longitude", y = "Latitude", size="M") +
  geom_tooltip(aes(tooltip = paste0(
    "<b>", Town,
    ":</b><br />",
    note),
    color=region, size = M),
    real.geom = geom_point) +  
  geom_point(shape=21) +  
  ggtitle("Bezdarbs un MAT9 rezult\u0101ti pa\u0161vald\u012Bb\u0101s") + 
  theme(
    legend.title=element_text(size=12),  
    panel.background = element_rect(fill = 'white', colour = 'darkgreen'),
    panel.grid.minor = element_line(colour="lightgray", size=0.5, linetype="dotted"),
    panel.grid.major = element_line(colour="black", size=0.5, linetype="dotted"),
    plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "in"),
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
  
    scale_size_area(max_size=16, guide = FALSE)
grid.script(jscript)
dev.off()

#get the latest version of gridSVG
#install.packages("gridSVG", repos="http://R-Forge.R-project.org")
require(ggplot2)
require(gridSVG)
require(XML)

setwd("/home/st/demografija-lv/visc/")

set.seed(955)
# Make some noisily increasing data
dat <- data.frame(cond = rep(c("A", "B"), each=10),
                  xvar = 1:20 + rnorm(20,sd=3),
                  yvar = 1:20 + rnorm(20,sd=3))
# cond         xvar         yvar
#    A -4.252354091  3.473157275
#    A  1.702317971  0.005939612
#   ... 
#    B 17.793359218 19.718587761
#    B 19.319909163 19.647899863
g4 <- ggplot(dat, aes(x=xvar, y=yvar)) +
  geom_smooth(method="loess") +  #we'll see why order is important
  geom_point(shape=19, aes(color = cond), size=5) 
g4



#print our ggplot2 graphic again
g4
#export to SVG file and R object
#grid.export deprecates the older gridToSVG
g4.svg <- grid.export("plot1.svg",addClasses=TRUE)
#print our newly exported SVG inline
#cat(XML::saveXML(g4.svg$svg))

str(g4)
head(g4$data)


sink('analysis-output.svg')
cat(
  '<script> ourdata=',
  rjson::toJSON(apply(g4$data,MARGIN=1,FUN=function(x)return(list(x)))),
  '</script>'
)

cat(
  '<script> dataToBind = ',
  'd3.entries(ourdata.map(function(d,i) {return d[0]}))',
  '</script>'
)

cat(
  '<script>\n',
  'scatterPoints = d3.select(".points").selectAll("use");\n',
  'scatterPoints.data(dataToBind)',
  '</script>\n'
)


cat('<script>\n',
    'scatterPoints  
    .on("mouseover", function(d) {      
    //Create the tooltip label
    var tooltip = d3.select(this.parentNode).append("g");
    tooltip
    .attr("id","tooltip")
    .attr("transform","translate("+(d3.select(this).attr("x")+10)+","+d3.select(this).attr("y")+")")
    .append("rect")
    .attr("stroke","white")
    .attr("stroke-opacity",.5)
    .attr("fill","white")
    .attr("fill-opacity",.5)
    .attr("height",30)
    .attr("width",50)
    .attr("rx",5)
    .attr("x",2)
    .attr("y",5);
    tooltip.append("text")
    .attr("transform","scale(1,-1)")
    .attr("x",5)
    .attr("y",-22)
    .attr("text-anchor","start")
    .attr("stroke","gray")
    .attr("fill","gray")
    .attr("fill-opacity",1)
    .attr("opacity",1)
    .text("x:" + Math.round(d.value.xvar*100)/100);
    tooltip.append("text")
    .attr("transform","scale(1,-1)")
    .attr("x",5)
    .attr("y",-10)
    .attr("text-anchor","start")
    .attr("stroke","gray")
    .attr("fill","gray")      
    .attr("fill-opacity",1)
    .attr("opacity",1)
    .text("y:" + Math.round(d.value.yvar*100)/100);
    })              
    .on("mouseout", function(d) {       
    d3.select("#tooltip").remove();  
    });',
'</script>'
)
sink()



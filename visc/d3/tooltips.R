###
### http://www.r-bloggers.com/tooltips-with-r-and-jquery/
###

panel.circles <- function(x, y, identifier,...){
  for (i in seq_along(x)){
    grid.circle(x[i], y[i], r=unit(5, 'mm'),
                default.units='native',
                ## Add the identifier
                name = paste('myPoint', identifier[i], sep='.'), 
                gp=gpar(fill='gray',...))
  }}

library(gridSVG)
library(lattice)

x <- rnorm(10)
y <- rnorm(10)
id <- seq_along(x)
df <- data.frame(x, y, id)

xyplot(y ~ x, data=df,
       panel=panel.circles,
       ## Identifier of each circle
       identifier=df$id)


for (i in 1:nrow(df)){
  idPoint <- df[i, "id"]
  ## Each circle is identified with myPoint.1, myPoint.2,etc.
  namePoint <- paste('myPoint', idPoint, sep='.')
  point <- signif(df[i, ], 2)
  ## Text to be included in the 'title' attribute
  info <- paste('x = ', point$x, 
                '<br />y = ', point$y, 
                '<br />ID = ', idPoint, sep='')
  imageURL <- 'http://developer.r-project.org/Logo/Rlogo-5.png'
  imageInfo <- paste("<img src=", imageURL, " width='50' />", sep='')
  tooltip <- paste(imageInfo, info, sep='<br />')
  grid.garnish(namePoint, title=tooltip)
}



## jQuery
grid.script(file='http://ajax.googleapis.com/ajax/libs/jquery/1.8.0/jquery.min.js')
## tooltipster (downloaded from http://calebjacob.com/tooltipster/)
grid.script(file='js/jquery.tooltipster.min.js')


# grid.script(file='js/myTooltipLattice.js')


grid.export('tooltipLattice.svg')

htmlBegin <- '<!DOCTYPE html>
  <html>
  <head>
  <title>Tooltips with jQuery and gridSVG</title>
  <link rel="stylesheet" type="text/css" href="css/tooltipster.css" />
  </head>
  <body>'

htmlEnd <- '</body>
  </html>'

svgText <- paste(readLines('tooltipLattice.svg'), collapse='\n')

writeLines(paste(htmlBegin, svgText, htmlEnd, sep='\n'),
           'tooltipLattice.html')







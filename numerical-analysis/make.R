setwd("/home/st/demografija-lv/numerical-analysis/")

#if(!require(installr)) { install.packages("installr"); require(installr)} #load / install+load installr 
#install.pandoc()

if (!"knitr" %in% installed.packages()) install.packages("knitr")
if (!"markdown" %in% installed.packages()) install.packages("markdown")
if (!"rmarkdown" %in% installed.packages()) install.packages("rmarkdown")

library(knitr)
library(markdown)
library(rmarkdown)

files <- c("runge-function")
for (file in files) {
   knit(paste0(file,".rmd"))
   options(markdown.HTML.options = NULL)   
   markdownToHTML(
     paste0(file,".md"), 
     paste0(file,".html"),
     options=c("mathjax","highlight_code"))
#   system(paste0("pandoc -o ", 
#                 file, 
#                 ".html ", 
#                 file, 
#                 ".md"))

#  knit2html(paste0(file,".rmd"))
  
#  rmarkdown::render(paste0(file,".rmd"))
#   writeLines(
#     iconv(readLines(paste0(file,".html")), 
#           from = "UTF-8", 
#           to = "latin1"), paste0(file,".new.html"))
  
#  options(encoding = "UTF-8")


#   knit(paste0(file,".rmd"))
#   markdownToHTML(paste0(file,".md"), 
#                  paste0(file,".html"))
}



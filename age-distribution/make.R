setwd("/home/st/demografija-lv/age-distribution/")

#if(!require(installr)) { install.packages("installr"); require(installr)} #load / install+load installr 
#install.pandoc()

if (!"knitr" %in% installed.packages()) install.packages("knitr")
if (!"markdown" %in% installed.packages()) install.packages("markdown")

library(knitr)
library(markdown)

files <- c("age-histogram1","age-histogram5","population-pyramid")
for (file in files) {
  knit(paste0(file,".rmd"))
  options(markdown.HTML.options = NULL)   
  markdownToHTML(
    paste0(file,".md"), 
    paste0(file,".temp.html"),
    options=c("mathjax","highlight_code"))
  lines <- readLines(paste0(file,".temp.html"),encoding="UTF-8")
  writeLines(lines,paste0(file,".html"))
  file.remove(paste0(file,".temp.html"))
}
unlink("temp", recursive=TRUE)


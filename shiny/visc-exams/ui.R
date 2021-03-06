if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"shiny" %in% installed.packages()) install.packages("shiny")

library(ggplot2)
library(plyr)
library(shiny) 


shinyUI(pageWithSidebar( 
  headerPanel("VISC Dati: Centraliz\u0113tie eks\u0101meni"), 
  sidebarPanel( 
    sliderInput(inputId = "histBars",
                label = "Stabi\u0146i histogramm\u0101",
                min = 10,
                max = 100,
                value = 40,
                step = 5),
    radioButtons(inputId = "annoStyle",
                 label = "Anot\u0101cija",
                 choices = list("m+IQR" = "m-iqr",
                                "\u03BC+\u03C3" = "mu-sigma")),
    selectInput(inputId = "groupBy",
                label = "Grup\u0113\u0161ana",
                choices = list("P\u0113c dzimuma" = "dzimums",
                               "P\u0113c valodas" = "valoda",
                               "P\u0113c re\u01E7iona" = "region",
                               "P\u0113c urbaniz\u0101cijas" = "urban",
                               "P\u0113c skolas tipa" = "tips",
                               "P\u0113c bezdarba" = "joblessCat"),
                selected="dzimums"),
    radioButtons(inputId = "gads",
                 label = "Gads",
                 choices = list("2014" = "2014",
                                "2013" = "2013",
                                "2012" = "2012")),
    selectInput(inputId = "prieksmets",
                label = "Priek\u0161mets",
                choices = list("Matem\u0101tika" = "MAT",
                               "Anglu valoda" = "ANG",
                               "Latvie\u0161u valoda" = "VLL",
                               "Biolo\u01E7ija"="BIO",
                               "Fizika"="FIZ",
                               "Fran\u010Du valoda"="FRA",
                               "\u0136\u012Bmija"="KIM",
                               "Krievu valoda"="KRV",
                               "V\u0101cu valoda"="VAC",
                               "V\u0113sture"="VES",
                               "Latv.val. 9-kl."="LV9")),
    checkboxGroupInput(inputId = "schoolType",
                       label = "Skolas tips",
                       choices = list("Vidusskola" = "3",
                                      "Vakarskola" = "4",
                                      "\u0122imn\u0101zija" = "5",
                                      "Valsts \u01E7imn." = "6", 
                                      "Prof.&M\u0101kslas" = "7",
                                      "Specskola" = "8",
                                      "Augstskola" = "9"),
                       selected=c("3","5","6","8","9")),
    selectInput(inputId = "region",
                label = "Re\u01E7ions",
                choices = list("Visi" = "A",
                               "Kurzeme" = "K",
                               "Latgale" = "L",
                               "Pier\u12Bga" = "R",
                               "R\u012Bga" = "C",
                               "Vidzeme" = "V",
                               "Zemgale" = "Z"),
                selected="A"),
    
    checkboxGroupInput(inputId = "urban",
                label = "Urbaniz\u0101cija",
                choices = list("R\u012Bga" = "1",
                               "Lielpils\u0113tas" = "2",
                               "Mazpils\u0113tas" = "3",
                               "Lauki" = "4"),
                selected=c("1","2","3","4")),
    selectInput(inputId = "valoda",
                label = "M\u0101c\u012Bbu valoda",
                choices = list(
                  "visas" = "0",
                  "latvie\u0161u" ="1",
                  "krievu"="2",
                  "jauktas"="3",
                  "citas"="4"),
                selected="0"),
    selectInput(inputId = "joblessCat",
                label="Bezdarbs",
                choices = list(
                  "vienalga" = "0",
                  "(2.5;5]" = "1",
                  "(5;7.5]" = "2",
                  "(7.5;10]" = "3",
                  "(10;12.5]" = "4",
                  "(12.5;15]" = "5",
                  "(15;30]" = "6"
                ),
                selected="0")
  ),
  mainPanel(
    tabsetPanel( 
      tabPanel("Apraksts", htmlOutput("textDisplay")), 
      tabPanel("Sal\u012Bdzin\u0101jumi", plotOutput("comparisonGraph", height="800px")),
      tabPanel("Kopsalikums", plotOutput("totalGraph", height="800px"))
    )
  )
)) 
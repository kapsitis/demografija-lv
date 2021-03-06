#### 
#### Caller specifies working directory
####

# neSchools <- c()
# neMunicipalities <- c()
# for (i in 1:nrow(munAndSch)) {
getStudentNum <- function(sh) {
  vect <- schoolPop[which(schoolPop$SchoolName == sh), 
                    c("class01", "class02", "class03", 
                      "class04", "class05", "class06", 
                      "class07", "class08", "class09")
                    ]
  if (nrow(vect) == 1) {
    return(sum(vect[7:9])/3)
  }
  if (nrow(vect) == 0) {
    return(22.5)
  }
  if (nrow(vect) > 1) {
    print(paste0("Student number ambiguous for school '",sh, "'"))
    return(22.5)
  }
}



# Extract municipality/city from school's address
extractRegion <- function(address) {
  # Drop Zipcode
  xx <- sub(pattern = ", LV-[0-9]{4}$", replacement="", x = address)
  # Split by commas; rewrite to lower case
  yy <- strsplit(tolower(xx),",")[[1]]
  # get the last element
  zz <- yy[length(yy)]
  # trim whitespace from start and from end
  tt <- gsub("^\\s+|\\s+$", "", zz)
  # capitalize the first letter
  return(capitalize(tt))
}


factorNames <- c("UnemploymentRate",
                 "DependencyRatio",
                 "PopulationChangePerYear",
                 "PerCapitaIncomeTax",
                 "ExpenditurePerStudent",
                 "PerCapitaRealEstateTax",
                 "FemaleRatio")

examTypes <- list(ANG9 = "9.kl. angļu valoda",
                  INF12 = "12.kl. informātika",
                  MAT9 = "9.kl. matemātika", 
                  VES9 = "9.kl. informātika",
                  VLL9 = "9.kl. latviešu valoda")

indicatorsShort <- list(UnemploymentRate = "bezdarbs",
                        DependencyRatio = "demogr.slodze", 
                        PopulationChangePerYear = "iedz.sk.izmai\u0146a",
                        PerCapitaIncomeTax = "IIN uz 1 iedz.",
                        ExpenditurePerStudent = "1 skol\u0113na izmaksas",
                        PerCapitaRealEstateTax = "N\u012AN uz 1 iedz.",
                        FemaleRatio = "sievie\u0161u %")

xScales <- list(
  UnemploymentRate = scale_x_continuous(
    name="Darba meklētāji %", 
    breaks=seq(5,25,by=5),
    minor_breaks=seq(0,27.5,by=2.5)),
  DependencyRatio = scale_x_continuous(
    name="Apgādājamie uz 1000 darbspējas vecuma iedz.", 
    breaks=seq(450,650,by=50)),
  PopulationChangePerYear = scale_x_continuous(
    name="Iedz. skaita izmaiņa uz 1000 iedz. gadā", 
    breaks=seq(-3,3,by=1)),
  PerCapitaIncomeTax = scale_x_log10(
    name="IIN uz 1 iedz. gadā", 
    breaks=c(200,400,800)), 
  ExpenditurePerStudent = scale_x_continuous(
    name="Izdevumi uz 1 skolēnu gadā", 
    breaks=seq(1000,3000,by=1000),
    minor_breaks=seq(500,3500,by=1000)),
  PerCapitaRealEstateTax = scale_x_log10(
    name="NĪ uz 1 iedz. gadā", 
    breaks=c(25,50,100,200)),
  FemaleRatio = scale_x_continuous(
    name="Sieviešu īpatsvars", 
    breaks=seq(49,55,by=1),
    minor_breaks=seq(48.5,55.5,by=1))
)

csvEncoding <- ""


### Read social indicators from RAIM.gov.lv datasets (years 2009-2013)
getSocialIndicators <- function(path) {
  setwd(path) 
  inputFiles <- list.files(pattern="raim-lv-[0-9]{4}")
  filenames <- paste0(path,"/",inputFiles)
  socialIndicators = ldply(filenames, function(filename) {    
    dum = read.table(
      file=filename, 
      header=FALSE,
      sep=",",
      row.names=NULL,  
      col.names=c("Municipality",factorNames),
      skip=1,
      fileEncoding=csvEncoding
    )
    pathSegments <- strsplit(filename,"/")[[1]]
    bareName <- pathSegments[length(pathSegments)]
    print(paste0("Reading CSV file ",bareName))
    segments <- strsplit(bareName,split=c("[-\\.]"))[[1]]
    dum$year <- as.numeric(segments[3])+1
    dum$UnemploymentRate <- 
      as.numeric(sub("%","",dum$UnemploymentRate))
    dum$PopulationChangePerYear <- 
      as.numeric(sub("%","",dum$PopulationChangePerYear))
    dum$ExpenditurePerStudent <- 
      as.numeric(sub(",","",dum$ExpenditurePerStudent))
    dum$FemaleRatio <- 
      as.numeric(sub("%","",dum$FemaleRatio))
    return(dum)
  })
  return(socialIndicators)
}







# centralized exam data
getCentralizedData <- function(path, yyyy) {
  inputFiles <- c(paste0("visi_dati_", yyyy, ".csv"))
  
  filenames <- paste0(path,"/",inputFiles)
  
  visc_exams = ldply(filenames, function(filename) {
    dum = read.table(
      file=filename, 
      header=FALSE,
      sep=",",
      row.names=NULL,  
      col.names=c("prieksmets", "nosaukums", "skolnkods", 
                  "gK","gL","gR","gV","gX","kop",
                  "pK","pL","pR","pV","pX","koppro",
                  "limenis", "ValstsValodasPakape",
                  "dzimums","valdes_kod","klase",
                  "urban","tips", "valoda",
                  "LauksVisiVieni","gads"),
      skip=1,
      fileEncoding=csvEncoding
    )   
    return(dum)
  })
  
  sbr <- read.table(
    file="social-indicators-by-region.csv", 
    header=FALSE,
    sep=",",
    row.names=NULL,  
    col.names=c("Num","municipality","region",
                "jobless","income",
                "schools","visc_code"),
    skip=1,
    fileEncoding=csvEncoding)
  
  
  visi_dati <- merge(visc_exams, sbr, 
                     by.x = "valdes_kod",
                     by.y = "visc_code")
  
  return(visi_dati)
}


standardizeMunicipality <- function(m) {
  if (grepl("Rīgas pilsēta \\(.+\\)",m)) {
    return("Rīga")
  } else if (grepl("(Daugavpils|Jēkabpils|Ventspils) pilsēta",m)) {
    return(sub(" pilsēta","",m))
  } else if (grepl(" pilsēta",m)) {
    return(sub("s pilsēta","",m))
  } else if (grepl("Valmieras novads",m)) {
    return("Kocēnu novads")
  } else { return(m)}
}


getSchoolPopulations <- function(path) {
  setwd(path) 
  schoolPop <- read.table(
    file="Statistika_skoleni_IZM.csv", 
    header=FALSE,
    sep=",",
    row.names=NULL,  
    col.names=c("RegNumber","IZMName","SchoolName",
                "class01","class02",
                "class03", "class04", "class05", 
                "class06", "class07", "class08", 
                "class09"),
    skip=1,
    fileEncoding=csvEncoding)
  return(schoolPop)
}

findMunBySch <- function(sch) {
  rnm <- schoolPop$RegNumber[schoolPop$SchoolName == sch]
  if (length(rnm) != 1) {
    print(paste0("ERROR: RegNumber has ", length(rnm), " values for school '", sch, "'"))
  }
  myRegion <- "*****"
  if (rnm %in% viisDati$RegistracijasNumurs) {
    theAddress <- viisDati$Adrese[min(which(viisDati$RegistracijasNumurs == rnm))]
    myRegion <- extractRegion(theAddress)
  } else if (sch %in% skolaPasvaldiba$Skola) {
    myRegion <- as.character((skolaPasvaldiba$Pasvaldiba[skolaPasvaldiba$Skola == sch])[1])
  } else if (sch %in% skolaPasvaldibaAlt$Skola) {
    myRegion <- as.character(skolaPasvaldibaAlt$Pasvaldiba[skolaPasvaldibaAlt$Skola == sch][1])
  }
  
  return(myRegion)
}








getRegionData <- function(path) {
  setwd(path) 
  sbr <- read.table(
    file="region-data.csv", 
    header=FALSE,
    sep=",",
    row.names=NULL,  
    col.names=c("municipality","region","schools"),
    skip=1,
    fileEncoding=csvEncoding)
  return(sbr)
}



# Nav visai noderiigs; daudzu skolu tur nav
getSkolaPasvaldiba <- function(path, theFile) {
  setwd(path) 
  skolaPasvaldiba <- read.table(
    file=theFile, 
    header=FALSE,
    sep=",",
    row.names=NULL,  
    col.names=c("Skola","Pasvaldiba"),
    skip=1,
    fileEncoding=csvEncoding)
  return(skolaPasvaldiba)
}



# Nav visai noderiigs; daudzu skolu tur nav
getViisDati <- function(path) {
  setwd(path) 
  viisDati <- read.table(
    file="iestades_viis_visc.csv", 
    header=FALSE,
    sep=",",
    row.names=NULL,  
    col.names=c("Nosaukums",
                "RegistracijasNumurs",
                "JuridiskaisStatuss",
                "Vaditajs",
                "FilialuSkaits",
                "StrukturvienibuSkaits",
                "Veids",
                "Tips",
                "FaktiskaisDibinatajs",
                "DeklaretaisDibinatajs",
                "Adrese",
                "Talrunis",
                "Epasts",
                "Novads",
                "Likvideta",
                "viscKods",
                "pasvaldiba",
                "urban",
                "valoda",
                "lat",
                "lng"),
    skip=1, comment.char="#",
    fileEncoding=csvEncoding)
  return(viisDati)
}

getRenamings <- function(path) {
  setwd(path)
  renamings <- read.table(
    file="renamings.csv", 
    header=FALSE,
    sep=",",
    row.names=NULL,  
    col.names=c("municipality",
                "school",
                "renamed"),
    skip=1, comment.char="#", na=FALSE,fileEncoding=csvEncoding)
  return(renamings)
  
}


findSchoolRenaming <- function(mun,sch) {
  nResults <- sum(renamings$renamed != "NA" & 
                    renamings$municipality == mun & 
                    renamings$school == sch)
  if (nResults == 0) {
    return(sch)
  } else if (nResults == 1) {
    newName <- renamings$renamed[renamings$renamed != "NA" & 
                                   renamings$municipality == mun & 
                                   renamings$school == sch]
    return(as.character(newName))
  } else {
    print(paste0("Ambiguous mapping for '",mun,"', '",sch,"'"))
    return(sch)
  }
}


# read pattern-files and return dataframe of non-centralized exam results
getNonCentralizedDF <- function(path) {
  setwd(path)
  
  inputFiles <- list.files(pattern="[0-9]{4}-necentralizetie-(ANG9|INF12|MAT9|VES9|VLL9)")
  filenames <- paste0(path,"/",inputFiles)  
  schoolData <- ldply(filenames, function(filename) {    
    dum = read.table(
      file=filename, 
      header=FALSE,
      sep=",",
      row.names=NULL,  
      col.names=c("municipality","school","result"),
      skip=1,
      fileEncoding=csvEncoding
    )
    pathSegments <- strsplit(filename,"/")[[1]]
    bareName <- pathSegments[length(pathSegments)]
    print(paste0("Reading CSV file ",bareName))
    segments <- strsplit(bareName,split=c("[-\\.]"))[[1]]
    dum$year <- segments[1]
    dum$subject <- segments[3]
    dum$result <- as.numeric(sub("%","",dum$result))
    dum$municipality <- sapply(as.vector(dum$municipality), standardizeMunicipality)
    
    return(dum)
  })
  schoolData[which(schoolData$school=="Mērsraga vidusskola"),"municipality"] <- "Mērsraga novads"
  
  schoolData[which(schoolData$school=="Krustpils pamatskola" & 
                     schoolData$municipality=="Jēkabpils"),"municipality"] <- 
    "Krustpils novads"  
  schoolRenamed <- sapply(1:nrow(schoolData), 
                          function(i) {
                            mmm <- as.character(schoolData$municipality[i])
                            sss <- as.character(schoolData$school[i])
                            findSchoolRenaming(mmm,sss)
                          })
  schoolData$renamed <- schoolRenamed  
  return(schoolData)
}


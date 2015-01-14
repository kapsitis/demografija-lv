\name{getAllData}
\alias{getAllData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getAllData()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    inputFiles <- c("visi_dati_2012.csv", "visi_dati_2013.csv", 
        "visi_dati_2014.csv")
    path <- getwd()
    filenames <- paste0(path, "/", inputFiles)
    visc_exams = ldply(filenames, function(filename) {
        dum = read.table(file = filename, header = FALSE, sep = ",", 
            row.names = NULL, col.names = c("prieksmets", "nosaukums", 
                "skolnkods", "gK", "gL", "gR", "gV", "gX", "kop", 
                "pK", "pL", "pR", "pV", "pX", "koppro", "limenis", 
                "ValstsValodasPakape", "dzimums", "valdes_kod", 
                "klase", "urban", "tips", "valoda", "LauksVisiVieni", 
                "gads"), skip = 1)
        return(dum)
    })
    sbr <- read.table(file = "social-indicators-by-region.csv", 
        header = FALSE, sep = ",", row.names = NULL, col.names = c("Num", 
            "municipality", "region", "jobless", "income", "schools", 
            "visc_code"), skip = 1)
    visi_dati <- merge(visc_exams, sbr, by.x = "valdes_kod", 
        by.y = "visc_code")
    return(visi_dati)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

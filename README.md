demografija-lv
==============

This is a mini-project to deploy site http://www.demografija.lv/. It consists mainly of data sets (in CSV format), R scripts and also KnitR reports. The project also has a bit of Scala scaffolding to ensure that all the R/KnitR reports are properly built and deployed to a remote server. 

If you wish to reuse any of this stuff, please look at the subprojects (listed under src/main/resources/site-properties.xml); each subdirectory is independent from the reset - it has **make.R** file that executes KnitR and generates HTML/PNG/GIF results.

packages <- c( "doBy","downloader",
               "lme4", "lmtest", "car", ## used for regressions
               "ggplot2", "grid", # package for elegant data visualization using the Grammar of Graphics
               "Hmisc", # generate a detailled describtion of a given dataset 
               "AER",  # interesting datasets
               "lattice", 
               "MASS", 
               "gvlma",
               "VGAM",
               "aod",
               "fields", 
               "scatterplot3d", "cluster", 
               "ade4",  "psych", 
               "stringr", # manipulation of string data
               "ellipse",
               "pastecs","car","XML",
               "devtools", # package used to load packages hosted in github -- install CURL before and separately
               "plyr",
               "RCurl", "downloader",
               "xml2",
               "vcd", # Visualisation of categorical data
               "reshape2", # package to easily melt data to long form
               "RColorBrewer", # a package offering color palette from 
               "extrafont", ##" load additional font
               "sp","maptools","rgdal","rgeos","ggmap","hexbin", ## packages used for the maps --
               #"PBSmapping", 
               ## install gdal and geos separately before  http://robinlovelace.net/r/2013/11/26/installing-rgdal-on-ubuntu.html
               "classInt",  ## used for all calissification
               "raster","lubridate","date","gdata","gridExtra","scales",
               "ggthemes", ## load different custmised theme: excel, stata, economist, tufte, wall street journal...
               "xkcd", ## Style from the xkcd comics 
               "XML","xml2","methods","plyr",
               "formatR", #"gWidgetsRGtk2", # used to format the code
               "readxl"
               #"XLConnect" ## Read and write excel files
)
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

rm(packages)

# loads packages into memory
library(doBy)
library(lattice)
#library(gmodels)
library(car)
library(lme4)
library(lmtest)

library(plyr)

library(RCurl)
library(xml2)

library(ggplot2) ## The grammar of graphics!
library(grid)

library(extrafont) ## Additional fonts
library(ggthemes) ## Additional themes for gplot2
library(zoo) ## Manage reformatting of date
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast
library(maptools) ## Create maps
library(rgdal) ## Open geographic files
library(rgeos)
# library(PBSmapping)
library(ggmap) ## get background map from google map
library(sp) ## Spatial library
library(raster) ## Managing raster dataset
library(RColorBrewer) ## Color palette
library(classInt) ## Classififcation
library(hexbin) ## Hexa binning
library(plyr)
gpclibPermit()
library(lubridate)
library(date)
library(gdata)
library(gridExtra)
library(scales)
library(formatR)

##### Load the packages required to read XML files.
#install.packages("XML")
#install.packages("xml2")
library(XML)
library(xml2)
library(methods)

library(readxl)

# install.packages("RCurl")

library(RCurl)
library(downloader)

library(plyr)
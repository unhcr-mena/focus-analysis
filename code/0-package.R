## A curated list of packages
## @edouard_lgp

##This should detect and install missing packages before loading them –
packages <- c(
  
  ##################################################################
  ### Packages for Premodeling Stage

  
  ## Data Manipulation
  "purrr",
  "lubridate","date","gdata","zoo", ## playing with date
  "ggseas", ## seasonal adjustemnt with GGplot2
  "dplyr",  "data.table", "doBy","tidyr", ## Data manipulation
  "reshape2", # package to easily melt data to long form
  "stringr","stringdist","stringi", ## string manipulation
  
  ## Missing value imputation
  "missForest",  "missMDA", "Amelia",
  
  
  ## Outlier Detection
  "outliers",  "evir",
  
  ## Feature Selection
  "features",  "RRF", 
  "Boruta", # wrapper for feature selection algorythm
  
  ## Dimension Reduction
  "CCP", # Dimension Reduction
  "FactoMineR", "ade4",  ## multivariate analysis - MCA
  
  
  ##### Packages for Visualisation
  "lattice", # Visualisation
  "ggplot2", ## advanced graphics
  "ggrepel", ## getting nice labels in ggplot2
  "ggvis", ## interactive grammar of graphics
  "ggthemes", ## Customised themes for ggplot2: excel, stata, economist, tufte, wall street journal...
  "grid", "gridExtra", # Assembel graphcis together
  "gtable", #Arrange 'Grobs' in Tables
  "vcd", # Visualisation of categorical data
  "RColorBrewer", # a package offering color palette from
  "scales", #Scale Functions for Visualization
  "extrafont", ##" load additional font
  "hexbin", ## Hexagrid viz
  "xkcd", ## Style from the xkcd comics
  "scatterplot3d",
  "corrplot", # Visualiation of correlation Matrix
  "igraph", #network analysis and visualisation
  "ellipse",  ## drawing ellipses and ellipse-like confidence regions
  "factoextra", ## Visualize the Results of Multivariate Data Analyses
  
  ##### Packages for Mapping  
  "sp","maptools","rgdal","rgeos", ## standard Geo manipulation packages
  "ggmap", ## get background from webmapping API
  "raster","cartography", ## packages used for the maps --
  "classInt",  ## used for univariate classification
  "deldir", # delaunay triangulation & Voronoi  
  "viridis", # Default Color Maps from 'matplotlib'
  "fields", ## Tools for Spatial Data
  
  ##################################################################
  ### Packages for Modeling Stage  
  
  
  "Hmisc", # generate a detailled describtion of a given dataset
  "gbm", # Generalized Boosted Regression Models
  "car", ## ## Companion to Applied Regression
  "rminer", "CORElearn",  # ordinal Regression
  "caret", # Gradient Boosting & AdaBoost 
  "bigRR",  ## Classification
  
  
  
  "e1071", #SVM (Support Vector Machine)
  "knncat", # KNN (K- Nearest Neighbors)
  "randomForest", # randomForest
  "stats", # Dimensionality Reduction Algorithms princomp
  ## Time Series
  "forecast", "ltsa",
  
  # survival analysis
  "survival", "BaSTA",
  "pastecs", #Analysis of Space-Time Ecological Series
  
  # Lasso and Elastic-Net Regularized Generalized Linear Models
  "glmnet", 
  "lme4", # Linear Mixed-Effects Models
  
  "MASS", 
  "VGAM", #Vector Generalized Linear and Additive Models
  "aod", ## Analysis of Overdispersed Data
  
  ## Cluster analysis
  "cluster", "cba", "Rankcluster", 
  
  ##################################################################
  ### Packages for Post Modeling Stage  
  
  "lmtest", # Testing Linear Regression Models
  
  "gvlma", #Global Validation of Linear Models Assumptions
  
  "lsmeans", "comparison", #general Model Validation
  "regtest", "ACD", #Regression validation
  
  "binomTools","Daim", ## classification validation
  "clusteval","sigclust", ## Clustering valisation
  
  "pROC","timeROC", # ROC Analysis  
  
  ## Recursive Partitioning and Regression Trees
  "rpart", "rpart.plot",
  
  ##################################################################
  ### Packages for Survey data management  
  "sampling", ## Survey Sampling
  "survey",  ##Analysis of Complex Survey Samples
  
  ##################################################################
  ### Other Packages
  
  
  "psych", ## Procedures for Psychological, Psychometric, and Personality Research
  
  "Benchmarking", #Benchmark and Frontier Analysis Using Data Envelopmenbt Aanalysis
  
  "pwr", # Power Analysis allows  to determine the sample size required to detect an effect of a given size with a given degree of confidence.
  
  ## text mining
  "tm", "twitteR" , 
  "wordcloud", #Word Clouds
  "LDAvis", # Interactive Visualization of Topic Models
  
  "AER",  # Applied economtrics with R
  
  "formatR", #  used to format the code
  "xtable", "knitr", "pander", ## Package for Rmd
  "downloader",
  
  "parallel", ## Improve performance
  "Rcpp", ## used to compile some pacjckages
  
  "foreign", ## read data from SPSS, SAS or Stata
  "sqldf", "RODBC", "RMongo","RSQLite", ## Direct connection with databases
  
  "rJava", "XLConnect", ## Read and write excel files
  "readxl", ## Read Excel files
  
  "httr", "rjson","jsonlite", ## get data from API
  "XML", "xml2", ## Manipulation of xml  
  
  "RCurl", ##used to download files from API -install CURL before and separately
  "devtools", # package used to load packages hosted in github -- 
  
  "gmailr", # Access gmail api
  
  "rattle" ## GUI for data mining
)

## identify packages not installed yet
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

rm(packages)



# loads packages into memory
library(doBy)
library(purrr)
library(plyr)
library(ggplot2) ## The grammar of graphics!
library(grid)
library(extrafont) ## Additional fonts
library(ggthemes) ## Additional themes for gplot2
library(zoo) ## Manage reformatting of date
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast
library(RColorBrewer) ## Color palette
library(classInt) ## Classififcation
library(plyr)
library(lubridate)
library(date)
library(gdata)
library(gridExtra)
library(scales)
library(formatR)
library(XML)
library(xml2)
library(methods)
library(readxl)
library(RCurl)
library(downloader)
library(plyr)


#gpclibPermit()
#library(lattice)
#library(gmodels)
#library(car)
#library(lme4)
#library(lmtest)
#library(maptools) ## Create maps
#library(rgdal) ## Open geographic files
#library(rgeos)
# library(PBSmapping)
#library(ggmap) ## get background map from google map
#library(sp) ## Spatial library
#library(raster) ## Managing raster dataset
#library(hexbin) ## Hexa binning

##### Load the packages required to read XML files.
#install.packages("XML")
#install.packages("xml2")

# install.packages("RCurl")



format_si <- function(...) {
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}

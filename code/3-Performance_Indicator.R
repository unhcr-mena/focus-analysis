
### Function that parse all country plan in opreference in order to create a consolidated list of all indicators
#source("code/1-parse_reference.R")
source("code/0-package.R")

opreferencemena <- read.csv("data/opreferencemena.csv")
#names(opreferencemena)

##################################################3 
## Pb with parsing some plans -- Need to be fixed
opreferencemena$plandel <- paste(opreferencemena$operationName, opreferencemena$planningPeriod, sep = " ")
opreferencemena.perf <- opreferencemena[ !(opreferencemena$plandel %in% c('Algeria 2015',
                                                                      'Egypt 2013', 'Egypt 2014',
                                                                      'Israel 2013', 'Israel 2014', 'Israel 2015',  'Israel 2016',  'Israel 2017', 'Israel 2018', 
                                                                      'Morocco 2015','Morocco 2016', 'Morocco 2017',
                                                                      'Syrian Arab Republic 2014',
                                                                      'Lebanon 2015', 
                                                                      'Iraq 2015',
                                                                      'Saudi Arabia 2017')), ]
                                                                      
                                                                     # 'Djibouti 2015',
                                                                     # 'Colombia 2015', 
                                                                     # 'RO bangkok 2013', 'RO bangkok 2014', 
                                                                      #'Turkmenistan 2013', 'Turkmenistan 2014', 'Turkmenistan 2015',
                                                                      #'Mali 2014'

opreference <- opreferencemena.perf[ , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                           "plantype",  "operationName","regionanme", "idregion","idoperation")] 

## Loop through urls and download all plan 
perfindicatorall <- NULL
#names(opreference)

nindic <-nrow(opreference)
for(i in 1:nindic)
{
  #i <- 20
  idplan <- as.character(opreference[ i , 2])
  operationID <- as.character(opreference[ i , 1])
  planid <- as.character(opreference[ i , 3])
  planname <- as.character(opreference[ i , 4])
  planningPeriod <- as.character(opreference[ i , 5])
  plantype <- as.character(opreference[ i , 6])
  operationName <- as.character(opreference[ i , 7])
  regionanme <- as.character(opreference[ i , 8])
  idregion <- as.character(opreference[ i , 9])
  idoperation <- as.character(opreference[ i , 10])
  plancountryid <- paste( "data/plan/Plan_", idplan ,".xml", sep = "")
  
  print(paste (i , "Now loading Operation Plan for ", operationName ," for year ", planningPeriod ," from ", plancountryid, sep = " - ", collapse = NULL) )
  
  plancountryparse <- xmlTreeParse(plancountryid, useInternal = TRUE)
  
  xp <- function (doc, tag){
    n <- xpathSApply(doc, tag, xmlValue)
    if (length(n) > 0) 
      # paste multiple values?  BILCOD and probably others..
      paste0(n, collapse="; ") 
    else NA
  }
  
  
  
  z <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/indicators/Indicator")
  n <-length(z)
  notices <-vector("list",n)
  for(i in 1:n)
  {
    z2<-xmlDoc(z[[i]])
    notices[[i]] <- data.frame(
      indicatorid     =  xpathSApply(z2, "//Indicator", xmlGetAttr, 'ID'),
      indicatorrfid   =  xpathSApply(z2, "//Indicator", xmlGetAttr, 'RFID'),
      Indicator        = xp(z2, "//name"),
      Type             = xp(z2, "//isPerformance"),
      GSP              = xp(z2, "//isGSP"),
      Standard         = as.numeric(xp(z2, "//standard")),
      OP.Target        = as.numeric(xp(z2, "//impTarget")),
      OL.Target        = as.numeric(xp(z2, "//compTarget")),
      Mid.Year         = as.numeric(xp(z2, "//midYearValue")),
      Year.End         = as.numeric(xp(z2, "//yearEndValue")),
      Disag.           = xp(z2, "//disAggrStr"),
      Reporting.Level  = xp(z2, "//reportingLevel"),
      reversal         = xp(z2, "//reversal"),
      stringsAsFactors=FALSE)
    free(z2)  
  }
  perfindicatortemp <- as.data.frame(do.call("rbind", notices))
  
  
  
  ## If we have only one population group, it will be difficult to join: need to test
  ppgnum <- getNodeSet(plancountryparse, "//ppgs/PPG/name")
  indicnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/indicators/Indicator")
  
  if (as.numeric(length(ppgnum))> 1) {
    print(paste ("There is ",length(ppgnum) , "population groups and ",length(indicnum), "performance indicators", sep = " ", collapse = NULL) )
    
    temp <-  xpathSApply(plancountryparse, "//ppgs/PPG", function(x) 
      cbind(
        Population.Group = xpathSApply(x, "name", xmlValue),
        Goal             = xpathSApply(x, "goals/Goal/name", xmlValue),
        Goalid           = xpathSApply(x, "goals/Goal", xmlGetAttr, 'ID'),
        indicatorid      = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/indicators/Indicator", xmlGetAttr, 'ID')
      )
    )
    #str(temp)
    
    perfindicatorobj1 <- as.data.frame(do.call("rbind", temp))
    perfindicatorobj <- as.data.frame(lapply(perfindicatorobj1, function(X) unname(unlist(X))))
    perfindicatortemp1 <- merge (perfindicatorobj, perfindicatortemp , by="indicatorid")
    
  } else {
    print(paste ("Only one population group in this Operation Plan  and ",length(indicnum), "performance indicators", sep = " ", collapse = NULL) )
    
    temp <-  cbind(
      Population.Group = xpathSApply(plancountryparse, "//ppgs/PPG/name", xmlValue),
      Goal             = xpathSApply(plancountryparse, "//ppgs/PPG/goals/Goal/name", xmlValue),
      Goalid           = xpathSApply(plancountryparse, "//ppgs/PPG/goals/Goal", xmlGetAttr, 'ID')
    )
    perfindicatorobj <- as.data.frame(temp)
    perfindicatortemp1 <- cbind (perfindicatorobj, perfindicatortemp)
  }

  
  perfindicatortemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, perfindicatortemp1)
  perfindicatorall <- rbind(perfindicatorall, perfindicatortemp2 )
  rm(perfindicatortemp2, perfindicatortemp1, perfindicatortemp, perfindicatorobj, perfindicatorobj1 )
  
}

## that's it


#str(perfindicatorall)

########################################3
## Difference between baseline & review
 data <- perfindicatorall
 rm(perfindicatorall)

## Difference between OL target & review
data$mid2targetol <- ifelse(data$OL.Target==0, 0, (data$OL.Target- data$Mid.Year) / data$OL.Target)
data$mid2targetol.sit <- as.factor(findCols(classIntervals(data$mid2targetol, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.3, 0.7, 1, 5000))))
data$mid2targetol.sit <- revalue(data$mid2targetol.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$mid2targetol.sit  <- factor(data$mid2targetol.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))


## Difference between OL target & endyear
data$year2targetol <- ifelse(data$OL.Target==0, 0, (data$OL.Target- data$Year.End) / data$OL.Target)
data$year2targetol.sit <- as.factor(findCols(classIntervals(data$mid2targetol, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.5, 0.9, 1, 5000))))
data$year2targetol.sit <- revalue(data$year2targetol.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$year2targetol.sit  <- factor(data$year2targetol.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))



## Difference between OP target & review
data$mid2targetop <- ifelse(data$OP.Target==0, 0, (data$OP.Target- data$Mid.Year) / data$OP.Target)
data$mid2targetop.sit <- as.factor(findCols(classIntervals(data$mid2targetop, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.3, 0.7, 1, 5000))))
data$mid2targetop.sit <- revalue(data$mid2targetop.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$mid2targetop.sit  <- factor(data$mid2targetop.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))


## Difference between OP target & endyear
data$year2targetop <- ifelse(data$OP.Target==0, 0, (data$OP.Target- data$Year.End) / data$OP.Target)
data$year2targetop.sit <- as.factor(findCols(classIntervals(data$mid2targetop, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.5, 0.9, 1, 5000))))
data$year2targetop.sit <- revalue(data$year2targetop.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$year2targetop.sit  <- factor(data$year2targetop.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))


data$feedback1[is.na(data$OL.Target)] <- "No OL Target"
data$feedback2[is.na(data$OP.Target)] <- "No OP Target"
data$feedback3[is.na(data$Mid.Year)] <- "No Mid Year report"
data$feedback4[is.na(data$Year.End)] <- "No End Year report"

data.performance <- data
##########################################################################
## Load Result Based Management framework -- in order to get # of indic
## Join is done on RFID

framework <- read_excel("data/UNHCR-Result-Based-Management.xlsx", sheet = 1) 
#names(framework)
framework<- framework[ !(is.na(framework$Indicator)) ,  ]
framework<- framework[ !(framework$dup2 %in% c('dup')) ,  ]

framework <- framework[ ,c("rid" , "oid" , "iid" ,"numindic", "indicatorrfid",
                           "Indicator", "protection.related", "subtype", "RightsGroup", "Objective", "Source"  )]

data <- join(x=data, y= framework, by="indicatorrfid", type="left" )

data.performance <- data
write.csv(data.performance, "data/performance.csv", row.names = FALSE)

rm(api,apihead,bin,con,passw,upw,urlend,urlendsp2,urlendsp1,user,url)
rm(i, idoperation, idplan,idregion,n,nindic,notices,operationID,operationName,plancountryid,plancountryparse,planid,planname,
   planname,planningPeriod,plantype,regionanme,z,z2,xp,temp, indicnum, plan, plandctr,plandescr, ppgnum)


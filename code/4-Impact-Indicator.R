### Function that parse all country plan in opreference in order to create a consolidated list of all indicators
#source("code/1-parse_reference.R")
source("code/0-package.R")


opreferencemnea <- read.csv("data/opreferencemena.csv")
#opreferenceope <- read.csv("data/opreferenceope.csv")
names(opreferencemnea)

opreferencemnea$plandel <- paste(opreferencemnea$operationName, opreferencemnea$planningPeriod, sep = " ")

#### 
## Pb with parsing some plans -- Need to be fixed

opreferencemnea.imp <- opreferencemnea[ !(opreferencemnea$plandel %in% c('Morocco 2016',
                                                                         'Algeria 2015',
                                                                         'Egypt 2013', 'Egypt 2014',
                                                                         'Israel 2013', 'Israel 2014',
                                                                         'Lebanon 2015',
                                                                         'Syrian Arab Republic 2014',
                                                                         'Iraq 2015',
                                                                         'Turkmenistan 2013', 'Turkmenistan 2014', 'Turkmenistan 2015',
                                                                         'Regional Activities in Middle East & North Africa (MENA) 2017',
                                                                         'Regional Activities in Middle East & North Africa (MENA) 2016',
                                                                         'Regional Activities in Middle East & North Africa (MENA) 2015',
                                                                         'Regional Activities in Middle East & North Africa (MENA) 2014',
                                                                         'Regional Activities in Middle East & North Africa (MENA) 2013',
                                                                         'Syria Regional Refugee Coordination Office in Amman 2017',
                                                                         'Syria Regional Refugee Coordination Office in Amman 2016',
                                                                         'Syria Regional Refugee Coordination Office in Amman 2015',
                                                                         'Syria Regional Refugee Coordination Office in Amman 2014',
                                                                         'Syria Regional Refugee Coordination Office in Amman 2013',
                                                                         
                                                                         'Djibouti 2015','Colombia 2015', 'Mali 2014',
                                                                         'RO bangkok 2013', 'RO bangkok 2014')), ]

opreference <- opreferencemnea.imp[ , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                         "plantype",  "operationName","regionanme", "idregion","idoperation")] 

write.csv(opreferencemnea.imp, "data/opereferencemenaimp.csv")

#############################################################
## Loop through urls and download all plan 
impindicatorall <- NULL
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
  
  print(paste (i , "Now loading Operation Plan for ", operationName ," for year ", planningPeriod ,#" from ", plancountryid, 
               sep = " - ", collapse = NULL) )
  
  plancountryparse <- xmlTreeParse(plancountryid, useInternal = TRUE)
  
  xp <- function (doc, tag){
    n <- xpathSApply(doc, tag, xmlValue)
    if (length(n) > 0) 
      # paste multiple values?  BILCOD and probably others..
      paste0(n, collapse="; ") 
    else NA
  }
  
  z <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/indicators/Indicator")
  n <-length(z)
  notices <-vector("list",n)
  for(i in 1:n)
  {
    z2<-xmlDoc(z[[i]])
    notices[[i]] <- data.frame(
      indicatorid      =  xpathSApply(z2, "//Indicator", xmlGetAttr, 'ID'),
      indicatorrfid    =  xpathSApply(z2, "//Indicator", xmlGetAttr, 'RFID'),
      Indicator        =  xp(z2, "//name"),
      baseline1        =  as.numeric(xp(z2, "//Baseline")),
      Baseline         =  as.numeric(xp(z2, "//storedBaseline")),
      thresholdRed     =  as.numeric(xp(z2, "//thresholdRed")),
      thresholdGreen   =  as.numeric(xp(z2, "//thresholdGreen")),
      OP.Target        =  as.numeric(xp(z2, "//impTarget")),
      OL.Target        =  as.numeric(xp(z2, "//compTarget")),
      Mid.Year         =  as.numeric(xp(z2, "//midYearValue")),
      Year.End         =  as.numeric(xp(z2, "//yearEndValue")),
      Type             =  xp(z2, "//isPerformance"),
      GSP              =  xp(z2, "//isGSP"),
      isgspcommitted   =  xp(z2, "//isgspcommitted"),
      Disag.           =  xp(z2, "//disAggrStr"),
      Reporting.Level  =  xp(z2, "//reportingLevel"),
      stringsAsFactors=FALSE)
    free(z2)  
  }
  impindicatortemp <- as.data.frame(do.call("rbind", notices))
  rm(temp)
  
  ## If we have only one population group, it will be difficult to join: need to test
  ppgnum <- getNodeSet(plancountryparse, "//ppgs/PPG/name")
  indicnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/indicators/Indicator")
  
  if (as.numeric(length(ppgnum))> 1) {
    print(paste ("There is ",length(ppgnum) , "population groups and ",length(indicnum), "impact indicators", sep = " ", collapse = NULL) )
    
    temp <-  xpathSApply(plancountryparse, "//ppgs/PPG", function(x) 
      cbind(
        Population.Group = xpathSApply(x, "name", xmlValue),
        Goal             = xpathSApply(x, "goals/Goal/name", xmlValue),
        Goalid           = xpathSApply(x, "goals/Goal", xmlGetAttr, 'ID'),
        #  Goalrfid        = xpathSApply(x, "goals/Goal", xmlGetAttr, 'RFID'),
        #  RightsGroup     = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/name", xmlValue),
        #  RightsGroupid    = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup", xmlGetAttr, 'ID'),
        #  RightsGrouprfid  = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup", xmlGetAttr, 'RFID'),
        #  Problem          = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/problemName", xmlValue),
        #  Objective        = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveName", xmlValue),
        indicatorid      = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/indicators/Indicator", xmlGetAttr, 'ID')
      )
    )
    #str(temp)
    impindicatorobj1 <- as.data.frame(do.call("rbind", temp))
    impindicatorobj <- as.data.frame(lapply(impindicatorobj1, function(X) unname(unlist(X))))
    impindicatortemp1 <- merge (impindicatorobj, impindicatortemp , by="indicatorid")
    
  } else {
    print(paste ("Only one population group in this Operation Plan  and ",length(indicnum), "impact indicators", sep = " ", collapse = NULL) )
    
    temp <-  cbind(
      Population.Group = xpathSApply(plancountryparse, "//ppgs/PPG/name", xmlValue),
      Goal             = xpathSApply(plancountryparse, "//ppgs/PPG/goals/Goal/name", xmlValue),
      Goalid           = xpathSApply(plancountryparse, "//ppgs/PPG/goals/Goal", xmlGetAttr, 'ID')
      #,
      #  indicatorid      = xpathSApply(plancountryparse, "PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/indicators/Indicator", xmlGetAttr, 'ID')
    )
    impindicatorobj <- as.data.frame(temp)
    impindicatortemp1 <- cbind (impindicatorobj, impindicatortemp)
    
  }
  
  impindicatortemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, impindicatortemp1)
  
  ## Now merging with the rest of the loop
  impindicatorall <- rbind(impindicatorall, impindicatortemp2 )
  rm(impindicatortemp2, impindicatortemp1,impindicatortemp, impindicatorobj, impindicatorobj1 )
}

## that's it


#str(perfindicatorall)

########################################3
## Difference between baseline & review
data <- impindicatorall

rm(impindicatorall) 


########################################
####### 
######################################
data$feedback1[is.na(data$OL.Target)] <- "No OL Target"
data$feedback2[is.na(data$OP.Target)] <- "No OP Target"
data$feedback3[is.na(data$Mid.Year)] <- "No Mid Year report"
data$feedback4[is.na(data$Year.End)] <- "No End Year report"
data$feedback5[is.na(data$Baseline)] <- "No Baseline"

#"No End Year report", "No Mid Year report", "No Baseline","No OL Target", "No OP Target",

##########################################################
#### Target vs Mid year

## Difference between OL target & review
data$mid2targetol <- ifelse(data$OL.Target==0, 0, (data$OL.Target - data$Mid.Year) / data$OL.Target)
data$mid2targetol.sit <- as.factor(findCols(classIntervals(data$mid2targetol, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.3, 0.7, 1, 5000))))
data$mid2targetol.sit <- revalue(data$mid2targetol.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$mid2targetol.sit  <- factor(data$mid2targetol.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))
data$mid2targetol.sit <- as.character(data$mid2targetol.sit)
data$mid2targetol.sit[data$OL.Target==0] <- "OL Target is 0"
data$mid2targetol.sit[data$Mid.Year==0] <- "Mid Year report is 0"
data$mid2targetol.sit[is.na(data$OL.Target)] <- "No OL Target"
data$mid2targetol.sit[is.na(data$Mid.Year)] <- "No Mid Year report"
data$mid2targetol.sit  <- factor(data$mid2targetol.sit, levels = c("No Mid Year report","Mid Year report is 0","No OL Target","OL Target is 0",  "Underplan", "Underperf", "Normal", "Overperf","Overplan"))

## Difference between OP target & review
data$mid2targetop <- ifelse(data$OP.Target==0, 0, (data$OP.Target- data$Mid.Year) / data$OP.Target)
data$mid2targetop.sit <- as.factor(findCols(classIntervals(data$mid2targetop, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.3, 0.7, 1, 5000))))
data$mid2targetop.sit <- revalue(data$mid2targetop.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$mid2targetop.sit  <- factor(data$mid2targetop.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))
data$mid2targetop.sit <- as.character(data$mid2targetop.sit)
data$mid2targetop.sit[data$OP.Target==0] <- "OP Target is 0"
data$mid2targetop.sit[data$Mid.Year==0] <- "Mid Year report is 0"
data$mid2targetop.sit[is.na(data$OP.Target)] <- "No OP Target"
data$mid2targetop.sit[is.na(data$Mid.Year)] <- "No Mid Year report"
data$mid2targetop.sit  <- factor(data$mid2targetop.sit, levels = c("No Mid Year report","Mid Year report is 0", "No OP Target","OP Target is 0", "Underplan", "Underperf", "Normal", "Overperf","Overplan"))



##########################################################
#### Target vs End year

## Difference between OL target & endyear
data$year2targetol <- ifelse(data$OL.Target==0, 0, (data$OL.Target- data$Year.End) / data$OL.Target)
data$year2targetol.sit <- as.factor(findCols(classIntervals(data$mid2targetol, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0 , 0.5, 0.9, 1, 5000))))
data$year2targetol.sit <- revalue(data$year2targetol.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$year2targetol.sit  <- factor(data$year2targetol.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))
data$year2targetol.sit <- as.character(data$year2targetol.sit)
data$year2targetol.sit[data$OL.Target==0] <- "OL Target is 0"
data$year2targetol.sit[data$Year.End==0] <- "End Year report is 0"
data$year2targetol.sit[is.na(data$OL.Target)] <- "No OL Target"
data$year2targetol.sit[is.na(data$Year.End)] <- "No End Year report"
data$year2targetol.sit  <- factor(data$year2targetol.sit, levels = c("No End Year report","End Year report is 0","No OL Target","OL Target is 0","Underplan", "Underperf", "Normal", "Overperf","Overplan"))



## Difference between OP target & endyear
data$year2targetop <- ifelse(data$OP.Target==0, 0, (data$OP.Target- data$Year.End) / data$OP.Target)
data$year2targetop.sit <- as.factor(findCols(classIntervals(data$mid2targetop, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0 , 0.5, 0.9, 1, 5000))))
data$year2targetop.sit <- revalue(data$year2targetop.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$year2targetop.sit  <- factor(data$year2targetop.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))
data$year2targetop.sit <- as.character(data$year2targetop.sit)
data$year2targetop.sit[data$OP.Target==0] <- "OP Target is 0"
data$year2targetop.sit[data$Year.End==0] <- "End Year report is 0"
data$year2targetop.sit[is.na(data$OP.Target)] <- "No OP Target"
data$year2targetop.sit[is.na(data$Year.End)] <- "No End Year report"
data$year2targetop.sit  <- factor(data$year2targetop.sit, levels = c("No End Year report","End Year report is 0","No OP Target","OP Target is 0","Underplan", "Underperf", "Normal", "Overperf","Overplan"))



##########################################################
#### Reference to baseline

## Difference between base & Year.End
data$end2base <- ifelse(data$Baseline==0, 0, (data$Year.End - data$Baseline) / data$Baseline)
#hist(data$end2base)
data$end2base.sit <- as.factor(findCols(classIntervals(data$end2base, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.5, 0.9, 1, 5000))))
data$end2base.sit <- revalue(data$end2base.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$end2base.sit  <- factor(data$end2base.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))
data$end2base.sit <- as.character(data$end2base.sit)
data$end2base.sit[data$Baseline==0] <- "Baseline is 0"
data$end2base.sit[data$Year.End==0] <- "End Year report is 0"
data$end2base.sit[is.na(data$Baseline)] <- "No Baseline"
data$end2base.sit[is.na(data$Year.End)] <- "No End Year report"
data$end2base.sit  <- factor(data$end2base.sit, levels = c("No End Year report","End Year report is 0", "No Baseline","Baseline is 0", "Underplan", "Underperf", "Normal", "Overperf","Overplan"))


## Difference between base & mid.End
data$mid2base <- ifelse(data$Baseline==0, 0, (data$Mid.Year - data$Baseline) / data$Baseline)
#hist(data$mid2base)
data$mid2base.sit <- as.factor(findCols(classIntervals(data$mid2base, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.5, 0.9, 1, 5000))))
data$mid2base.sit <- revalue(data$mid2base.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$mid2base.sit  <- factor(data$mid2base.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))
data$mid2base.sit <- as.character(data$mid2base.sit)
data$mid2base.sit[data$Baseline==0] <- "Baseline is 0"
data$mid2base.sit[data$Mid.Year==0] <- "Mid Year report is 0"
data$mid2base.sit[is.na(data$Baseline)] <- "No Baseline"
data$mid2base.sit[is.na(data$Mid.Year)] <- "No Mid Year report"
data$mid2base.sit  <- factor(data$mid2base.sit, levels = c("No Mid Year report","Mid Year report is 0", "No Baseline","Baseline is 0", "Underplan", "Underperf", "Normal", "Overperf","Overplan"))



##########################################################
#### Absolute Gain

## Difference between absolute gain & absolute difference at mid year
data$mid2net <- ifelse((data$OL.Target - data$Baseline)==0, 0, (data$Mid.Year- data$Baseline) / (data$OL.Target - data$Baseline))
data$mid2net.sit <- as.factor(findCols(classIntervals(data$mid2net, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.3, 0.7, 1, 5000))))
data$mid2net.sit <- revalue(data$mid2net.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$mid2net.sit  <- factor(data$mid2net.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))
data$mid2net.sit <- as.character(data$mid2net.sit)
data$mid2net.sit[data$Mid.Year==0] <- "Mid Year report is 0"
data$mid2net.sit[data$Baseline==0] <- "Baseline is 0"
data$mid2net.sit[data$OPL.Target==0] <- "OL Target is 0"
data$mid2net.sit[is.na(data$OL.Target)] <- "No OL Target"
data$mid2net.sit[is.na(data$Baseline)] <- "No Baseline"
data$mid2net.sit[is.na(data$Mid.Year)] <- "No Mid Year report"
data$mid2net.sit  <- factor(data$mid2net.sit, levels = c("No Mid Year report","Mid Year report is 0",   "No Baseline","Baseline is 0","No OL Target","OL Target is 0","Underplan", "Underperf", "Normal", "Overperf","Overplan"))

## Difference between absolute gain & absolute difference at end year
data$end2net <- ifelse((data$OL.Target - data$Baseline)==0, 0, (data$Year.End - data$Baseline) / (data$OL.Target - data$Baseline))
data$end2net.sit <- as.factor(findCols(classIntervals(data$end2net, n = 5, style = "fixed", fixedBreaks = c(-5000 ,0, 0.5, 0.9, 1, 5000))))
data$end2net.sit <- revalue(data$end2net.sit, c(`1` = "Underplan", `2` ="Underperf", `3` = "Normal", `4` = "Overperf", `5` = "Overplan"))
data$end2net.sit  <- factor(data$end2net.sit, levels = c("Underplan", "Underperf", "Normal", "Overperf","Overplan"))
data$end2net.sit <- as.character(data$end2net.sit)
data$end2net.sit[data$Year.End==0] <- "End Year report is 0"
data$end2net.sit[data$Baseline==0] <- "Baseline is 0"
data$end2net.sit[data$OPL.Target==0] <- "OL Target is 0"
data$end2net.sit[is.na(data$OL.Target)] <- "No OL Target"
data$end2net.sit[is.na(data$Baseline)] <- "No Baseline"
data$end2net.sit[is.na(data$Year.End)] <- "No End Year report"
data$end2net.sit  <- factor(data$end2net.sit, levels = c("No End Year report","End Year report is 0", "No Baseline","Baseline is 0","No OL Target",  "OL Target is 0", "Underplan", "Underperf", "Normal", "Overperf","Overplan"))



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

data.impact <- data

## create a concatenated name for the record
data.impact$idrecord <- paste(data.impact$operationName,data.impact$Goal, data.impact$Population.Group, sep="-")

names(data.impact)
write.csv(data.impact, "data/impact.csv", row.names = FALSE)

rm(api,apihead,bin,con,passw,upw,urlend,urlendsp2,user)
rm(i, idoperation, idplan,idregion,n,nindic,notices,operationID,operationName,plancountryid,plancountryparse,planid,planname,
   planname,planningPeriod,plantype,regionanme,z,z2,xp,temp, indicnum, ppgnum)

### Function that parse all country plan in opreference in order to create a consolidated list of all indicators
#source("code/1-parse_reference.R")
source("code/0-package.R")


opreferencemena <- read.csv("data/opreferencemena.csv")
#opreferenceope <- read.csv("data/opreferenceope.csv")
names(opreferencemena)

opreferencemena$plandel <- paste(opreferencemena$operationName, opreferencemena$planningPeriod, sep = " ")

#### 
## Pb with parsing some plans -- Need to be fixed


opreferencemena.imp <- opreferencemena

opreference <- opreferencemena.imp[ opreferencemena.imp$planningPeriod %in% c("2016","2017","2018") , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                         "plantype",  "operationName","regionanme", "idregion","idoperation")] 

write.csv(opreferencemena.imp, "data/opereferencemenaimp.csv")

#############################################################
## Loop through urls and download all plan 
impindicatorall <- NULL
#names(opreference)

  xp <- function (doc, tag){
    n <- xpathSApply(doc, tag, xmlValue)
    if (length(n) > 0) 
      # paste multiple values?  BILCOD and probably others..
      paste0(n, collapse="; ") 
    else NA
  }
  
### First getting the reference of the plan
nimp <- 0
nimp1 <- 0
nimp2 <- 0
  

for(i in 1:nrow(opreference))
{
  #i <- 2
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
  plancountryparse <- xmlTreeParse(plancountryid, useInternal = TRUE)
  
  lastRefreshed   <-  as.data.frame(xpathSApply(plancountryparse, "//Plan/lastRefreshed", xmlValue))
  names(lastRefreshed)[1] <- "lastRefreshed"  
  Refresheddate <- as.character(lastRefreshed$lastRefreshed)
  
  print(paste (i , "Now loading Operation Plan for ", operationName ," for year ", planningPeriod ," from ", plancountryid, " last edited on ", Refresheddate,
               sep = " - ", collapse = NULL) )
  

  
  z <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/indicators/Indicator")
  n <-length(z)
  notices <-vector("list",n)
  for(i in 1:n)
  {
    # i <- 
    z2<-xmlDoc(z[[i]])
    notices[[i]] <- data.frame(
      indicatorid      =  xpathSApply(z2, "//Indicator", xmlGetAttr, 'ID'),
      indicatorrfid    =  xpathSApply(z2, "//Indicator", xmlGetAttr, 'RFID'),
      Type             =  xp(z2, "//isPerformance"),
      Indicator        =  xp(z2, "//name"),
      GSP              =  xp(z2, "//isGSP"),
      Standard         =  as.numeric(xp(z2, "//standard")),
      reversal        =  xp(z2, "//reversal"),
      thresholdRed     =  as.numeric(xp(z2, "//thresholdRed")),
      thresholdGreen   =  as.numeric(xp(z2, "//thresholdGreen")),
      OP.Target        =  as.numeric(xp(z2, "//impTarget")),
      OL.Target        =  as.numeric(xp(z2, "//compTarget")),
      reportingLevel   =  xp(z2, "//reportingLevel"),
      disAggrStr       =  xp(z2, "//disAggrStr"),
      Mid.Year1        = xp(z2, "//midYearValue"),
      Mid.Year2        = as.numeric(xp(z2, "//midYearValue")),
      Year.End1        =  xp(z2, "//yearEndValue"),
      Year.End2        =  as.numeric(xp(z2, "//yearEndValue")),
      baseline1        =  as.numeric(xp(z2, "//Baseline")),
      Baseline         =  as.numeric(xp(z2, "//storedBaseline")),
      isgspcommitted   =  xp(z2, "//isgspcommitted"),
      stringsAsFactors=FALSE)
    free(z2)  
  }
  
  impindicatortemp <- as.data.frame(do.call("rbind", notices))
  impindicatortemp$Year.End <- NULL
  impindicatortemp$Mid.Year <- NULL
  for(i in 1:nrow(impindicatortemp))
    {
    if(is.na(impindicatortemp[i, c("Mid.Year2")])){
    impindicatortemp[i, c("Mid.Year")] <- as.numeric(substr(impindicatortemp[i, c("Mid.Year1")] , 1,(regexpr(";", impindicatortemp[i, c("Mid.Year1")] , ignore.case=FALSE, fixed=TRUE))-1))
    } else { impindicatortemp[i, c("Mid.Year")] <- impindicatortemp[i, c("Mid.Year2")] }
    if(is.na(impindicatortemp[i, c("Year.End2")])){
      impindicatortemp[i, c("Year.End")] <- as.numeric(substr(impindicatortemp[i, c("Year.End1")] , 1,(regexpr(";", impindicatortemp[i, c("Year.End1")] , ignore.case=FALSE, fixed=TRUE))-1))
    } else { impindicatortemp[i, c("Year.End")] <- impindicatortemp[i, c("Year.End2")] }
  }
  
  ## If we have only one population group, it will be difficult to join: need to test
  ppgnum <- getNodeSet(plancountryparse, "//ppgs/PPG/name")
  goalnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/name")
  indicnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/indicators/Indicator")
  
  
    print(paste ("There is ",length(ppgnum) , "population groups",length(goalnum) , "goals and ",length(indicnum), "impact indicators", sep = " ", collapse = NULL) )
    
    rm(temp)
    getPPGContent =
      function(x)
      {
        goal = xpathSApply(x, "./goals/Goal/name", xmlValue)
        pillar = xpathSApply(x, "./goals/Goal/pillar", xmlValue)
        situationCode = xpathSApply(x, "./goals/Goal/situationCode", xmlValue)
        indicator = xpathSApply(x, "./goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/indicators/Indicator", xmlGetAttr, 'ID')
        cbind(
          Population.Group = xpathSApply(x, "./name", xmlValue),
          Goal             = if(length(goal)) goal else NA,
          pillar             = if(length(pillar)) pillar else NA,
          situationCode             = if(length(situationCode)) situationCode else NA,
          indicatorid      = if(length(indicator)) indicator else NA
        )
      }
    

    temp <-  xpathApply(plancountryparse, "//ppgs/PPG", getPPGContent)
    #str(temp)
    # as.data.frame(temp)
    impindicatorobj <- as.data.frame(do.call("rbind", temp))
   
    impindicatortemp1 <- merge (x=impindicatortemp , y=impindicatorobj,  by="indicatorid", all.x=TRUE)
    
    
    nimp <- nimp + nrow(impindicatorobj)
    print(paste ("Loaded ", nrow(impindicatorobj) , "impact indicator Lines, total of", nimp , "impact indicator Lines.", sep = " ", collapse = NULL) )
    
    nimp2 <- nimp2 + nrow(impindicatortemp)
    print(paste ("Loaded ", nrow(impindicatortemp) , "impact indicator Lines, total of", nimp2 , "impact indicator Lines.", sep = " ", collapse = NULL) )
    
    nimp1 <- nimp1 + nrow(impindicatortemp1)
    print(paste ("Merged ", nrow(impindicatortemp1) , "impact indicator Lines, total of", nimp1 , "impact indicator Lines.", sep = " ", collapse = NULL) )
    
  
  impindicatortemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, impindicatortemp1,lastRefreshed)
  
  ## Now merging with the rest of the loop
  impindicatorall <- rbind(impindicatorall, impindicatortemp2 )
  
  rm(impindicatortemp2, impindicatortemp1,impindicatortemp, impindicatorobj, lastRefreshed )
}

## that's it


#str(perfindicatorall)

########################################3
## Difference between baseline & review
data <- impindicatorall

rm(impindicatorall) 


data$Baseline <- as.numeric(data$Baseline)
data$OL.Target <- as.numeric(data$OL.Target)
data$OP.Target <- as.numeric(data$OP.Target)
data$Mid.Year <- as.numeric(data$Mid.Year)
data$Year.End <- as.numeric(data$Year.End)


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

framework <- read_excel("config/UNHCR-Result-Based-Management.xlsx", sheet = 1) 
#names(framework)
framework<- framework[ !(is.na(framework$Indicator)) ,  ]
framework<- framework[ !(framework$dup2 %in% c('dup')) ,  ]

framework <- framework[ ,c("rid" , "oid" , "iid" ,"numindic", "indicatorrfid",
                           "Indicator", "protection.related", "subtype","subtype.obj", "RightsGroup", "Objective", "Source"  )]

data <- join(x=data, y= framework, by="indicatorrfid", type="left" )


SituationCode <- read.csv("config/SituationCode.csv")
data <- join(x=data, y= SituationCode, by="situationCode", type="left")

data.impact <- data

## create a concatenated name for the record
data.impact$idrecord <- paste(data.impact$operationName,data.impact$Goal, data.impact$Population.Group, sep="-")

names(data.impact)
write.csv(data.impact, "data/impact.csv", row.names = FALSE)

rm(api,apihead,bin,con,passw,upw,urlend,urlendsp2,user)
rm(i, idoperation, idplan,idregion,n,nindic,notices,operationID,operationName,plancountryid,plancountryparse,planid,planname,
   planname,planningPeriod,plantype,regionanme,z,z2,xp,temp, indicnum, ppgnum)

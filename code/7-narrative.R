
### Function that parse all country plan in opreference in order to create a consolidated list of all narrative lines
#source("code/1-parse_reference.R")
source("code/0-package.R")

opreferencemena <- read.csv("data/opreferencemena.csv")
#names(opreferencemena)
opreferencemena$plandel <- paste(opreferencemena$operationName, opreferencemena$planningPeriod, sep = " ")

#### 
## Pb with parsing some plans -- Need to be fixed
opreferencemena.narrative <- opreferencemena
opreference <- opreferencemena.narrative[ , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                               "plantype",  "operationName","regionanme", "idregion","idoperation")] 
## Loop through urls and download all plan 

narrativeall <- NULL
#names(opreference)

xp <- function (doc, tag){
    n <- xpathSApply(doc, tag, xmlValue)
    if (length(n) > 0) 
      # paste multiple values?  BILCOD and probably others..
      paste0(n, collapse="; ") 
    else NA
}

### First getting the reference of the plan
nindic <-nrow(opreference)
for(i in 1:nindic)
{
  # i <- 18
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
  

  z <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveNarratives/ElementSection")
  n1 <-length(z)
  
  if (n1==0) { print(paste (i , "There's no narrartive linked to objectives for this report ", collapse = NULL) )
    
    } else { 
  
        notices <-vector("list",n1)
        
        ## Now parsing all budget lines
        for(i in 1:n1)
        {
          z2<-xmlDoc(z[[i]])
          notices[[i]] <- data.frame(
            sectionid     =  xpathSApply(z2, "//ElementSection", xmlGetAttr, 'ID'),
            reportID             = xp(z2, "//reportID"),
            reportName           = xp(z2, "//reportName"),
            sectionID            = xp(z2, "//sectionID"),
            sectionName          = xp(z2, "//sectionName"),
            isExternal           = xp(z2, "//isExternal"),
            text                 = xp(z2, "//text"),
            stringsAsFactors=FALSE)
          free(z2)  
        }
        narrativetemp <- as.data.frame(do.call("rbind", notices))
        
        ## If we have only one population group, it will be difficult to join: need to test
        ppgnum <- getNodeSet(plancountryparse, "//ppgs/PPG/name")
        goalnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/name")
        sectnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveNarratives/ElementSection")
      
          print(paste ("This plan includes ",length(ppgnum) , "population groups, ",length(goalnum), "goalnum and ",length(sectnum), "narrative sections", sep = " ", collapse = NULL) )
        
      
          getPPGContent =
            function(x)
            {
              goal           = xpathSApply(x, "./goals/Goal/name", xmlValue)
              pillar = xpathSApply(x, "./goals/Goal/pillar", xmlValue)
              situationCode = xpathSApply(x, "./goals/Goal/situationCode", xmlValue)
             # RightsGroup      = xpathSApply(x, "./goals/Goal/rightsGroups/RightsGroup/name", xmlValue)
             # Objective      = xpathSApply(x, "./goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveName", xmlValue)
              sectionid      = xpathSApply(x, "./goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveNarratives/ElementSection", xmlGetAttr, 'ID')
            
              cbind(
                Population.Group = xpathSApply(x, "./name", xmlValue),
                goal             = if(length(goal)) goal else NA,
                pillar             = if(length(pillar)) pillar else NA,
                situationCode             = if(length(situationCode)) situationCode else NA,
             #   RightsGroup             = if(length(RightsGroup)) RightsGroup else NA,
             #   Objective             = if(length(Objective)) Objective else NA,
                sectionid      = if(length(sectionid)) sectionid else NA
              )
            }
           temp <-  xpathApply(plancountryparse, "//ppgs/PPG", getPPGContent)
          
          
          narrativeobj <- as.data.frame(do.call("rbind", temp))
          narrativetemp1 <- merge (narrativeobj, narrativetemp , by="sectionid")
       
        narrativetemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, narrativetemp1,lastRefreshed)
        
        ## Now merging with the rest of the loop
        narrativeall <- rbind(narrativeall, narrativetemp2)
        
        rm(narrativetemp2, narrativetemp1,narrativetemp, narrativeobj, narrativeobj1,lastRefreshed )
    }
}  

## that's it



#str(narrativeall)
data.narrative <- narrativeall
write.csv(data.narrative, "data/narrative1.csv")
rm(narrativeall, narrativetemp)

#data.narrative2 <- merge(x=data.narrative, y= framework.out22, by=c("Objective", "RightsGroup"), all.x=TRUE)
data.narrative2 <- data.narrative

## create a concatenated name for the record
data.narrative2$idrecord <- paste(data.narrative2$operationName,data.narrative2$Goal, data.narrative2$Population.Group, sep="-")

SituationCode <- read.csv("config/SituationCode.csv")
data.narrative2 <- join(x=data.narrative2, y= SituationCode, by="situationCode", type="left")

## check that it's parsed correctly
data.narrative2test <- data.narrative2[ data.narrative2$Objective =="Civil registration and civil status documentation strengthened" &
                                          data.narrative2$operationName =="Jordan" & 
                                          data.narrative2$sectionName == "Prioritized Response - Prioritized Response",
                                        c("sectionid","planningPeriod","sectionName",# "Objective",
                                          "Population.Group", "reportName", "text")]

#names(data.narrative2)
write.csv(data.narrative2, "data/narrative.csv", row.names = FALSE)


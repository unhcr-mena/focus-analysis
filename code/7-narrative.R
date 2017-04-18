
### Function that parse all country plan in opreference in order to create a consolidated list of all narrative lines
#source("code/1-parse_reference.R")
source("code/0-package.R")

opreferencemena <- read.csv("data/opreferencemena.csv")
#names(opreferencemena)
opreferencemena$plandel <- paste(opreferencemena$operationName, opreferencemena$planningPeriod, sep = " ")

#opreferencemena <- opreferencemena[ !(opreferencemena$plandel %in% c('Tunisia 2016', 'Morocco 2018')), ]

#### 
## Pb with parsing some plans -- Need to be fixed
opreferencemena.narrative <- opreferencemena
opreference <- opreferencemena.narrative[opreferencemena.narrative$planningPeriod %in% c("2016","2017","2018") , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
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
nindic0 <- 0
nindic  <- 0
nindic1 <- 0
nindic11 <- 0
nindic2 <- 0


for(i in 1:nrow(opreference))
{
  # i <- 16
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
            #  Objective      = xpathSApply(x, "./goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveName", xmlValue)
              sectionid      = xpathSApply(x, "./goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveNarratives/ElementSection", xmlGetAttr, 'ID')
              
              if (length(sectionid)!=0){
                  cbind(
                    Population.Group = xpathSApply(x, "./name", xmlValue),
                    goal             = if(length(goal)) goal else NA,
                    pillar             = if(length(pillar)) pillar else NA,
                    situationCode             = if(length(situationCode)) situationCode else NA,
                   # RightsGroup             = if(length(RightsGroup)) RightsGroup else NA,
                   # Objective             = if(length(Objective)) Objective else NA,
                    sectionid      = if(length(sectionid)) sectionid else NA
                  )
              } else { cat("nothing to parse \n")}
            }
          
           temp <-  xpathApply(plancountryparse, "//ppgs/PPG", getPPGContent)
           narrativeobj <- as.data.frame(do.call("rbind", temp))
           
           ## Some test
           #narrativeobjtest <- narrativeobj[narrativeobj$Objective =="Operations management, coordination and support strengthened and optimized", ]
          # narrativeobj$sectionid <- as.character(narrativeobj$sectionid)
          #  narrativeobjtest <- narrativeobj[narrativeobj$sectionid %in%
           #c("206c0594-15b0-4a84-a669-56d43bccccd8",
          # "aa912a21-4aec-4ffe-864a-1114538e358b"   ) , ]
           
           
           narrativeobj <- narrativeobj[!(is.na(narrativeobj$sectionid )), ]
           
           ## Restore hierachy with RBM
            getobjContent =
              function(x)
              {
                sectionid      = xpathSApply(x, "./objectiveNarratives/ElementSection", xmlGetAttr, 'ID')
                if (length(sectionid)!=0){
                cbind(
                  Objective = xpathSApply(x, "./objectiveName", xmlValue),
                  objectivemsrp = xpathSApply(x, "./msrpcode", xmlValue),
                  sectionid      = if(length(sectionid)) sectionid else NA
                )
                } else { cat("nothing to parse \n")}
              }
            
             temp2 <-  xpathApply(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective", getobjContent)
             narrativeobj2 <- as.data.frame(do.call("rbind", temp2))
             narrativeobj2 <- narrativeobj2[!(is.na(narrativeobj2$sectionid )), ]
             narrativeobj <- join(x=narrativeobj, y=narrativeobj2,  by="sectionid", type="left")
           
           narrativetemp1 <- join(x=narrativetemp, y=narrativeobj,  by="sectionid", type="left")
          
           narrativetemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, narrativetemp1,lastRefreshed)
          
           nindic0 <- nindic0 + nrow(narrativetemp)
           print(paste ("Loaded ", nrow(narrativetemp) , "sections, total of", nindic0 , "sections.", sep = " ", collapse = NULL) )
          
           nindic <- nindic + nrow(narrativeobj)
           print(paste ("Loaded ", nrow(narrativeobj) , "sections to ppg, total of", nindic , "sections.", sep = " ", collapse = NULL) )
           
           nindic11 <- nindic11 + nrow(narrativeobj2)
           print(paste ("Loaded ", nrow(narrativeobj2) , "sections to right, total of", nindic11 , "sections.", sep = " ", collapse = NULL) )
           
           nindic1 <- nindic1 + nrow(narrativetemp1)
           print(paste ("Loaded ", nrow(narrativetemp1) , "sections, total of", nindic1 , "sections.", sep = " ", collapse = NULL) )
           
           nindic2 <- nindic2 + nrow(narrativetemp2)
           print(paste ("Loaded ", nrow(narrativetemp2) , "sections, total of", nindic2 , "sections.", sep = " ", collapse = NULL) )
          
          
       
        ## Now merging with the rest of the loop
        narrativeall <- rbind(narrativeall, narrativetemp2)
        
      #  rm(narrativetemp2, narrativetemp1,narrativetemp, narrativeobj,  narrativeobj2, narrativeobj1,lastRefreshed, goalnum,i,idoperation,
      #     idplan,idregion,n1,
      #     notices,operationID,operationName,plancountryid,planid,plancountryparse,planningPeriod,plantype,ppgnum,planname,
      #     Refresheddate,regionanme,sectnum,temp,temp2,z,z2)
    }
}  

## that's it
rm(nindic,nindic0,nindic1,nindic11)

### Check that we do not duplicate
nindic22 <- unique(narrativeall$sectionid)
print(length(nindic22))

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
data.narrative2test <- data.narrative2[ #data.narrative2$Objective =="Civil registration and civil status documentation strengthened" &
                                          data.narrative2$operationName =="Israel" & 
                                          data.narrative2$sectionName == "Prioritized Response - Prioritized Response",
                                        c("sectionid","planningPeriod","sectionName",# "Objective",
                                          "Population.Group", "reportName", "text")]

#names(data.narrative2)
write.csv(data.narrative2, "data/narrative.csv", row.names = FALSE)

rm(SituationCode)



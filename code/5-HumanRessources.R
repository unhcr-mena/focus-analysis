##################################3
### Parsing position

#######
#source("code/1-parse_reference.R")
source("code/0-package.R")

### Function that parse all country plan in opreference in order to create a consolidated list of all indicators

officetype <- read.csv("data/officetype.csv")
opreferencemnea <- read.csv("data/opreferencemena.csv")

#names(opreferencemnea)
opreferencemnea$plandel <- paste(opreferencemnea$operationName, opreferencemnea$planningPeriod, sep = " ")

#### 
## Pb with parsing some plans -- Need to be fixed

opreferencemnea.hr <- opreferencemnea[ !(opreferencemnea$plandel %in% c('Regional Activities in Middle East & North Africa (MENA) 2017',
                                                                      'Regional Activities in Middle East & North Africa (MENA) 2016',
                                                                      'Regional Activities in Middle East & North Africa (MENA) 2015',
                                                                      'Regional Activities in Middle East & North Africa (MENA) 2014',
                                                                      'Regional Activities in Middle East & North Africa (MENA) 2013',
                                                                      'Syria Regional Refugee Coordination Office in Amman 2017',
                                                                      'Syria Regional Refugee Coordination Office in Amman 2016',
                                                                      'Syria Regional Refugee Coordination Office in Amman 2015',
                                                                      'Syria Regional Refugee Coordination Office in Amman 2014',
                                                                      'Syria Regional Refugee Coordination Office in Amman 2013')), ]

opreference <- opreferencemnea.hr[ , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                           "plantype",  "operationName","regionanme", "idregion","idoperation")] 


################################################################################
function_extractpost <- function(doc, xpathxml, positiontype, officeid , officename){
  officeid  <-  xpathSApply(doc, officeidpp, xmlGetAttr, 'ID')
  officename  <-  xpathSApply(doc, officenamepp, xmlValue)
  z <- getNodeSet(doc, xpathxml )
  n <-length(z)
  positions <-vector("list",n)
  for(i in 1:n)
  {
    z2<-xmlDoc(z[[i]])
    positions[[i]] <- data.frame(
      officeid ,
      officename ,
      #  parentposition     =  xpathSApply(z2, "/subPosition/..", xmlGetAttr, 'ID'),
      positiontype  ,
      position           =  xpathSApply(z2, "/subPosition", xmlGetAttr, 'ID'),
      positionid         = xp(z2, "/subPosition/positionID"),
      title              = xp(z2, "/subPosition/title"),
      incumbent          = xp(z2, "/subPosition/incumbent"),
      type               = xp(z2, "/subPosition/type"),
      grade              = xp(z2, "/subPosition/grade"),
      epmJobCode         = xp(z2, "/subPosition/epmJobCode"),
      hrJobCodep         = xp(z2, "/subPosition/hrJobCode"),
      fastTrack          = xp(z2, "/subPosition/fastTrack"),
      timedCost          = as.numeric(xp(z2, "/subPosition/timedCost")),
      stringsAsFactors=FALSE)
    free(z2)  
  }
  result <- do.call("rbind", positions)
  return(result )
}




xp <- function (doc, tag){
  n <- xpathSApply(doc, tag, xmlValue)
  if (length(n) > 0) 
    # paste multiple values?  BILCOD and probably others..
    paste0(n, collapse="; ") 
  else NA
}

## Loop through urls and download all plan 

positionsofficeall <- NULL

nindic <-nrow(opreference)
for(i in 1:nindic)
  
{
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
  
  
  positionsofficetemp1 <- NULL
  for(i in 1:2)
  {
    doc         <- xmlTreeParse(plancountryid, useInternal = TRUE) 
    officeidpp  <-  as.character(officetype[ i , 1])
    officenamepp  <-  as.character(officetype[ i , 2])
    positiontype <-  as.character(officetype[ i , 3])
    xpathxml <- as.character(officetype[ i , 4])
    positionsofficetemp <- function_extractpost(doc,xpathxml,positiontype, officeid , officename)
    
    positionsofficetemp1 <- rbind(positionsofficetemp1, positionsofficetemp )
  }
  
  
  positionsofficetemp2 <-cbind(positionsofficetemp1, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation)
  positionsofficeall <- rbind(positionsofficeall, positionsofficetemp2 )
}

write.csv(positionsofficeall ,"data/personnel.csv")

## that's it

# Normalising title
title <- as.data.frame(levels(as.factor(positionsofficeall$title)))
write.csv(title, "data/title.csv") 

rm(positionsofficetemp,positionsofficetemp1, positionsofficetemp2,
   doc, officeidpp, officenamepp, positiontype, xpathxml, function_extractpost) 

rm(i, idoperation, idplan,idregion,n,nindic,notices,operationID,operationName,
   plancountryid,plancountryparse,planid,planname,
   planname,planningPeriod,plantype,regionanme,z,z2,xp,temp, indicnum, plan, plandctr,plandescr, ppgnum)



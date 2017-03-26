
### Function that parse all country plan in opreference.mena in order to create a consolidated list of all indicators


#source("code/1-parse_reference.R")



opreferencemena <- read.csv("data/opreferencemena.csv")
#names(opreferencemena)

opreferencemena$plandel <- paste(opreferencemena$operationName, opreferencemena$planningPeriod, sep = " ")

#### 
## Pb with parsing some plans -- Need to be fixed

opreferencemena.budg <- opreferencemena[ !(opreferencemena$plandel %in% c('Algeria 2015','Algeria 2014',
                                                                          'Egypt 2013', 'Egypt 2014',
                                                                          'Israel 2013', 'Israel 2014', 'Israel 2015',  'Israel 2016',  'Israel 2017', 'Israel 2018', 
                                                                          'Morocco 2015','Morocco 2016', 'Morocco 2017',
                                                                          'Syrian Arab Republic 2014',
                                                                          'Lebanon 2015', 
                                                                          'Iraq 2015',
                                                                          'Saudi Arabia 2017')), ]


opreference.mena <- opreferencemena.budg[ , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                         "plantype",  "operationName","regionanme", "idregion","idoperation")] 
## Loop through urls and download all plan 

budgetall <- NULL
#names(opreference.mena)

nindic <-nrow(opreference.mena)
for(i in 1:nindic)
{
  idplan <- as.character(opreference.mena[ i , 2])
  operationID <- as.character(opreference.mena[ i , 1])
  planid <- as.character(opreference.mena[ i , 3])
  planname <- as.character(opreference.mena[ i , 4])
  planningPeriod <- as.character(opreference.mena[ i , 5])
  plantype <- as.character(opreference.mena[ i , 6])
  operationName <- as.character(opreference.mena[ i , 7])
  regionanme <- as.character(opreference.mena[ i , 8])
  idregion <- as.character(opreference.mena[ i , 9])
  idoperation <- as.character(opreference.mena[ i , 10])
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
  
  
  z <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/budgetLines/BudgetLine")
  n <-length(z)
  notices <-vector("list",n)
  for(i in 1:n)
  {
    z2<-xmlDoc(z[[i]])
    notices[[i]] <- data.frame(
      BudgetLineid   =  xpathSApply(z2, "//BudgetLine", xmlGetAttr, 'ID'),
      scenario       = xp(z2, "//scenario"),
      Type           = xp(z2, "//type"),
      costCenter     = xp(z2, "//costCenter"),
      accountCode    = xp(z2, "//accountCode"),
      accountName    = xp(z2, "//accountName"),
      quantity       = as.numeric(xp(z2, "//quantity")),
      currency       = xp(z2, "//currency"),
      unitCost       = as.numeric(xp(z2, "//unitCost")),
      localCost      = as.numeric(xp(z2, "//localCost")),
      amount         = as.numeric(xp(z2, "//amount")),
      stringsAsFactors=FALSE)
    free(z2)  
  }
  budgettemp <- as.data.frame(do.call("rbind", notices))
  
#  temp <- xpathSApply(plancountryparse, "//ppgs/PPG", function(x) 
#    cbind(
#      Population.Group = xpathSApply(x, "name", xmlValue),
#      Goal             = xpathSApply(x, "goals/Goal/name", xmlValue),
#      Goalid           = xpathSApply(x, "goals/Goal", xmlGetAttr, 'ID'),
#      Goalrfid         = xpathSApply(x, "goals/Goal", xmlGetAttr, 'RFID'),
#      RightsGroup      = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/name", xmlValue),
#      RightsGroupid    = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup", xmlGetAttr, 'ID'),
#      RightsGrouprfid  = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup", xmlGetAttr, 'RFID'),
#      Problem          = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/problemName", xmlValue),
#      Objective        = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveName", xmlValue),
#      Outputid         = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output", xmlGetAttr, 'ID'),
#      Outputrfid       = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output", xmlGetAttr, 'RFID'),
#      Output           = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/name", xmlValue),
#      OutputPriority   = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/priority", xmlValue),
#      BudgetLineid     = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/budgetLines/BudgetLine", xmlGetAttr, 'ID')
#      )
#    )
#  budgetobj1 <- as.data.frame(do.call("rbind", temp))
#  budgetobj <- as.data.frame(lapply(budgetobj1, function(X) unname(unlist(X))))
  
#  budgettemp1 <- merge (budgetobj, budgettemp , by="BudgetLineid")
  
  
#  budgettemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, budgettemp1)
  
  budgettemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, budgettemp)
  budgetall <- rbind(budgetall, budgettemp2 )

    budgetall <- rbind(budgetall, budgettemp )
#  rm(budgettemp2, budgettemp1,budgetobj, budgetobj1 )
}

## that's it


#str(budgetall)

########################################3
## Difference between baseline & review
 data.budget <- budgetall
 write.csv(data.budget, "data/budget.csv")
 rm(budgetall, budgettemp)
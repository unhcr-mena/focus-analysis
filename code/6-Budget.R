
### Function that parse all country plan in opreference in order to create a consolidated list of all indicators


#source("code/1-parse_reference.R")



opreferencemena <- read.csv("data/opreferencemena.csv")
#names(opreferencemena)

opreferencemena$plandel <- paste(opreferencemena$operationName, opreferencemena$planningPeriod, sep = " ")

#### 
## Pb with parsing some plans -- Need to be fixed

opreferencemena.budg <- opreferencemena[ !(opreferencemena$plandel %in% c('Algeria 2015','Algeria 2014',
                                                                          'Egypt 2013', 'Egypt 2014',
                                                                          'Israel 2013', 'Israel 2014', 'Israel 2015',  'Israel 2016',  'Israel 2017', 'Israel 2018', 
                                                                          'Morocco 2014','Morocco 2015','Morocco 2016', 'Morocco 2017',
                                                                          'Syrian Arab Republic 2014',
                                                                          'Lebanon 2015', 
                                                                          'Iraq 2015',
                                                                          'Saudi Arabia 2017')), ]


opreference <- opreferencemena.budg[ , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                         "plantype",  "operationName","regionanme", "idregion","idoperation")] 
## Loop through urls and download all plan 

budgetall <- NULL
#names(opreference)

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
  
  ## If we have only one population group, it will be difficult to join: need to test
  ppgnum <- getNodeSet(plancountryparse, "//ppgs/PPG/name")
  budgetnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/indicators/Indicator")
  
  if (as.numeric(length(ppgnum))> 1) {
    print(paste ("There is ",length(ppgnum) , "population groups and ",length(budgetnum), "budget lines", sep = " ", collapse = NULL) )
    
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
        #  Problem          = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/problemName", xmlValue),
        ProblemObjectiverfid        = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective", xmlGetAttr, 'RFID'),
        ProblemObjectiveid        = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective", xmlGetAttr, 'ID'),
        BudgetLineid      = xpathSApply(x, "goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/budgetLines/BudgetLine", xmlGetAttr, 'ID')
      )
    )
    #str(temp)
    budgetobj1 <- as.data.frame(do.call("rbind", temp))
    budgetobj <- as.data.frame(lapply(budgetobj1, function(X) unname(unlist(X))))
    budgettemp1 <- merge (budgetobj, budgettemp , by="BudgetLineid")
    
  } else {
    print(paste ("Only one population group in this Operation Plan  and ",length(budgetnum), "budget lines", sep = " ", collapse = NULL) )
    
    temp <-  cbind(
      Population.Group = xpathSApply(plancountryparse, "//ppgs/PPG/name", xmlValue),
      Goal             = xpathSApply(plancountryparse, "//ppgs/PPG/goals/Goal/name", xmlValue),
      Goalid           = xpathSApply(plancountryparse, "//ppgs/PPG/goals/Goal", xmlGetAttr, 'ID')
    )
    budgetobj <- as.data.frame(temp)
    budgettemp1 <- cbind (budgetobj, budgettemp)
    
  }
  
  budgettemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, budgettemp1)
  
  ## Now merging with the rest of the loop
  budgetall <- rbind(budgetall, budgettemp2 )
  rm(budgettemp2, budgettemp1,budgettemp, budgetobj, budgetobj1 )
}  

## that's it

#str(budgetall)

########################################3
## Difference between baseline & review
 data.budget <- budgetall
 write.csv(data.budget, "data/budget.csv")
 rm(budgetall, budgettemp)
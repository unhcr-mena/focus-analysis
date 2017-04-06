
### Function that parse all country plan in opreference in order to create a consolidated list of all budget lines
#source("code/1-parse_reference.R")
source("code/0-package.R")



opreferencemena <- read.csv("data/opreferencemena.csv")
#names(opreferencemena)

opreferencemena$plandel <- paste(opreferencemena$operationName, opreferencemena$planningPeriod, sep = " ")

opreferencemena <- opreferencemena[ !(opreferencemena$plandel %in% c('Saudi Arabia 2016', 'United Arab Emirates 2018', 'Tunisia 2018')), ]
#### 
## Pb with parsing some plans -- Need to be fixed

opreferencemena.budg <- opreferencemena

opreference <- opreferencemena.budg[ opreferencemena.budg$planningPeriod %in% c("2016","2017","2018") , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                         "plantype",  "operationName","regionanme", "idregion","idoperation")] 
## Loop through urls and download all plan 

budgetall <- NULL
#names(opreference)

  
xp <- function (doc, tag){
    n <- xpathSApply(doc, tag, xmlValue)
    if (length(n) > 0) 
      # paste multiple values?  BILCOD and probably others..
      paste0(n, collapse="; ") 
    else NA
}


### First getting the reference of the plan
nbudg <- 0
nbudg1 <- 0
nbudg2 <- 0

for(i in 1:nrow(opreference))
{
  # i <- 1
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
  

  z <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/budgetLines/BudgetLine")
  n1 <-length(z)
  notices <-vector("list",n1)
  
  ## Now parsing all budget lines
  for(i in 1:n1)
    {
      z2<-xmlDoc(z[[i]])
      notices[[i]] <- data.frame(
        BudgetLineid     =  xpathSApply(z2, "//BudgetLine", xmlGetAttr, 'ID'),
        scenario         = xp(z2, "//scenario"),
        Type             = xp(z2, "//type"),
        costCenter       = xp(z2, "//costCenter"),
        implementerCode  = xp(z2, "//implementerCode"),
        implementerName  = xp(z2, "//implementerName"),
        accountCode      = xp(z2, "//accountCode"),
        accountName      = xp(z2, "//accountName"),
        quantity         = as.numeric(xp(z2, "//quantity")),
        currency         = xp(z2, "//currency"),
        unitCost         = as.numeric(xp(z2, "//unitCost")),
        localCost        = as.numeric(xp(z2, "//localCost")),
        amount           = as.numeric(xp(z2, "//amount")),
        stringsAsFactors=FALSE)
      free(z2)  
    }
  budgettemp <- as.data.frame(do.call("rbind", notices))
  
  ## If we have only one population group, it will be difficult to join: need to test
  ppgnum <- getNodeSet(plancountryparse, "//ppgs/PPG/name")
  goalnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/name")
  budgetnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/budgetLines/BudgetLine")
  outputnum <- getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/name")
  
  print(paste ("This plan includes ",length(ppgnum) , "population groups, ",length(goalnum), "goals, ",length(outputnum), "outputs and ",length(budgetnum), "budget lines", sep = " ", collapse = NULL) )
  

    getPPGContent =
      function(x)
      {
        goal = xpathSApply(x, "./goals/Goal/name", xmlValue)
        pillar = xpathSApply(x, "./goals/Goal/pillar", xmlValue)
        situationCode = xpathSApply(x, "./goals/Goal/situationCode", xmlValue)
       # outputrfid = xpathSApply(x, "./goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output", xmlGetAttr, 'RFID')
        BudgetLineid      = xpathSApply(x, "./goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/budgetLines/BudgetLine", xmlGetAttr, 'ID')
        cbind(
          Population.Group = xpathSApply(x, "./name", xmlValue),
          Goal             = if(length(goal)) goal else NA,
          pillar             = if(length(pillar)) pillar else NA,
          situationCode             = if(length(situationCode)) situationCode else NA,
        #  outputrfid      = if(length(outputrfid)) outputrfid else NA,
          BudgetLineid      = if(length(BudgetLineid)) BudgetLineid else NA
        )
      }
     temp <-  xpathApply(plancountryparse, "//ppgs/PPG", getPPGContent)
    budgetobj <- as.data.frame(do.call("rbind", temp))
    
    
    ## Restore hierachy with RBM
    getoutContent =
      function(x)
      {
        BudgetLineid      = xpathSApply(x, "./Output/budgetLines/BudgetLine", xmlGetAttr, 'ID')
        cbind(
          outputrfid = xpathSApply(x, "./Output", xmlGetAttr, 'RFID'),
        #  outputrfid      = if(length(outputrfid)) outputrfid else NA,
          BudgetLineid      = if(length(BudgetLineid)) BudgetLineid else NA
        )
      }
    
    
    #BudgetLineidtest <- as.data.frame(xpathSApply(plancountryparse, "//ppgs/PPG//goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/budgetLines/BudgetLine", xmlGetAttr, 'ID'))
   # outputrfidtest <- as.data.frame(xpathSApply(plancountryparse, "//ppgs/PPG//goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output", xmlGetAttr, 'RFID'))
    temp2 <-  xpathApply(plancountryparse, "//ppgs/PPG//goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs", getoutContent)
   # str(temp2)
    
    budgetobj2 <- as.data.frame(do.call("rbind", temp2))
    budgetobj2 <- budgetobj2[!(is.na(budgetobj2$BudgetLineid )), ]
    budgetobj <- join(x=budgetobj, y=budgetobj2,  by="BudgetLineid", type="left")
    
    budgettemp1 <- merge(x=budgettemp, y=budgetobj,  by="BudgetLineid", all.x=TRUE)
    
    
    nbudg <- nbudg + nrow(budgetobj)
    print(paste ("Loaded ", nrow(budgetobj) , "Budget Lines, total of", nbudg , "Budget Lines.", sep = " ", collapse = NULL) )
    
    nbudg2 <- nbudg2 + nrow(budgettemp)
    print(paste ("Loaded ", nrow(budgettemp) , "Budget Lines, total of", nbudg2 , "Budget Lines.", sep = " ", collapse = NULL) )
    
    nbudg1 <- nbudg1 + nrow(budgettemp1)
    print(paste ("Merged ", nrow(budgettemp1) , "Budget Lines, total of", nbudg1 , "Budget Lines.", sep = " ", collapse = NULL) )
 
  budgettemp2 <-cbind(idplan, operationID, planid, planname,  planningPeriod , plantype , operationName , regionanme, idregion, idoperation, budgettemp1,lastRefreshed)
  
  ## Now merging with the rest of the loop
  budgetall <- rbind(budgetall, budgettemp2)
  
  rm(budgettemp2, budgettemp1,budgettemp, budgetobj, budgetobj1,lastRefreshed, goalnum,i,idoperation,
     idplan,idregion,n1,
     notices,operationID,operationName,plancountryid,planid,plancountryparse,planningPeriod,plantype,ppgnum,planname,
     Refresheddate,regionanme,sectnum,temp,temp2,z,z2)
}  

## that's it

#str(budgetall)
data.budget <- budgetall
write.csv(data.budget, "data/budget1.csv")
rm(budgetall, budgettemp)

##########################################################################
## Load Result Based Management framework -- in order to get # of indic
## Join is done on RFID

framework <- read_excel("config/UNHCR-Result-Based-Management.xlsx", sheet = 1) 
#names(framework)
framework<- framework[ !(is.na(framework$Indicator)) ,  ]
framework<- framework[ !(framework$dup2 %in% c('dup')) ,  ]
framework.out <- framework[ !(is.na(framework$Output)),c( "protection.related", "subtype","subtype.obj", "RightsGroup", "Objective", "Output", "outputrfid")]
framework.out1 <- unique(framework.out[ ,c(  "RightsGroup", "Objective", "Output","subtype.obj", "outputrfid")])
framework.out11 <- unique(framework.out[ ,c(  "RightsGroup", "Objective", "Output", "outputrfid")])
framework.out2 <- unique(framework.out[ ,c( "protection.related", "subtype", "RightsGroup", "Objective", "Output", "outputrfid")])

data.budget2 <- join(x=data.budget, y= framework.out1, by="outputrfid", type="left" )

## create a concatenated name for the record
data.budget2$idrecord <- paste(data.budget2$operationName,data.budget2$Goal, data.budget2$Population.Group, sep="-")



## Get cost center name
costCenter <- read.csv("config/costCenter.csv")
data.budget2 <- join(x=data.budget2, y= costCenter, by="costCenter", type="left")

SituationCode <- read.csv("config/SituationCode.csv")
data.budget2 <- join(x=data.budget2, y= SituationCode, by="situationCode", type="left")


#names(data.budget2)
write.csv(data.budget2, "data/budget.csv", row.names = FALSE)

#prop.table(table(data.budget2$CenterName))

#prop.table(table(data.budget2$accountName))

#prop.table(table(data.budget2$implementerName, data.budget2$operationName),2)


### Function that parse all country plan in opreference in order to create a consolidated list of all budget lines
#source("code/1-parse_reference.R")
source("code/0-package.R")



opreferencemena <- read.csv("data/opreferencemena.csv")
#names(opreferencemena)

opreferencemena$plandel <- paste(opreferencemena$operationName, opreferencemena$planningPeriod, sep = " ")

#opreferencemena <- opreferencemena[ !(opreferencemena$plandel %in% c('Saudi Arabia 2016', 'United Arab Emirates 2018', 'Tunisia 2018', 'Western Sahara 2018')), ]
#### 
## Pb with parsing some plans -- Need to be fixed

opreferencemena.budg <- opreferencemena

opreference <- opreferencemena.budg[ opreferencemena.budg$planningPeriod %in% c("2016","2017","2018") , c( "operationID",    "attr" ,"planid" ,"planname", "planningPeriod",
                                                                                                           "plantype",  "operationName","regionanme", "idregion","idoperation")] 
## Loop through urls and download all plan 

objective <- c("")
objectivemsrp <- c("")
outputmsrp <- c("")
output <- c("")


msrpcodeall <- as.data.frame(cbind(objectivemsrp,objective, output,outputmsrp))
#names(opreference)




for(i in 1:nrow(opreference))
{
  # i <- 6
  idplan <- as.character(opreference[ i , 2])
  plancountryid <- paste( "data/plan/Plan_", idplan ,".xml", sep = "")
  ### Get MSRP Code 
  
  plancountryparse <- xmlTreeParse(plancountryid, useInternal = TRUE)
  
  
  objective <- as.data.frame(xpathSApply(plancountryparse, "//ppgs/PPG//goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/objectiveName", xmlValue))
  objectivemsrp <- as.data.frame(xpathSApply(plancountryparse, "//ppgs/PPG//goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/msrpcode", xmlValue))
  objectivelist <- cbind(objective, objectivemsrp)
  names(objectivelist)[2] <- "objectivemsrp"
  names(objectivelist)[1] <- "objective"
  
  
  output <- as.data.frame(xpathSApply(plancountryparse, "//ppgs/PPG//goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/name", xmlValue))
  outputmsrp <- as.data.frame(xpathSApply(plancountryparse, "//ppgs/PPG//goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/msrpcode", xmlValue))
  outputlist <- cbind(output, outputmsrp) 
  
  names(outputlist)[2] <- "outputmsrp"
  names(outputlist)[1] <- "output"
  
  outputlist$objectivemsrp <- substring(outputlist$outputmsrp,1,3)
  
  msrpcode <- merge(objectivelist,outputlist)
  msrpcode$objective <- as.character(msrpcode$objective)
  msrpcode$objectivemsrp <- as.character(msrpcode$objectivemsrp)
  msrpcode$outputmsrp <- as.character(msrpcode$outputmsrp)
  msrpcode$output <- as.character(msrpcode$output)
  
  msrpcodeall <- rbind(msrpcodeall, msrpcode)

}  

msrpcodeall1 <- as.data.frame(unique(msrpcodeall))
msrpcodeall1$objoutput <- paste(msrpcodeall1$objective, msrpcodeall1$output, sep=" - ")
write.csv(msrpcodeall1,"config/msrpcodeall1.csv")

### Function that parse all country plan in opreference in order to create a consolidated list of all indicators
#source("code/1-parse_reference.R")
source("code/0-package.R")

opreferencemena <- read.csv("data/opreferencemena.csv")
#names(opreferencemena)

##################################################3
## Pb with parsing some plans -- Need to be fixed
opreferencemena$plandel <-
  paste(opreferencemena$operationName,
        opreferencemena$planningPeriod,
        sep = " ")

#opreferencemena <- opreferencemena[ !(opreferencemena$plandel %in% c('Saudi Arabia 2016', 'United Arab Emirates 2018', 'Tunisia 2018', 'Lebanon 2018')), ]

opreferencemena.perf <- opreferencemena


opreference <-
  opreferencemena.perf[opreferencemena.perf$planningPeriod %in% c("2016", "2017", "2018","2019") , c(
    "operationID",
    "attr" ,
    "planid" ,
    "planname",
    "planningPeriod",
    "plantype",
    "operationName",
    "regionanme",
    "idregion",
    "idoperation"
  )]

## Loop through urls and download all plan

perfindicatorall <- NULL
#names(opreference)

## Parsing functions for XML
xp <- function (doc, tag) {
  n <- xpathSApply(doc, tag, xmlValue)
  if (length(n) > 0)
    # paste multiple values?  BILCOD and probably others..
    paste0(n, collapse = "; ")
  else
    NA
}
### First getting the reference of the plan
nperf <- 0
nperf1 <- 0
nperf2 <- 0


for (i in 1:nrow(opreference))
{
  #i <- 13
  idplan <- as.character(opreference[i , 2])
  operationID <- as.character(opreference[i , 1])
  planid <- as.character(opreference[i , 3])
  planname <- as.character(opreference[i , 4])
  planningPeriod <- as.character(opreference[i , 5])
  plantype <- as.character(opreference[i , 6])
  operationName <- as.character(opreference[i , 7])
  regionanme <- as.character(opreference[i , 8])
  idregion <- as.character(opreference[i , 9])
  idoperation <- as.character(opreference[i , 10])
  plancountryid <-
    paste("data/plan/Plan_", idplan , ".xml", sep = "")

  plancountryparse <-
    xmlTreeParse(plancountryid, useInternal = TRUE)
  lastRefreshed   <-
    as.data.frame(xpathSApply(plancountryparse, "//Plan/lastRefreshed", xmlValue))
  names(lastRefreshed)[1] <- "lastRefreshed"
  Refresheddate <- as.character(lastRefreshed$lastRefreshed)

  print(
    paste(
      i ,
      "Now loading Operation Plan for ",
      operationName ,
      " for year ",
      planningPeriod ,
      " from ",
      plancountryid,
      " last edited on ",
      Refresheddate,
      sep = " - ",
      collapse = NULL
    )
  )

  z <-
    getNodeSet(
      plancountryparse,
      "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/indicators/Indicator"
    )
  n <- length(z)
  notices <- vector("list", n)
  for (i in 1:n)
  {
    z2 <- xmlDoc(z[[i]])
    notices[[i]] <- data.frame(
      indicatorid     =  xpathSApply(z2, "//Indicator", xmlGetAttr, 'ID'),
      indicatorrfid   =  xpathSApply(z2, "//Indicator", xmlGetAttr, 'RFID'),
      Indicator        = xp(z2, "//name"),
      Type             = xp(z2, "//isPerformance"),
      GSP              = xp(z2, "//isGSP"),
      Standard         = as.numeric(xp(z2, "//standard")),
      OP.Target        = as.numeric(xp(z2, "//impTarget")),
      OL.Target        = as.numeric(xp(z2, "//compTarget")),
      Mid.Year1        = xp(z2, "//midYearValue"),
      Mid.Year2        = as.numeric(xp(z2, "//midYearValue")),
      Year.End1        = xp(z2, "//yearEndValue"),
      Year.End2        = as.numeric(xp(z2, "//yearEndValue")),
      Disag.           = xp(z2, "//disAggrStr"),
      Reporting.Level  = xp(z2, "//reportingLevel"),
      reversal         = xp(z2, "//reversal"),
      stringsAsFactors = FALSE
    )
    free(z2)
  }



  perfindicatortemp <- as.data.frame(do.call("rbind", notices))
  perfindicatortemp$Year.End <- NULL
  perfindicatortemp$Mid.Year <- NULL
  for (i in 1:nrow(perfindicatortemp))
  {
    if (is.na(perfindicatortemp[i, c("Year.End2")])) {
      perfindicatortemp[i, c("Year.End")] <-
        as.numeric(substr(perfindicatortemp[i, c("Year.End1")] , 1, (
          regexpr(
            ";",
            perfindicatortemp[i, c("Year.End1")] ,
            ignore.case = FALSE,
            fixed = TRUE
          )
        ) - 1))
    } else {
      perfindicatortemp[i, c("Year.End")] <-
        perfindicatortemp[i, c("Year.End2")]
    }

    if (is.na(perfindicatortemp[i, c("Mid.Year2")])) {
      perfindicatortemp[i, c("Mid.Year")] <-
        as.numeric(substr(perfindicatortemp[i, c("Mid.Year1")] , 1, (
          regexpr(
            ";",
            perfindicatortemp[i, c("Mid.Year1")] ,
            ignore.case = FALSE,
            fixed = TRUE
          )
        ) - 1))
    } else {
      perfindicatortemp[i, c("Mid.Year")] <-
        perfindicatortemp[i, c("Mid.Year2")]
    }
  }


  ## If we have only one population group, it will be difficult to join: need to test
  ppgnum <- getNodeSet(plancountryparse, "//ppgs/PPG/name")
  goalnum <-
    getNodeSet(plancountryparse, "//ppgs/PPG/goals/Goal/name")
  indicnum <-
    getNodeSet(
      plancountryparse,
      "//ppgs/PPG/goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/indicators/Indicator"
    )

  print(
    paste(
      "There is ",
      length(ppgnum) ,
      "population groups",
      length(goalnum) ,
      "goals and ",
      length(indicnum),
      "performance indicators",
      sep = " ",
      collapse = NULL
    )
  )

  rm(temp)
  getPPGContent =
    function(x)
    {
      goal = xpathSApply(x, "./goals/Goal/name", xmlValue)
      pillar = xpathSApply(x, "./goals/Goal/pillar", xmlValue)
      situationCode = xpathSApply(x, "./goals/Goal/situationCode", xmlValue)
      indicator = xpathSApply(
        x,
        "./goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs/Output/indicators/Indicator",
        xmlGetAttr,
        'ID'
      )
      if (length(indicator) != 0) {
        cbind(
          Population.Group = xpathSApply(x, "./name", xmlValue),
          Goal             = if (length(goal))
            goal
          else
            NA,
          pillar             = if (length(pillar))
            pillar
          else
            NA,
          situationCode             = if (length(situationCode))
            situationCode
          else
            NA,
          indicatorid      = if (length(indicator))
            indicator
          else
            NA
        )
      } else {
        cat("nothing to parse \n")
      }
    }
  temp <-  xpathApply(plancountryparse, "//ppgs/PPG", getPPGContent)
  perfindicatorobj <- as.data.frame(do.call("rbind", temp))

  ## Restore hierachy with RBM

  getoutContent =
    function(x)
    {
      indicator = xpathSApply(x, "./Output/indicators/Indicator", xmlGetAttr, 'ID')
      if (length(indicator) != 0) {
        cbind(
          outputmsrp = xpathSApply(x, "./Output/msrpcode", xmlValue),
          indicatorid      = if (length(indicator))
            indicator
          else
            NA
        )
      } else {
        cat("nothing to parse \n")
      }
    }

  temp2 <-
    xpathApply(
      plancountryparse,
      "//ppgs/PPG//goals/Goal/rightsGroups/RightsGroup/problemObjectives/ProblemObjective/outputs",
      getoutContent
    )
  perfindicatorobj2 <- as.data.frame(do.call("rbind", temp2))
  perfindicatorobj2 <-
    perfindicatorobj2[!(is.na(perfindicatorobj2$sectionid)),]
  perfindicatorobj <-
    join(x = perfindicatorobj,
         y = perfindicatorobj2,
         by = "indicatorid",
         type = "left")

  perfindicatortemp1 <-
    merge (x = perfindicatortemp,
           y = perfindicatorobj,
           by = "indicatorid",
           all.x = TRUE)


  nperf <- nperf + nrow(perfindicatorobj)

  print(
    paste(
      "Loaded ",
      nrow(perfindicatorobj) ,
      "performance indicator Lines, total of",
      nperf ,
      "performance indicator Lines.",
      sep = " ",
      collapse = NULL
    )
  )

  nperf2 <- nperf2 + nrow(perfindicatortemp)
  print(
    paste(
      "Loaded ",
      nrow(perfindicatortemp) ,
      "prformance indicator Lines, total of",
      nperf2 ,
      "prformance indicator Lines.",
      sep = " ",
      collapse = NULL
    )
  )

  nperf1 <- nperf1 + nrow(perfindicatortemp1)
  print(
    paste(
      "Merged ",
      nrow(perfindicatortemp1) ,
      "prformance indicator Lines, total of",
      nperf1 ,
      "prformance indicator Lines.",
      sep = " ",
      collapse = NULL
    )
  )



  perfindicatortemp2 <-
    cbind(
      idplan,
      operationID,
      planid,
      planname,
      planningPeriod ,
      plantype ,
      operationName ,
      regionanme,
      idregion,
      idoperation,
      perfindicatortemp1,
      lastRefreshed
    )


  perfindicatorall <- rbind(perfindicatorall, perfindicatortemp2)


  rm(
    perfindicatortemp2,
    perfindicatortemp1,
    perfindicatortemp,
    perfindicatorobj,
    perfindicatorobj1,
    lastRefreshed,
    goalnum,
    i,
    idoperation,
    idplan,
    idregion,
    n1,
    notices,
    operationID,
    operationName,
    plancountryid,
    planid,
    plancountryparse,
    planningPeriod,
    plantype,
    ppgnum,
    planname,
    Refresheddate,
    regionanme,
    sectnum,
    temp,
    temp2,
    z,
    z2
  )

}

## that's it


#str(perfindicatorall)

########################################
## Difference between baseline & review
data <- perfindicatorall
rm(perfindicatorall)


rm(api,
   apihead,
   bin,
   con,
   passw,
   upw,
   urlend,
   urlendsp2,
   urlendsp1,
   user,
   url)
rm(
  i,
  idoperation,
  idplan,
  idregion,
  n,
  nperf,
  nperf1,
  nperf2,
  notices,
  operationID,
  operationName,
  plancountryid,
  plancountryparse,
  planid,
  planname,
  planname,
  planningPeriod,
  plantype,
  regionanme,
  z,
  z2,
  xp,
  temp,
  indicnum,
  plan,
  plandctr,
  plandescr,
  ppgnum,
  perfindicatorobj2,
  opreferencemena, opreferencemena.perf
)


##########################################################################
# str(data)
data$Standard <- as.numeric(data$Standard)
data$OL.Target <- as.numeric(data$OL.Target)
data$OP.Target <- as.numeric(data$OP.Target)
data$Mid.Year <- as.numeric(data$Mid.Year)
data$Year.End <- as.numeric(data$Year.End)


## Difference between OL target & review
data$mid2targetol <-
  ifelse(data$OL.Target == 0,
         0,
         (data$OL.Target - data$Mid.Year) / data$OL.Target)
data$mid2targetol.sit <-
  as.factor(findCols(
    classIntervals(
      data$mid2targetol,
      n = 5,
      style = "fixed",
      fixedBreaks = c(-5000 , 0, 0.3, 0.7, 1, 5000)
    )
  ))
data$mid2targetol.sit <-
  revalue(
    data$mid2targetol.sit,
    c(
      `1` = "Underplan",
      `2` = "Underperf",
      `3` = "Normal",
      `4` = "Overperf",
      `5` = "Overplan"
    )
  )
data$mid2targetol.sit  <-
  factor(
    data$mid2targetol.sit,
    levels = c("Underplan", "Underperf", "Normal", "Overperf", "Overplan")
  )


## Difference between OL target & endyear
data$year2targetol <-
  ifelse(data$OL.Target == 0,
         0,
         (data$OL.Target - data$Year.End) / data$OL.Target)
data$year2targetol.sit <-
  as.factor(findCols(
    classIntervals(
      data$mid2targetol,
      n = 5,
      style = "fixed",
      fixedBreaks = c(-5000 , 0, 0.5, 0.9, 1, 5000)
    )
  ))
data$year2targetol.sit <-
  revalue(
    data$year2targetol.sit,
    c(
      `1` = "Underplan",
      `2` = "Underperf",
      `3` = "Normal",
      `4` = "Overperf",
      `5` = "Overplan"
    )
  )
data$year2targetol.sit  <-
  factor(
    data$year2targetol.sit,
    levels = c("Underplan", "Underperf", "Normal", "Overperf", "Overplan")
  )



## Difference between OP target & review
data$mid2targetop <-
  ifelse(data$OP.Target == 0,
         0,
         (data$OP.Target - data$Mid.Year) / data$OP.Target)
data$mid2targetop.sit <-
  as.factor(findCols(
    classIntervals(
      data$mid2targetop,
      n = 5,
      style = "fixed",
      fixedBreaks = c(-5000 , 0, 0.3, 0.7, 1, 5000)
    )
  ))
data$mid2targetop.sit <-
  revalue(
    data$mid2targetop.sit,
    c(
      `1` = "Underplan",
      `2` = "Underperf",
      `3` = "Normal",
      `4` = "Overperf",
      `5` = "Overplan"
    )
  )
data$mid2targetop.sit  <-
  factor(
    data$mid2targetop.sit,
    levels = c("Underplan", "Underperf", "Normal", "Overperf", "Overplan")
  )


## Difference between OP target & endyear
data$year2targetop <-
  ifelse(data$OP.Target == 0,
         0,
         (data$OP.Target - data$Year.End) / data$OP.Target)
data$year2targetop.sit <-
  as.factor(findCols(
    classIntervals(
      data$mid2targetop,
      n = 5,
      style = "fixed",
      fixedBreaks = c(-5000 , 0, 0.5, 0.9, 1, 5000)
    )
  ))
data$year2targetop.sit <-
  revalue(
    data$year2targetop.sit,
    c(
      `1` = "Underplan",
      `2` = "Underperf",
      `3` = "Normal",
      `4` = "Overperf",
      `5` = "Overplan"
    )
  )
data$year2targetop.sit  <-
  factor(
    data$year2targetop.sit,
    levels = c("Underplan", "Underperf", "Normal", "Overperf", "Overplan")
  )


data$feedback1[is.na(data$OL.Target)] <- "No OL Target"
data$feedback2[is.na(data$OP.Target)] <- "No OP Target"
data$feedback3[is.na(data$Mid.Year)] <- "No Mid Year report"
data$feedback4[is.na(data$Year.End)] <- "No End Year report"

data.performance <- data
##########################################################################
## Load Result Based Management framework -- in order to get # of indic
## Join is done on RFID

framework <-
  read_excel("config/UNHCR-Result-Based-Management2.xlsx", sheet = 1)
#names(framework)
framework <- framework[!(is.na(framework$Indicator)) ,]
framework <- framework[!(framework$dup2 %in% c('dup')) ,]

framework <-
  framework[, c(
    "rid" ,
    "oid" ,
    "iid" ,
    "numindic",
    "indicatorrfid",
    "Indicator",
    "protection.related",
    "subtype",
    "subtype.obj",
    "RightsGroup",
    "Objective",
    "Source"
  )]

data2 <-
  join(x = data,
       y = framework,
       by = "indicatorrfid",
       type = "left")


SituationCode <- read.csv("config/SituationCode.csv")
data2 <-
  join(x = data2,
       y = SituationCode,
       by = "situationCode",
       type = "left")


## Seeems we need to remove outputrfid
#data <- data[ , -c("outputrfid")]
data$outputrfid <- NULL

data.performance <- as.data.frame(data)
str(data.performance)

write.csv(data.performance, "data/performance.csv", row.names = FALSE)


#str(data.performance)
write.csv(data.performance, "data/performance.csv", row.names = FALSE)

rm(data, SituationCode,framework, opreference, data2, getoutContent, getPPGContent)
#rm(data.performance)

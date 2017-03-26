source("code/0-package.R")

### Producing data frame from FOCUS XML structure
### Find ways within the API

## External file for easier reference 
source("perso/api_get_perso.R")

upw = paste(user, passw, sep = ":")

bin <- getBinaryURL(apihead, userpwd = upw, httpauth = 1L, ssl.verifypeer=FALSE  )
con <- file("data/OperationHeaders.zip", open = "wb")
writeBin(bin, con)
close(con)
unzip ("data/OperationHeaders.zip", overwrite = TRUE, exdir = "./data")

# Now parsing the file

OperationList <- xmlTreeParse("data/OperationHeaders.xml", useInternal = TRUE)
#class(OperationList)


################################################
#### Data frame of all plan header

plan.attr <- data.frame(xpathSApply(OperationList, "//subgroups/OperationGroup/subgroups/OperationGroup/operations/OperationHeader/plans/PlanHeader", xmlGetAttr, 'ID'))
plan.planid <- data.frame(sapply(OperationList["//subgroups/OperationGroup/subgroups/OperationGroup/operations/OperationHeader/plans/PlanHeader/planID"], xmlValue))
plan.planname <- data.frame(sapply(OperationList["//subgroups/OperationGroup/subgroups/OperationGroup/operations/OperationHeader/plans/PlanHeader/name"], xmlValue))
plan.planningPeriod <- data.frame(sapply(OperationList["//subgroups/OperationGroup/subgroups/OperationGroup/operations/OperationHeader/plans/PlanHeader/planningPeriod"], xmlValue))
plan.plantype <- data.frame(sapply(OperationList["//subgroups/OperationGroup/subgroups/OperationGroup/operations/OperationHeader/plans/PlanHeader/type"], xmlValue))
plan <- cbind(plan.attr, plan.planid, plan.planname,  plan.planningPeriod , plan.plantype)

names(plan)[1] <- "attr"
names(plan)[2] <- "planid"
names(plan)[3] <- "planname"
names(plan)[4] <- "planningPeriod"
names(plan)[5] <- "plantype"

rm(plan.attr, plan.planid, plan.planname,  plan.planningPeriod , plan.plantype)

#write.csv(plan,"out/plan.csv")

L <- xpathSApply(OperationList, "//OperationHeader", function(x) 
  cbind(operationID = xmlValue(x[["operationID"]]),
        #operationName = xmlValue(x[["name"]]),
       # plan = xpathSApply(x, "plans/PlanHeader/name", xmlValue),
        attr = xpathSApply(x, "plans/PlanHeader", xmlGetAttr, 'ID') #,
       # planid = xpathSApply(x, "plans/PlanHeader/planID", xmlValue),
       # planningPeriod = xpathSApply(x, "plans/PlanHeader/planningPeriod", xmlValue),
       # plantype = xpathSApply(x, "plans/PlanHeader/type", xmlValue)
        
  )
)

planoperation <- as.data.frame(do.call("rbind", L))

str(planoperation)
str(plan)
planoperation1 <- merge(x=planoperation, y=plan, by="attr" #, all.x=TRUE
                        )

#write.csv(planoperation1,"out/operationplan.csv")


operation <- data.frame(
  idoperation = xpathSApply(OperationList, "//subgroups/OperationGroup/subgroups/OperationGroup/operations/OperationHeader", xmlGetAttr, 'ID'),
  operationID=sapply(OperationList["//subgroups/OperationGroup/subgroups/OperationGroup/operations/OperationHeader/operationID"], xmlValue),
  operationName =sapply(OperationList["//subgroups/OperationGroup/subgroups/OperationGroup/operations/OperationHeader/name"], xmlValue)
)
#write.csv(operation,"data/out/operation.csv")

region <- data.frame(
  idregion = xpathSApply(OperationList, "//subgroups/OperationGroup/subgroups/OperationGroup", xmlGetAttr, 'ID'),
  regionanme =sapply(OperationList["//subgroups/OperationGroup/subgroups/OperationGroup/name"], xmlValue)
)


L2 <- xpathSApply(OperationList, "//subgroups/OperationGroup/subgroups/OperationGroup", function(x) 
  cbind(regionanme = xmlValue(x[["name"]]),
        operationName = xpathSApply(x, "operations/OperationHeader/name", xmlValue)
  )
)
regionop <- do.call("rbind", L2)

regionop1 <- merge(regionop,region)
regionop2 <- merge(regionop1,operation, by="operationName")


opreference <- merge(planoperation1,regionop2, by="operationID")
write.csv(opreference,"data/opreference.csv")

#names(opreference)
#levels(opreference$plantype)
#levels(opreference$planningPeriod)
#levels(opreference$regionanme)
#levels(opreference$planname)

## Define the reference to be used

opreference.mena <- opreference[ (opreference$regionanme %in% c('Middle East', 'North Africa')) & 
                                (opreference$planningPeriod %in% c('2013', '2014', '2015','2016','2017','2018')) &
                                 (opreference$planname=='Operations Plan') ,  ]

opreference.mena <- opreference.mena[ ! (opreference.mena$operationName %in% c('Regional Activities in Middle East & North Africa (MENA)','Syria Regional Refugee Coordination Office in Amman')) ,  ]

write.csv(opreference.mena,"data/opreferencemena.csv", row.names=FALSE)


opreference.ope <- opreference[ (opreference$regionanme %in% c('Central Africa and the Great Lakes', 'Central Asia',
                                                               'East and Horn of Africa', 'East Asia and the Pacific',
                                                               'Eastern Europe', 'Latin America',
                                                               'Middle East', 'North Africa',
                                                               'North America and the Caribbean', 'Northern, Western, Central and Southern Europe',
                                                               'South-Eastern Europe', 'South-West Asia',
                                                               'South Asia', 'South East Asia',
                                                               'Southern Africa', 'West Africa' )) & 
                                   (opreference$planningPeriod %in% c('2013', '2014', '2015')) &
                                   (opreference$planname=='Operations Plan') ,  ]


### We need to exclude certains operations for analysis purpose
#levels(opreference.ope$operationName)
opreference.ope <- opreference.ope[ !(opreference.ope$operationName %in% c('Syria Regional Refugee Coordination Office in Amman',
                                                               'Regional Activities in Middle East & North Africa (MENA)',
                                                               'Regional Activities in Africa', 'Regional Activities in Asia',
                                                               'Regional Activities in Americas', 'Regional Activities in Europe',
                                                               'Regional Legal Unit Costa Rica',
                                                               'Reg Supp Hub Nairobi',
                                                               'RR Brussels (RRWE)',
                                                               'RR Budapest (RRCE)',
                                                               'Addis Ababa RAUECA',
                                                               'RO Almaty',
                                                               'RO Canberra',
                                                               'RO Dakar',
                                                               'RO Kyiv',
                                                               'RO Panama',
                                                               'RO Pretoria',
                                                               'RO Rome',
                                                               'RO Sarajevo',
                                                               'RO Stockholm',
                                                               'RO Tbilisi')) ,  ]



write.csv(opreference.ope,"data/opreferenceope.csv", row.names=FALSE)

rm(regionop,regionop1,regionop2,planoperation,planoperation1,operation,plan,region, L, L2, Operation, OperationList)

write.csv(unique(opreference$operationName),"data/listop.csv", row.names=FALSE)

rm(api,apihead,bin,con,passw,upw,urlend,urlendsp2,user)
#rm(opreference.ope1)

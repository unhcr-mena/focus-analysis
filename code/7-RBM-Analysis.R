
##########################################################
## loading all elements 
#########################################################


#source("code/3-Performance_Indicator.R")
#source("code/4-Impact_Indicator.R")
#source("code/5-HumanRessources.R")
#source("code/6-Budget.R")

data.performance <- read.csv("data/performance.csv")
data.impact <- read.csv("data/impact.csv")
data.personnel <- read.csv("data/personnel.csv")
#data.budget <- read.csv("data/budget.csv")

## We can merge rbind Indicators : performance & Impact 


## We can Cbind Indicators & Budget

## We need to define area to Bind Budget & Personnel


########################################################################################################
## Now loading the Result Based Framework with columns that indicates protection related indicators
########################################################################################################

### RBM with additional analytical column
rbmreference <- read.csv("data/All-RBM-withCat.csv", na.strings="")
names(rbmreference)


### Add strategies as defined within http://axis.unhcr.org
strategies <- read.csv("data/Strategies_Indicators.csv", na.strings="")
names(strategies)
library(reshape2)
strategy <- melt(strategies, id=c(1), measure=c(2)) 
strategy.indic <- dcast(strategy, value ~ name  )
str(strategy.indic)

names(strategy.indic)[2] <- "Community.mobilization.Strategy" 
names(strategy.indic)[3] <- "Disability.Strategy"
names(strategy.indic)[4] <- "Child.Protection.Strategy"                                  
names(strategy.indic)[5] <- "Education.Strategy"
names(strategy.indic)[6] <- "Livelihoods.Strategy"  
names(strategy.indic)[7] <- "Energy.Strategy"
names(strategy.indic)[8] <- "SGBV.Strategy" 

rbmreference <- merge(x=rbmreference, y=strategy.indic, by.x="indicatorrfid", by.y="value", all.x=TRUE)

### Cast the filed from strategies

rbmreference$Community.mobilization.Strategy[is.na(rbmreference$Community.mobilization.Strategy)] <-  "--" 
rbmreference$Community.mobilization.Strategy[rbmreference$Community.mobilization.Strategy==0] <-  "--" 
rbmreference$Community.mobilization.Strategy[rbmreference$Community.mobilization.Strategy==1] <-  "Selected" 
rbmreference$Community.mobilization.Strategy[rbmreference$Community.mobilization.Strategy==2] <-  "Selected" 
rbmreference$Community.mobilization.Strategy[rbmreference$Community.mobilization.Strategy==3] <-  "Selected" 
rbmreference$Community.mobilization.Strategy[rbmreference$Community.mobilization.Strategy==4] <-  "Selected" 


rbmreference$Disability.Strategy[is.na(rbmreference$Disability.Strategy)] <-  "--" 
rbmreference$Disability.Strategy[rbmreference$Disability.Strategy==0] <-  "--" 
rbmreference$Disability.Strategy[rbmreference$Disability.Strategy==1] <-  "Selected" 
rbmreference$Disability.Strategy[rbmreference$Disability.Strategy==2] <-  "Selected" 
rbmreference$Disability.Strategy[rbmreference$Disability.Strategy==3] <-  "Selected" 
rbmreference$Disability.Strategy[rbmreference$Disability.Strategy==4] <-  "Selected" 


rbmreference$Child.Protection.Strategy[is.na(rbmreference$Child.Protection.Strategy)] <-  "--" 
rbmreference$Child.Protection.Strategy[rbmreference$Child.Protection.Strategy==0] <-  "--" 
rbmreference$Child.Protection.Strategy[rbmreference$Child.Protection.Strategy==1] <-  "Selected" 
rbmreference$Child.Protection.Strategy[rbmreference$Child.Protection.Strategy==2] <-  "Selected"
rbmreference$Child.Protection.Strategy[rbmreference$Child.Protection.Strategy==3] <-  "Selected"
rbmreference$Child.Protection.Strategy[rbmreference$Child.Protection.Strategy==4] <-  "Selected" 


rbmreference$Education.Strategy[is.na(rbmreference$Education.Strategy)] <-  "--" 
rbmreference$Education.Strategy[rbmreference$Education.Strategy==0] <-  "--" 
rbmreference$Education.Strategy[rbmreference$Education.Strategy==1] <-  "Selected" 
rbmreference$Education.Strategy[rbmreference$Education.Strategy==2] <-  "Selected" 
rbmreference$Education.Strategy[rbmreference$Education.Strategy==3] <-  "Selected" 
rbmreference$Education.Strategy[rbmreference$Education.Strategy==4] <-  "Selected" 


rbmreference$Livelihoods.Strategy[is.na(rbmreference$Livelihoods.Strategy)] <-  "--" 
rbmreference$Livelihoods.Strategy[rbmreference$Livelihoods.Strategy==0] <-  "--" 
rbmreference$Livelihoods.Strategy[rbmreference$Livelihoods.Strategy==1] <-  "Selected" 
rbmreference$Livelihoods.Strategy[rbmreference$Livelihoods.Strategy==2] <-  "Selected" 
rbmreference$Livelihoods.Strategy[rbmreference$Livelihoods.Strategy==3] <-  "Selected" 
rbmreference$Livelihoods.Strategy[rbmreference$Livelihoods.Strategy==4] <-  "Selected" 

rbmreference$Energy.Strategy[is.na(rbmreference$Energy.Strategy)] <-  "--" 
rbmreference$Energy.Strategy[rbmreference$Energy.Strategy==0] <-  "--" 
rbmreference$Energy.Strategy[rbmreference$Energy.Strategy==1] <-  "Selected" 
rbmreference$Energy.Strategy[rbmreference$Energy.Strategy==2] <-  "Selected"  
rbmreference$Energy.Strategy[rbmreference$Energy.Strategy==3] <-  "Selected"  
rbmreference$Energy.Strategy[rbmreference$Energy.Strategy==4] <-  "Selected" 

rbmreference$SGBV.Strategy[is.na(rbmreference$SGBV.Strategy)] <-  "--" 
rbmreference$SGBV.Strategy[rbmreference$SGBV.Strategy==0] <-  "--" 
rbmreference$SGBV.Strategy[rbmreference$SGBV.Strategy==1] <-  "Selected" 
rbmreference$SGBV.Strategy[rbmreference$SGBV.Strategy==2] <-  "Selected" 
rbmreference$SGBV.Strategy[rbmreference$SGBV.Strategy==3] <-  "Selected" 
rbmreference$SGBV.Strategy[rbmreference$SGBV.Strategy==4] <-  "Selected" 



########### linking data with RBM reference
#names(data.performance)
data.performance1 <- data.performance[ , !(colnames(data.performance) %in% c("Indicator"))]
data.performance.rbm <- merge (x=data.performance1, y=rbmreference, by=c("indicatorrfid"), all.x=TRUE)
data.impact1 <- data.impact[ , !(colnames(data.impact) %in% c("Indicator"))]
data.impact.rbm <- merge (x=data.impact1, y=rbmreference, by=c("indicatorrfid"), all.x=TRUE)


write.csv(data.impact.rbm,"data/dataimpactrbm.csv", na = "", row.names = FALSE)
write.csv(data.performance.rbm,"data/dataperformancerbm.csv", na = "", row.names = FALSE)

### Writing in Excel
#library(xlsx)
#write.xlsx(data.impact.rbm, "data/out/focus-plan-analysis.xlsx", sheetName="Impact-Indicator-n", append=TRUE) 
#write.xlsx(data.performance.rbm, "data/out/focus-plan-analysis.xlsx", sheetName="Performance-Indicator-n", append=TRUE) 


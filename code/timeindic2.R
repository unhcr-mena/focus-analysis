################################################################################################
### STEP 1: Subset data 
#names(focus.performance)
focus.performance.indic <- focus.performance[focus.performance$operationName==ctrname , c("operationName","indicatorid","Objective","Indicator","planningPeriod","subtype",
                                                                                          "Population.Group","indicatorrfid","Standard",
                                                                           #"Baseline",
                                                                           "Mid.Year", "Year.End",
                                                                           ##"thresholdRed", "thresholdGreen" , 
                                                                           "OP.Target",  "OL.Target"  ) ]

#focusimpact.indic.t <- dcast(formloc,  thresholdRed + thresholdGreen + OP.Target +OL.Target ~ indicatorid + indicatorrfid +Indicator + planningPeriod , value.var = "IDPs_In_Baladiya_HH", fun=sum )


################################################################################################
### STEP 2: Reshape data by time

focus.performance.indic.t <- melt(focus.performance.indic, id=c("indicatorid", "Indicator","planningPeriod","subtype","Population.Group","operationName"))
#levels(focus.performance.indic.t$variable)
#focusimpact.indic.t <- focusimpact.indic.t[order("Indicator"), c("indicatorid", "Indicator","planningPeriod","variable","value")]

focus.performance.indic.t$moment <- paste(focus.performance.indic.t$planningPeriod, focus.performance.indic.t$variable, sep="-")
focus.performance.indic.t$planningPeriod <- as.character(focus.performance.indic.t$planningPeriod)

focus.performance.indic.t <- focus.performance.indic.t[focus.performance.indic.t$operationName==ctrname, ]
################################################################################################
### STEP 3: Build time line table
## list indicators & ppg for the country
#indic.country <- as.data.frame(unique(focus.performance.indic[ , c("Indicator", "indicatorrfid","subtype")]))

indic.country <- as.data.frame(unique(focus.performance.indic[ , c("Objective","Indicator", "indicatorrfid","subtype")]))
ppg.country <- as.data.frame(unique(focus.performance.indic[ ,  c("Population.Group","operationName")]))
names(ppg.country)[1] <- "ppg.country"
names(ppg.country)[2] <- "operationName"


indic.country.date = data.frame(
  date = c( "2014-07-01", "2014-12-15",
            "2015-07-01", "2015-12-15",
            "2016-07-01", "2016-12-15",
            "2017-07-01", "2017-12-15",
            "2018-07-01", "2018-12-15"),
  label = c( "2014\n midyear", "2014\n endyear",
             "2015\n midyear", "2015\n endyear",
             "2016\n midyear", "2016\n endyear",
             "2017\n midyear", "2017\n endyear",
             "2018\n midyear", "2018\n endyear"),
  phase = c( "Mid.Year", "Year.End",
             "Mid.Year", "Year.End",
             "Mid.Year", "Year.End",
             "Mid.Year", "Year.End",
             "Mid.Year", "Year.End"),
  year = c( "2014", "2014",
            "2015", "2015",
            "2016", "2016",
            "2017", "2017",
            "2018", "2018"))

indic.country.date3 <- merge(indic.country.date, indic.country)
indic.country.date3 <- merge(indic.country.date3, ppg.country)

## Create order in levels for label
indic.country.date3$label <- factor(indic.country.date3$label, c("2014\n midyear", "2014\n endyear",
                                                                  "2015\n midyear", "2015\n endyear",
                                                                  "2016\n midyear", "2016\n endyear",
                                                                  "2017\n midyear", "2017\n endyear",
                                                                  "2018\n midyear", "2018\n endyear"))

##Create fields for values
indic.country.date3$year <- as.character(indic.country.date3$year)
indic.country.date3$record <- as.numeric(NA)
indic.country.date3$Standard <- as.numeric(NA)
indic.country.date3$OP.Target <- as.numeric(NA)
indic.country.date3$OL.Target <- as.numeric(NA)

#indic.country.date3 <- indic.country.date3

#names(indic.country.date3)
#str(indic.country.date3)
#str(focus.performance.indic.t)
#names(focus.performance.indic.t)

################################################################################################
### STEP 4: Fill time line table
## now we fill the 5 values -- Actual Record  & reference - Standard ,OP.target, OL.Target
n <- nrow(indic.country.date3)
#print(n)
for (i in 1:n ) {
  #i <- 1
  rm(date1,phase1,Indicator1,ppgcountry1,record,Standard,OP.Target,OL.Target)
  date1 <- as.character(indic.country.date3[i, 4 ])
  phase1 <-  as.character(indic.country.date3[i, 3 ])
  Indicator1 <-  as.character(indic.country.date3[i, 6 ])
  ppgcountry1 <-  as.character(indic.country.date3[i, 9])
  country1 <-  as.character(indic.country.date3[i, 10])
  
  record <- subset(focus.performance.indic.t , Population.Group==ppgcountry1 &                                  
                     operationName==country1 &                                          
                     planningPeriod==date1 &                                  
                     variable==phase1 &                                  
                     Indicator==Indicator1  )["value"]
  
  Standard <- subset(focus.performance.indic.t , Population.Group==ppgcountry1 &                                  
                           operationName==country1 &                                         
                           planningPeriod==date1 &                                
                           variable=="Standard" &                                  
                           Indicator==Indicator1 )["value"] 
  

  
  OP.Target <- subset(focus.performance.indic.t ,Population.Group==ppgcountry1 &                                  
                        operationName==country1 &                                         
                        planningPeriod==date1 &                                
                        variable=="OP.Target" &  
                        Indicator==Indicator1)["value"] 
  
  OL.Target <- subset(focus.performance.indic.t , Population.Group==ppgcountry1 &                                    
                        operationName==country1 &                                       
                        planningPeriod==date1 &                                
                        variable=="OL.Target" & 
                        Indicator==Indicator1 )["value"]
  
  ### Now inset the values into the timeline
  # cat(paste("looking at ",i,"\n ", sep="  "))
  
  
  # names(indic.country.date3)
  #if(is.na(record,numeric(0))){
  #if(identical(record,0-length)){
  if(nrow(record)==0){
    #  cat('No matched values\n')
  }else{
    #  cat(paste("Record for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), record,"\n ", sep="\n "))
    indic.country.date3[i, 11 ] <- record
  }
  
  if(nrow(Standard)==0){
    #  cat('No matched values\n')
  }else{
    # cat(paste("Standard for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), Standard,"\n ", sep="\n"))
    indic.country.date3[i, 12 ] <- Standard
  }
  
  if(nrow(OP.Target)==0){
    # cat('No matched values\n')
  }else{
    # cat(paste("OP.Target for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), OP.Target,"\n ", sep="\n"))
    indic.country.date3[i, 13 ] <- OP.Target
  }
  
  if(nrow(OL.Target)==0){
    # cat('No matched values\n')
  }else{
    #  cat(paste("OL.Target for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), OL.Target,"\n ", sep="\n"))
    indic.country.date3[i, 14 ] <- OL.Target
  }
}


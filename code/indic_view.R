
ctrname <- "Jordan"
yearreport <- "2017"


focus.impact <- read.csv("data/impact.csv")
## load all packages
source("code/0-package.R")
################################################################################################
### OBJECTIVE: Create a table for impact indicator that will display timeline analysis --
################################################################################################

################################################################################################
### STEP 1: Subset data
#names(focus.impact)
focus.impact.indic <- focus.impact[focus.impact$operationName==ctrname , c("operationName","indicatorid", "Indicator","planningPeriod","subtype","Population.Group","indicatorrfid",
                                          "Baseline","Mid.Year", "Year.End",
                                          "thresholdRed", "thresholdGreen" , "OP.Target",  "OL.Target"  ) ]

#focusimpact.indic.t <- dcast(formloc,  thresholdRed + thresholdGreen + OP.Target +OL.Target ~ indicatorid + indicatorrfid +Indicator + planningPeriod , value.var = "IDPs_In_Baladiya_HH", fun=sum )


################################################################################################
### STEP 2: Reshape data by time

focus.impact.indic.t <- melt(focus.impact.indic, id=c("indicatorid", "Indicator","planningPeriod","subtype","Population.Group","operationName"))
#levels(focus.impact.indic.t$variable)
#focusimpact.indic.t <- focusimpact.indic.t[order("Indicator"), c("indicatorid", "Indicator","planningPeriod","variable","value")]

focus.impact.indic.t$moment <- paste(focus.impact.indic.t$planningPeriod, focus.impact.indic.t$variable, sep="-")
focus.impact.indic.t$planningPeriod <- as.character(focus.impact.indic.t$planningPeriod)

focus.impact.indic.t <- focus.impact.indic.t[focus.impact.indic.t$operationName==ctrname, ]
################################################################################################
### STEP 3: Build time line table
## list indicators & ppg for the country
#indic.country <- as.data.frame(unique(focus.impact.indic[ , c("Indicator", "indicatorrfid","subtype")]))

indic.country <- as.data.frame(unique(focus.impact.indic[ , c("Indicator", "indicatorrfid","subtype")]))
ppg.country <- as.data.frame(unique(focus.impact.indic[ ,  c("Population.Group","operationName")]))
names(ppg.country)[1] <- "ppg.country"
names(ppg.country)[2] <- "operationName"


indic.country.date = data.frame(
  date = c("2014-01-02", "2014-07-01", "2014-12-15",
           "2015-01-02", "2015-07-01", "2015-12-15",
           "2016-01-02", "2016-07-01", "2016-12-15",
           "2017-01-02", "2017-07-01", "2017-12-15",
           "2018-01-02", "2018-07-01", "2018-12-15"),
  label = c("2014-baseline", "2014-midyear", "2014-endyear",
           "2015-baseline", "2015-midyear", "2015-endyear",
           "2016-baseline", "2016-midyear", "2016-endyear",
           "2017-baseline", "2017-midyear", "2017-endyear",
           "2018-baseline", "2018-midyear", "2018-endyear"),
  phase = c("Baseline", "Mid.Year", "Year.End",
            "Baseline", "Mid.Year", "Year.End",
            "Baseline", "Mid.Year", "Year.End",
            "Baseline", "Mid.Year", "Year.End",
            "Baseline", "Mid.Year", "Year.End"),
  year = c("2014", "2014", "2014",
           "2015", "2015", "2015",
           "2016", "2016", "2016",
           "2017", "2017", "2017",
           "2018", "2018", "2018"))

indic.country.date2 <- merge(indic.country.date, indic.country)
indic.country.date2 <- merge(indic.country.date2, ppg.country)

## Create order in levels for label
indic.country.date2$label <- factor(indic.country.date2$label, c("2014-baseline", "2014-midyear", "2014-endyear",
                                                     "2015-baseline", "2015-midyear", "2015-endyear",
                                                     "2016-baseline", "2016-midyear", "2016-endyear",
                                                     "2017-baseline", "2017-midyear", "2017-endyear",
                                                     "2018-baseline", "2018-midyear", "2018-endyear"))

##Create fields for values
indic.country.date2$year <- as.character(indic.country.date2$year)
indic.country.date2$record <- as.numeric(NA)
indic.country.date2$thresholdRed <- as.numeric(NA)
indic.country.date2$thresholdGreen <- as.numeric(NA)
indic.country.date2$OP.Target <- as.numeric(NA)
indic.country.date2$OL.Target <- as.numeric(NA)

#indic.country.date3 <- indic.country.date2

#names(indic.country.date2)
#str(indic.country.date2)
#str(focus.impact.indic.t)
#names(focus.impact.indic.t)

################################################################################################
### STEP 4: Fill time line table
## now we fill the 5 values -- Actual Record  & reference - thresholdgreen, thresholdred,OP.target, OL.Target
n <- nrow(indic.country.date2)
#print(n)
for (i in 1:n ) {
  #i <- 1
  rm(date1,phase1,Indicator1,ppgcountry1,record,thresholdGreen,thresholdRed,OP.Target,OL.Target)
  date1 <- as.character(indic.country.date2[i, 4 ])
  phase1 <-  as.character(indic.country.date2[i, 3 ])
  Indicator1 <-  as.character(indic.country.date2[i, 5 ])
  ppgcountry1 <-  as.character(indic.country.date2[i, 8])
  country1 <-  as.character(indic.country.date2[i, 9])

  record <- subset(focus.impact.indic.t , Population.Group == ppgcountry1 &
                                          operationName == country1 &
                                          planningPeriod == date1 &
                                          variable == phase1 &
                                          Indicator == Indicator1  )["value"]

  thresholdRed <- subset(focus.impact.indic.t , Population.Group==ppgcountry1 &
                                                operationName==country1 &
                                                planningPeriod==date1 &
                                                variable=="thresholdRed" &
                                                Indicator==Indicator1 )["value"]

  thresholdGreen <- subset(focus.impact.indic.t , Population.Group==ppgcountry1 &
                                                  operationName==country1 &
                                                  planningPeriod==date1 &
                                                  variable=="thresholdGreen" &
                                                  Indicator==Indicator1  )["value"]

  OP.Target <- subset(focus.impact.indic.t ,Population.Group==ppgcountry1 &
                                            operationName==country1 &
                                            planningPeriod==date1 &
                                            variable=="OP.Target" &
                                            Indicator==Indicator1)["value"]

  OL.Target <- subset(focus.impact.indic.t , Population.Group==ppgcountry1 &
                                             operationName==country1 &
                                             planningPeriod==date1 &
                                             variable=="OL.Target" &
                                             Indicator==Indicator1 )["value"]

  ### Now inset the values into the timeline
 # cat(paste("looking at ",i,"\n ", sep="  "))

  #if(is.na(record,numeric(0))){
  #if(identical(record,0-length)){
 if(nrow(record)==0){
  #  cat('No matched values\n')
  }else{
  #  cat(paste("Record for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), record,"\n ", sep="\n "))
    indic.country.date2[i, 10 ] <- record
  }

  if(nrow(thresholdRed)==0){
  #  cat('No matched values\n')
  }else{
   # cat(paste("thresholdRed for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), thresholdRed,"\n ", sep="\n"))
    indic.country.date2[i, 11 ] <- thresholdRed
  }

  if(nrow(thresholdGreen)==0){
  #  cat('No matched values\n')
  }else{
   # cat(paste("thresholdGreen for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), thresholdGreen,"\n ", sep="\n"))
    indic.country.date2[i, 12 ] <- thresholdGreen
  }

  if(nrow(OP.Target)==0){
   # cat('No matched values\n')
  }else{
   # cat(paste("OP.Target for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), OP.Target,"\n ", sep="\n"))
    indic.country.date2[i, 13 ] <- OP.Target
  }

  if(nrow(OL.Target)==0){
   # cat('No matched values\n')
  }else{
  #  cat(paste("OL.Target for ", paste(i, Indicator1,ppgcountry1,date1, sep="-"), OL.Target,"\n ", sep="\n"))
    indic.country.date2[i, 14 ] <- OL.Target
  }
}

################################################################################################
### STEP 5: Generate regional average for local comparison "regional","subregional"



################################################################################################
### STEP 6: Plot each indicator in a loop
#names(indic.country.date2)

indic.country.this <- as.data.frame(unique(focus.impact.indic[focus.impact.indic$operationName == ctrname , c("Indicator", "indicatorrfid","subtype")]))
indic.country.this2 <- as.data.frame(unique(indic.country.date2[focus.impact.indic$operationName == ctrname , c("Indicator", "indicatorrfid","subtype")]))

#indic.country.this <- as.data.frame(unique(focus.impact.indic[focus.impact.indic$operationName %in% c("regional","subregional") , c("Indicator", "indicatorrfid","subtype")]))

n <- nrow(indic.country.this)


#print(n)
for (i in 1:n ) {
  #i <- 27
  rm(indicplot,plotindic,dataplot)
  indicplot <- as.character(indic.country.this[i, 1])
  dataplot <- indic.country.date2[ indic.country.date2$Indicator==indicplot, ]
 # str(dataplot)
 # dataplot

  if (is.na(dataplot$record)) {
    sprintf("No Record")
  } else {

  plotindic <-  ggplot(dataplot, aes(label)) +
    geom_bar(aes(y = record), stat="identity", fill= "slateblue4") +
    facet_wrap(~ ppg.country , ncol=1) +
    geom_line(aes(y = thresholdRed, group = 1, color = "thresholdRed")) +
    geom_line(aes(y = thresholdGreen, group = 1, color = "thresholdGreen")) +
    geom_line(aes(y = OP.Target, group = 1, color = "OP.Target")) +
    geom_line(aes(y = OL.Target, group = 1, color = "OL.Target")) +
    scale_colour_manual(" ", values=c("record" = "blue", "thresholdRed" = "red", "thresholdGreen" = "green", "OP.Target" = "black", "OL.Target" = "black"))+
    #scale_size_manual(" ", values=c( "thresholdRed" = 1, "thresholdGreen" = 1, "OP.Target" = 2, "OL.Target" = 2))+
    #scale_fill_manual("",values="blue")+
    ggtitle(indicplot) +
    xlab("") + ylab("") +
    theme_wsj()+
    theme(legend.key=element_blank(),
          plot.title=element_text(face="bold", size=12),
          axis.text = element_text(size = 7, face="plain"),
          #legend.title=element_blank(),
          legend.box="horizontal")
  ggsave(filename=paste("out/indic/",i,ctrname,"_indicplot.png",sep=""), plot=plotindic , width=12, height=6,units="in", dpi=300)
  }
  # width=8, height=6,units="in",
}

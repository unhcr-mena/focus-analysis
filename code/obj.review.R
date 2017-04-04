
### Compile analysis per Objective

#this.subtype <- "Registration"

### List objective
#	Budget evolution 2016 to 2018 at objective level – stacked by scenario. 

## get list of related objective
temp.obj <- unique(focus1.budget[ focus1.budget$subtype.obj==this.subtype &  focus1.budget$planningPeriod == yearreport , c("Objective")])

if ( length(temp.obj) == 0) {
  
  cat("There's no objective related to this functional area in the plan.\n")
  } else {
  
    names(temp.obj)[1] <- "Objective"
    temp.obj <- as.character(temp.obj)
    
    #temp.obj <- as.data.frame(unique(focus1.budget[ focus1.budget$subtype.obj==this.subtype &  focus1.budget$planningPeriod == yearreport , c("Objective")]))
    #
    #row.names(temp.obj) <- NULL
    #temp.obj1 <- as.character(temp.obj)
    
    focus1.budget.obj <- focus1.budget[focus1.budget$planningPeriod %in% c("2016","2017","2018") & focus1.budget$Objective %in% temp.obj, ]
    
    cat("### Budget per Objective.\n")
    
    plotbudjobj <- ggplot(focus1.budget.obj, aes(x=  planningPeriod , fill=scenario )) + 
      geom_bar(aes(y = amount), stat="identity") +
      facet_wrap(~ Objective, ncol=1) +
      # ggtitle(paste0("Output: ", indicplot)) +
      ggtitle("Budget per objective & year (in USD for Operating level only",  
              subtitle = "Breakdown per scenario ")+
      coord_flip()+
      xlab("") + ylab("") +
      scale_y_continuous(labels=format_si())+
      theme_economist_white()+
      theme(legend.key=element_blank(),
            plot.title=element_text(face="bold", size=12),
            axis.text = element_text(size = 7, face="plain"),
            #legend.title=element_blank(),
            legend.box="horizontal")
    print(plotbudjobj)
    cat(" \n")
    
    ### List indicators
    #	Impact indicators From 2016 year end till 2018 baseline – Notify if GSP or not in the title of the chart.
    
    temp.ind <- unique(focus1.impact[ focus1.impact$Objective %in% temp.obj &  focus1.impact$planningPeriod %in% c("2016","2017","2018")  ,
                                      c("Objective",  "Indicator","indicatorrfid")])
    #temp.ind  <- temp.ind [ !(is.na(temp.ind $Indicator)), ]
    #temp.ind  <- temp.ind [with(temp.ind , order(Objective)), ]
    row.names(temp.ind ) <- NULL
    
    cat(" \n")
    cat("### Review of impact indicators.\n")
    
    if ( nrow(temp.ind) > 0) {
      
      for (i in 1:nrow(temp.ind) ) {
        #i <- 1
        rm(indicplot,plotindic,dataplot)
        indicplot <- as.character(temp.ind[i, 2])
        dataplot <- indic.country.date2[ indic.country.date2$Indicator==indicplot & indic.country.date2$year %in% c("2016","2017","2018"), ]
        objectivelab <- as.character(temp.ind[i, 1])
        # str(dataplot)
        # dataplot
        dataplot$record <- as.numeric(dataplot$record)
        dataplot$thresholdRed <- as.numeric(dataplot$thresholdRed)
        dataplot$thresholdGreen <- as.numeric(dataplot$thresholdGreen)
        dataplot$OP.Target <- as.numeric(dataplot$OP.Target)
        dataplot$OL.Target <- as.numeric(dataplot$OL.Target)
        
        ## Get rid of old PPG
        dataplot<- dataplot[dataplot$ppg.country %in% ppg.ctr, ]
        
        dataplot$ppg.country <- as.character(dataplot$ppg.country)
        dataplot$ppg.country <- as.factor(dataplot$ppg.country)
        
        #if (is.na(dataplot$record)) {
        #  sprintf("No Record")
        #} else {
          
          cat(paste0("Objective: ", objectivelab , "\n"))
          plotindic <-  ggplot(dataplot, aes(label)) + 
            geom_bar(aes(y = record), stat="identity", fill= "slateblue4") +
            facet_wrap(~ ppg.country , ncol=1) +
            geom_line(aes(y = thresholdRed, group = 1, color = "thresholdRed")) +
            geom_line(aes(y = thresholdGreen, group = 1, color = "thresholdGreen")) +
            geom_line(aes(y = OP.Target, group = 1, color = "OP.Target")) +
            geom_line(aes(y = OL.Target, group = 1, color = "OL.Target"), linetype = 2) +
            scale_colour_manual(" ", values=c("record" = "blue", "thresholdRed" = "red", "thresholdGreen" = "green", "OP.Target" = "black", "OL.Target" = "blue"))+
            #scale_size_manual(" ", values=c( "thresholdRed" = 1, "thresholdGreen" = 1, "OP.Target" = 2, "OL.Target" = 2))+
            #scale_fill_manual("",values="blue")+
            ggtitle(indicplot) +
            xlab("") + ylab("") +
            theme_economist_white()+
            theme(legend.key=element_blank(),
                  plot.title=element_text(face="bold", size=12),
                  axis.text = element_text(size = 7, face="plain"),
                  #legend.title=element_blank(),
                  legend.box="horizontal")
          print(plotindic)
          cat(" \n")
        #}
      } 
      
    } else {cat(paste0("No indicators for this category\n"))}
    
    
    
    cat(" \n")
    cat("### Comparison of Narratives.\n")
    
    cat(" The table below presents narrative from the current & previous Country Operation Plan: Results and Impact for 2016 and  Prioritized Response for 2017 & 2018. \n")
    ### 
    ##	Comparison of Narrative 2016– “Results and Impact - Results and Impact”, 2017& 2018 “Problem/Objective - Prioritized Response” 
    
    ## names(focus1.narrative)
    # &  focus1.narrative$planningPeriod %in% c("2016","2017","2018") 
    
    temp.narr <- unique(focus1.narrative[ focus1.narrative$Objective %in% temp.obj & focus1.narrative$planningPeriod %in% c("2016","2017","2018"), ])
    
    ## list objective
    temp.obj1 <- as.data.frame(temp.obj)
    
    ## List ppg goal
    
    ppggoallist <- as.data.frame(unique(focus1.narrative[focus1.narrative$planningPeriod==2018, c("ppggoal")]))
    names(ppggoallist)[1] <- "ppggoal"
    
    # str(temp.narr)
    ## Now looping around to create the tables
    
    # "Comprehensive Impact - Comprehensive Impact"                                                                            
    # "Constraints - Constraints"                                                                                              
    # "Objective - Constraints (O)"                                                                                            
    # "Objective - Results and Impact (M)"                                                                                     
    # "Objective - Unmet Needs (O)"                                                                                            
    # "Operating Impact - Operating Impact"                                                                                    
    # "Prioritized Response - Prioritized Response"                                                                            
    # "Problem Assessment and Comprehensive Response - Problem Assessment and Comprehensive Response"                          
    # "Problem Assessment, Comprehensive and Prioritised Response - Problem Assessment, Comprehensive and Prioritised Response"
    # "Problem/Objective - Prioritized Response"                                                                               
    # "Problem/Objective - Problem Assessment and Comprehensive Response"                                                      
    # "Results and Impact - Results and Impact"                                                                                
    # "Unmet Needs - Unmet Needs"  
    
    
    for(i in 1:nrow(temp.obj1))
    { 
      for(j in 1:nrow(ppggoallist))
      { 
        # i <- 2
        # j <- 2
        objectivethis <- as.character(temp.obj1[i, 1])
        ppggoalthis <- as.character(ppggoallist[j, 1])
        tempsummary <- as.data.frame(objectivethis)
        temp.narrtest <- temp.narr[ temp.narr$planningPeriod == "2017" &  temp.narr$sectionName == "Prioritized Response - Prioritized Response" &
                                      temp.narr$Objective == objectivethis &  temp.narr$ppggoal == ppggoalthis  , c("reportID","planningPeriod","sectionName", "Objective", "ppggoal", "reportName", "text") ]
        
        summary2016 <- as.character(temp.narr[temp.narr$planningPeriod == "2016" &  temp.narr$sectionName == "Results and Impact - Results and Impact" &
                                                temp.narr$Objective == objectivethis &   temp.narr$ppggoal == ppggoalthis , c("text") ])
        
        summary2017 <- as.character(temp.narr[temp.narr$planningPeriod == "2017" &  temp.narr$sectionName == "Prioritized Response - Prioritized Response" &
                                                temp.narr$Objective == objectivethis &  temp.narr$ppggoal == ppggoalthis , c("text") ]) 
        
        summary2018 <- as.character(temp.narr[temp.narr$planningPeriod == "2018" &   temp.narr$sectionName == "Prioritized Response - Prioritized Response" &
                                                temp.narr$Objective == objectivethis &   temp.narr$ppggoal == ppggoalthis , c("text") ])
        
        #  summary2018  <- subset(temp.narr[temp.narr$planningPeriod == "2018" & temp.narr$sectionName == "Prioritized Response - Prioritized Response" &
        #                       temp.narr$Objective == objectivethis &  temp.narr$ppggoal == ppggoalthis )["text"]
        
       # if(length(summary2016)==0){ tempsummary[2,1] <- "No narrative for2016 " } else {  tempsummary[2,1] <- summary2016  }
        # if(length(summary2017)==0){ tempsummary[3,1] <- "No narrative for 2017 " } else {  tempsummary[3,1] <- summary2017  }
        # if(length(summary2018)==0){ tempsummary[4,1] <- "No narrative for 2018 " } else {  tempsummary[4,1] <- summary2018  }
        
        
        kable(tempsummary, rownames = NULL, col.names= "Narratives for")
        cat(" \n")
      }
    }

    
}


### Compile analysis per Objective####################################################
#	Budget evolution 2017 to 2020 at objective level – stacked by scenario. #######
### List objective ###############################################

# Get list of related objective ###############################################
#names(focus1.budget)
#this.subtype <- "Registration"
temp.obj <- as.data.frame(unique(focus1.budget[ focus1.budget$subtype.obj == this.subtype &
                                                  focus1.budget$planningPeriod == yearreport ,
                                                c("objectivemsrp","Objective")]))

names(temp.obj)[1] <- "objectivemsrp"
names(temp.obj)[2] <- "Objective"

temp.obj <-  temp.obj[!(is.na(temp.obj$objectivemsrp)),]

temp.obj2 <- temp.obj
temp.obj <- as.character(temp.obj$objectivemsrp)

if ( nrow(temp.obj2) == 0) { cat("There's no objective related to this functional area in the plan.\n") } else {

  cat(" \n")
  ##	Comparison of Budget###########################################################################################################
  cat("## Budget per Objective.\n")
  cat(" The following chart presents budget evolution over years between Operating and Above Operating levels. \n")

  focus1.budget.obj <- focus1.budget[focus1.budget$planningPeriod %in% c("2017","2018","2019","2020") &
                                       focus1.budget$objectivemsrp %in% temp.obj, ]

  plotbudjobj <- ggplot(focus1.budget.obj, aes(x =  planningPeriod , fill = scenario )) +
    geom_bar(aes(y = amount), stat = "identity") +
    facet_wrap(~ Objective, ncol = 1) +
    ggtitle("Budget per objective & year (in USD)",
            subtitle = "Breakdown per scenario ") +
    coord_flip() +
    xlab("") + ylab("") +
    scale_y_continuous(labels = format_si()) +
    theme_economist_white() +
    theme(legend.key = element_blank(),
          plot.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 7, face = "plain"),
          #legend.title=element_blank(),
          legend.box = "horizontal")
  print(plotbudjobj)
  cat(" \n")
  cat(" \n")

  ##	Comparison of Impact indicators###################################################################################
  cat("## Review of impact indicators.\n")
  cat(" The following chart presents evolution of impact indicators over years.
      Actual reported Indicators values are represented as bars and can be compared between baseline, mid-year and end-year.
      Different types of target are represented as lines in the same chart: as defined by budget: OL, as defined by needs: OP and as defined by global standards: Green and Red Threshold. \n")
  cat("  A specific note included for the indicators falling under Global Strategic Priorities - GSP. Note that operations are not forced to report mid-year values for all their impact indicators.\n")
  cat(" The chart allows to visualize progress, gaps, missing values & inconsistencies (for instance when the baseline for the next year differs from the end-year value from the previous years).\n\n")

  temp.ind <- unique(focus1.impact[ focus1.impact$objectivemsrp %in% temp.obj &
                                      focus1.impact$planningPeriod %in% c("2017","2018","2019","2020")  ,
                                    c("Objective",  "Indicator","indicatorrfid","GSP")])

  row.names(temp.ind ) <- NULL
  if ( nrow(temp.ind) > 0) {
    for (i in 1:nrow(temp.ind) ) {
      #i <- 1
      rm(indicplot,plotindic,dataplot)
      indicplot <- as.character(temp.ind[i, 2])
      gsp <- as.character(temp.ind[i, 4])
      gsp[gsp == "false"] <- ""
      gsp[gsp == "true"] <-  "This indicator corresponds to a Global Strategic Priority (GSP)"
      dataplot <- indic.country.date2[ indic.country.date2$Indicator == indicplot &
                                         indic.country.date2$year %in% c("2017","2018","2019","2020"), ]
      objectivelab <- as.character(temp.ind[i, 1])
      # str(dataplot)
      # dataplot
      dataplot$record <- as.numeric(dataplot$record)
      dataplot$thresholdRed <- as.numeric(dataplot$thresholdRed)
      dataplot$thresholdGreen <- as.numeric(dataplot$thresholdGreen)
      dataplot$OP.Target <- as.numeric(dataplot$OP.Target)
      dataplot$OL.Target <- as.numeric(dataplot$OL.Target)

      ## Get rid of old PPG
      # dataplot <- dataplot[dataplot$ppg.country %in% ppg.ctr, ]

      dataplot$ppg.country <- as.character(dataplot$ppg.country)
      dataplot$ppg.country <- as.factor(dataplot$ppg.country)

      #if (is.na(dataplot$record)) {
      #  sprintf("No Record")
      #} else {

      cat(paste0("Objective: ", objectivelab , "\n"))
      ### Now plotting graphs
      plotindic <-  ggplot(dataplot, aes(label)) +
        geom_bar(aes(y = record), stat = "identity", fill = "slateblue4") +
       # geom_text(aes(dataplot, y = record, label=record)) +
        facet_wrap(~ ppg.country , ncol = 1) +
        geom_line(aes(y = thresholdRed, group = 1, color = "thresholdRed"), size = 1.4) +
        geom_line(aes(y = thresholdGreen, group = 1, color = "thresholdGreen"), size = 1.4) +
        geom_line(aes(y = OP.Target, group = 1, color = "OP.Target"), size = 1.4) +
        geom_line(aes(y = OL.Target, group = 1, color = "OL.Target"), linetype = 2, size = 1.4) +
        scale_colour_manual(" ", values = c("record" = "blue", "thresholdRed" = "red", "thresholdGreen" = "green", "OP.Target" = "black", "OL.Target" = "blue")) +
        geom_label_repel(aes(y = record, label = record), fill = 'black', color = 'white') +
        #scale_size_manual(" ", values=c( "thresholdRed" = 1, "thresholdGreen" = 1, "OP.Target" = 2, "OL.Target" = 2))+
        #scale_fill_manual("",values="blue")+
        ggtitle(indicplot,
                subtitle = gsp) +
        xlab("") + ylab("") +
        theme_economist_white() +
        theme(legend.key = element_blank(),
              plot.title = element_text(face = "bold", size = 12),
              axis.text = element_text(size = 9, face = "plain"),
              #legend.title=element_blank(),+
              legend.position = "top",
              legend.direction = "horizontal")
             # legend.box="horizontal")
      print(plotindic)
      cat(" \n")
      #}
    }

  } else {cat(paste0("No indicators for this category\n"))}

  cat(" \n")
  ##	Comparison of Narrative######################################################################################
  cat("## Comparison of narrative.\n")
  cat(" The following tables present narratives from the current & previous Country Operation Plan: Results and Impact for 2018,  Problem Assessment for 2019 & 2020. \n")

  temp.narr <- unique(focus1.narrative[ focus1.narrative$objectivemsrp %in% temp.obj &
                                          focus1.narrative$planningPeriod %in% c("2017","2018","2019","2020") &
                                          focus1.narrative$reportName %in% c("Year-End Report","Operations Plan Document"), ])
  temp.narr$objectivemsrp <- as.character(temp.narr$objectivemsrp)
  ## List ppg goal
  ppggoallist <- as.data.frame(unique(as.character(focus1.narrative[focus1.narrative$planningPeriod == yearreport, c("ppggoal")])))
 # ppggoallist <- as.data.frame(unique(as.character(focus1.narrative[focus1.narrative$planningPeriod == 2020, c("ppggoal")])))
  names(ppggoallist)[1] <- "ppggoal"


  for (f in 1:nrow(temp.obj2) ) {
    for (g in 1:nrow(ppggoallist) ) {

      # f <- 1
      # g <- 1
      ## Get objective name corresponding to MSRP code
      objectivethis <- as.character(temp.obj2[f, 2])
      objectivemsrpthis <- as.character(temp.obj2[f, 1])
      ppggoalthis <- as.character(ppggoallist[g, 1])

      tempsummary <- as.data.frame(objectivethis)
      #str(tempsummary)
      tempsummary$objectivethis <- as.character(tempsummary$objectivethis)
      names(tempsummary)[1] <- paste( "Narrative Comparison: ", objectivethis, "for", ppggoalthis, sep = " ")

      temp.narrtest <- temp.narr[ temp.narr$objectivemsrp == objectivemsrpthis &
                                    temp.narr$ppggoal == ppggoalthis  ,
                                  c("reportID","planningPeriod","sectionName", "Objective","objectivemsrp", "ppggoal", "reportName", "text") ]



      summary2018 <- as.data.frame(temp.narr[temp.narr$planningPeriod == "2018" &
                                               temp.narr$sectionName == "Results and Impact" &
                                               temp.narr$objectivemsrp == objectivemsrpthis &
                                               temp.narr$ppggoal == ppggoalthis ,
                                             c("text")])

      summary2019 <- as.data.frame(temp.narr[temp.narr$planningPeriod == "2019" &
                                               temp.narr$sectionName == "Problem Assessment, Comprehensive and Prioritised Response" &
                                               temp.narr$objectivemsrp == objectivemsrpthis &
                                               temp.narr$ppggoal == ppggoalthis ,
                                             c("text") ])

      summary2020 <- as.data.frame(temp.narr[temp.narr$planningPeriod == "2020" &
                                               temp.narr$sectionName == "Problem Assessment, Comprehensive and Prioritised Response" &
                                               temp.narr$objectivemsrp == objectivemsrpthis &
                                               temp.narr$ppggoal == ppggoalthis ,
                                             c("text") ])

      tempsummary[3,1] <- "__Results and Impact for 2018__"
      if (nrow(summary2018) == 0) {
        tempsummary[4,1] <- "No narrative for 2018 " } else {tempsummary[4,1] <-  as.character(summary2018[1,1])  }

      tempsummary[5,1] <- "__Problem Assessment, Comprehensive and Prioritised Response for 2019__"
      if (nrow(summary2019) == 0) {
        tempsummary[6,1] <- "No narrative for 2019 " } else {tempsummary[6,1] <-  as.character(summary2019[1,1])  }

      tempsummary[7,1] <- "__Problem Assessment and Comprehensive Response for 2020__"
      if (nrow(summary2020) == 0) {
        tempsummary[8,1] <- "No narrative for 2020 " } else {tempsummary[8,1] <-  as.character(summary2020[1,1])  }

      #print(kable(tempsummary3, rownames = NA,  longtable = TRUE, padding = 2))
      if (nrow(summary2020) == 0) { cat("") } else {
        print(pandoc.table(tempsummary, rownames = NULL, split.table = 300, split.cells = c( "100%"), justify = 'left'))
        # use.hyphening = TRUE,
        cat(" \n") }
    }
  }

  cat(" \n")

}

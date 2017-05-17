### This code allow to print the indicator review graph

### Remove indicator that are not selected -- i.e. no OP target for 2018
indic.country.this <- as.data.frame(unique(focus.impact.indic[focus.impact.indic$operationName==ctrname & focus.impact.indic$subtype==this.subtype , 
                                                              c("Indicator", "indicatorrfid","subtype","Objective","planningPeriod")]))

indic.country.this <- indic.country.this[indic.country.this$planningPeriod==2018,  c("Indicator", "indicatorrfid","subtype","Objective")]

indic.country.this <- indic.country.this[ !(is.na(indic.country.this$Indicator)), ]

indic.country.this <- indic.country.this[with(indic.country.this, order(Objective)), ]

n <- nrow(indic.country.this)

if (n>0) {

  for (i in 1:n ) {
    #i <- 1
    rm(indicplot,plotindic,dataplot)
    indicplot <- as.character(indic.country.this[i, 1])
    dataplot <- indic.country.date2[ indic.country.date2$Indicator==indicplot, ]
    objectivelab <- as.character(indic.country.this[i, 4])
    # str(dataplot)
    # dataplot
    dataplot$record <- as.numeric(dataplot$record)
    dataplot$thresholdRed <- as.numeric(dataplot$thresholdRed)
    dataplot$thresholdGreen <- as.numeric(dataplot$thresholdGreen)
    dataplot$OP.Target <- as.numeric(dataplot$OP.Target)
    dataplot$OL.Target <- as.numeric(dataplot$OL.Target)
    
    if (is.na(dataplot$record)) {
      sprintf("No Record")
    } else {
      
    cat(paste0("Objective: ", objectivelab , "\n"))
    plotindic <-  ggplot(dataplot, aes(label)) + 
        geom_bar(aes(y = record), stat="identity", fill= "slateblue4") +
        facet_wrap(~ ppg.country , ncol=1) +
        geom_line(aes(y = thresholdRed, group = 1, color = "thresholdRed"), size=1.4) +
        geom_line(aes(y = thresholdGreen, group = 1, color = "thresholdGreen"), size=1.4) +
        geom_line(aes(y = OP.Target, group = 1, color = "OP.Target"), size=1.4) +
        geom_line(aes(y = OL.Target, group = 1, color = "OL.Target"), linetype = 2, size=1.4) +
        scale_colour_manual(" ", values=c("record" = "blue", "thresholdRed" = "red", "thresholdGreen" = "green", "OP.Target" = "black", "OL.Target" = "blue"))+
        geom_label_repel(aes(y = record, label = record), fill = 'black', color = 'white') +
        xlab("") + ylab("") +
        theme_economist_white()+
        theme(legend.key=element_blank(),
              plot.title=element_text(face="bold", size=12),
              axis.text = element_text(size = 7, face="plain"),
              legend.position="top", 
              legend.direction="horizontal")
    print(plotindic)
    }
  } 

} else {cat(paste0("No indicators for this category\n"))}

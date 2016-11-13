### This code allow to print the indicator review graph


indic.country.this <- as.data.frame(unique(focus.impact.indic[focus.impact.indic$operationName==ctrname & focus.impact.indic$subtype==this.subtype  , c("Indicator", "indicatorrfid","subtype")]))
n <- nrow(indic.country.this)

if (n>0) {

  for (i in 1:n ) {
    #i <- 1
    rm(indicplot,plotindic,dataplot)
    indicplot <- as.character(indic.country.this[i, 1])
    dataplot <- indic.country.date2[ indic.country.date2$Indicator==indicplot, ]
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
        theme_wsj()+
        theme(legend.key=element_blank(),
              plot.title=element_text(face="bold", size=12),
              axis.text = element_text(size = 7, face="plain"),
              #legend.title=element_blank(),
              legend.box="horizontal")
    print(plotindic)
    }
  } 

} else {}
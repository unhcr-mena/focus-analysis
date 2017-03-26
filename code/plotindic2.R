### This code allow to print the indicator review graph


indic.country.this.perf <- as.data.frame(unique(focus.performance.indic[focus.performance.indic$operationName==ctrname & focus.performance.indic$subtype==this.subtype  ,
                                                                        c("Indicator", "indicatorrfid","subtype","Objective")]))
indic.country.this.perf <- indic.country.this.perf[ !(is.na(indic.country.this.perf$Indicator)), ]
indic.country.this.perf <- indic.country.this.perf[with(indic.country.this.perf, order(Objective)), ]

n <- nrow(indic.country.this.perf)

if (n>0) {

  for (i in 1:n ) {
    #i <- 3
    rm(indicplot,plotindic,dataplot)
    indicplot <- as.character(indic.country.this.perf[i, 1])
    indicid <- as.character(indic.country.this.perf[i, 2])
    objectivelab <- as.character(indic.country.this.perf[i, 4])
    dataplot <- indic.country.date3[ indic.country.date3$Indicator==indicplot, ]
    # str(dataplot)
    # dataplot
    dataplot$record <- as.numeric(dataplot$record)
    dataplot$Standard <- as.numeric(dataplot$Standard) 
    dataplot$OP.Target <- as.numeric(dataplot$OP.Target)
    dataplot$OL.Target <- as.numeric(dataplot$OL.Target)
    
      #  if (is.na(dataplot$record)) {
      #    cat(paste0("No Record for indicator: ", indicplot , "\n"))
      #  } else {
          
        cat(paste0("Objective: ", objectivelab , "\n"))
        plotindic2 <-  ggplot(dataplot, aes(label)) + 
            geom_bar(aes(y = record), stat="identity", fill= "slateblue4") +
            facet_wrap(~ ppg.country , ncol=1) +
            geom_line(aes(y = Standard, group = 1, color = "Standard")) +
          #  geom_line(aes(y = thresholdGreen, group = 1, color = "thresholdGreen")) +
            geom_line(aes(y = OP.Target, group = 1, color = "OP.Target")) +
            geom_line(aes(y = OL.Target, group = 1, color = "OL.Target"), linetype = 2) +
            scale_colour_manual(" ", values=c("record" = "blue", "Standard" = "red",  "OP.Target" = "black", "OL.Target" = "blue"))+
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
        print(plotindic2)
       # ggsave(filename=paste("out/",indicid,".png",sep=""), plot=plotindic2, width=10, height=10,units="in", dpi=300)
    
       # }
    } 

    } else { cat(paste0("No indicators for this category\n"))}
### This code allow to print the indicator review graph

### Remove indicator that are not selected -- i.e. no OP target for 2018
framework.this <- as.data.frame(unique(framework[framework$subtype==this.subtype , c("RightsGroup","Objective",  "Output")]))
framework.this <- framework.this [ !(is.na(framework.this$Output)), ]

framework.this2 <- as.character(framework.this [ , c("Output") ])

budget.this <- focus1.budget[ focus1.budget$Output %in% framework.this2 ,  ]

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
          
        cat(paste0("Objective: ", objectivelab , "\n"))
        plotindic2 <-  ggplot(dataplot, aes(label)) + 
            geom_bar(aes(y = record), stat="identity", fill= "slateblue4") +
            facet_wrap(~ ppg.country , ncol=1) +
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
    } 

} else { cat(paste0("No related budget for output linked to this category\n"))}
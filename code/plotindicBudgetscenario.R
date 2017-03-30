### This code allow to print the indicator review graph

### Remove indicator that are not selected -- i.e. no OP target for 2018
framework.this <- as.data.frame(unique(framework[framework$subtype==this.subtype , c("RightsGroup","Objective",  "Output","outputrfid")]))
framework.this <- framework.this [ !(is.na(framework.this$Output)), ]

framework.this2 <- as.character(framework.this [ , c("outputrfid") ])

budget.this <- focus1.budget[ focus1.budget$outputrfid %in% framework.this2 ,  ]

budget.this1 <- as.data.frame(unique(budget.this[ , c("Output","outputrfid","Objective","planningPeriod")]))

budget.this1 <- budget.this1[budget.this1$planningPeriod==2018,  c("Output","outputrfid","Objective")]

budget.this1 <- budget.this1[ !(is.na(budget.this1$Output)), ]

budget.this1 <- budget.this1[with(budget.this1, order(Objective)), ]



n <- nrow(budget.this1)

if (n>0) {

  for (i in 1:n ) {
    #i <- 1
    rm(indicplot,plotindic,dataplot)
    indicplot <- as.character(budget.this1[i, 1])
    outputrfid1 <- as.character(budget.this1[i, 2])
    objectivelab <- as.character(budget.this1[i, 3])
    dataplot <- budget.this[ budget.this$outputrfid==outputrfid1, ]
    # str(dataplot)
    # dataplot
    dataplot$amount <- as.numeric(dataplot$amount)
    
    ## Subset for operating level
    #dataplot <- dataplot[dataplot$scenario=="Operating Level", ]
          
        cat(paste0("Objective: ", objectivelab , "\n"))
        plotbudget1 <-  ggplot(dataplot, aes(x= planningPeriod, fill=scenario)) + 
            geom_bar(aes(y = amount), stat="identity") +
           # facet_wrap(~  , ncol=1) +
           # ggtitle(paste0("Output: ", indicplot)) +
            ggtitle(paste0("Output: ", indicplot),  
                  subtitle = paste0("Objective: ", objectivelab))+
          
            xlab("") + ylab("") +
            scale_y_continuous(labels=format_si())+
            theme_economist_white()+
            theme(legend.key=element_blank(),
                  plot.title=element_text(face="bold", size=12),
                  axis.text = element_text(size = 7, face="plain"),
                  #legend.title=element_blank(),
                  legend.box="horizontal")
        print(plotbudget1)
       #ggsave(filename=paste("out/budget_",outputrfid1,".png",sep=""), plot=plotbudget1, width=10, height=10,units="in", dpi=300)
    } 

} else { cat(paste0("No related budget for output linked to this category\n"))}
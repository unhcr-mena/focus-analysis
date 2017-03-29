### This code allow to print the indicator sourcing table

temp <- unique(focus1.performance[ focus1.performance$subtype==this.subtype &  focus1.performance$planningPeriod == yearreport , c("Objective",  "Indicator","Source")])


temp <- temp[ !(is.na(temp$Indicator)), ]

temp <- temp[with(temp, order(Objective)), ]

row.names(temp) <- NULL


if (nrow(temp) > 0) {
  #emphasize.strong.cells(1)
  print(pandoc.table(temp, split.cells = c("35%", "50%","15%"), use.hyphening = TRUE, justify = 'left', caption = "List of related Performance indicators selected in the next plan"))
#  print(pander(temp, split.cells = c("35%", "50%","15%"), use.hyphening = TRUE, justify = 'left',  caption = "List of related Performance indicators selected in the next plan"))
  
  
  } else {
  cat("No indicator in this area for next year")
}
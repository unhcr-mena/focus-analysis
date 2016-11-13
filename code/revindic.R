### This code allow to print the indicator sourcing table

temp <- unique(focus1.impact[ focus1.impact$subtype==this.subtype &  focus1.impact$planningPeriod == yearreport , c("Objective",  "Indicator","Source")])
row.names(temp) <- NULL

if (nrow(temp) > 0) {
  #emphasize.strong.cells(1)
  print(pandoc.table(temp, split.cells = c("35%", "50%","15%"), use.hyphening = TRUE, justify = 'left', caption = "List of related impact indicators selected in the next plan"))
} else {
  sprintf("No indicator in this area for next year")
}
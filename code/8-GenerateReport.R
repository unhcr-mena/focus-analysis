####################################################################################################
### This scripts generate country report one by one based on the report template: 8-Report.Rmd
#####################################################################################################

## load all packages
source("code/0-package.R")

library(knitr)
#devtools::install_github('rstudio/rmarkdown')
library(rmarkdown)

#### load restructured data
#focus.performance <- read.csv("data/impact.csv")
focus.impact <- read.csv("data/impact.csv")

#source("code/1-parse_reference.R")
#listcountry <- as.data.frame(unique(opreference.mena$operationName))

opreferencemenaimp <- read.csv("data/opereferencemenaimp.csv")
listcountry <- as.data.frame(unique(opreferencemenaimp$operationName))

nindic <- nrow(listcountry)
for(i in 1:nindic)
{
  #i <- 4
  ctrname <- as.character(listcountry[ i , 1]) 
  yearreport <- "2017"
  #render("code/Report-impact-Country.Rmd", pdf_document(latex_engine='xelatex'), output_options=self_contained)
  render("code/Report-Impact-Country.Rmd", pdf_document(latex_engine='xelatex'), 
         output_options=list(pdf_document = list(fig_caption=yes,number_sections=yes,toc=yes,toc_depth=3)))
         
  file.rename("code/Report-Impact-Country.pdf", paste0("out/",ctrname,"_FOCUS_Plan_Impact_Indicator_Analysis_Report.pdf"))
}
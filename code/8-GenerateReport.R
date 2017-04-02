####################################################################################################
### This scripts generate country report one by one based on the report template: 8-Report.Rmd
#####################################################################################################

## load all packages
source("code/0-package.R")

### Refresh data

source("code/2-api_get_plan.R")

source("code/3-Performance_Indicator.R")
source("code/4-Impact-Indicator.R")
source("code/6-Budget.R")
source("code/7-narrative.R")

### Now Generating Reports

opreferencemena <- read.csv("data/opereferencemena.csv")

listcountry <- as.data.frame(unique(opreferencemena$operationName))

nindic <- nrow(listcountry)
for(i in 1:nindic)
{
  #i <- 4
  ctrname <- as.character(listcountry[ i , 1]) 
  yearreport <- "2018"
  #render("code/Report-impact-Country.Rmd", pdf_document(latex_engine='xelatex'), output_options=self_contained)
  render("code/Report-Annual-Review-Country.Rmd"
            #"code/Report-Programme-Review-Country.Rmd", 
            #pdf_document(latex_engine='xelatex'), 
            # pdf_document(latex_engine='pdflatex'), 
            #output_options=list(pdf_document = list(fig_caption=yes,number_sections=yes,toc=yes,toc_depth=3))
        )
         
  #file.rename("code/Report-Impact-Country.pdf", paste0("out/",ctrname,"_FOCUS_Plan_Impact_Indicator_Analysis_Report.pdf"))
  #file.rename("code/Report-Impact-Country.docx", paste0("out/",ctrname,"_FOCUS_Plan_Impact_Indicator_Analysis_Report.docx"))
  #file.rename("code/Report-Programme-Review-Country.docx", paste0("out/",ctrname,"_FOCUS_Plan_Programme-Review_Analysis_Report.docx"))
  file.rename("code/Report-Annual-Review-Country.docx", paste0("out/",ctrname,"_FOCUS_Plan_Annual-Review_Analysis_Report.docx"))
}
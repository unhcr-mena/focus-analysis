####################################################################################################
### This scripts generate country report one by one based on the report template: 8-Report.Rmd
#####################################################################################################
setwd("D:/R-project/focus-analysis") 
## load all packages
source("code/0-package.R")

### Refresh data

source("code/2-api_get_plan.R")

#source("code/3-Performance_Indicator.R")

source("code/4-Impact-Indicator.R")

source("code/6-Budget.R")

source("code/7-narrative.R")
#
### Now Generating Reports

#opreferencemena <- read.csv("data/opreferencemena.csv")
#listcountry <- as.data.frame(unique(opreferencemena$operationName))

listcountry <- as.data.frame(c("Iraq", # Worked #
                               "Jordan",    # Worked # 
                               "Lebanon", # Worked #
                               "Israel", # Worked #
                               "Saudi Arabia",
                               "Syrian Arab Republic", # did not Worked #
                               "United Arab Emirates",
                               "Yemen",
                               "Algeria",
                               "Egypt",
                               "Libya",
                               "Mauritania",
                               "Morocco",
                               "Tunisia",
                               "Western Sahara"))

## Generate reports for Annual Review
for(i in 1:nrow(listcountry))
{
  ctrname <- as.character(listcountry[ i , 1])
  cat(paste(i, " - Render report for ",ctrname ))
  yearreport <- "2018"
  render("code/Report-Annual-Review-Country.Rmd")
  file.rename("code/Report-Annual-Review-Country.docx", paste0("out/COP-Review/",ctrname,Sys.Date(),"_FOCUS_Plan_Annual-Review_Analysis_Report.docx"))
}


## generate Report for Perfomance Review
for(i in 1:nrow(listcountry))
{
  ctrname <- as.character(listcountry[ i , 1])
  cat(paste(i, " - Render report for ",ctrname ))
  yearreport <- "2018"
  render("code/Report-performance-Country.Rmd")
  file.rename("code/Report-performance-Country.docx", paste0("out/Performance-Review/",ctrname,Sys.Date(),"_FOCUS_performance_Analysis_Report.docx"))
}


## Generate report for indepth programme review
for(i in 1:nrow(listcountry))
{
  ctrname <- as.character(listcountry[ i , 1])
  cat(paste(i, " - Render report for ",ctrname ))
  yearreport <- "2018"
  render("code/Report-Programme-Review-Country.Rmd")
  file.rename("code/Report-Programme-Review-Country.docx", paste0("out/Programme-Review/",ctrname,Sys.Date(),"_FOCUS_Programme-Review_Analysis_Report.docx"))
}


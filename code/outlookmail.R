###################
#install.packages("RDCOMClient", dep = T)
library(RDCOMClient)
## init com api
OutApp <- COMCreate("Outlook.Application")
## create an email 
outMail = OutApp$CreateItem(0)
## configure  email parameter 
outMail[["To"]] = "egoupil@unhcr.org"
outMail[["subject"]] = "some subject"
outMail[["body"]] = "some body"
## send it    
outMail[["Attachments"]]$Add("D:/R-project/focus-analysis/perso/outlookmail.R")
outMail$Send()


## Script to schedule Report

#install.packages("data.table")
#install.packages("knitr")
#install.packages("miniUI")
#install.packages("shiny")
#install.packages("taskscheduleR", repos = "http://www.datatailor.be/rcube", type = "source")

#https://trinkerrstuff.wordpress.com/2015/02/11/scheduling-r-tasks-via-windows-task-scheduler/

#SUCCESS: The scheduled task "outlookmail.R" was successfully deleted.
#Creating task schedule: schtasks /Create /TN "outlookmail.R" /TR
#"cmd /c C:/Users/legoupil/DOCUME~1/R/R-33~1.2/bin/Rscript.exe \
#"C:/Users/legoupil/Documents/R/R-3.3.2/library/taskscheduleR/extdata/outlookmail.R\" 
#>> \"C:/Users/legoupil/Documents/R/R-3.3.2/library/taskscheduleR/extdata/outlookmail.log\" 
#2>&1" /SC ONCE /ST 22:30 

#library(taskscheduleR)
library("taskscheduleR", lib.loc="D:/R-project/library")
#myscript <- system.file("D:/R-project/focus-analysis/code", "8-GenerateReport.R", package = "taskscheduleR")

myscript <- system.file("D:/R-project/focus-analysis/perso", "outlookmail.R", package = "taskscheduleR")
myscript <- system.file("C:/Users/legoupil/Downloads/outlookmail.R", "outlookmail.R", package = "taskscheduleR", mustWork = TRUE)

myscript <- system.file("C:/Users/legoupil/Downloads/outlookmail.R", 
                        package = "taskscheduleR", mustWork = TRUE)

#setwd("D:/R-project/focus-analysis")
getwd()
myscript <- system.file("/perso", "outlookmail.R", 
                        package = "taskscheduleR", mustWork = TRUE)

## run script once within 62 seconds
taskscheduler_create(taskname = "myfancyscript", rscript = myscript,   schedule = "ONCE", starttime = format(Sys.time() + 30, "%H:%M"))

## Run every day at the same time on 09:10, starting from tomorrow on
## Mark: change the format of startdate to your locale if needed (e.g. US: %m/%d/%Y)
taskscheduler_create(taskname = "myfancyscriptdaily", rscript = myscript,  schedule = "DAILY", starttime = "09:10", startdate = format(Sys.Date()+1, "%d/%m/%Y"))

## Run every week on Sunday at 09:10
taskscheduler_create(taskname = "report_fri", rscript = myscript,   schedule = "WEEKLY", starttime = "07:10", days = 'FRI')

## get a data.frame of all tasks
tasks <- taskscheduler_ls()
#str(tasks)

## delete the tasks
taskscheduler_delete(taskname = "myfancyscript")
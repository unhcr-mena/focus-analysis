############################################
## get plan from Focus Reader API
############################################



## authenticate - Uncomment
# user <- readline("Give the username:")
# passw <- readline("Give the password:")
# api <- readline("provide the URL to the API:")
# urlendsp2 <- readline("provide the URL extnesion for user tracking:")

## External file for easier reference 
source("code/0-package.R")
#source("code/1-parse_reference.R")
source("perso/api_get_perso.R")

urlendsp1 <- ".zip?user="
urlend <- paste(urlendsp1,user,urlendsp2,sep = "")
upw = paste(user, passw, sep = ":")


## Load list of plan
#planheader <- read.csv("data/planheader.csv") 

opreference.mena <- read.csv("data/opreferencemena.csv")


nindic <-nrow(opreference.mena)

## Loop through urls and download all plan 
for(i in 1:nindic)
    {
      planid <- as.character(opreference.mena[ i , 2])
      plandescr <- as.character(opreference.mena[ i , 5]) 
      plandctr <- as.character(opreference.mena[ i , 7])
      plan <- paste( "Plan_", planid , sep = "")
      url <- paste(api,plan, urlend, sep = "")
     # print(url)
      
      print(paste (i, " Now downloading from Focus Reader the plan for ", plandctr," for year ", plandescr ," from URL: ", url , sep = " - ", collapse = NULL) )
      bin <- getBinaryURL(url, userpwd = upw, httpauth = 1L, ssl.verifypeer=FALSE  )
      con <- file("data/plantemp.zip", open = "wb")
      writeBin(bin, con)
      close(con)
      unzip ("data/plantemp.zip", overwrite = TRUE, exdir = "./data/plan")
    }

## That's it, doc !



##################################################
## get all of them for benchmarking analysis

#nindic <-nrow(opreference.ope)

## Loop through urls and download all plan 
#for(i in 1:nindic)
#{
#  planid <- as.character(opreference.ope[ i , 2])
#  plandescr <- as.character(opreference.ope[ i , 5]) 
#  plandctr <- as.character(opreference.ope[ i , 7])
#  plan <- paste( "Plan_", planid , sep = "")
#  url <- paste(api,plan, urlend, sep = "")
  # print(url)
  
#  print(paste (i, " Now downloading from Focus Reader the plan for ", plandctr," for year ", plandescr ," from URL: ", url , sep = " - ", collapse = NULL) )
#  bin <- getBinaryURL(url, userpwd = upw, httpauth = 1L, ssl.verifypeer=FALSE  )
#  con <- file("data/plantemp.zip", open = "wb")
#  writeBin(bin, con)
#  close(con)
#  unzip ("data/plantemp.zip", overwrite = TRUE, exdir = "./data/plan")
#}

## That's it, doc !

rm(planid,plandescr,plandctr,plan,url,urlendsp1,urlend,upw)
## Extract from http://popdata.unhcr.org


#pf_country_of_origin <- read.csv("data/planningfigure/pf_country_of_origin_2017.csv")
#pf_population_planning_groups <- read.csv("data/planningfigure/pf_population_planning_groups_2017.csv")
#pf_specific_planning_figures <- read.csv("data/planningfigure/pf_specific_planning_figures_2017.csv")


pop_idp_2016 <- read.csv("data/popstat/asr_idps_2016.csv")
pop_refugee_2016 <- read.csv("data/popstat/asr_refugees_2016.csv")
pop_returnee_2016 <- read.csv("data/popstat/asr_returnees_2016.csv")
pop_stateless_2016 <- read.csv("data/popstat/asr_stateless_2016.csv")

pop_idp_2015 <- read.csv("data/popstat/asr_idps_2015.csv")
pop_refugee_2015 <- read.csv("data/popstat/asr_refugees_2015.csv")
pop_returnee_2015 <- read.csv("data/popstat/asr_returnees_2015.csv")
pop_stateless_2015 <- read.csv("data/popstat/asr_stateless_2015.csv")

pop_idp_2014 <- read.csv("data/popstat/asr_idps_2014.csv")
pop_refugee_2014 <- read.csv("data/popstat/asr_refugees_2014.csv")
pop_returnee_2014 <- read.csv("data/popstat/asr_returnees_2014.csv")
pop_stateless_2014 <- read.csv("data/popstat/asr_stateless_2014.csv")

pop_idp_2013 <- read.csv("data/popstat/asr_idps_2013.csv")
pop_refugee_2013 <- read.csv("data/popstat/asr_refugees_2013.csv")
pop_returnee_2013 <- read.csv("data/popstat/asr_returnees_2013.csv")
pop_stateless_2013 <- read.csv("data/popstat/asr_stateless_2013.csv")

pop_idp <- rbind(pop_idp_2016,pop_idp_2015,pop_idp_2014,pop_idp_2013)
pop_refugee <- rbind(pop_refugee_2016,pop_refugee_2015,pop_refugee_2014,pop_refugee_2013)
pop_returnee <- rbind(pop_returnee_2016,pop_returnee_2015,pop_returnee_2014,pop_returnee_2013)
pop_stateless <- rbind(pop_stateless_2016,pop_stateless_2015,pop_stateless_2014,pop_stateless_2013)

rm(pop_idp_2016,pop_idp_2015,pop_idp_2014,pop_idp_2013)
rm(pop_refugee_2016,pop_refugee_2015,pop_refugee_2014,pop_refugee_2013)
rm(pop_returnee_2016,pop_returnee_2015,pop_returnee_2014,pop_returnee_2013)
rm(pop_stateless_2016,pop_stateless_2015,pop_stateless_2014,pop_stateless_2013)

#names(pop_idp)
#names(pop_refugee)
#names(pop_returnee)
#names(pop_stateless)

## list of ISO code for MENA
library(readr)
#mena <- read_delim("config/mena.csv",   ";", escape_double = FALSE, trim_ws = TRUE)
mena <- read_delim("config/mena2.csv",   ",", escape_double = FALSE, trim_ws = TRUE)
#ctrall <- read_delim("config/ctryall.csv",   ",", escape_double = FALSE, trim_ws = TRUE)
ctrall2 <- read_delim("config/ctryall2.csv",   ",", escape_double = FALSE, trim_ws = TRUE)

names(mena)

mena <- merge(x=mena, y=ctrall2, by="iso3", all.x=TRUE )
mena <- mena[,c("iso  "region"          "hcr3"                "status"              "gis_name"  )]
iso <- as.character(mena$hcr3)

#pop_idp <- pop_idp[pop_idp$country.of.asylum %in% iso & pop_idp$country.of.origin %in% iso, ]
mena_idp <- pop_idp[pop_idp$country.of.origin %in% iso, ]
mena_refugee <- pop_refugee[pop_refugee$country.of.asylum %in% iso, ]
mena_returnee <- pop_returnee[pop_returnee$country.of.asylum %in% iso, ]
mena_stateless <- pop_stateless[pop_stateless$country.of.asylum %in% iso, ]

write.csv(mena_idp, "out/mena_idp.csv")
write.csv(mena_refugee, "out/mena_refugee.csv")
write.csv(mena_returnee, "out/mena_returnee.csv")
write.csv(mena_stateless, "out/mena_stateless.csv")

library(xlsx) #load the package
write.xlsx(x = mena_idp, file = "menadata.xlsx", sheetName = "mena_idp", row.names = FALSE)
workbook.sheets <- addDataFrame(x = mena_refugee, sheet = mena_refugee, row.names = FALSE, startColumn = 1) # write data to sheet starting on line 1, column 4
saveWorkbook(workbook.sheets, "menadata.xlsx") # and of course you need t
workbook.sheets <- addDataFrame(x = mena_refugee, sheet = mena_refugee, row.names = FALSE, startColumn = 1) # write data to sheet starting on line 1, column 4
saveWorkbook(workbook.sheets, "menadata.xlsx") # and of course you need t
workbook.sheets <- addDataFrame(x = mena_refugee, sheet = mena_refugee, row.names = FALSE, startColumn = 1) # write data to sheet starting on line 1, column 4
saveWorkbook(workbook.sheets, "menadata.xlsx") # and of course you need t




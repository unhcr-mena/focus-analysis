## load all R packages for the analysis
source("code/0-package.R")


#### Checking indicators

framework <- read_excel("config/UNHCR-Result-Based-Management.xlsx", sheet = 1) 
framework<- framework[ !(is.na(framework$Indicator)) ,  ]
framework<- framework[ !(framework$dup2 %in% c('dup')) ,  ]


framework.GSP <- framework[framework$GSP=="Yes", ]

### Analysis of GSP

data.impact <- read.csv("data/impact.csv")
data.impact$idrecord1 <- paste(data.impact$operationName,data.impact$planningPeriod, sep="-")
data.impact$idrecord2 <- paste(data.impact$Goal, data.impact$Population.Group, sep="-")

data.impact.GSP <- data.impact[data.impact$GSP=="true", c("idrecord1","idrecord2","Indicator","indicatorrfid","Objective","RightsGroup","planningPeriod", "operationName") ]

data.impact.GSP <- data.impact.GSP[ data.impact.GSP$operationName %in% c('Egypt' , 'Iraq' , 'Jordan' , 'Lebanon','Syrian Arab Republic' ,  'Turkey'),  ]


data.impact.GSP.cast1 <- dcast(data.impact.GSP, RightsGroup + Objective + Indicator ~ idrecord1 + idrecord2)
write.csv(data.impact.GSP.cast1, "out/gsp-analysis1.csv", row.names = FALSE)

data.impact.GSP.cast2 <- dcast(data.impact.GSP, RightsGroup + Objective + Indicator ~ idrecord1)
write.csv(data.impact.GSP.cast2, "out/gsp-analysis2.csv", row.names = FALSE)

data.impact.GSP.cast3 <- dcast(data.impact.GSP[ data.impact.GSP$planningPeriod=="2018", ], RightsGroup + Objective + Indicator ~ idrecord1 + idrecord2)
write.csv(data.impact.GSP.cast3, "out/gsp-analysis3.csv", row.names = FALSE)

data.impact.GSP.cast4 <- dcast(data.impact.GSP[ data.impact.GSP$planningPeriod=="2018", ], RightsGroup + Objective + Indicator ~ idrecord1 )
write.csv(data.impact.GSP.cast4, "out/gsp-analysis4.csv", row.names = FALSE)

table(data.impact$GSP)



















## PCA: Principal Componnent Analysis on Impact indicators

data.impact <- read.csv("data/impact.csv")
data.impact <- data.frame(data.impact[,-1], row.names=data.impact[,1])
# data.impact <- data
## create a concatenated name for the record
data.impact$idrecord <- paste(data.impact$operationName,data.impact$Goal, data.impact$Population.Group, sep="-")


names(data.impact)


## Building the index framework
## Index
## Sub-index format: si.1 
## pillar format:p.01 --> "RightsGroup" 
## Sub-pillar: sp.01 -- "Objective"
## Indicator: ind.01  --> "Indicator"

### yes No question to be coded as -1 / 1 


# Compile Indicators occurence list

indicator.unique



## Ordering the frame

data.impact <- data.impact[order(data.impact$rid,  data.impact$oid , data.impact$iid,
                                 data.impact$regionanme, data.impact$operationName), ]
names(data.impact)

## Records are id pplan, Column are impact indicator per name, cell are the value of the indicator
data.impact.ana <- data.impact[ , c("idrecord", "planningPeriod" ,"numindic", 
                                    "Year.End", "year2targetol", "year2targetop", "end2net" )]

data.impact.2015 <- data.impact.ana[ data.impact.ana$planningPeriod=="2015", ]
data.impact.2014 <- data.impact.ana[ data.impact.ana$planningPeriod=="2014", ]
data.impact.2013 <- data.impact.ana[ data.impact.ana$planningPeriod=="2013", ]

write.csv(data.impact.2015, "data/composite/dataimpact2015.csv", na = "n/a", row.names = FALSE)
write.csv(data.impact.2014, "data/composite/dataimpact2014.csv", na = "n/a", row.names = FALSE)
write.csv(data.impact.2013, "data/composite/dataimpact2013.csv", na = "n/a", row.names = FALSE)

#rm(test, test2)
#names(data.impact.2015)
#test <- melt(data.impact.2013, id.vars = c("idrecord", "Indicator"),  value.name = "Year.End", na.rm = TRUE)
#test2 <- data.impact.2013[,  c("idrecord", "Indicator", "Year.End")]
test3 <- dcast(data.impact.2013, idrecord  + Year.End ~ Indicator , fun=sum )


impact.2015.Year.End <- dcast(data.impact.2015, idrecord ~  numindic, value.var = "Year.End", fun=sum )
impact.2015.end2net <- dcast(data.impact.2015, idrecord ~  numindic, value.var = "year2targetol", fun=sum )
impact.2015.year2targetol <- dcast(data.impact.2015, idrecord ~  numindic, value.var = "year2targetol", fun=sum )
write.csv(impact.2015.Year.End, "data/composite/dataimpact2015YearEnd.csv", na = "n/a", row.names = FALSE)
write.csv(impact.2015.end2net, "data/composite/dataimpact2015end2net.csv", na = "n/a", row.names = FALSE)
write.csv(impact.2015.year2targetol, "data/composite/dataimpact2015year2targetol.csv", na = "n/a", row.names = FALSE)

impact.2014.Year.End <- dcast(data.impact.2014, idrecord ~  numindic, value.var = "Year.End" , fun=sum )
impact.2014.end2net <- dcast(data.impact.2014, idrecord ~  numindic, value.var = "year2targetol", fun=sum )
impact.2014.year2targetol <- dcast(data.impact.2014, idrecord ~  numindic, value.var = "year2targetol", fun=sum )
write.csv(impact.2014.Year.End, "data/composite/dataimpact2014YearEnd.csv", na = "n/a", row.names = FALSE)
write.csv(impact.2014.end2net, "data/composite/dataimpact2014end2net.csv", na = "n/a", row.names = FALSE)
write.csv(impact.2014.year2targetol, "data/composite/dataimpact2014year2targetol.csv", na = "n/a", row.names = FALSE)


impact.2013.Year.End <- dcast(data.impact.2013, idrecord ~  numindic, value.var = "Year.End", fun=sum )
impact.2013.end2net <- dcast(data.impact.2013, idrecord ~  numindic, value.var = "year2targetol", fun=sum )
impact.2013.year2targetol <- dcast(data.impact.2013, idrecord ~  numindic, value.var = "year2targetol", fun=sum )
write.csv(impact.2013.Year.End, "data/composite/dataimpact2013YearEnd.csv", na = "n/a", row.names = FALSE)
write.csv(impact.2013.end2net, "data/composite/dataimpact2013end2net.csv", na = "n/a", row.names = FALSE)
write.csv(impact.2013.year2targetol, "data/composite/dataimpact2013year2targetol.csv", na = "n/a", row.names = FALSE)





data.impact.cast1 <- data.frame(impact.2013.end2net[,-c(1)], row.names=impact.2013.end2net[,1])


library(FactoMineR)
library(factoextra)


print.PCA(data.impact.cast1)

data.impact.pca <- fviz_pca(data.impact.cast1)

impact.pca <- PCA(data.impact.cast1)
summary(impact.pca)


prc <- prcomp(data.impact.cast1, center=T, scale=T, retx=T)
varimax7 <- varimax(prc$rotation[,1:7])
print(varimax7)

res.pca <- prcomp(data.impact.cast2,  scale = TRUE)
res.pca <- prcomp(data.impact.cast2)

ncomp <- 2

pca_impact        <- prcomp(data.impact.cast1, center=T, scale=T)
rawLoadings     <- pca_impact$rotation[,1:ncomp] %*% diag(pca_impact$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings

# install.packages("pracma")
library("pracma")
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- scale(data.impact.cast1) %*% invLoadings
print(scores[1:5,])                   # Scores computed via rotated loadings

scores <- scale(pca_impact$x[,1:2]) %*% varimax(rawLoadings)$rotmat
print(scores[1:5,])                   # Scores computed via rotating the scores


# Extract eigenvalues/variances
get_eig(res.pca)

summary(res.pca)

res.pca.get <- get_pca(res.pca)
print(res.pca.get$contrib) # contributions of variables
# Visualize eigenvalues/variances
fviz_eig(res.pca)
# Add labels, change theme
fviz_screeplot(res.pca,  addlabels=TRUE) +
  theme_minimal()

# Graph of variables
fviz_pca_var(res.pca)
# Control variable colors using their contributions
# Use gradient color
fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint = 96) + theme_minimal()

# Graph of individuals
fviz_pca_ind(res.pca)
# Color by groups and add ellipse
fviz_pca_ind(res.pca, label="none",
             habillage=iris$Species,
             addEllipses=TRUE, ellipse.level=0.95)
# Biplot
fviz_pca_biplot(res.pca,  label="var",
                habillage=iris$Species, addEllipses=TRUE, ellipse.level=0.95)


rm(data.impact.2013,data.impact.2014,data.impact.2015,data.impact.ana,data.impactt,
   impact.2015.Year.End, impact.2015.end2net, impact.2015.year2targetol,
   impact.2014.Year.End , impact.2014.end2net,  impact.2014.year2targetol,
   impact.2013.Year.End , impact.2013.end2net ,impact.2013.year2targetol,
   framework2)

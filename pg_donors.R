setwd("C:/Users/pawlusm/Desktop/decTree") # set to where ever your csv files live

library(ggplot2)
library(caret)
library(RCurl)

x <- getURL("https://raw.githubusercontent.com/michaelpawlus/pg_donors/master/pg_donors.csv")
pg <- read.csv(text = x)

## pg <- read.csv("pg_donors.csv") ## this would be the format to read in your own file

## exploratory functions
names(pg)
str(pg)
summary(pg)

## check for duplicates
table(duplicated(pg$pg_id))

## remove duplicates
pg <- pg[order(pg$gillett, pg$degree_count, pg$stu_grp, pg$med_inc, pg$rating, decreasing=TRUE),]
pg <- pg[!duplicated(pg$pg_id),]
table(duplicated(pg$pg_id))

## remove NA rows
pg <- pg[rowSums(is.na(pg)) == 0,]

#### impute na or 0 to the median ####

## if na:
pg$med_inc <- as.numeric(as.character(pg$med_inc))
pg$med_inc[is.na(pg$med_inc)] <- median(pg$med_inc, na.rm=TRUE)

## if 0:
pg$med_inc[pg$med_inc==0] <- median(pg$med_inc, na.rm=TRUE)

## make binary classifiers into factors
pg$cae <- as.factor(pg$cae)
pg$const <- as.factor(pg$const)
pg$married <- as.factor(pg$married)
pg$evnt <- as.factor(pg$evnt)
pg$gillett <- as.factor(pg$gillett)

####  creating tables  ####

## gillett members (baseline)
table(pg$gillett)

## by rating (maybe)
cbind(table(pg$rating,pg$gillett),prop.table(table(pg$rating,pg$gillett),1))

## by median income (no)
range(pg$med_inc)
pg$med_grp <- cut(pg$med_inc, breaks = seq(10000, 220000, by = 10000), label=FALSE)
# head(pg)
cbind(table(pg$med_grp,pg$gillett),prop.table(table(pg$med_grp,pg$gillett),1))

## by age (yes)
range(pg$age, na.rm=TRUE)
pg$age_grp <- cut(pg$age, breaks = seq(10, 110, by = 10), label=FALSE)
cbind(table(pg$age_grp,pg$gillett),prop.table(table(pg$age_grp,pg$gillett),1))

## by pg_age (yes)
range(pg$pg_age, na.rm=TRUE)
pg$pg_age_grp <- cut(pg$pg_age, breaks = seq(10, 110, by = 10), label=FALSE)
cbind(table(pg$pg_age_grp,pg$gillett),prop.table(table(pg$pg_age_grp,pg$gillett),1))

## by CAE (no)
cbind(table(pg$cae,pg$gillett),prop.table(table(pg$cae,pg$gillett),1))

## by constituent type (yes)
cbind(table(pg$const,pg$gillett),prop.table(table(pg$const,pg$gillett),1))

## by event (yes -- however, gillett are invited to events)
cbind(table(pg$evnt,pg$gillett),prop.table(table(pg$evnt,pg$gillett),1))

## by married (yes)
cbind(table(pg$married,pg$gillett),prop.table(table(pg$married,pg$gillett),1))

## by student groups (maybe)
cbind(table(pg$stu_grp,pg$gillett),prop.table(table(pg$stu_grp,pg$gillett),1))

## by lubbers years (yes)  ## consecutive giving
cbind(table(pg$lub_yrs,pg$gillett),prop.table(table(pg$lub_yrs,pg$gillett),1))
pg$lub_grp <- 0
pg$lub_grp[pg$lub_yrs>10] <- 1
pg$lub_grp[pg$lub_yrs<11] <- 0
cbind(table(pg$lub_grp,pg$gillett),prop.table(table(pg$lub_grp,pg$gillett),1))

## by degree count (no)
cbind(table(pg$degree_count,pg$gillett),prop.table(table(pg$degree_count,pg$gillett),1))
pg$deg_grp <- 0
pg$deg_grp[pg$degree_count>1] <- 2
pg$deg_grp[pg$degree_count==1] <- 1
cbind(table(pg$deg_grp,pg$gillett),prop.table(table(pg$deg_grp,pg$gillett),1))

## by child count (no)
cbind(table(pg$child_count,pg$gillett),prop.table(table(pg$child_count,pg$gillett),1))
pg$ch_grp <- 0
pg$ch_grp[pg$child_count>0] <- 1
pg$ch_grp[pg$child_count==0] <- 0
cbind(table(pg$ch_grp,pg$gillett),prop.table(table(pg$ch_grp,pg$gillett),1))

## by log total giving (yes)
pg$logtg_grp <- cut(pg$log_tg, breaks = seq(0, 9, by = 1), label=FALSE)
cbind(table(pg$logtg_grp,pg$gillett),prop.table(table(pg$logtg_grp,pg$gillett),1))

## by log prior giving (yes)
pg$log_pgiv <- log10(pg$prior_giv+1)
pg$log_pgiv_grp <- cut(pg$log_pgiv, breaks = seq(0, 9, by = 1), label=FALSE)
cbind(table(pg$log_pgiv_grp,pg$gillett),prop.table(table(pg$log_pgiv_grp,pg$gillett),1))

#### Exploratory Plots ####

qplot(age, lub_yrs,data=pg,color=married,size=gillett,shape=gillett,main="Married PG Donors")
qplot(age, lub_yrs,data=pg,color=const,size=gillett,shape=gillett,main="Constituency PG Donors")
qplot(age, lub_yrs,data=pg,color=const,size=gillett,shape=married,main="Constituency/Married PG Donors")
qplot(pg_age, log_pgiv,data=pg,color=const,shape=married,main="Constituency/Married PG Donors")
qplot(age, lub_yrs,data=pg,color=cae,size=gillett,shape=married,main="CAE/Married PG Donors")
qplot(pg_age, log_pgiv,data=pg,color=cae,size=gillett,shape=married,main="CAE/Married PG Donors")



setwd("C:/Users/pawlusm/Desktop/decTree")

pg <- read.csv("pg_donors.csv")

library(ggplot2)
library(caret)
library(lubridate)

## exploratory functions
names(pg)
str(pg)
summary(pg)

## convert date column to date type (doesn't work)
pg$birth_date <- as.Date(pg$birth_date)

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
head(pg)
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

## by lubbers years (yes)
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


#### create a subset ####

pg_sub <- pg[pg$age>49,]
summary(pg_sub)
pg_sub <- pg_sub[pg_sub$married==1,]
table(pg_sub$gillett)
prop.table(table(pg_sub$gillett))

#### dummy variables  (not working at the moment)

# dummies <- dummyVars(gillett ~ const, data = pg_sub)
# head(predict(dummies, newdata = pg_sub, fullRank = TRUE))
# constcd <- as.data.frame(predict(dummies, newdata = pg_sub, fullRank = TRUE))
# pg_sub <- cbind(pg_sub, as.data.frame(predict(dummies, newdata = pg_sub)))
# 
# pg_sub$evnt <- as.numeric(as.character(pg_sub$evnt))

## create some models
set.seed(325)
inTrain <- createDataPartition(y=pg_sub$gillett,
                               p=0.7, list=FALSE)
training <- pg_sub[inTrain,]
testing <- pg_sub[-inTrain,]
dim(training)
dim(testing)

ctrl <- trainControl(method = "repeatedcv", repeats = 10, number = 3)

lmMod <- train(gillett ~ rating  + age + lub_yrs + evnt + log_tg, 
               data=training, 
               method = "glm",
               preProc = c("center", "scale"),
               trControl = ctrl,
               family="binomial"
)
summary(lmMod)

confint(lmMod$finalModel)
confint.default(lmMod$finalModel)
exp(coef(lmMod$finalModel))
exp(cbind(OR = coef(lmMod$finalModel), confint(lmMod$finalModel)))

lmMod <- train(gillett ~ rating  + age + lub_yrs + evnt + log_tg, 
               data=training, 
               method = "gbm",
               preProc = c("center", "scale"),
               verbose=FALSE
)
summary(lmMod)
lmMod$finalModel

lmMod <- train(gillett ~ rating  + age + lub_yrs + evnt + log_tg + const + cae, 
               data=training, 
               method = "rf",
               preProc = c("center", "scale")
)
summary(lmMod)
lmMod$finalModel
lmMod

lmPred <- predict(lmMod, newdata = data.frame(testing)) 
lmTest <- cbind(testing,lmPred)
confusionMatrix(lmPred,testing$gillett)

#### try a combined model  ####

set.seed(62433)
model_rf = train(gillett ~ rating  + age + lub_yrs + evnt + log_tg + const + cae, method = 'rf', data = training)
model_gbm = train(gillett ~ rating  + age + lub_yrs + evnt + log_tg + const + cae, method = 'gbm', data = training,verbose=FALSE)


pred_rf = predict(model_rf, training)
pred_gbm = predict(model_gbm, training)


comb_data = data.frame(rf = pred_rf, gbm = pred_gbm, gillett = training$gillett)
model_comb = train(gillett ~ ., method = 'rf', data = comb_data)

pred_rf_test = predict(model_rf, testing)
pred_gbm_test = predict(model_gbm, testing)
comb_data_test = data.frame(rf = pred_rf_test, gbm = pred_gbm_test, gillett = testing$gillett)
pred_comb_test = predict(model_comb, comb_data_test)

accuracy_rf = sum(pred_rf_test == testing$gillett) / length(pred_rf_test)
accuracy_gbm = sum(pred_gbm_test == testing$gillett) / length(pred_gbm_test)


accuracy_comb = sum(pred_comb_test == comb_data_test$gillett) / length(pred_comb_test)


## check accuracy 
confusionMatrix(pred_rf_test,testing$gillett)
confusionMatrix(pred_gbm_test,testing$gillett)
confusionMatrix(pred_comb_test,testing$gillett)

## plot some results
model_gbm
plot(model_gbm, metric = "Kappa")
ggplot(model_gbm, metric = "Kappa")
summary(model_gbm,cBars=5)

ggplot(model_rf, metric = "Kappa")
ggplot(model_rf)
plot(varImp(model_rf))

rf.test <- cbind(testing,pred_rf_test)
str(rf.test)

rf.test <- rf.test[order(rf.test$lmPred2, decreasing = TRUE),]
options("scipen"=20)
head(rf.test, n=25) ## write a csv with this

write.csv(rf.test,file="pg_prospects.csv",row.names=FALSE)

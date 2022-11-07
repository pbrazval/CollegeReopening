# Copyright Pedro H. Braz Vallocci and Toshiya Yoshida

rm(list=ls())

library(mltools)
library(data.table)
library(glmnet)
library(gamlr)

load("clean_data.Rda")
clean_data_2 = clean_data[clean_data$Category!="Public, 2-year",]
expl_vars = clean_data[,c(-2, -4, -5, -7, -8, -10, -11, -16, -24, -43, -47)]
rownames(expl_vars) = expl_vars[,1]
expl_vars = expl_vars[,-1]
newdata <- one_hot(as.data.table(expl_vars))
newdata2 = as.data.frame(scale(newdata))
newdata$Mode = clean_data$Mode

model_md = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_MD` + `State_TX`  + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 , data = newdata, family = binomial("logit"))

#model_nolog = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` +  `State_TX`  + `Endowment.per.Full.Time.Student` + POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

model_logendw  = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_TX` +  `log.Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

model_std = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` +  `State_TX`  + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

#model9 = glm(Mode ~ Category + HorT + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

# Repeating Evaluation of predictive performance
holdout <- function(data, trainprop, repeat_times, formula){
  accuracy = rep(0,repeat_times)
  n = dim(data)[1]
  for (i in 1:repeat_times){
    trainid = sample(1:n, trainprop*n, replace=F)
    train = data[trainid, ]
    test = data[-trainid, ]
    modeltest = glm(formula, data = train, family = binomial("logit"))
    probs = predict(modeltest, newdata=test, type="response")
    pred = rep("online", length(probs))
    pred[probs > .5] = "in-person"
    table(pred, test$Mode)[2:1,]
    accuracy[i] = mean(pred==test$Mode)
  }
  return(accuracy)}

model_1 = holdout(newdata,0.9,5000,model_std$formula)
hist(model_1)
summary(model_1)

model_2 = holdout(newdata,0.9,5000,model_md$formula)
hist(model_2)
summary(model_2)

model_3 = holdout(newdata,0.9,5000,model_logendw$formula)
hist(model_3)
summary(model_3)


#glm.probs=predict(model,type="response")
#glm.pred=rep(0,nrow(newdata))
#glm.pred[glm.probs >.5]=1
#table(glm.pred,newdata$Mode)

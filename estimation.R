rm(list=ls())

library(mltools)
library(data.table)
library(glmnet)
library(gamlr)
load("clean_data.Rda")

#clean_data = clean_data[clean_data$Category!="Public, 2-year",]
clean_data$GRND_TOTAL_REVENUE[is.na(clean_data$GRND_TOTAL_REVENUE)] = 0

model = glm(Mode ~ Category + COVID_Last60days_per1k + COVID_total_per1k + Hospital + HorT + Enrollment + State + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + Percentage.of.International.Students + per_dem + GRND_TOTAL_REVENUE, data = clean_data, family = binomial("logit"))

model2 = glm(Mode ~ Category + COVID_Last60days_per1k + COVID_total_per1k + Hospital + HorT + Enrollment + State + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem + GRND_TOTAL_REVENUE, data = clean_data, family = binomial("logit"))

model3 = glm(Mode ~ Category + COVID_Last60days_per1k + COVID_total_per1k + Hospital + HorT + Enrollment + State + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

model4 = glm(Mode ~ Category + COVID_Last60days_per1k + COVID_total_per1k + Hospital + HorT + State + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

model5 = glm(Mode ~ Category + COVID_Last60days_per1k + COVID_total_per1k + HorT + State + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

model6 = glm(Mode ~ Category + COVID_total_per1k + HorT + State + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

model7 = glm(Mode ~ Category + HorT + State + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

model8 = glm(Mode ~ Category*State + HorT + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

model9 = glm(Mode ~ Category + HorT + Undergraduate.Admit.Rate + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

model10 = glm(Mode ~ Category + State + Endowment.per.Full.Time.Student + per_dem, data = clean_data, family = binomial("logit"))

expl_vars = clean_data[,c(-2, -4, -5, -7, -8, -10, -11, -16, -24, -43, -47)]
rownames(expl_vars) = expl_vars[,1]
expl_vars = expl_vars[,-1]
newdata <- one_hot(as.data.table(expl_vars))
#num_data = clean_data[,c(-2, -3, -4, -5, -6, -7, -8, -10, -11, -15, -16, -24, -43, -47)]
# rownames(num_data) = clean_data[,1]
# num_data = num_data[,-1]
# pr.out = prcomp(num_data, scale = TRUE)
# round(pr.out$rotation, 2)
# model = glm((clean_data$Mode) ~ as.numeric(clean_data$Category) + clean_data$COVID_Last60days 
#             + as.numeric(clean_data$per_dem) + (clean_data$HorT) + (clean_data$Hospital)
#             + (clean_data$State))

model = gamlr(scale(newdata), clean_data$Mode, family = "binomial", gamma = 0 )

cv.model = cv.gamlr(scale(newdata), clean_data$Mode, family = "binomial", gamma = 0 )

newdata2 = as.data.frame(scale(newdata))
newdata$Mode = clean_data$Mode
newdata2$Mode = clean_data$Mode

model_lasso = glm(Mode ~ Category + State + Credential.Score + `Instructional.Wages.per.Full-Time.Student.PCT.Rank`+ Endowment.per.Full.Time.Student + log(POP_ESTIMATE_2019) + per_dem, data = clean_data, family = binomial("logit"))

model_lasso2 = glm(Mode ~ Category + State + `Instructional.Wages.per.Full-Time.Student.PCT.Rank`+ Endowment.per.Full.Time.Student + POP_ESTIMATE_2019 + per_dem, data = clean_data, family = binomial("logit"))

model_lasso3 = glm(Mode ~ Category + State + `Instructional.Wages.per.Full-Time.Student.PCT.Rank`+ Endowment.per.Full.Time.Student  + per_dem, data = clean_data, family = binomial("logit"))

model_lasso4 = glm(Mode ~ Category + State + Endowment.per.Full.Time.Student  + per_dem, data = clean_data, family = binomial("logit"))

model_lasso5 = glm(Mode ~ State + Category*Endowment.per.Full.Time.Student*per_dem, data = clean_data, family = binomial("logit"))

model_lasso6 = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_DC` + `State_MD` + `State_TX` + `State_VT`+ `Credential.Score` + `Instructional.Wages.per.Full-Time.Student.PCT.Rank` + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

model_lasso7 = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_MD` + `State_TX` + `State_VT`+ `Instructional.Wages.per.Full-Time.Student.PCT.Rank` + `Endowment.per.Full.Time.Student` + `POP_ESTIMATE_2019` + `per_dem` , data = newdata2, family = binomial("logit"))

model_lasso8 = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_MD` + `State_TX` +  `Credential.Score` + `Instructional.Wages.per.Full-Time.Student.PCT.Rank` + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

model_lasso9 = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_MD` + `State_TX` +  `Instructional.Wages.per.Full-Time.Student.PCT.Rank` + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

model_lasso10 = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_MD` + `State_TX` + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

model_lasso11 = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_MD` + `State_TX` +  `Credential.Score` + `Instructional.Wages.per.Full-Time.Student.PCT.Rank` + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

model_lasso12 = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_MD` + `State_TX`  + `Instructional.Wages.per.Full-Time.Student.PCT.Rank` + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

model_lasso13 = glm(Mode ~ `Category_Private nonprofit, 4-year` +  `State_CA` + `State_MD` + `State_TX`  + `Endowment.per.Full.Time.Student` + log.POP_ESTIMATE_2019 + `per_dem` , data = newdata, family = binomial("logit"))

pr.out=prcomp(scale(newdata))

# anova(model)
# anova(model, test="Chisq")

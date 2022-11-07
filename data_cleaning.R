# Copyright Pedro H. Braz Vallocci and Toshiya Yoshida

library(openxlsx)
library(ggplot2)
library(nnet)
library(AER)
library(plyr)

rm(list=ls())

# Loading datasets
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/STAT204 Group project")
reopening = read.csv('scraping/reo.csv', header = FALSE)
galloway = read.xlsx("Galloway_renamed.xlsx")[,c(-1)]
colnames(galloway)[1] = "College"
pop = read.xlsx('PopulationEstimates.xlsx',startRow=3)[,c(4,21)]

# Creating datasets: df2 and df3
# df2 is the datasets from The Chronicle of Higher Education (2804 colleges)
# df3 is the combined datasets of df2 and galloway (343 colleges)
df = reopening[,1:11]
covid_total = reopening[,12]
covid_last60days = apply(reopening[,13:72], 1, sum)
df2 = cbind(df,covid_total, covid_last60days)
colnames(df2) = c("College","Category","County","URL","Mode","Hospital","HorT","Quarter","Enrollment","Gender","Race","COVID_total", "COVID_Last60days")

df2 = subset(df2, Mode!="Other")
df2["College"] = lapply(df2["College"], gsub, pattern="&amp;", replacement = "&")
df2["College"] = lapply(df2["College"], gsub, pattern=" at ", replacement = "-")
df2["County"] = lapply(df2["County"], gsub, pattern="City", replacement = "city")
pop[pop$County == "Doña Ana County, NM",]$County = "Dona Ana County, NM"

# galloway["College"] = lapply(galloway["College"], gsub, pattern=" at ", replacement = "-")
# galloway["College"] = lapply(galloway["College"], gsub, pattern="The ", replacement = "")
# 
# for (oldname in setdiff(galloway$College, df2$College)){
#   my.name <- readline(prompt=paste("Find the best name for the college ", oldname, ":"))
#   galloway[galloway$College==oldname, "College"] = my.name
# }

df2["Enrollment"] = as.integer(df2$Enrollment)
df3 = merge(df2, galloway)
dftm = merge(df3, pop, by='County')
dftm[,"COVID_total_per1k"] = 1000*dftm[,"COVID_total"] /  dftm[,"POP_ESTIMATE_2019"]
dftm[,"COVID_Last60days_per1k"] = 1000*dftm[,"COVID_Last60days"] /  dftm[,"POP_ESTIMATE_2019"]
dftm[,"Mode"] = as.factor(dftm[,"Mode"]) 
dftm = dftm[dftm$Category!="Public, 2-year",]
dftm[,"Category"] = as.factor(dftm[,"Category"]) 
dftm[,"Hospital"] = as.factor(dftm[,"Hospital"]) 
dftm[,"HorT"] = as.factor(dftm[,"HorT"]) 
dftm[,"ModeFull"] = factor(dftm[,"Mode"], levels = c("Fully online", "Primarily online", "Hybrid", "Primarily in person", "Fully in person"), order = TRUE)
dftm[,"Mode"] = factor(dftm[,"Mode"], levels = c("Fully online", "Primarily online", "Hybrid", "Primarily in person", "Fully in person"), labels = c("online", "online", "in-person", "in-person", "in-person"),  order = TRUE)

dup = duplicated(dftm[,c("College", "County")])
dftm = dftm[!dup,]

# ncaa_short = ncaa[,c("institution_name", "GRND_TOTAL_REVENUE")]
# names(ncaa)[2] = "College"
# ncaa["College"] = lapply(ncaa["College"], gsub, pattern=" at ", replacement = "-")
# ncaa["College"] = lapply(ncaa["College"], gsub, pattern="The ", replacement = "")
# 
# for (oldname in setdiff(galloway$College, ncaa$College)){
#   my.name <- readline(prompt=paste("Find the best name for the college ", oldname, ":"))
#   ncaa[ncaa$College==oldname, "College"] = my.name
# }
vote = read.csv('2020Presidential.csv', header = FALSE)
names(vote) = vote[c(1),]
vote = vote[c(-1),]
names(vote)[1] = "County"
vote = vote[,c("County", "per_dem")]
dftm["County"] = lapply(dftm["County"], gsub, pattern="city", replacement = "City")
vote["County"] = lapply(vote["County"], gsub, pattern="St ", replacement = "St. ")
dftm["County"] = lapply(dftm["County"], gsub, pattern="Parish", replacement = "County")
dftm["County"] = lapply(dftm["County"], gsub, pattern="DeKalb", replacement = "De Kalb")
dftm["County"] = lapply(dftm["County"], gsub, pattern="DuPage", replacement = "Du Page")
dftm["County"] = lapply(dftm["County"], gsub, pattern="\\'s", replacement = "s")
dftm["County"] = lapply(dftm["County"], gsub, pattern="District of Columbia, DC", replacement = "Washington County, DC")
vote["County"] = lapply(vote["County"], gsub, pattern="City County", replacement = "City")
dftm_vote = merge(dftm, vote, by='County')

ncaa = read.xlsx("NCAA_renamed.xlsx")
# names(ncaa)[2] = 'College'
# ncaa["College"] = lapply(ncaa["College"], gsub, pattern="The ", replacement = "")
# ncaa["College"] = lapply(ncaa["College"], gsub, pattern=" at ", replacement = "-")
# ncaa["College"] = lapply(ncaa["College"], gsub, pattern="-Main Campus", replacement = "")
ncaa_short = ncaa[,c("College", "GRND_TOTAL_REVENUE")]

clean_data = merge(dftm_vote, ncaa_short, by="College", all.x = TRUE)
clean_data[,"State"] = as.factor(clean_data[,"State"]) 
clean_data[,"per_dem"] = as.numeric(clean_data[,"per_dem"]) 

dup = duplicated(clean_data[,c("College")])
clean_data = clean_data[!dup,]
clean_data$GRND_TOTAL_REVENUE[is.na(clean_data$GRND_TOTAL_REVENUE)] = 1
clean_data[, "log.Enrollment"] = log(clean_data$Enrollment)
clean_data[, "log.Full.Time.Enrollment"] = log(clean_data$Full.Time.Enrollment)
clean_data[, "log.GRND_TOTAL_REVENUE"] = log(clean_data$GRND_TOTAL_REVENUE + 1)
clean_data[, "log.POP_ESTIMATE_2019"] = log(clean_data$POP_ESTIMATE_2019)
clean_data[, "log.Endowment.per.Full.Time.Student"] = log(clean_data$Endowment.per.Full.Time.Student)
clean_data[, "log.Instructional.Wages.per.Full-Time.Student"] = log(clean_data$`Instructional.Wages.per.Full-Time.Student`)


# for (oldname in setdiff(dftm_vote$College, ncaa$College)){
#    my.name <- readline(prompt=paste("Find the best name for the college ", oldname, ":"))
#    if (my.name == "0")
#    {
#      next
#    }
#    else
#    {
#    ncaa[ncaa$College==my.name, "College"] = oldname
#    }
# }

save(clean_data,file="clean_data.Rda")

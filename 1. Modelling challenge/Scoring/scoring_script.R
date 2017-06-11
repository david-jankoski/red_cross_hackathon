
#Adjust the file path & beware of the correct separator (your Excel might store csv with ',' or ';')

#getwd()
setwd("C:/Users/JannisV/Desktop/Hackathon/1. Modelling challenge/Scoring")
actuals <- read.csv(file="./actuals.csv", header=TRUE, sep=",")
#View(actuals)
preds <- read.csv(file="./predictions.csv", header=TRUE, sep=",")
#View(preds)

data <- merge(x = actuals, y = preds, by.x=c("typhoon_name","admin_L3_code"), by.y=c("typhoon_name","admin_L3_code"))
#View(data)

R2 <- 1 - (sum((data$actuals-data$predictions)^2)/sum((data$actuals-mean(data$actuals))^2))




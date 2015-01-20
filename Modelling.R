# (c) Isidro Hidalgo Arellano
# https://www.freelancer.com.es/u/ishidalgo.html
# mail: ishidalgo@gmail.com

rm(list = ls(all.names = TRUE))

load("dataProcessed.RData")
data = dataProcessed[dataProcessed$Date>"2011-12-31",
                     c("HalfScore", "HL0", "HL1", "HL2", "HL3", "HL4",
                       "AV0", "AV1", "AV2", "AV3","AV4",
                       "p",
                       "Homescore", "Homereceived", "HomereceivedLast",
                       "Awayscore", "Awayreceived",
                       "GoalsPredicted", "GoalsPredictedLast", "Score")]
rm(dataProcessed)

data = data[complete.cases(data),]

library(gbm)
library(caret)

gbmGrid = expand.grid(n.trees = seq(100, 700, by = 200),
     interaction.depth = seq(1,7, by = 2),
     shrinkage = seq(0, 0.2, by = 0.05))
x.train = data[,names(data) != "Score"]
gbmTune = train(x = x.train, y = data$Score,
                method = "gbm",
                tuneGrid = gbmGrid,
                trControl = trainControl(method = "cv", number = 5))
save(gbmTune, file = "gbmTune.RData")

finalModel = gbmTune$finalModel
gbmResults = gbmTune[[4]]

which.min(gbmResults$RMSE)

gbmResultsS0.1 = gbmResults[gbmResults$shrinkage == .1,]
plot(gbmResults$RMSE[gbmResults$n.trees == 100] ~ gbmResults$shrinkage[gbmResults$n.trees == 100])

gbmModel = gbm.fit(x.train, y.train, distribution = "gaussian")

plot(finalModel$fit, data$Score)

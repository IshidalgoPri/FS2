# (c) Isidro Hidalgo Arellano
# https://www.freelancer.com.es/u/ishidalgo.html
# mail: ishidalgo@gmail.com

# Cleaning workspace
rm(list = ls(all.names = TRUE))

# Loading auxiliary functions
source('AuxiliaryFunctions.R')

# Loading past data cleaned
load("dataProcessed.RData") # We will save the processed data because the calculation of all variables is hard for the computer. Saving the past and cleaned data saves a lot of work...

# Loading new data
fichero = dir(path = "Data/New", full.names = TRUE)[1]
long = nchar(fichero)
renombrar = is.na(as.numeric(substr(fichero, long - 7, long - 4)))
if (renombrar) {RenameFiles("Data/New", moreName = "_2014")}

additionalData = LoadData("Data/New") # When a year is finished its directory must be put away into "../Data/"

# Checking if there is new data or all data is already processed ###############
newdata = compare.dataframe(df1 = dataProcessed, df2 = additionalData,
                            columns = c("Date","HomeTeam", "AwayTeam"))
if (sum(newdata) != 0){ # THERE'S NEW DATA
    # Processing new data ######################################################
    additionalData = additionalData[newdata,]
    Data = rbind(dataProcessed, additionalData)
    dataProcessed = ProcessData(Data)
    save(dataProcessed, file = "dataProcessed.RData")
    }
# END OF PROCESSING ############################################################

# Selecting matches to bet in the next 7 days (default)
predictData = scrappFutureMatches()
Data = rbind(dataProcessed, predictData)
dataPredicted = ProcessData(Data)




# Variable scale _______________________________________________________________
dataSim = Data[complete.cases(Data[, !(names(Data) %in% c("PredictedScore", "BootScore"))]),]
variablesToUse = c("HTHG", "HTAG", "BbMxMore2.5", "BbAvMore2.5", "HalfScore",
                   "HL0", "HL1", "HL2", "HL3", "HL4",
                   "AV0", "AV1", "AV2", "AV3", "AV4",
                   "p",
                   "Homescore", "Homereceived", "HomescoreLast", "HomereceivedLast",
                   "Awayscore", "Awayreceived", "AwayscoreLast", "AwayreceivedLast",
                   "GoalsPredicted", "GoalsPredictedLast",
                   "InvMxMore2.5", "InvAvMore2.5",
                   "BootScore")
scaleData = ScalebyDiv(dataSim[, variablesToUse], div = dataSim$Div)
scaleCenters = scaleData$centers
scaleScale = scaleData$scales
scaleData = scaleData$data
#_______________________________________________________________________________


variablesToUse = c("HTHG", "HTAG", "HL0", "HL1", "HL2", "AV4", "GoalsPredicted")
variablesToUse = paste0(variablesToUse, "S")
dataScaledSome = scaleData[, variablesToUse]


DataInteractionsSome = CalculateInteractions(dataScaledSome, variablesToUse)
data = cbind(Score = dataSim$Score, dataScaledSome, DataInteractionsSome)

PredictedScore = simulate(datos = data, model = "glm", VarDate = dataSim$Date, VarDiv = dataSim$Div)
summary(PredictedScore)
dataSim$PredictedScore = PredictedScore
dataSimREG = dataSim
save(dataSimREG, file = "dataSimREG.RData")
plot(dataSimREG$Score, dataSimREG$PredictedScore)
boxplot(dataSimREG$PredictedScore ~ dataSimREG$More2)
hist(dataSimREG$PredictedScore[dataSimREG$More2 == TRUE & dataSimREG$PredictedScore > 2], breaks = 50)
table(dataSimREG$PredictedScore > 2, dataSimREG$More2)
table(dataSimREG$PredictedScore > 3, dataSimREG$More2)
table(dataSimREG$PredictedScore > 4, dataSimREG$More2)

# library(randomForest)
# PredictedScore = simulate(datos = data, VarDate = dataSim$Date, VarDiv = dataSim$Div)
# summary(PredictedScore)
# dataSim$PredictedScore = PredictedScore
# dataSimRF = data
# save(dataSimRF, file = "dataSimRF.RData")
# plot(dataSimRF$Score, dataSimRF$PredictedScore)
# boxplot(dataSimRF$PredictedScore ~ dataSimRF$More2)
# hist(dataSimRF$PredictedScore[dataSimRF$More2 == TRUE & dataSimRF$PredictedScore > 0], breaks = 50)
#
#
#
# variablesToUse = c("HTHG", "HTAG", "BbMxMore2.5", "BbAvMore2.5", "HalfScore",
#                    "HL0", "HL1", "HL2", "HL3", "HL4",
#                    "AV0", "AV1", "AV2", "AV3", "AV4",
#                    "p",
#                    "Homescore", "Homereceived", "HomescoreLast", "HomereceivedLast",
#                    "Awayscore", "Awayreceived", "AwayscoreLast", "AwayreceivedLast",
#                    "GoalsPredicted", "GoalsPredictedLast",
#                    "InvMxMore2.5", "InvAvMore2.5",
#                    "BootScore")
# variablesToUse = paste0(variablesToUse, "S")
# DataInteractions = CalculateInteractions(scaleData, variablesToUse)
# data = cbind(Score = dataSim$Score, scaleData, DataInteractions)
#
# PredictedScore = simulate(datos = data, div = "B1", model = "glm", VarDate = dataSim$Date, VarDiv = dataSim$Div)
# dataSim$PredictedScore = PredictedScore
# summary(PredictedScore)
# dataSimREGall = dataSim
# save(dataSimREGall, file = "dataSimREGall.RData")
# plot(dataSimREGall$Score, dataSimREGall$PredictedScore)
# boxplot(dataSimREGall$PredictedScore ~ dataSimREGall$More2)
# hist(dataSimREGall$PredictedScore[dataSimREGall$More2 == TRUE & dataSimREGall$PredictedScore > 0], breaks = 50)
#
#
# data = simulate(datos = dataSim, VarDate = dataSim$Date, VarDiv = dataSim$Div)
# summary(data$PredictedScore)
# dataSimRFall = data
# save(dataSimRFall, file = "dataSimRFall.RData")
# plot(dataSimRFall$Score, dataSimRFall$PredictedScore)
# boxplot(dataSimRFall$PredictedScore ~ dataSimRFall$More2)
# hist(dataSimRFall$PredictedScore[dataSimRFall$More2 == TRUE & dataSimRFall$PredictedScore > 0], breaks = 50)


################################################################################
# ANALYSIS OF SIMULATIONS ######################################################
################################################################################
load("dataSimREG.RData")
data = dataSimREG[dataSimREG$Date>'2013-01-01',]
rm(dataSimREG)

apply(is.na(data),2,sum) # NA in BootScore

hist(data$PredictedScore)
plot(data$PredictedScore, data$Score)
cor(data$Score, data$PredictedScore)
boxplot(data$PredictedScore ~ data$More2)

# Matches with HalfScore == 0
data0 = data[data$HalfScore == 0, ]
hist(data0$PredictedScore)
plot(data0$PredictedScore, data0$Score)
cor(data0$Score, data0$PredictedScore)
boxplot(data0$PredictedScore ~ data0$More2)

# Matches with HalfScore == 1
data1 = data[data$HalfScore == 1, ]
hist(data1$PredictedScore)
plot(data1$PredictedScore, data1$Score)
cor(data1$Score, data1$PredictedScore)
boxplot(data1$PredictedScore ~ data1$More2)
boxplot(data1$PredictedScore[data1$PredictedScore>3.3] ~ data1$More2[data1$PredictedScore>3.3])
table(data1$More2[data1$PredictedScore>3.3])

# Matches with HalfScore == 2
data2 = data[data$HalfScore == 2, ]
hist(data2$PredictedScore)
plot(data2$PredictedScore, data2$Score)
cor(data2$Score, data2$PredictedScore)
boxplot(data2$PredictedScore ~ data2$More2)
boxplot(data2$PredictedScore[data2$PredictedScore>4] ~ data2$More2[data2$PredictedScore>4])
table(data2$More2[data2$PredictedScore>4])
oddsAv = data2$BbAvMore2.5[data2$PredictedScore>4]
hist(oddsAv)
oddsMx = data2$BbMxMore2.5[data2$PredictedScore>4]
hist(oddsMx)



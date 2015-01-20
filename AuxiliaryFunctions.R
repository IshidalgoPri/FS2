# (c) Isidro Hidalgo Arellano
# https://www.freelancer.com.es/u/ishidalgo.html
# mail: ishidalgo@gmail.com

LoadData = function(directory, ...) {
    # Process all files in the directory specified. Files must be downloaded from
    # http://www.football-data.co.uk/data.php in .CSV format
    # INPUT: directory (character) in which the .CSV files are
    # OUTPUT: data frame with cleaned and structured data

    archivos = dir(path = directory, full.names = TRUE, recursive = TRUE)

    data = read.csv(archivos[1], stringsAsFactors = FALSE)[,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG",
                                                              "FTAG", "FTR", "HTHG","HTAG", "HTR", "BbMx.2.5",
                                                              "BbAv.2.5")]

    csvLoc = gregexpr(pattern ='.csv',archivos[1])[[1]][1]-1
    data$Year = as.numeric(substr(archivos[1], start = csvLoc - 3, stop = csvLoc))

    for (i in 2:length(archivos)){
        newData = read.csv(archivos[i], stringsAsFactors = FALSE)[,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG",
                                                                     "FTAG", "FTR", "HTHG","HTAG", "HTR", "BbMx.2.5",
                                                                     "BbAv.2.5")]
        csvLoc = gregexpr(pattern ='.csv',archivos[i])[[1]][1]-1
        newData$Year = as.numeric(substr(archivos[i], start = csvLoc - 3, stop = csvLoc))

        data = rbind(data, newData)
    }
    data$HalfScore = as.numeric(data$HTHG + data$HTAG)
    data$Score = as.numeric(data$FTHG + data$FTAG)
    data$More2 = as.factor(data$Score > 2)

    data$Date = as.Date(data$Date, "%d/%m/%y")
    data = data[with(data, order(Date)),]
    row.names(data) = NULL
    data$Hour = NA
    data = cbind(data[,c("Div", "Date", "Hour", "HomeTeam", "AwayTeam",
                         "FTHG", "FTAG", "FTR",
                         "HTHG", "HTAG", "HTR",
                         "BbMx.2.5", "BbAv.2.5",
                         "Year", "HalfScore", "Score", "More2")],
                 matrix(NA, nrow = dim(data)[1], ncol = 26))
    names(data)= c("Div", "Date", "Hour", "HomeTeam", "AwayTeam",
                  "FTHG", "FTAG", "FTR",
                  "HTHG", "HTAG", "HTR",
                  "BbMxMore2.5", "BbAvMore2.5",
                  "Year", "HalfScore", "Score", "More2",
                  "HL0", "HL1", "HL2", "HL3", "HL4",
                  "AV0", "AV1", "AV2", "AV3", "AV4",
                  "p",
                  "Homescore", "Homereceived",
                  "HomescoreLast", "HomereceivedLast",
                  "Awayscore", "Awayreceived",
                  "AwayscoreLast", "AwayreceivedLast",
                  "GoalsPredicted", "GoalsPredictedLast",
                  "InvMxMore2.5", "InvAvMore2.5",
                  "BootScore", "Processed", "ToPredict")


    data$HomeTeam = gsub(" ", "", data$HomeTeam)
    data$AwayTeam = gsub(" ", "", data$AwayTeam)

    data$Processed = 0
    data$ToPredict = 0
    data$Match = paste(data$HomeTeam, data$AwayTeam, sep = "-")

    return(data)
}



ProcessData = function(datos,...) {
    from_date = min(datos$Date[datos$Processed == 0])
    print(paste0("Processing from ", from_date, "..."))

    hometeams = table(datos$HomeTeam[datos$Date >= from_date])
    awayteams = table(datos$AwayTeam[datos$Date >= from_date])
    teams = c(attr(hometeams, "dimnames")[[1]], attr(awayteams, "dimnames")[[1]])
    teams = unique(teams)

    datos = datos[order(datos$Date),]
    rownames(datos) = 1:nrow(datos)

    for (team in teams) {
        print(paste0("Processing data of ", team, "..."))

        idx = as.numeric(rownames(datos))[datos$HomeTeam == team]
        if (length(idx) > 1) { # HomeTeam as local
            data = datos[idx, ]
            from_idx = which(idx == as.numeric(rownames(data[data$Processed == 0,]))[1])

            for (i in from_idx:nrow(data)){
                if (i == 1) next else {
                    scores = data[1:(i-1),"FTHG"]
                    n_matches = sum(!is.na(scores))
                    data[i, "HL0"] = sum(scores == 0, na.rm = TRUE) / n_matches * 100
                    data[i, "HL1"] = sum(scores == 1, na.rm = TRUE) / n_matches * 100
                    data[i, "HL2"] = sum(scores == 2, na.rm = TRUE) / n_matches * 100
                    data[i, "HL3"] = sum(scores == 3, na.rm = TRUE) / n_matches * 100
                    data[i, "HL4"] = sum(scores > 3, na.rm = TRUE) / n_matches * 100

                    data[i, "Homescore"] = BootstrapScores(scores, last = TRUE, nlast = 15)
                    data[i, "HomescoreLast"] = BootstrapScores(scores, last = TRUE)

                    scores = data[1:(i-1),"FTAG"]
                    data[i, "Homereceived"] = BootstrapScores(scores, last = TRUE, nlast = 15)
                    data[i, "HomereceivedLast"] = BootstrapScores(scores, Last = TRUE)
                }
            }
            datos[idx, ] = data
        }

        idx = as.numeric(rownames(datos))[datos$AwayTeam == team]
        if (length(idx) > 1) { # AwayTeam as visitor
            data = datos[idx, ]
            from_idx = which(idx == as.numeric(rownames(data[data$Processed == 0,]))[1])

            for (i in from_idx:nrow(data)){
                if (i == 1) next else {
                    scores = data[1:(i-1),"FTAG"]
                    n_matches = sum(!is.na(scores))
                    data[i, "AV0"] = sum(scores == 0, na.rm = TRUE) / n_matches * 100
                    data[i, "AV1"] = sum(scores == 1, na.rm = TRUE) / n_matches * 100
                    data[i, "AV2"] = sum(scores == 2, na.rm = TRUE) / n_matches * 100
                    data[i, "AV3"] = sum(scores == 3, na.rm = TRUE) / n_matches * 100
                    data[i, "AV4"] = sum(scores > 3, na.rm = TRUE) / n_matches * 100

                    data[i, "Awayscore"] = BootstrapScores(scores, last = TRUE, nlast = 15)
                    data[i, "AwayscoreLast"] = BootstrapScores(scores, last = TRUE)

                    scores = data[1:(i-1),"FTHG"]
                    data[i, "Awayreceived"] = BootstrapScores(scores, last = TRUE, nlast = 15)
                    data[i, "AwayreceivedLast"] = BootstrapScores(scores, last = TRUE)
                }
            }
            datos[idx, ] = data
        }
    }

    datos$p = datos$HL3 * datos$AV3 +
            datos$HL3 * datos$AV2 +
            datos$HL2 * datos$AV3 +
            datos$HL3 * datos$AV1 +
            datos$HL1 * datos$AV3 +
            datos$HL3 +
            datos$AV3 +
            datos$HL2 * datos$AV2 +
            datos$HL2 * datos$AV1 +
            datos$HL1 * datos$AV2

    datos$GoalsPredicted = (datos$Homescore + datos$Homereceived +
                                datos$Awayscore + datos$Awayreceived) * .5
    datos$GoalsPredictedLast = (datos$HomescoreLast + datos$HomereceivedLast +
                                    datos$AwayscoreLast + datos$AwayreceivedLast) * .5

    datos$InvMxMore2.5 = 1 / datos$BbMxMore2.5
    datos$InvAvMore2.5 = 1 / datos$BbAvMore2.5

    print("Bootstrapping matches...")
    matches = unique(datos$Match[datos$Processed == 0])

    for (match in matches){
        print(match)
        idx = as.numeric(rownames(datos))[datos$Match == match]
        if (length(idx) > 1) {
            data = datos[idx, ]
            from_idx = which(idx == as.numeric(rownames(data[data$Processed == 0,]))[1])

            for (i in from_idx:nrow(data)){
                if (i == 1) next else {
                    scores = data[1:(i-1),"Score"]
                    data[i, "BootScore"] = BootstrapScores(scores)
                }
            }
            datos[idx,] = data
        }
    }

    datos$Processed = 1
    return(datos)
}

scrappFutureMatches = function(days = 3, ...){
    # Loading tables
    teamConversion = read.csv("teamConversion.csv", sep = ";", stringsAsFactors = FALSE)
    urls = paste0("http://football-data.enetpulse.com/getContent.php?d=", 1:days - 1,
                  "&showLeagues=all", sep = "")
    fechas = Sys.Date() + 0:(days - 1)
    data = as.data.frame(matrix(NA, 1, 4))
    names(data) = c("Hour", "Scrapped", "AwayScrapped", "Date")
    print("Reading future matches from the Web")
    for (i in 1:days){
        print(paste0(i, "/", days, sep = "")
        football.doc <- htmlParse(urls[[i]])
        tabla <- readHTMLTable(football.doc, stringsAsFactors = F)[[1]][,c(1,2,4)]
        tabla$Date = fechas[i]
        names(tabla) = names(data)
        rownames(tabla) = 1:nrow(tabla)
        idx = (tabla$Scrapped == "Standings")
        data = rbind(data, tabla[!idx,])
    }
    data = merge(data, teamConversion, by = "Scrapped")
    data$Scrapped = NULL
    names(data)[c(2,5)] = c("Scrapped", "HomeTeam")
    data = merge(data, teamConversion, by = "Scrapped")
    data$Scrapped = NULL
    data$Div.y = NULL
    names(data)[c(3,5)] = c("Div", "AwayTeam")
    data = cbind(data[, c("Div", "Date", "Hour", "HomeTeam", "AwayTeam")],
                 matrix(NA, nrow = dim(data)[1], ncol = 40,
                        dimnames = list(NULL,
                                        c("FTHG", "FTAG", "FTR",
                                        "HTHG", "HTAG", "HTR",
                                        "BbMxMore2.5", "BbAvMore2.5",
                                        "Year", "HalfScore", "Score", "More2",
                                        "HL0", "HL1", "HL2", "HL3", "HL4",
                                        "AV0", "AV1", "AV2", "AV3", "AV4",
                                        "p",
                                        "Homescore", "Homereceived",
                                        "HomescoreLast", "HomereceivedLast",
                                        "Awayscore", "Awayreceived",
                                        "AwayscoreLast", "AwayreceivedLast",
                                        "GoalsPredicted", "GoalsPredictedLast",
                                        "InvMxMore2.5", "InvAvMore2.5",
                                        "BootScore", "PredictedScore",
                                        "Processed", "Match", "ToPredict"))))
    data$Date = as.Date(data$Date, origin = "1970-01-01")
    data$Match = paste(data$HomeTeam, data$AwayTeam, sep = "-")
    return(data)
}


ScalebyDiv = function(datos, div){
    competitions = table(div)
    competitions = attr(competitions, "dimnames")[[1]]
    centers = list()
    scales = list()
    for (competition in competitions){
        escalado = scale(datos[div == competition,])
        datos[div == competition,] = escalado
        centers[[competition]] = attr(escalado, "scaled:center")
        scales[[competition]] = attr(escalado, "scaled:scale")
    }
    names(datos) = paste0(names(datos),"S")
    return(list(data = datos, centers = centers, scales = scales))
}

CalculateInteractions = function(datos, variablesToUse){
    data = datos[, variablesToUse]
    n = length(variablesToUse)
    dataReturn = as.data.frame(matrix(NA, nrow(datos), n * (n - 1) / 2))
    n_name = 1
    for (i in 1:(n - 1)){
        for (j in (i + 1):n){
            names(dataReturn)[n_name] = paste0(names(data)[i],
                                               "_", names(data)[j])
            dataReturn[, n_name] = data[, i] * data[, j]
            print(paste0(n_name, ".- ", i, ":", j))
            n_name = n_name +1
        }
    }
    return(dataReturn)
}

compare.dataframe <- function(df1 = dataProcessed, df2 = additionalData,
                              columns = c("Date","HomeTeam", "AwayTeam"), ...){
    idx1 <- do.call("paste", df1[, columns])
    idx2 <- do.call("paste", df2[, columns])
    return(! idx2 %in% idx1)
}



simulate = function(datos = Data, div = NA, VarDate, VarDiv, from_date = "2013-01-01", model = "rf", ...){

    datos = datos[order(VarDate),]
    rownames(datos) = 1:nrow(datos)
    datos$PredictedScore = NA

    if (is.na(div)){
        competitions = table(VarDiv[VarDate > from_date])
        competitions = attr(competitions, "dimnames")[[1]]
    } else {
        competitions = div
    }

    if (model == "glm"){
        for (competition in competitions){
            print(competition)
            data = datos[VarDiv == competition, names(datos) != "PredictedScore"]
            fechas = table(VarDate[VarDiv == competition][VarDate[VarDiv == competition] > from_date])
            fechas = attr(fechas, "dimnames")[[1]]
            for (fecha in fechas){
                idx = as.numeric(rownames(datos[VarDate == fecha & VarDiv == competition,]))
                train = data[VarDate[VarDiv == competition] < fecha, ]
                test = data[VarDate[VarDiv == competition] == fecha, ]
                test$Score = NULL
                glmmodel = glm(Score ~ ., data = train)
                datos$PredictedScore[idx] = predict(glmmodel, newdata = test)
            }
        }
    }
    if (model == "rf") {
        for (competition in competitions){
            print(competition)
            data = datos[VarDiv == competition, names(datos) != "PredictedScore"]
            fechas = table(VarDate[VarDiv == competition][VarDate[VarDiv == competition] > from_date])
            fechas = attr(fechas, "dimnames")[[1]]
            for (fecha in fechas){
                idx = as.numeric(rownames(datos[VarDate == fecha & VarDiv == competition,]))
                train = data[VarDate[VarDiv == competition] < fecha, ]
                test = data[VarDate[VarDiv == competition] == fecha, ]
                test$Score = NULL
                rfmodel = randomForest(Score ~ ., data = train)
                datos$PredictedScore[idx] = predict(rfmodel, newdata = test)
            }
        }
    }
    if (!model %in% c("glm", "rf")) print("Wrong model...")
    return(datos$PredictedScore)
}

plotSuccessRatio = function(data, graphic = TRUE, ...){
    seq_plot_x = (1:50) / 10
    ratios = matrix(NA, 3, 50)
    colnames(ratios) = seq_plot_x
    rownames(ratios) = 0:2
    for (i in 0:2){
        for (j in seq_plot_x){
            print(paste0(i,"-",j))
            tabla = table(data$More2[!is.na(data$PredictedScore) &
                                         (data$HalfScore == i & data$PredictedScore >= j)])

            ratios[i+1, j * 10] = tabla["TRUE"] / sum(tabla) * 100
        }
    }
    if (graphic){
        plot(seq_plot_x[is.finite(ratios[1,])], ratios[1,][is.finite(ratios[1,])],
             main = "Success ratio for score before start of second half",
             xlab = "Minimum Predicted Score to Bet",
             ylab = "Success ratio",
             xlim = c(0, 5),
             ylim = c(0, 100),
             col = "lightblue", type = "l"
        )
        lines(seq_plot_x[is.finite(ratios[2,])], ratios[2,][is.finite(ratios[2,])],
              col = "blue")
        lines(seq_plot_x[is.finite(ratios[3,])], ratios[3,][is.finite(ratios[3,])],
              col = "darkblue", lw = 2)
        legend("bottomright",
               legend = c("Half Time scores 0", "Half Time scores 1", "Half Time scores 2"),
               cex = .6, fill = c("lightblue", "blue", "darkblue"))
    }
    return(ratios)
}


# Renaming the files to better store
RenameFiles = function(directory, moreName = "_2010") {
    archivos = dir(path = directory, full.names = TRUE)
    for (i in 1:length(archivos)){
        csvLoc = gregexpr(pattern ='.csv',archivos[i])[[1]][1]-1
        file.rename(archivos[i], paste0(substr(archivos[i],
                                               start = 1, stop = csvLoc), moreName, ".csv"))
    }
}


BootstrapScores = function(scores, n = 1000, nlast = 5, nsd = 1, last = FALSE, ...){
    size = length(scores)
    scoreboot = rep(NA, n)
    if (last){
        from_size = max(size-4, 1)
        for (i in 1:n){
            idx = sample(from_size:size, nlast, replace = TRUE)
            scoreboot[i] = mean(scores[idx], na.rm = T)
        }
    } else {
        for (i in 1:n){
            idx = sample(size, size, replace = TRUE)
            scoreboot[i] = mean(scores[idx], na.rm = T)
        }
    }
    return(mean(scoreboot) - nsd * sd(scoreboot))
}
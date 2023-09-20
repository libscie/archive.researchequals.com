### Author: Judith ter Schure
### E-mail: j.a.terschure@amsterdamumc.nl / schure@cwi.nl
### Institute: Amsterdam UMC
###            AMC Location, Meibergdreef 9 1105 AZ Amsterdam, NETHERLANDS
### Institute: CWI - Machine Learning group
###            CWI is the national research institute 
###            for mathematics and computer science in the Netherlands
###            Science Park 123, 1098 XG Amsterdam, NETHERLANDS
### Date: 6 February 2023
### Licence: CC-BY 4.0

### This code is part of the Replication Package (https://www.researchequals.com/collections/kyep-h9/) 
### for the publication:
###   Ter Schure et al (2022) Bacillus Calmette-Guérin vaccine to reduce COVID-19 infections and hospitalisations 
###   in healthcare workers – a living systematic review and prospective ALL-IN meta-analysis of 
###   individual participant data from randomised controlled trials

### Details on the OS, system and software that produced the results
### in the paper:

# Platform: x86_64-w64-mingw32/x64 (64-bit)
# OS: Microsoft Windows
# System: Windows 10 x64 (build 19042)
# R version: 4.2.0 (2022-04-22 ucrt) -- "Vigorous Calisthenics"

### The following packages were used:
# safestats version 0.8.7
# dplyr version 1.0.9
# tibble version 3.1.7
# survival version 3.3-1

# stats version 4.2.0
# graphics version 4.2.0
# grDevices version 4.2.0
# utils version 4.2.0
# methods version 4.2.0


######### Content #########
######### 
######### 
######
###### 1. Code for preprocessing of the data (e.g. make sure dates are dates)
###### 2. Recreate the designObject that specifies the predetermined design 
######    from the Statistical Analysis Plan (SAP)
######    (see the replication package: https://www.researchequals.com/collections/kyep-h9/)
###### 3. Function procSeq() that is exlained in the Tutorial on processing e-values by 
######    calendar date and the Tutorial on Left-truncation 
######    (see the replication package: https://www.researchequals.com/collections/kyep-h9/)
######
#########
#########
###########################


options(scipen=999)
options(warn=1)

library(safestats)
library(dplyr)
library(tibble)
library(survival)

## If you place this code in the same folder as the unzipped folder Data and DataSynthetic,
## all code should work if you set the working directory to this file's location
## in R studio: use Session -> Set Working Directory -> To Source File Location


##########
##### 
##### 1. Code for preprocessing of the data (e.g. make sure dates are dates)
##### 
##########

### If you have the true data available
#dataNL <- read.csv("./Data/data_NL_2022-11-06_final.csv", row.names = 1, stringsAsFactors = TRUE)
#dataSA <- read.csv("./Data/data_SA_2022-12-02_final.csv", row.names = 1, stringsAsFactors = TRUE)
#dataUS <- read.csv("./Data/data_US_2022-12-05_final.csv", row.names = 1, stringsAsFactors = TRUE)
#dataDK <- read.csv("./Data/data_DK_2022-08-23_final.csv", row.names = 1, stringsAsFactors = TRUE)
#dataHU <- read.csv("./Data/data_HU_2021-01-15_final.csv", row.names = 1, stringsAsFactors = TRUE)
#dataBR <- read.csv("./Data/data_BR_2022-12-01_final.csv", row.names = 1, stringsAsFactors = TRUE)
#dataAF <- read.csv("./Data/data_AF_2022-12-12_final.csv", row.names = 1, stringsAsFactors = TRUE)

### If you do not have the true data available: synthetic data for demonstration
dataNL <- read.csv("./DataSynthetic/dataNLsynth.csv", row.names = 1, stringsAsFactors = TRUE)
dataSA <- read.csv("./DataSynthetic/dataSAsynth.csv", row.names = 1, stringsAsFactors = TRUE)
dataUS <- read.csv("./DataSynthetic/dataUSsynth.csv", row.names = 1, stringsAsFactors = TRUE)
dataDK <- read.csv("./DataSynthetic/dataDKsynth.csv", row.names = 1, stringsAsFactors = TRUE)
dataHU <- read.csv("./DataSynthetic/dataHUsynth.csv", row.names = 1, stringsAsFactors = TRUE)
dataBR <- read.csv("./DataSynthetic/dataBRsynth.csv", row.names = 1, stringsAsFactors = TRUE)
dataAF <- read.csv("./DataSynthetic/dataAFsynth.csv", row.names = 1, stringsAsFactors = TRUE)


dataAll <- list("NL" = dataNL,
                "SA" = dataSA,
                "US" = dataUS,
                "DK" = dataDK,
                "HU" = dataHU,
                "BR" = dataBR,
                "AF" = dataAF)

trials <- names(dataAll)


for(trial in trials) {
  dataAll[[trial]]$intervention <- factor(dataAll[[trial]]$intervention, levels = c("control", "BCG"))
  
  dataAll[[trial]]$dateRand <- as.Date(dataAll[[trial]]$dateRand, format = "%Y-%m-%d")
  dataAll[[trial]]$dateLastFup <- as.Date(dataAll[[trial]]$dateLastFup, format = "%Y-%m-%d")
  
  dataAll[[trial]]$dateCOV19 <- as.Date(dataAll[[trial]]$dateCOV19, format = "%Y-%m-%d")
  dataAll[[trial]]$dateCOV19hosp <- as.Date(dataAll[[trial]]$dateCOV19hosp, format = "%Y-%m-%d")
}

startDate <- as.Date("2020-01-01")  # some date before the first randomization in the first trial
for(trial in trials) {  # create survival objects
  dataAll[[trial]]$timeCOV19 <- ifelse(dataAll[[trial]]$COV19 %in% c("Yes", "yes"),
                                       dataAll[[trial]]$dateCOV19 - startDate,
                                       dataAll[[trial]]$dateLastFup - startDate)
  dataAll[[trial]]$statusCOV19 <- ifelse(dataAll[[trial]]$COV19 %in% c("Yes", "yes"),
                                         2, 1)
  dataAll[[trial]]$survObjCOV19 <- Surv(time = dataAll[[trial]]$dateRand - startDate, 
                                        time2 = dataAll[[trial]]$timeCOV19,
                                        event = dataAll[[trial]]$statusCOV19,
                                        type = "counting")
  
  if(any(dataAll[[trial]]$COV19hosp %in% c("Yes", "yes"))) {# Gives problems if there are no events
    
    dataAll[[trial]]$timeCOV19hosp <- ifelse(dataAll[[trial]]$COV19hosp %in% c("Yes", "yes"),
                                             dataAll[[trial]]$dateCOV19hosp - startDate,
                                             dataAll[[trial]]$dateLastFup - startDate)
    dataAll[[trial]]$statusCOV19hosp <- ifelse(dataAll[[trial]]$COV19hosp %in% c("Yes", "yes"),
                                               2, 1)    
    dataAll[[trial]]$survObjCOV19hosp <- Surv(time = dataAll[[trial]]$dateRand - startDate, 
                                              time2 = dataAll[[trial]]$timeCOV19hosp,
                                              event = dataAll[[trial]]$statusCOV19hosp,
                                              type = "counting")
  }
}

### A common indication that there are date coding errors in the data
for(trial in trials){
  cat(paste(trial, " has dateCOV19 > dateLastFup: ",
            any(!is.na(dataAll[[trial]]$dateCOV19) & (dataAll[[trial]]$dateCOV19 > dataAll[[trial]]$dateLastFup)),
            "\n"))
}

# The dataALL R object now also contains left-truncated survival data summaries
# check one example data set for an example
dataAll["US"]




##########
##### 
##### 2. Recreate the designObject that specifies the predetermined design 
#####    from the Statistical Analysis Plan (SAP)
#####    (see the replication package: https://www.researchequals.com/collections/kyep-h9/)
##### 
##########

designObj <- list(NULL)

designObj$COV19L <- designSafeLogrank(hrMin = 0.8, h0 = 1,
                                      alpha = 0.1*0.025,
                                      alternative = "less",  # one-sided test hr < 1
                                      ratio = 1)
designObj$COV19G <- designSafeLogrank(hrMin = 1/0.8, h0 = 1,
                                      alpha = 0.1*0.025,
                                      alternative = "greater",  # one-sided test hr > 1
                                      ratio = 1)
designObj$COV19hospL <- designSafeLogrank(hrMin = 0.7,
                                          alpha = 0.9*0.025,
                                          alternative = "less",  # one-sided test hr < 1
                                          ratio = 1)
designObj$COV19hospG <- designSafeLogrank(hrMin = 1/0.7,
                                          alpha = 0.9*0.025,
                                          alternative = "greater",  # one-sided test hr > 1
                                          ratio = 1)

##########
##### 
##### 3. Function procSeq() that is exlained in the Tutorial on processing e-values by 
#####    calendar date and the Tutorial on Left-truncation
#####    (see the replication package: https://www.researchequals.com/collections/kyep-h9/)
##### 
##########

procEseq <- function(data, eventName, statusVar, timeVar, randDateVar, eventDateVar, interventionVar,
                     designObjL, designObjG, timeScale = "calendar",
                     startDate, endDate) {
  
  eValuesL <- 
    eValuesG <- structure(rep(1,
                              times = endDate - startDate + 1),
                          names = as.character(seq.Date(from = startDate,
                                                        to = endDate, 
                                                        by = "day")))
  # before you observe any event, your e-value is 1
  # return all 1 sequence if there are no events
  if (sum(data[[statusVar]] == 2) == 0) {
    eValues <- structure(list(eValuesL, eValuesG), 
                         names = c(paste0(eventName, "L"), paste0(eventName, "G")))
    return(eValues)
  }
  
  interimCalDates <- as.character(sort(unique(data[[eventDateVar]])))
  
  # loop trough the calendar dates
  for (calDate in as.character(seq.Date(from = as.Date(interimCalDates[1]),
                                        to = endDate,
                                        by = "day"))) {
    # at dates without an event
    if (!(calDate %in% interimCalDates)) {
      # evidence stays the same as the day before
      eValuesL[calDate] <- eValuesL[as.character(as.Date(calDate) - 1)]         
      eValuesG[calDate] <- eValuesG[as.character(as.Date(calDate) - 1)]
    } else {
      # all participants that enter after today, are not yet in my data set so far
      dataSoFar <- data[data[[randDateVar]] < as.Date(calDate), ]
      
      if (timeScale == "calendar") {
        # I don't know how long participants are at risk whose events happen in the future
        # only that they are still at risk today:
        dataSoFar[[timeVar]] <- pmin(dataSoFar[[timeVar]], as.Date(calDate) - startDate)
        # the status of participants with events in the future is censored:
        dataSoFar[[statusVar]][dataSoFar[[eventDateVar]] > as.Date(calDate)] <- 1
        dataSoFar$survObj <- Surv(time = dataSoFar[[randDateVar]] - startDate, 
                                  time2 = dataSoFar[[timeVar]],
                                  event = dataSoFar[[statusVar]],
                                  type = "counting")
      }
      if (timeScale == "participant") {
        # I don't know how long participants are at risk whose events happen in the future
        # only that they are still at risk today:
        dataSoFar[[timeVar]] <- pmin(dataSoFar[[timeVar]], as.Date(calDate) - dataSoFar[[randDateVar]])
        # the status of participants with events in the future is censored:
        dataSoFar[[statusVar]][dataSoFar[[eventDateVar]] > as.Date(calDate)] <- 1
        dataSoFar$survObj <- Surv(dataSoFar[[timeVar]], dataSoFar[[statusVar]])
      }
      
      eValuesL[calDate] <- safeLogrankTest(dataSoFar$survObj ~ dataSoFar[[interventionVar]],
                                           designObj = designObjL
      )$eValue
      eValuesG[calDate] <- safeLogrankTest(dataSoFar$survObj ~ dataSoFar[[interventionVar]],
                                           designObj = designObjG
      )$eValue
    }
  }
  eValues <- structure(list(eValuesL, eValuesG), 
                       names = c(paste0(eventName, "L"), paste0(eventName, "G")))
  return(eValues)
}




startDate <- as.Date("2020-01-01")  # a date before the first participant 
                                    # in the first trial is randomized

returnEvaluesStrByHosp <- function(data) { # this function differentiates the use of procEseq() 
                                           # between data with a single hospital and data with
                                           # more than one hospital: for the latter e-values
                                           # are calculated stratified by hospital and multiplied
                                           # together
  eValues <- as.list(NULL)
  if(length(unique(data$hospital)) == 1) {      # single hospital
    eValuesCOV19 <- procEseq(data = data,
                             eventName = "COV19", 
                             statusVar = "statusCOV19", timeVar = "timeCOV19", 
                             randDateVar = "dateRand", eventDateVar = "dateCOV19", 
                             interventionVar = "intervention",
                             designObjL = designObj$COV19L, designObjG = designObj$COV19G,
                             timeScale = "calendar",
                             startDate = startDate, 
                             endDate = max(data$dateLastFup, na.rm = T))
    eValues$COV19L <- eValuesCOV19$COV19L
    eValues$COV19G <- eValuesCOV19$COV19G
    
    eValuesCOV19hosp <- procEseq(data = data,
                                 eventName = "COV19hosp", 
                                 statusVar = "statusCOV19hosp", timeVar = "timeCOV19hosp", 
                                 randDateVar = "dateRand", eventDateVar = "dateCOV19hosp", 
                                 interventionVar = "intervention",
                                 designObjL = designObj$COV19hospL, designObjG = designObj$COV19hospG,
                                 timeScale = "calendar",
                                 startDate = startDate, 
                                 endDate = max(data$dateLastFup, na.rm = T))
    eValues$COV19hospL <- eValuesCOV19hosp$COV19hospL
    eValues$COV19hospG <- eValuesCOV19hosp$COV19hospG
  
  } else {                                      # more than one hospital
    hospitals <- as.character(unique(data$hospital))
    names(hospitals) <- as.character(hospitals)
    
    eValuesCOV19.str <- lapply(hospitals, function(hospital) 
        procEseq(data = data[data$hospital == hospital, ],
                 eventName = "COV19", 
                 statusVar = "statusCOV19", timeVar = "timeCOV19", 
                 randDateVar = "dateRand", eventDateVar = "dateCOV19", 
                 interventionVar = "intervention",
                 designObjL = designObj$COV19L, designObjG = designObj$COV19G,
                 timeScale = "calendar",
                 startDate = startDate, 
                 endDate = max(data$dateLastFup, na.rm = T)))
    eValuesCOV19hosp.str <- lapply(hospitals, function(hospital) 
        procEseq(data = data[data$hospital == hospital, ],
                 eventName = "COV19hosp", 
                 statusVar = "statusCOV19hosp", timeVar = "timeCOV19hosp", 
                 randDateVar = "dateRand", eventDateVar = "dateCOV19hosp", 
                 interventionVar = "intervention",
                 designObjL = designObj$COV19hospL, designObjG = designObj$COV19hospG,
                 timeScale = "calendar",
                 startDate = startDate,
                 endDate = max(data$dateLastFup, na.rm = T)))
    eValues <- list("COV19L" = apply(sapply(hospitals, 
                                     function(hospital) eValuesCOV19.str[[hospital]]$COV19L),
                                     MARGIN = 1, FUN = prod, na.rm = T),
                    "COV19G" = apply(sapply(hospitals, 
                                     function(hospital) eValuesCOV19.str[[hospital]]$COV19G),
                                     MARGIN = 1, FUN = prod, na.rm = T),
                    "COV19hospL" = apply(sapply(hospitals, 
                                         function(hospital) eValuesCOV19hosp.str[[hospital]]$COV19hospL),
                                         MARGIN = 1, FUN = prod, na.rm = T),
                    "COV19hospG" = apply(sapply(hospitals, 
                                         function(hospital) eValuesCOV19hosp.str[[hospital]]$COV19hospG),
                                         MARGIN = 1, FUN = prod, na.rm = T))
      
  }
  # the dates that were the row names get their own date column
  return(rownames_to_column(as.data.frame(eValues), var = "calendarDate"))  
}
 
eValuesTrials <- bind_rows(lapply(dataAll, returnEvaluesStrByHosp), .id = "trial")

### Were these calculated on the actual data or synthetic data?
### 
### If actual data
#write.csv(eValuesTrials, "./Data/eValuesTrials.csv")

### If synthetic data
write.csv(eValuesTrials, "./DataSynthetic/SYNTHETICeValuesTrials.csv")


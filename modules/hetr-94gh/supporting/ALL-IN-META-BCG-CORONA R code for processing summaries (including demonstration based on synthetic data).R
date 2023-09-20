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
######    (see the replication package for the SAP: https://www.researchequals.com/collections/kyep-h9/)
###### 3. Function procSummary() that processes summary statistics per hospital within trial
###### 4. Function calcMeta() that combines summary statistics per hospital within trial, and
######    per trial within the complete meta-analysis
###### 5. Running these functions on the processed data for a primary and secondary analysis
###### 6. Construct the csv files for tables in the manuscript
######    a. trialSummaryPrimary and trialSummarySecondary together make Table 2 and Appendix Table 12
######    b. resultsEvaluePrimary  and resultsEvalueSecondary together make Table 3 and Appendix Table 13
######    c. resultsIntervalPrimary and resultsIntervalSecondary together make Table 4 and Appendix Table 14
######    and to support creating the plots
######    d. resultsIntervalPrimary and resultsIntervalSecondary are plotted in the Forest plots Figure 5 and 6
######    e. resultsRecruitment for the subplot below Figure 4 and 7 showing the recruitment and follow-up
######       phase per trial
######    f. resultsDateCOV19 for the subplot of Figure 7 showing the event dates
###### 7. Stratified one-stage IPD analysis for COV19hosp
######
#########
#########
###########################


options(scipen=999)
options(warn=1)

library(safestats)
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
#####    (see the replication package for the SAP: https://www.researchequals.com/collections/kyep-h9/)
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
##### 3. Function procSummary() that processes summary statistics per hospital within trial
##### 
##########

procSummary <- function(data, trial, hospital) {
  
  firstDateRand <- as.Date(min(data$dateRand))[[1]]
  lastDateRand <- as.Date(max(data$dateRand))[[1]]
  lastDateFup <- as.Date(max(data$dateLastFup))[[1]]
  numRandControl <- table(data$intervention)[[1]]
  numRandBCG <- table(data$intervention)[[2]]
  nEventsCOV19 <- sum(data$COV19 %in% c("Yes", "yes"))
  nEventsCOV19BCG <- sum(data$COV19 %in% c("Yes", "yes") & data$intervention == "BCG")
  nEventsCOV19control <- sum(data$COV19 %in% c("Yes", "yes") & data$intervention == "control")
  nEventsCOV19hosp <- sum(data$COV19hosp %in% c("Yes", "yes"))
  nEventsCOV19hospBCG <- sum(data$COV19hosp %in% c("Yes", "yes") & data$intervention == "BCG")
  nEventsCOV19hospControl <- sum(data$COV19hosp %in% c("Yes", "yes") & data$intervention == "control")
  fupPersWeeksControl <- sum(data$dateLastFup[data$intervention == "control"] - data$dateRand[data$intervention == "control"])/7
  fupPersWeeksBCG <- sum(data$dateLastFup[data$intervention == "BCG"] - data$dateRand[data$intervention == "BCG"])/7
  
  
  if(nEventsCOV19 > 0) {
    testObjCOV19ExactL <- safeLogrankTest(data$survObjCOV19 ~ data$intervention,
                                          designObj = designObj$COV19L, exact = TRUE)
    testObjCOV19ExactG <- safeLogrankTest(data$survObjCOV19 ~ data$intervention,
                                          designObj = designObj$COV19G, exact = TRUE)
    testObjCOV19GaussL <- safeLogrankTest(data$survObjCOV19 ~ data$intervention,
                                            designObj = designObj$COV19L, exact = FALSE, ciValue = 0.95)
    testObjCOV19GaussG <- safeLogrankTest(data$survObjCOV19 ~ data$intervention,
                                            designObj = designObj$COV19G, exact = FALSE, ciValue = 0.95)
    
    if(testObjCOV19ExactL$n != nEventsCOV19) print(paste("Not all events are included in survObjCOV19 for trial",
                                                         trial, "hospital", hospital))
    
    sumOminECOV19 <- testObjCOV19GaussL$sumStats$sumOMinE
    sumVarOminECOV19 <- testObjCOV19GaussL$sumStats$sumVarOMinE
    zCOV19 <- testObjCOV19GaussL$sumStats$z
    
    eValueCOV19exactL <- testObjCOV19ExactL$eValue[[1]]
    eValueCOV19exactG <- testObjCOV19ExactG$eValue[[1]]
    eValueCOV19GaussL <- testObjCOV19GaussL$eValue
    eValueCOV19GaussG <- testObjCOV19GaussG$eValue

  } else {
    sumOminECOV19 <- NA
    sumVarOminECOV19 <- NA
    zCOV19 <- NA
    
    eValueCOV19exactL <- 1
    eValueCOV19exactG <- 1
    eValueCOV19GaussL <- 1
    eValueCOV19GaussG <- 1
    
  }
  
  if(nEventsCOV19hosp > 0) {
    testObjCOV19hospExactL <- safeLogrankTest(data$survObjCOV19hosp ~ data$intervention,
                                              designObj = designObj$COV19hospL, exact = TRUE)
    testObjCOV19hospExactG <- safeLogrankTest(data$survObjCOV19hosp ~ data$intervention,
                                              designObj = designObj$COV19hospG, exact = TRUE)
    testObjCOV19hospGaussL <- safeLogrankTest(data$survObjCOV19hosp ~ data$intervention,
                                                designObj = designObj$COV19hospL, exact = FALSE, ciValue = 0.95)
    testObjCOV19hospGaussG <- safeLogrankTest(data$survObjCOV19hosp ~ data$intervention,
                                                designObj = designObj$COV19hospG, exact = FALSE, ciValue = 0.95)
    
    if(testObjCOV19hospExactL$n != nEventsCOV19hosp) print(paste("Not all events are included in survObjCOV19hosp for trial",
                                                         trial, "hospital", hospital))
    
    sumOminECOV19hosp <- testObjCOV19hospGaussL$sumStats$sumOMinE
    sumVarOminECOV19hosp <- testObjCOV19hospGaussL$sumStats$sumVarOMinE
    zCOV19hosp <- testObjCOV19hospGaussL$sumStats$z
    
    eValueCOV19hospexactL <- testObjCOV19hospExactL$eValue[[1]]
    eValueCOV19hospexactG <- testObjCOV19hospExactG$eValue[[1]]
    eValueCOV19hospGaussL <- testObjCOV19hospGaussL$eValue
    eValueCOV19hospGaussG <- testObjCOV19hospGaussG$eValue
    
  } else {
    sumOminECOV19hosp <- NA
    sumVarOminECOV19hosp <- NA
    zCOV19hosp <- NA
    
    eValueCOV19hospexactL <- 1
    eValueCOV19hospexactG <- 1
    eValueCOV19hospGaussL <- 1
    eValueCOV19hospGaussG <- 1
    
  }
  
  return(list("trial" = trial, "hospital" = hospital, 
              "firstDateRand" = firstDateRand, "lastDateRand" = lastDateRand, 
              "lastDateFup" = lastDateFup, 
              "numRandControl" = numRandControl, "numRandBCG" = numRandBCG, 
              "fupPersWeeksControl" = fupPersWeeksControl, "fupPersWeeksBCG" = fupPersWeeksBCG,
              ##### COV19 #####
              "nEventsCOV19" = nEventsCOV19, 
              "nEventsCOV19BCG" = nEventsCOV19BCG, "nEventsCOV19control" = nEventsCOV19control, 
              "sumOminECOV19" = sumOminECOV19, 
              "sumVarOminECOV19" = sumVarOminECOV19, 
              "zCOV19" = zCOV19,
              "eValueCOV19exactL" = eValueCOV19exactL, "eValueCOV19exactG" = eValueCOV19exactG, 
              "eValueCOV19GaussL" = eValueCOV19GaussL, "eValueCOV19GaussG" = eValueCOV19GaussG,
              "MLEhrCOV19" = NA, "seLogMLEhrCOV19" = NA,
              "lowerMLEhrCOV19CI95" = NA, "upperMLEhrCOV19CI95" = NA,
              "lowerMLEhrCOV19CI995" = NA, "upperMLEhrCOV19CI995" = NA,
              ##### COV19hosp #####
              "nEventsCOV19hosp" = nEventsCOV19hosp, 
              "nEventsCOV19hospBCG" = nEventsCOV19hospBCG, "nEventsCOV19hospControl" = nEventsCOV19hospControl,
              "sumOminECOV19hosp" = sumOminECOV19hosp, 
              "sumVarOminECOV19hosp" = sumVarOminECOV19hosp, 
              "zCOV19hosp" = zCOV19hosp,
              "eValueCOV19hospexactL" = eValueCOV19hospexactL, "eValueCOV19hospexactG" = eValueCOV19hospexactG, 
              "eValueCOV19hospGaussL" = eValueCOV19hospGaussL, "eValueCOV19hospGaussG" = eValueCOV19hospGaussG,
              "MLEhrStratCOV19hosp" = NA, "seLogMLEhrStratCOV19hosp" = NA,
              "lowerMLEhrStratCOV19hospCI95" = NA, "upperMLEhrStratCOV19hospCI95" = NA,
              "lowerMLEhrStratCOV19hospCI955" = NA, "upperMLEhrStratCOV19hospCI955" = NA))
}


##########
##### 
##### 4. Function calcMeta() that combines summary statistics per hospital within trial
#####     (hospital = "total"), and per trial within the complete meta-analysis (trial = "ALL-IN")
##### 
##########

calcMeta <- function(summary, trial, hospital){
  
  firstDateRand <- min(summary$firstDateRand)
  lastDateRand <- max(summary$lastDateRand)
  lastDateFup <- max(summary$lastDateFup)
  numRandControl <- sum(summary$numRandControl)
  numRandBCG <- sum(summary$numRandBCG)
  nEventsCOV19 <- sum(summary$nEventsCOV19, na.rm = TRUE)
  nEventsCOV19BCG <- sum(summary$nEventsCOV19BCG, na.rm = TRUE)
  nEventsCOV19control <- sum(summary$nEventsCOV19control, na.rm = TRUE)
  nEventsCOV19hosp <- sum(summary$nEventsCOV19hosp, na.rm = TRUE)
  nEventsCOV19hospBCG <- sum(summary$nEventsCOV19hospBCG, na.rm = TRUE)
  nEventsCOV19hospControl <- sum(summary$nEventsCOV19hospControl, na.rm = TRUE)
  fupPersWeeksControl <- sum(summary$fupPersWeeksControl)
  fupPersWeeksBCG <- sum(summary$fupPersWeeksBCG)
  
  sumOminECOV19 <- sum(summary$sumOminECOV19, na.rm = TRUE)
  sumVarOminECOV19 <- sum(summary$sumVarOminECOV19, na.rm = TRUE)
  
  eValueCOV19exactL <- prod(summary$eValueCOV19exactL)
  eValueCOV19exactG <- prod(summary$eValueCOV19exactG)
  eValueCOV19GaussL <- prod(summary$eValueCOV19GaussL)
  eValueCOV19GaussG <- prod(summary$eValueCOV19GaussG)
  
  if(nEventsCOV19 > 0) {
    zCOV19 <- sumOminECOV19/sqrt(sumVarOminECOV19)
  } else {
    zCOV19 <- NA
  }
  if(any(!is.na(summary$MLEhrCOV19))) {
    MLEhrCOV19 <- exp(sum(log(summary$MLEhrCOV19)*(1/(summary$seLogMLEhrCOV19^2)), na.rm = TRUE)/
                      sum(1/(summary$seLogMLEhrCOV19^2), na.rm = TRUE))
    seLogMLEhrCOV19 <- sqrt(1/sum((1/summary$seLogMLEhrCOV19^2), na.rm = TRUE))
    
    MLEhrCOV19CI95 <- exp(computeConfidenceIntervalZ(nEff = 1, 
                                                     meanObs = log(MLEhrCOV19),
                                                     phiS = log(designObj$COV19L$parameter),
                                                     sigma = seLogMLEhrCOV19, 
                                                     ciValue = 0.95, alternative = "twoSided"))
    MLEhrCOV19CI995 <- exp(computeConfidenceIntervalZ(nEff = 1, 
                                                      meanObs = log(MLEhrCOV19),
                                                      phiS = log(designObj$COV19L$parameter),
                                                      sigma = seLogMLEhrCOV19, 
                                                      ciValue = 0.995, alternative = "twoSided"))
    lowerMLEhrCOV19CI95 <- MLEhrCOV19CI95[1]
    upperMLEhrCOV19CI95 <- MLEhrCOV19CI95[2]
    lowerMLEhrCOV19CI995 <- MLEhrCOV19CI995[1]
    upperMLEhrCOV19CI995 <- MLEhrCOV19CI995[2]
  } else {
    MLEhrCOV19 <- NA
    seLogMLEhrCOV19 <- NA
    
    lowerMLEhrCOV19CI95 <- NA
    upperMLEhrCOV19CI95 <- NA
    lowerMLEhrCOV19CI995 <- NA
    upperMLEhrCOV19CI995 <- NA
  }
  
  sumOminECOV19hosp <- sum(summary$sumOminECOV19hosp, na.rm = TRUE)
  sumVarOminECOV19hosp <- sum(summary$sumVarOminECOV19hosp, na.rm = TRUE)
  
  eValueCOV19hospexactL <- prod(summary$eValueCOV19hospexactL)
  eValueCOV19hospexactG <- prod(summary$eValueCOV19hospexactG)
  eValueCOV19hospGaussL <- prod(summary$eValueCOV19hospGaussL)
  eValueCOV19hospGaussG <- prod(summary$eValueCOV19hospGaussG)
  
  if(nEventsCOV19hosp > 0) {
    zCOV19hosp <- sumOminECOV19hosp/sqrt(sumVarOminECOV19hosp)
  } else {
    zCOV19hosp <- NA
  }
  return(list("trial" = trial, "hospital" = hospital, 
              "firstDateRand" = firstDateRand, "lastDateRand" = lastDateRand, 
              "lastDateFup" = lastDateFup, 
              "numRandControl" = numRandControl, "numRandBCG" = numRandBCG,
              "fupPersWeeksControl" = fupPersWeeksControl, "fupPersWeeksBCG" = fupPersWeeksBCG,
              ##### COV19 #####
              "nEventsCOV19" = nEventsCOV19,
              "nEventsCOV19BCG" = nEventsCOV19BCG, "nEventsCOV19control" = nEventsCOV19control,
              "sumOminECOV19" = sumOminECOV19, 
              "sumVarOminECOV19" = sumVarOminECOV19, 
              "zCOV19" = zCOV19,
              "eValueCOV19exactL" = eValueCOV19exactL, "eValueCOV19exactG" = eValueCOV19exactG, 
              "eValueCOV19GaussL" = eValueCOV19GaussL, "eValueCOV19GaussG" = eValueCOV19GaussG,
              "MLEhrCOV19" = MLEhrCOV19, "seLogMLEhrCOV19" = seLogMLEhrCOV19,
              "lowerMLEhrCOV19CI95" = lowerMLEhrCOV19CI95, "upperMLEhrCOV19CI95" = upperMLEhrCOV19CI95,
              "lowerMLEhrCOV19CI995" = lowerMLEhrCOV19CI995, "upperMLEhrCOV19CI995" = upperMLEhrCOV19CI995,
              ##### COV19hosp #####
              "nEventsCOV19hosp" = nEventsCOV19hosp, 
              "nEventsCOV19hospBCG" = nEventsCOV19hospBCG, "nEventsCOV19hospControl" = nEventsCOV19hospControl,
              "sumOminECOV19hosp" = sumOminECOV19hosp, 
              "sumVarOminECOV19hosp" = sumVarOminECOV19hosp, "zCOV19hosp" = zCOV19hosp,
              "eValueCOV19hospexactL" = eValueCOV19hospexactL, "eValueCOV19hospexactG" = eValueCOV19hospexactG, 
              "eValueCOV19hospGaussL" = eValueCOV19hospGaussL, "eValueCOV19hospGaussG" = eValueCOV19hospGaussG,
              "MLEhrStratCOV19hosp" = NA, "seLogMLEhrStratCOV19hosp" = NA,
              "lowerMLEhrStratCOV19hospCI95" = NA, "upperMLEhrStratCOV19hospCI95" = NA,
              "lowerMLEhrStratCOV19hospCI955" = NA, "upperMLEhrStratCOV19hospCI955" = NA))
}

##########
##### 
##### 5. Running these functions on the processed data for a primary and secondary analysis
##### 
##########


#### Primary analysis: the primary analysis excludes the AF trial, see the paper
#### 
summaryPrimary <- NULL

for(trial in trials[!(trials == "AF")]) {
  data <- dataAll[[trial]]
  hospitals <- sort(unique(data$hospital))
  
  summaryHospital <- NULL
  summaryTrial <- NULL
  
  for(hospital in hospitals) {
    summaryHospital <- procSummary(data[data$hospital == hospital, ], trial, hospital)
    summaryTrial <- rbind(summaryTrial, as.data.frame(summaryHospital))
  }
  totalTrial <- calcMeta(summaryTrial, trial, "total")
  if(totalTrial$nEventsCOV19 > 0) {
    MLEhrCOV19CoxCoeff <- coxph(survObjCOV19 ~ intervention + strata(hospital), data = data)
    
    totalTrial$MLEhrCOV19 <- exp(MLEhrCOV19CoxCoeff$coefficients[[1]])
    totalTrial$seLogMLEhrCOV19 <- sqrt(MLEhrCOV19CoxCoeff$var[[1]])
    
    MLEhrCOV19CI95 <- exp(computeConfidenceIntervalZ(nEff = 1, 
                                                     meanObs = log(totalTrial$MLEhrCOV19),
                                                     phiS = log(designObj$COV19L$parameter),
                                                     sigma = totalTrial$seLogMLEhrCOV19, 
                                                     ciValue = 0.95, alternative = "twoSided"))
    MLEhrCOV19CI995 <- exp(computeConfidenceIntervalZ(nEff = 1, 
                                                      meanObs = log(totalTrial$MLEhrCOV19),
                                                      phiS = log(designObj$COV19L$parameter),
                                                      sigma = totalTrial$seLogMLEhrCOV19, 
                                                      ciValue = 0.995, alternative = "twoSided"))
    totalTrial$lowerMLEhrCOV19CI95 <- MLEhrCOV19CI95[1]
    totalTrial$upperMLEhrCOV19CI95 <- MLEhrCOV19CI95[2]
    totalTrial$lowerMLEhrCOV19CI995 <- MLEhrCOV19CI995[1]
    totalTrial$upperMLEhrCOV19CI995 <- MLEhrCOV19CI995[2]
  }
  summaryTrial <- rbind(summaryTrial, as.data.frame(totalTrial))
  summaryPrimary <- rbind(summaryPrimary, summaryTrial)
}
summaryMeta <- calcMeta(summaryPrimary[summaryPrimary$hospital == "total", ], "ALL-IN META", "ALL-IN META")
summaryPrimary <- rbind(summaryPrimary, as.data.frame(summaryMeta))


#### Secondary analysis
#### 
summarySecondary <- NULL

for(trial in trials) {
  data <- dataAll[[trial]]
  hospitals <- sort(unique(data$hospital))
  
  summaryHospital <- NULL
  summaryTrial <- NULL
  
  for(hospital in hospitals) {
    summaryHospital <- procSummary(data[data$hospital == hospital, ], trial, hospital)
    summaryTrial <- rbind(summaryTrial, as.data.frame(summaryHospital))
  }
  totalTrial <- calcMeta(summaryTrial, trial, "total")
  if(totalTrial$nEventsCOV19 > 0) {
    MLEhrCOV19CoxCoeff <- coxph(survObjCOV19 ~ intervention + strata(hospital), data = data)
    
    totalTrial$MLEhrCOV19 <- exp(MLEhrCOV19CoxCoeff$coefficients[[1]])
    totalTrial$seLogMLEhrCOV19 <- sqrt(MLEhrCOV19CoxCoeff$var[[1]])
    
    MLEhrCOV19CI95 <- exp(computeConfidenceIntervalZ(nEff = 1, 
                                                     meanObs = log(totalTrial$MLEhrCOV19),
                                                     phiS = log(designObj$COV19L$parameter),
                                                     sigma = totalTrial$seLogMLEhrCOV19, 
                                                     ciValue = 0.95, alternative = "twoSided"))
    MLEhrCOV19CI995 <- exp(computeConfidenceIntervalZ(nEff = 1, 
                                                      meanObs = log(totalTrial$MLEhrCOV19),
                                                      phiS = log(designObj$COV19L$parameter),
                                                      sigma = totalTrial$seLogMLEhrCOV19, 
                                                      ciValue = 0.995, alternative = "twoSided"))
    totalTrial$lowerMLEhrCOV19CI95 <- MLEhrCOV19CI95[1]
    totalTrial$upperMLEhrCOV19CI95 <- MLEhrCOV19CI95[2]
    totalTrial$lowerMLEhrCOV19CI995 <- MLEhrCOV19CI995[1]
    totalTrial$upperMLEhrCOV19CI995 <- MLEhrCOV19CI995[2]
  }
  summaryTrial <- rbind(summaryTrial, as.data.frame(totalTrial))
  summarySecondary <- rbind(summarySecondary, summaryTrial)
}
summaryMeta <- calcMeta(summarySecondary[summarySecondary$hospital == "total", ], "ALL-IN META", "ALL-IN META")
summarySecondary <- rbind(summarySecondary, as.data.frame(summaryMeta))


### Dates of COV19 events
resultsDateCOV19 <- NULL
for(trial in trials) {  
  if(any(!is.na(dataAll[[trial]]$dateCOV19))) { # HU does not have events in the synthetic data
    newResultsDateCOV19 <- NULL
    newResultsDateCOV19$interimDates <- dataAll[[trial]]$dateCOV19[!is.na(dataAll[[trial]]$dateCOV19)]
    newResultsDateCOV19 <- as.data.frame(newResultsDateCOV19)
    newResultsDateCOV19$trial <- trial
    resultsDateCOV19 <- rbind(resultsDateCOV19, newResultsDateCOV19)
  }
}


##########
#####
##### 6. Construct the csv files for tables in the manuscript
#####    a. trialSummaryPrimary and trialSummarySecondary together make Table 2 and Appendix Table 12
#####    b. resultsEvaluePrimary  and resultsEvalueSecondary together make Table 3 and Appendix Table 13
#####    c. resultsIntervalPrimary and resultsIntervalSecondary together make Table 4 and Appendix Table 14
#####    and to support creating the plots
#####    d. resultsIntervalPrimary and resultsIntervalSecondary are plotted in the Forest plots Figure 5 and 6
#####    e. resultsRecruitment for the subplot below Figure 4 and 7 showing the recruitment and follow-up
#####       phase per trial
#####    f. resultsDateCOV19 (created above at 5.) for the subplot of Figure 7 showing the event dates
##########

trialSummaryPrimary <- summaryPrimary[ , c("trial", "hospital", 
                                                   "firstDateRand", "lastDateFup", 
                                                   "numRandBCG", "numRandControl",
                                                   "fupPersWeeksBCG", "fupPersWeeksControl",
                                                   "nEventsCOV19BCG", "nEventsCOV19control",
                                                   "nEventsCOV19hospBCG", "nEventsCOV19hospControl")]

trialSummarySecondary <- summarySecondary[ , c("trial", "hospital", 
                                                       "firstDateRand", "lastDateFup", 
                                                       "numRandBCG", "numRandControl",
                                                       "fupPersWeeksBCG", "fupPersWeeksControl",
                                                       "nEventsCOV19BCG", "nEventsCOV19control",
                                                       "nEventsCOV19hospBCG", "nEventsCOV19hospControl")]

resultsEvaluePrimary <- summaryPrimary[ , c("trial", "hospital", "nEventsCOV19",  
                                                    "eValueCOV19exactL", "eValueCOV19exactG", 
                                                    "zCOV19", 
                                                    "eValueCOV19GaussL", "eValueCOV19GaussG", 
                                                    "nEventsCOV19hosp", 
                                                    "eValueCOV19hospexactL", "eValueCOV19hospexactG", 
                                                    "zCOV19hosp", 
                                                    "eValueCOV19hospGaussL", "eValueCOV19hospGaussG")]

resultsEvalueSecondary <- summarySecondary[ , c("trial", "hospital", 
                                                        "nEventsCOV19", 
                                                        "eValueCOV19exactL", "eValueCOV19exactG", 
                                                        "zCOV19", 
                                                        "eValueCOV19GaussL", "eValueCOV19GaussG", 
                                                        "nEventsCOV19hosp", 
                                                        "eValueCOV19hospexactL", "eValueCOV19hospexactG", 
                                                        "zCOV19hosp", 
                                                        "eValueCOV19hospGaussL", "eValueCOV19hospGaussG")]

resultsIntervalPrimary <- summaryPrimary[summaryPrimary$hospital == "total" | summaryPrimary$hospital == "ALL-IN META", 
                                         c("trial", "hospital", "nEventsCOV19", "sumOminECOV19",
                                           "seLogMLEhrCOV19", "MLEhrCOV19", 
                                           "lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95", 
                                           "lowerMLEhrCOV19CI995", "upperMLEhrCOV19CI995")]

resultsIntervalSecondary <- summarySecondary[summarySecondary$hospital == "total" | summarySecondary$hospital == "ALL-IN META", 
                                         c("trial", "hospital", "nEventsCOV19", "sumOminECOV19",
                                           "seLogMLEhrCOV19", "MLEhrCOV19", 
                                           "lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95", 
                                           "lowerMLEhrCOV19CI995", "upperMLEhrCOV19CI995")]

resultsRecruitment <- summarySecondary[ , c("trial", "hospital", "firstDateRand", "lastDateRand", "lastDateFup")]


### Were these calculated on the actual data or synthetic data?
### 
### If actual data
#write.csv(trialSummaryPrimary, "./Data/trialSummaryPrimary.csv")
#write.csv(trialSummarySecondary, "./Data/trialSummarySecondary.csv")
#write.csv(resultsEvaluePrimary, "./Data/resultsEvaluePrimary.csv")
#write.csv(resultsEvalueSecondary, "./Data/resultsEvalueSecondary.csv")
#write.csv(resultsIntervalPrimary, "./Data/resultsIntervalPrimary.csv")
#write.csv(resultsIntervalSecondary, "./Data/resultsIntervalSecondary.csv")
#write.csv(resultsRecruitment, file = "./Data/resultsRecruitment.csv")
#write.csv(resultsDateCOV19, file = "./Data/resultsDateCOV19.csv")

### If synthetic data
write.csv(trialSummaryPrimary, "./DataSynthetic/SYNTHETICtrialSummaryPrimary.csv")
write.csv(trialSummarySecondary, "./DataSynthetic/SYNTHETICtrialSummarySecondary.csv")
write.csv(resultsEvaluePrimary, "./DataSynthetic/SYNTHETICresultsEvaluePrimary.csv")
write.csv(resultsEvalueSecondary, "./DataSynthetic/SYNTHETICresultsEvalueSecondary.csv")
write.csv(resultsIntervalPrimary, "./DataSynthetic/SYNTHETICresultsIntervalPrimary.csv")
write.csv(resultsIntervalSecondary, "./DataSynthetic/SYNTHETICresultsIntervalSecondary.csv")
write.csv(resultsRecruitment, file = "./DataSynthetic/SYNTHETICresultsRecruitment.csv")
write.csv(resultsDateCOV19, file = "./DataSynthetic/SYNTHETICresultsDateCOV19.csv")



##########
##### 
##### 7. Stratified one-stage IPD analysis for COV19hosp
##### 
##########

dataMerged <- NULL
for(trial in trials) {
  if(any(dataAll[[trial]]$COV19hosp %in% c("Yes", "yes"))) {
    dataTrial <- dataAll[[trial]][ , c("intervention", "dateRand", 
                                       "statusCOV19hosp", "timeCOV19hosp", "dateLastFup")]
    dataTrial$trial <- trial
    dataMerged <- rbind(dataMerged, dataTrial)
  }
}
dataMerged$trial <- as.factor(dataMerged$trial)
dataMerged$survObjCOV19hosp <- Surv(time = dataMerged$dateRand - as.Date(startDate), 
                                    time2 = dataMerged$timeCOV19hosp,
                                    event = dataMerged$statusCOV19hosp,
                                    type = "counting")

oneStageCOV19hospCox <- coxph(survObjCOV19hosp ~ intervention + strata(trial), data = dataMerged)
oneStageCOV19hospCox

MLEoneStageCOV19hosp <- exp(oneStageCOV19hospCox$coefficients[[1]])
seLogMLEoneStageCOV19hosp <- sqrt(oneStageCOV19hospCox$var[[1]])

MLEoneStageCOV19hospCI95 <- exp(computeConfidenceIntervalZ(nEff = 1, 
                                                          meanObs = log(MLEoneStageCOV19hosp),
                                                          phiS = log(designObj$COV19hospL$parameter),
                                                          sigma = seLogMLEoneStageCOV19hosp, 
                                                          ciValue = 0.95, alternative = "twoSided"))
lowerMLEoneStageCOV19hospCI95 <- MLEoneStageCOV19hospCI95[1]
upperMLEoneStageCOV19hospCI95 <- MLEoneStageCOV19hospCI95[2]

# Print result to include in paper
c(MLEoneStageCOV19hosp, lowerMLEoneStageCOV19hospCI95, upperMLEoneStageCOV19hospCI95)

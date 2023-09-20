### Author: Judith ter Schure
### E-mail: j.a.terschure@amsterdamumc.nl / schure@cwi.nl
### Institute: Amsterdam UMC
###            AMC Location, Meibergdreef 9, 1105 AZ Amsterdam, NETHERLANDS
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
# ggplot2 version 3.3.6
# ggside version 0.2.0
# dplyr version 1.0.9
# tibble version 3.1.7
# survival version 3.3-1
# survminer version 0.4.9
# meta version 5.5-0

# stats version 4.2.0
# graphics version 4.2.0
# grDevices version 4.2.0
# utils version 4.2.0
# methods version 4.2.0


######### Content #########
######### 
######### 
######
###### 1. Recreate the designObject that specifies the predetermined design 
######    from the Statistical Analysis Plan (SAP)
######    (needed for 2. and 6a.)
######    (see the replication package for the SAP: https://www.researchequals.com/collections/kyep-h9/)
###### 2. Code for forest plots
###### 3. Code for e-value plots
####### 3a. Calculate ALL-IN meta-analysis e-values from trial e-values
####### 3b. Plot trial e-values and ALL-IN meta-analysis e-values
###### 4. Code for hypothetical e-value plots
###### 5. Code for cumulative hazard plot NL data
###### 6. Code for anytime-valid 95%-confidence interval plots
####### 6a. Calculate ALL-IN meta-analysis 95%-confidence intervals from
#######     trial 95%-confidence intervals summary statistics
####### 6b. Filter confidence interval data for size of confidence interval and
#######     number of dates represented
####### 6c. Plot trial 95%-confidence intervals and 
#####       ALL-IN meta-analysis 95%-confidence intervals
######
#########
#########
###########################

library(safestats)
library(ggplot2)
library(ggside)
library(dplyr)
library(tibble)
library(survival)
library(survminer)
library(meta)


## If you place this code in the same folder as the unzipped folder Data and DataSynthetic,
## all code should work if you set the working directory to this file's location
## in R studio: use Session -> Set Working Directory -> To Source File Location

##########
##### 
##### 1. Recreate the designObject that specifies the predetermined design 
#####    from the Statistical Analysis Plan (SAP)
#####    (needed for 2. and 6a.)
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
##### 2. Code for forest plots
##### 
##########


# Synthetic results
#resultsIntervalPrimary <- read.csv("./DataSynthetic/SYNTHETICresultsIntervalPrimary.csv", row.names=1, stringsAsFactors=TRUE)
#resultsIntervalSecondary <- read.csv("./DataSynthetic/SYNTHETICresultsIntervalSecondary.csv", row.names=1, stringsAsFactors=TRUE)

# True results
resultsIntervalPrimary <- read.csv("./Data/resultsIntervalPrimary.csv", row.names=1, stringsAsFactors=TRUE)
resultsIntervalSecondary <- read.csv("./Data/resultsIntervalSecondary.csv", row.names=1, stringsAsFactors=TRUE)


# Recalculate ALL-IN meta-analysis anytime-valid 95%-confidence intervals
# from summary statistics in resultsIntervalPrimary

summary <- resultsIntervalPrimary[resultsIntervalPrimary$trial != "ALL-IN META", ]

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

# meta-analysis for forest plot using the meta package
mgPrimary <- metagen(log(MLEhrCOV19), seLogMLEhrCOV19, studlab = trial, data = summary, sm = "HR")

# Replace conventional confidence intervals by anytime-valid ones
mgPrimary$lower <- log(resultsIntervalPrimary$lowerMLEhrCOV19CI95[resultsIntervalPrimary$trial != "ALL-IN META"])
mgPrimary$upper <- log(resultsIntervalPrimary$upperMLEhrCOV19CI95[resultsIntervalPrimary$trial != "ALL-IN META"])
mgPrimary$lower[5] <- log(1/99)  # for the true data, the HU interval makes the plot too wide
mgPrimary$upper[5] <- log(99)    # "  

# Replace conventional meta-analysis confidence interval by anytime-valid one
mgPrimary$lower.common <- log(resultsIntervalPrimary$lowerMLEhrCOV19CI95[resultsIntervalPrimary$trial == "ALL-IN META"])
mgPrimary$upper.common <- log(resultsIntervalPrimary$upperMLEhrCOV19CI95[resultsIntervalPrimary$trial == "ALL-IN META"])

mgPrimary$label.left <- "favours BCG"
mgPrimary$label.right <- "favours control"
forestPrimary <- forest.meta(mgPrimary, random = FALSE,
                             text.common = "Typical effect anytime-valid confidence intervals\nfor Covid-19 infections (Primary analysis)",
                             title = "MLE hazard ratio for Covid-19 infections",
                             complab = "BCG",
                             label.e = "BCG", label.c = "control",
                             print.tau2 = FALSE, print.I2 = FALSE, print.pval.Q = FALSE)

mgSecondary <- metagen(log(MLEhrCOV19), seLogMLEhrCOV19, studlab = trial, data = resultsIntervalSecondary[resultsIntervalSecondary$trial != "ALL-IN META", ], sm = "HR")

# Replace conventional confidence intervals by anytime-valid ones
mgSecondary$lower <- log(resultsIntervalSecondary$lowerMLEhrCOV19CI95[resultsIntervalSecondary$trial != "ALL-IN META"])
mgSecondary$upper <- log(resultsIntervalSecondary$upperMLEhrCOV19CI95[resultsIntervalSecondary$trial != "ALL-IN META"])
mgSecondary$lower[5] <- log(1/99)  # for the true data, the HU interval makes the plot too wide
mgSecondary$upper[5] <- log(99)    # "  

# Replace conventional meta-analysis confidence interval by anytime-valid one
mgSecondary$lower.common <- log(resultsIntervalSecondary$lowerMLEhrCOV19CI95[resultsIntervalSecondary$trial == "ALL-IN META"])
mgSecondary$upper.common <- log(resultsIntervalSecondary$upperMLEhrCOV19CI95[resultsIntervalSecondary$trial == "ALL-IN META"])

mgSecondary$label.left <- "favours BCG"
mgSecondary$label.right <- "favours control"
forestSecondary <- forest.meta(mgSecondary, random = FALSE,
                               text.common = "Typical effect anytime-valid confidence intervals\nfor Covid-19 infections (Secondary analysis)",
                               title = "MLE hazard ratio for Covid-19 infections",
                               complab = "BCG",
                               label.e = "BCG", label.c = "control",
                               print.tau2 = FALSE, print.I2 = FALSE, print.pval.Q = FALSE)


##########
##### 
##### 3. Code for e-value plots
##### 
##########

# Synthetic results
#resultsRecruitment <- read.csv("./DataSynthetic/SYNTHETICresultsRecruitment.csv", row.names=1, stringsAsFactors=TRUE)
#eValuesTrials <- read.csv("./DataSynthetic/SYNTHETICeValuesTrials.csv", row.names=1, stringsAsFactors=TRUE)

# True results
resultsRecruitment <- read.csv("./Data/resultsRecruitment.csv", row.names=1, stringsAsFactors=TRUE)
eValuesTrials <- read.csv("./Data/eValuesTrials.csv", row.names=1, stringsAsFactors=TRUE)


resultsRecruitment$firstDateRand <- as.Date(resultsRecruitment$firstDateRand, , format = "%Y-%m-%d")
resultsRecruitment$lastDateRand <- as.Date(resultsRecruitment$lastDateRand, , format = "%Y-%m-%d")
resultsRecruitment$lastDateFup <- as.Date(resultsRecruitment$lastDateFup, , format = "%Y-%m-%d")
resultsRecruitment <- resultsRecruitment[resultsRecruitment$hospital == "total", ]

eValuesTrials$calendarDate <- as.Date(eValuesTrials$calendarDate, , format = "%Y-%m-%d")
startDate <- as.Date("2020-01-01", format = "%Y-%m-%d")

##########
##### 
##### 3a. Calculate ALL-IN meta-analysis e-values from trial e-values
##### 
##########

calcMeta <- function(eValuesTrials, eventTest, trials) {
  # This function creates a matrix of e-values by calendar date
  # and multiplies those together
  
  calendarDates <- as.character(seq.Date(from = as.Date(startDate), 
                                         to = max(eValuesTrials$calendarDate), 
                                         by = "day"))
  eValueMatrix <- matrix(NA, nrow = length(calendarDates),
                             ncol = length(trials),
                             dimnames = list(calendarDates, trials))
  for (trial in trials) {
    calendarDatesTrial <- as.character(eValuesTrials$calendarDate[eValuesTrials$trial == trial])
    #eValueMatrix[calendarDatesTrial, trial] <- eValuesObject[[trial]][[eventTest]][calendarDatesTrial]
    eValueMatrix[calendarDatesTrial, trial] <- eValuesTrials[[eventTest]][eValuesTrials$trial == trial]
    calendarDateLast <- as.Date(tail(calendarDatesTrial, n = 1))
    calendarDatesAfter <- as.character(seq.Date(from = calendarDateLast,
                                                to = max(eValuesTrials$calendarDate),
                                                by = "day"))
    eValueMatrix[calendarDatesAfter, trial] <- eValueMatrix[as.character(calendarDateLast), trial]
  }
  
  metaS <- apply(eValueMatrix, MARGIN = 1, FUN = prod)
  return(metaS)
}

metaEvaluesPrimaryList <- lapply(list("COV19L" = "COV19L", "COV19G" = "COV19G", 
                                  "COV19hospL" = "COV19hospL", "COV19hospG" = "COV19hospG"), 
                             function(t) calcMeta(eValuesTrials = eValuesTrials[eValuesTrials$trial != "AF", ], 
                                                  eventTest = t,
                                                  trials = c("NL", "HU", "BR", "US", 
                                                             "DK", "SA")))  # excluding AF
metaEvaluesSecundaryList <- lapply(list("COV19L" = "COV19L", "COV19G" = "COV19G", 
                                  "COV19hospL" = "COV19hospL", "COV19hospG" = "COV19hospG"), 
                             function(t) calcMeta(eValuesTrials = eValuesTrials, 
                                                  eventTest = t,
                                                  trials = c("NL", "HU", "BR", "US", 
                                                             "DK", "AF", "SA")))  # including AF

metaEvaluesPrimary <- rownames_to_column(as.data.frame(metaEvaluesPrimaryList), var = "calendarDate")
metaEvaluesPrimary$trial <- "ALL-IN (Primary, excl AF)"
metaEvaluesSecundary <- rownames_to_column(as.data.frame(metaEvaluesSecundaryList), var = "calendarDate")
metaEvaluesSecundary$trial <- "ALL-IN (Secondary, incl AF)"

eValues <- rbind(eValuesTrials, metaEvaluesPrimary)
eValuesSecondary <- rbind(eValuesTrials, metaEvaluesSecundary)


##########
##### 
##### 3b. Plot trial e-values and ALL-IN meta-analysis e-values
##### 
##########

# order of plotting in the subplot below the e-values
resultsRecruitment$rank <- 1:7
resultsRecruitment$position <- exp(14 - resultsRecruitment$rank*2)

cbbPalette <- c("NL" = "#D55E00", 
                "SA" = "#56B4E9",
                "US" = "#009E73", 
                "DK" = "#CC79A7",
                "HU" = "#F0E442", 
                "BR" = "#0072B2",  
                "AF" = "#E69F00",
                "ALL-IN (Primary, excl AF)" = "#000000")

Sys.setlocale("LC_TIME", "English")

ggplot(eValues) +
  geom_line(aes(x = calendarDate, y = COV19L, group = trial, col = trial), size = 0.8) +
  scale_color_manual(values = cbbPalette, name = "") +
  scale_y_log10(name = "e-value", limits = c(0.001, 1000), 
                breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), 
                labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"),
                sec.axis = sec_axis(trans = function(x) x, 
                                    breaks = c(0.001, 0.01, 0.1, 1, 10, 400),
                                    labels = c("0.001", "0.01", "0.1", "1", "10", "400"))) +
  scale_x_date(breaks = as.Date(c("2020-01-01",
                                  "2020-07-01",
                                  "2021-01-01",
                                  "2021-07-01",
                                  "2022-01-01",
                                  "2022-07-01")), date_labels = "%B %Y",
               minor_breaks = "1 month", name = "") +
  labs(size = "Recruitment") +
  expand_limits(x = as.Date(c("2020-01-01", "2022-07-01"))) +
  annotation_logticks(sides = "l") +
  annotation_logticks(sides = "r") +
  geom_hline(yintercept = 1, size = 0.5) +
  geom_hline(yintercept = 400, size = 0.5, linetype = "dashed") +
  geom_xsidesegment(aes(x = firstDateRand, xend = lastDateRand, y = position, 
                        yend = position, group = trial, col = trial, 
                        linetype = "period1",
                        size = "period1"), 
                    data = resultsRecruitment) +
  geom_xsidesegment(aes(x = lastDateRand, xend = lastDateFup, y = position, 
                        yend = position, group = trial, col = trial, 
                        linetype = "period2",
                        size = "period2"),
                    data = resultsRecruitment) +
  scale_size_manual(values = c("period1" = 2, "period2" = 0.6), labels = c("recruitment", "follow-up")) +
  scale_linetype_manual(values = c("period1" = 1, "period2" = 2), labels = c("recruitment", "follow-up")) +
  labs(size = "Period", linetype = "Period") +
  scale_xsidey_continuous(breaks = NULL, trans = "exp") + 
  ggside(x.pos = "bottom") +
  ggtitle("Test: COVID-19 infection hazard ratio (HR)", subtitle = "H0: HR = 1 against H1: HR < 0.8 (benefit)") +
  guides(size = guide_legend(order = 2), linetype = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme(text = element_text(size = 12))


### If actual data
ggsave(filename = "./Data/eCOV19L.jpg",
       height   = 90,
       width    = 297 - 60, 
       units    = "mm")

### If synthetic data
#ggsave(filename = "./DataSynthetic/SYNTHETICeCOV19L.jpg",
#       height   = 90,
#       width    = 297 - 60, 
#       units    = "mm")


ggplot(eValues) +
  geom_line(aes(x = calendarDate, y = COV19G, group = trial, col = trial), size = 0.8) +
  scale_color_manual(values = cbbPalette, name = "") +
  scale_y_log10(name = "e-value", limits = c(0.001, 1000), 
                breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), 
                labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"),
                sec.axis = sec_axis(trans = function(x) x, 
                                    breaks = c(0.001, 0.01, 0.1, 1, 10, 400),
                                    labels = c("0.001", "0.01", "0.1", "1", "10", "400"))) +
  scale_x_date(breaks = as.Date(c("2020-01-01",
                                  "2020-07-01",
                                  "2021-01-01",
                                  "2021-07-01",
                                  "2022-01-01",
                                  "2022-07-01")), date_labels = "%B %Y",
               minor_breaks = "1 month", name = "") +
  labs(size = "Recruitment") +
  expand_limits(x = as.Date(c("2020-01-01", "2022-07-01"))) +
  annotation_logticks(sides = "l") +
  annotation_logticks(sides = "r") +
  geom_hline(yintercept = 1, size = 0.5) +
  geom_hline(yintercept = 400, size = 0.5, linetype = "dashed") +
  ggtitle("Test: COVID-19 infection hazard ratio (HR)", subtitle = "H0: HR = 1 against H1: HR > 1/0.8 (harm)") +
  guides(size = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme(text = element_text(size = 12))


### If actual data
ggsave(filename = "./Data/eCOV19G.jpg",
       height   = 80,
       width    = 297 - 60, 
       units    = "mm")

### If synthetic data
#ggsave(filename = "./DataSynthetic/SYNTHETICeCOV19G.jpg",
#       height   = 80,
#       width    = 297 - 60, 
#       units    = "mm")

ggplot(eValues) +
  geom_line(aes(x = calendarDate, y = COV19hospL, group = trial, col = trial), size = 0.8) +
  scale_color_manual(values = cbbPalette, name = "") +
  scale_y_log10(name = "e-value", limits = c(0.1, 100), 
                breaks = c(0.1, 1, 10, 100), 
                labels = c("0.1", "1", "10", "100"),
                sec.axis = sec_axis(trans = function(x) x, 
                                    breaks = c(0.01, 0.1, 1, 10, 44),
                                    labels = c("0.01", "0.1", "1", "10", "44"))) +
  scale_x_date(breaks = as.Date(c("2020-01-01",
                                  "2020-07-01",
                                  "2021-01-01",
                                  "2021-07-01",
                                  "2022-01-01",
                                  "2022-07-01")), date_labels = "%B %Y",
               minor_breaks = "1 month", name = "") +
  labs(size = "Recruitment") +
  expand_limits(x = as.Date(c("2020-01-01", "2022-07-01"))) +
  annotation_logticks(sides = "l") +
  annotation_logticks(sides = "r") +
  geom_hline(yintercept = 1, size = 0.5) +
  geom_hline(yintercept = 44, size = 0.5, linetype = "dashed") +
  ggtitle("Test: COVID-19 hospitalisation hazard ratio (HR)", subtitle = "H0: HR = 1 against H1: HR < 0.7 (benefit)") +
  guides(size = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme(text = element_text(size = 12))

### If actual data
ggsave(filename = "./Data/eCOV19hospL.jpg",
       height   = 80,
       width    = 297 - 60, 
       units    = "mm")

### If synthetic data
#ggsave(filename = "./DataSynthetic/SYNTHETICeCOV19hospL.jpg",
#       height   = 80,
#       width    = 297 - 60, 
#       units    = "mm")

ggplot(eValues) +
  geom_line(aes(x = calendarDate, y = COV19hospG, group = trial, col = trial), size = 0.8) +
  scale_color_manual(values = cbbPalette, name = "") +
  scale_y_log10(name = "e-value", limits = c(0.1, 100), 
                breaks = c(0.1, 1, 10, 100), 
                labels = c("0.1", "1", "10", "100"),
                sec.axis = sec_axis(trans = function(x) x, 
                                    breaks = c(0.1, 1, 10, 44),
                                    labels = c("0.1", "1", "10", "44"))) +
  scale_x_date(breaks = as.Date(c("2020-01-01",
                                  "2020-07-01",
                                  "2021-01-01",
                                  "2021-07-01",
                                  "2022-01-01",
                                  "2022-07-01")), date_labels = "%B %Y",
               minor_breaks = "1 month", name = "") +
  labs(size = "Recruitment") +
  expand_limits(x = as.Date(c("2020-01-01", "2022-07-01"))) +
  annotation_logticks(sides = "l") +
  annotation_logticks(sides = "r") +
  geom_hline(yintercept = 1, size = 0.5) +
  geom_hline(yintercept = 44, size = 0.5, linetype = "dashed") +
  ggtitle("Test: COVID-19 hospitalisation hazard ratio (HR)", subtitle = "H0: HR = 1 against H1: HR > 1/0.7 (harm)") +
  guides(size = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme(text = element_text(size = 12))

### If actual data
ggsave(filename = "./Data/eCOV19hospG.jpg",
       height   = 80,
       width    = 297 - 60, 
       units    = "mm")

### If synthetic data
#ggsave(filename = "./DataSynthetic/SYNTHETICeCOV19hospG.jpg",
#       height   = 80,
#       width    = 297 - 60, 
#       units    = "mm")


##########
##### 
##### 4. Code for hypothetical e-value plots
##### 
##########

# Make sure eValuesSecondary is calculated on synthetic data (above at 2. and 2a. Code for e-value plots)
eValuesHyp <- eValuesSecondary
eValuesHyp$trial <- recode_factor(eValuesHyp$trial,
                                  "NL" = "hyp trial 1",
                                  "SA" = "hyp trial 2",
                                  "US" = "hyp trial 3",
                                  "DK" = "hyp trial 4",
                                  "HU" = "hyp trial 5",
                                  "BR" = "hyp trial 6",
                                  "AF" = "hyp trial 7",
                                  "ALL-IN (Secondary, incl AF)" = "hyp ALL-IN")

library(scales)
ggplotStandardPallette <- c(rev(hue_pal()(7)), "#000000")
names(ggplotStandardPallette) <- c("hyp trial 1", "hyp trial 4", "hyp trial 2", "hyp trial 5", "hyp trial 6", "hyp trial 7", "hyp trial 3",
                                   "hyp ALL-IN")
ggplotStandardPallette <- ggplotStandardPallette[c("hyp trial 1", "hyp trial 2", "hyp trial 3", "hyp trial 4", "hyp trial 5",
                                                   "hyp trial 6", "hyp trial 7", "hyp ALL-IN")]


resultsRecruitmentHyp <- resultsRecruitment
resultsRecruitmentHyp <- resultsRecruitment[resultsRecruitment$hospital == "total", ]
resultsRecruitmentHyp$trial <- factor(c("hyp trial 1", "hyp trial 2", "hyp trial 3", "hyp trial 4", "hyp trial 5", "hyp trial 6", "hyp trial 7"), 
                                   levels = c("hyp trial 1", "hyp trial 2", "hyp trial 3", "hyp trial 4", "hyp trial 5", "hyp trial 6", "hyp trial 7", "hyp ALL-IN"))
resultsRecruitmentHyp$rank <- 1:7
resultsRecruitmentHyp$position <- exp(14 - resultsRecruitmentHyp$rank*2)

Sys.setlocale("LC_TIME", "English")

ggplot(eValuesHyp) +
  geom_line(aes(x = calendarDate, y = COV19L, group = trial, col = trial), size = 0.8) +
  scale_color_manual(values = ggplotStandardPallette) +
  scale_y_log10(name = "e-value", limits = c(0.1, 1000), 
                breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), 
                labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"),
                sec.axis = sec_axis(trans = function(x) x, 
                                    breaks = c(0.001, 0.01, 0.1, 1, 10, 400),
                                    labels = c("0.001", "0.01", "0.1", "1", "10", "400"))) +
  scale_x_date(breaks = as.Date(c("2020-01-01",
                                  "2020-07-01",
                                  "2021-01-01",
                                  "2021-07-01",
                                  "2022-01-01",
                                  "2022-07-01")), date_labels = "%B %Y",
               minor_breaks = "1 month", name = "") +
  expand_limits(x = as.Date(c("2020-01-01", "2022-01-01"))) +
  annotation_logticks(sides = "l") +
  annotation_logticks(sides = "r") +
  geom_hline(yintercept = 1, size = 0.5) +
  geom_hline(yintercept = 400, size = 0.5, linetype = "dashed") +
  # point where the e-value crossed the 400 of different synthetic data
  #geom_vline(xintercept = as.Date("2021-03-06"), linetype = "dotted") +
  geom_xsidesegment(aes(x = firstDateRand, xend = lastDateRand, y = rank, 
                        yend = rank, group = trial, col = trial, 
                        linetype = "Period1",
                        size = "Period1"), 
                    data = resultsRecruitmentHyp) +
  geom_xsidesegment(aes(x = lastDateRand, xend = lastDateFup, y = rank, 
                        yend = rank, group = trial, col = trial, 
                        linetype = "Period2",
                        size = "Period2"),
                    data = resultsRecruitmentHyp) +
  scale_size_manual(values = c("Period1" = 2, "Period2" = 0.6), labels = c("recruitment", "follow-up")) +
  scale_linetype_manual(values = c("Period1" = 1, "Period2" = 2), labels = c("recruitment", "follow-up")) +
  labs(size = "Period", linetype = "Period", col = "Hypothetical trials \n simulated \n with HR = 0.7") +
  scale_xsidey_continuous(breaks = NULL, trans = "exp") + 
  ggside(x.pos = "bottom") +
  ggtitle("Hypothetical Test: COVID-19 infection hazard ratio (HR)", subtitle = "H0: HR = 1 against H1: HR < 0.8 (benefit)") +
  guides(size = guide_legend(order = 2), linetype = guide_legend(order = 2), color = guide_legend(order = 1)) +
  theme(text = element_text(size = 12))



ggsave(filename = "./DataSynthetic/SYNTHETICeCOV19Lhyp.jpg",
       height   = 120,
       width    = 297 - 60, 
       units    = "mm")



##########
##### 
##### 5. Code for cumulative hazard plot NL data
##### 
##########

# Synthetic results
#dataNL <- read.csv("./DataSynthetic/dataNLsynth.csv", row.names = 1, stringsAsFactors = TRUE)

# True results
dataNL <- read.csv("./Data/data_NL_2022-11-06_final.csv", row.names = 1, stringsAsFactors = TRUE)



dataNL$intervention <- factor(dataNL$intervention, levels = c("control", "BCG"))
dataNL$dateRand <- as.Date(dataNL$dateRand, format = "%Y-%m-%d")
dataNL$dateLastFup <- as.Date(dataNL$dateLastFup, format = "%Y-%m-%d")
dataNL$dateCOV19 <- as.Date(dataNL$dateCOV19, format = "%Y-%m-%d")

startDate <- as.Date("2020-01-01")  # some date before the first randomization in the first trial
dataNL$timeCOV19 <- ifelse(dataNL$COV19 %in% c("Yes", "yes"),
                           dataNL$dateCOV19 - startDate,
                           dataNL$dateLastFup - startDate)
dataNL$statusCOV19 <- ifelse(dataNL$COV19 %in% c("Yes", "yes"),
                                         2, 1)
dataNL$survObjCOV19 <- Surv(time = dataNL$dateRand - startDate, 
                            time2 = dataNL$timeCOV19,
                            event = dataNL$statusCOV19,
                            type = "counting")
  
fitDataNL <- survfit(survObjCOV19 ~ intervention, data = dataNL)
cumHazDataNL <- ggsurvplot(fitDataNL, data = dataNL, conf.int = TRUE, fun = "cumhaz", risk.table = TRUE,
                           xlim = c(0, 366 + 365 + 31+28+31+30+31+30), legend.labs = c("control                                           ", "BCG"), 
                           legend.title = "", x.title = "", palette = c("grey", "#D55E00"))

cumHazDataNL$plot + 
  scale_x_continuous(breaks = c(0, 
                                31+29+31+30+31+30,
                                366,
                                366 + 31+28+31+30+31+30,
                                366 + 365,
                                366 + 365 + 31+28+31+30+31+30),  
                     labels = c("January 2020",
                                "July 2020",
                                "January 2021",
                                "July 2021",
                                "January 2022",
                                "July 2022"),
                     name = "") +
  theme_grey() +
  ggtitle("Cumulative Hazard curve for COVID-19 infections", subtitle = "NL trial") +
  theme(text = element_text(size = 12))



### If actual data
ggsave(filename = "./Data/cumHaztrialNL.jpg",
       height   = 70,
       width    = 297 - 60, 
       units    = "mm")

### If synthetic data
#ggsave(filename = "./DataSynthetic/SYNTHETICcumHaztrialNL.jpg",
#       height   = 70,
#       width    = 297 - 60, 
#       units    = "mm")


##########
##### 
##### 6. Code for anytime-valid 95%-confidence interval plots
##### 
##########

# Synthetic results
#resultsRecruitment <- read.csv("./DataSynthetic/SYNTHETICresultsRecruitment.csv", row.names=1, stringsAsFactors=TRUE)
#confidenceSequencesTrials <- read.csv("./DataSynthetic/SYNTHETICconfidenceSequencesTrials.csv", row.names=1, stringsAsFactors=TRUE)
#resultsDateCOV19 <- read.csv("./DataSynthetic/SYNTHETICresultsDateCOV19.csv", row.names=1, stringsAsFactors=TRUE)

# True results
resultsRecruitment <- read.csv("./Data/resultsRecruitment.csv", row.names=1, stringsAsFactors=TRUE)
confidenceSequencesTrials <- read.csv("./Data/confidenceSequencesTrials.csv", row.names=1, stringsAsFactors=TRUE)
resultsDateCOV19 <- read.csv("./Data/resultsDateCOV19.csv", row.names=1, stringsAsFactors=TRUE)


confidenceSequencesTrials$calendarDate <- as.Date(confidenceSequencesTrials$calendarDate)

resultsRecruitment$firstDateRand <- as.Date(resultsRecruitment$firstDateRand, , format = "%Y-%m-%d")
resultsRecruitment$lastDateRand <- as.Date(resultsRecruitment$lastDateRand, , format = "%Y-%m-%d")
resultsRecruitment$lastDateFup <- as.Date(resultsRecruitment$lastDateFup, , format = "%Y-%m-%d")
resultsRecruitment <- resultsRecruitment[resultsRecruitment$hospital == "total", ]

resultsDateCOV19$interimDates <- as.Date(resultsDateCOV19$interimDates, format = "%Y-%m-%d")



##########
##### 
##### 6a. Calculate ALL-IN meta-analysis 95%-confidence intervals from
#####     trial 95%-confidence intervals summary statistics
##### 
##########

confidenceSequencesTrialsSecundary <- confidenceSequencesTrials
confidenceSequencesTrials <- confidenceSequencesTrials[!(confidenceSequencesTrials$trial == "AF"), ]
trials <- unique(confidenceSequencesTrials$trial)
trials <- trials[!(trials == "AF")]  # only Secondary analysis excluding AF

### Fill matrices
lastDateAnyTrial <- as.Date(max(confidenceSequencesTrials$calendarDate))
firstDateAnyTrial <- as.Date(min(confidenceSequencesTrials$calendarDate))
calendarDates <- as.character(seq.Date(from = firstDateAnyTrial, 
                                       to = lastDateAnyTrial, 
                                       by = "day"))
MLEhrCOV19Matrix <- matrix(NA, nrow = length(calendarDates),
                           ncol = length(trials),
                           dimnames = list(calendarDates, trials))
seLogMLEhrCOV19Matrix <- matrix(NA, nrow = length(calendarDates),
                                ncol = length(trials),
                                dimnames = list(calendarDates, trials))

for (trial in trials) {
  calendarDatesTrial <- as.character(confidenceSequencesTrials$calendarDate[confidenceSequencesTrials$trial == trial])
  
  MLEhrCOV19Matrix[calendarDatesTrial, trial] <- confidenceSequencesTrials$MLEhrCOV19[confidenceSequencesTrials$trial == trial]
  seLogMLEhrCOV19Matrix[calendarDatesTrial, trial] <- confidenceSequencesTrials$seLogMLEhrCOV19[confidenceSequencesTrials$trial == trial]
  
  calendarDateLast <- as.Date(tail(calendarDatesTrial, n = 1))
  calendarDatesAfter <- as.character(seq.Date(from = calendarDateLast,
                                              to = lastDateAnyTrial,
                                              by = "day"))
  MLEhrCOV19Matrix[calendarDatesAfter, trial] <- MLEhrCOV19Matrix[as.character(calendarDateLast), trial]
  seLogMLEhrCOV19Matrix[calendarDatesAfter, trial] <- seLogMLEhrCOV19Matrix[as.character(calendarDateLast), trial]
}

startDate <- min(confidenceSequencesTrials$calendarDate)
endDate <- max(confidenceSequencesTrials$calendarDate)
calendarDates <- as.character(seq.Date(from = as.Date(startDate),
                                       to = endDate,
                                       by = "day"))
fup <- length(calendarDates)

confSeq <- as.data.frame(matrix(NA, ncol = 7, nrow = fup, 
                                dimnames = list(calendarDates, 
                                                c("trial", "calendarDate", "MLEhrCOV19", "seLogMLEhrCOV19",
                                                  "weight", "lowerMLEhrCOV19CI95", 
                                                  "upperMLEhrCOV19CI95"))))


confSeq[, "trial"] <- rep("ALL-IN (Primary, excl AF)", times = fup)

# loop trough the calendar dates
for (calDate in calendarDates) {
  confSeq[calDate, "calendarDate"] <- calDate
  
  MLEhrCOV19 <- MLEhrCOV19Matrix[calDate, ]
  seLogMLEhrCOV19 <- seLogMLEhrCOV19Matrix[calDate, ]
  
  typMLEhrCOV19 <- exp(sum(log(MLEhrCOV19)*(1/(seLogMLEhrCOV19^2)), na.rm = TRUE)/
                         sum(1/(seLogMLEhrCOV19^2), na.rm = TRUE))
  seLogtypMLEhrCOV19 <- sqrt(1/sum((1/seLogMLEhrCOV19^2), na.rm = TRUE))
  
  typMLEhrCOV19CI95 <- exp(computeConfidenceIntervalZ(nEff = 1, 
                                                      meanObs = log(typMLEhrCOV19),
                                                      phiS = log(designObj$COV19L$parameter),
                                                      sigma = seLogtypMLEhrCOV19, 
                                                      ciValue = 0.95, alternative = "twoSided"))
  confSeq[calDate, "MLEhrCOV19"] <- typMLEhrCOV19
  confSeq[calDate, "seLogMLEhrCOV19"] <- seLogtypMLEhrCOV19
  confSeq[calDate, "lowerMLEhrCOV19CI95"] <- typMLEhrCOV19CI95[1]
  confSeq[calDate, "upperMLEhrCOV19CI95"] <- typMLEhrCOV19CI95[2]
}  

confidenceSequences <- rbind(confidenceSequencesTrialsSecundary, confSeq)



##########
##### 
##### 6b. Filter confidence interval data for size of confidence interval and
#####     number of dates represented
##### 
##########

# Filter for plotting: show interval with maximum with between hazard ratio 0.2 and 5
confidenceSequences$lowerMLEhrCOV19CI95[confidenceSequences$lowerMLEhrCOV19CI95 < 0.2] <- 0.2
confidenceSequences$upperMLEhrCOV19CI95[confidenceSequences$upperMLEhrCOV19CI95 > 5] <- 5
confidenceSequences$meta <- factor(ifelse(confidenceSequences$trial == "ALL-IN", "ALL-IN", "trials"),
                                   levels = c("trials", "ALL-IN"))

confidenceSequences$calendarDate <- as.Date(confidenceSequences$calendarDate, format = "%Y-%m-%d")


# Filter for plotting: show intervals only every 16 days, with the days shifted
# by trial (days modulo 16)

confidenceSequences[(confidenceSequences$trial == "NL") &
                     (as.numeric(confidenceSequences$calendarDate - startDate) %% 16) != 0, 
                     c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "ALL-IN (Primary, excl AF)") &
                      (as.numeric(confidenceSequences$calendarDate - startDate) %% 16) != 12, 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "DK") &
                      (as.numeric(confidenceSequences$calendarDate - startDate) %% 16) != 6, 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "US") &
                      (as.numeric(confidenceSequences$calendarDate - startDate) %% 16) != 10, 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "HU") &
                      (as.numeric(confidenceSequences$calendarDate - startDate) %% 16) != 2, 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "HU") & 
                      (confidenceSequences$calendarDate < as.Date("2020-12-01")), 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "BR") &
                      (as.numeric(confidenceSequences$calendarDate - startDate) %% 16) != 8, 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "BR") & 
                     (confidenceSequences$calendarDate < as.Date("2021-07-01")), 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "AF") &
                      (as.numeric(confidenceSequences$calendarDate - startDate) %% 16) != 12, 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA
confidenceSequences[(confidenceSequences$trial == "SA") &
                      (as.numeric(confidenceSequences$calendarDate - startDate) %% 16) != 4, 
                    c("lowerMLEhrCOV19CI95", "upperMLEhrCOV19CI95")] <- NA


##########
##### 
##### 6c. Plot trial 95%-confidence intervals and 
#####     ALL-IN meta-analysis 95%-confidence intervals
##### 
##########


cbbPalette <- c("NL" = "#D55E00", 
                "SA" = "#56B4E9",
                "US" = "#009E73", 
                "DK" = "#CC79A7",
                "HU" = "#F0E442", 
                "BR" = "#0072B2",  
                "AF" = "#E69F00",
                "ALL-IN (Primary, excl AF)" = "#000000")

# order of plotting in the subplot below the e-values
resultsRecruitment$rank <- 1:7
resultsDateCOV19$rank <- recode(resultsDateCOV19$trial, 
                                "NL" = 1,
                                "SA" = 2,
                                "US" = 3,
                                "DK" = 4,
                                "HU" = 5,
                                "BR" = 6,
                                "AF" = 7)
resultsRecruitment$position <- exp(14 - resultsRecruitment$rank*2)
resultsDateCOV19$position <- exp(14 - resultsDateCOV19$rank*2)



Sys.setlocale("LC_TIME", "English")
ggplot(as.data.frame(confidenceSequences)) + 
  geom_hline(yintercept = 1, size = 0.5) +
  geom_segment(aes(x = calendarDate, xend = calendarDate, y = lowerMLEhrCOV19CI95, yend = upperMLEhrCOV19CI95, colour = trial), size = 1) +
  scale_colour_manual(values = cbbPalette, name = "") +
  scale_y_continuous(trans = 'log10', 
                     limits = c(0.2, 5),
                     breaks = c(0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1, 
                                2, 5),
                     labels = c("0.05", "0.1", "0.2", "0.4", "0.6", "0.8", "1", 
                                "2", "5"),
                     sec.axis = sec_axis(trans = function(x) x,
                                         breaks = c(0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1),
                                         labels = c("95%", "90%", "80%", "60%", "40%", "20%", "0%"),
                                         name = "estimated vaccine efficacy (VE)")) +
  annotation_logticks(sides = "l", base = 10) +
  annotation_logticks(sides = "r", base = 10) +
  scale_x_date(breaks = as.Date(c("2020-01-01",
                                  "2020-07-01",
                                  "2021-01-01",
                                  "2021-07-01",
                                  "2022-01-01",
                                  "2022-07-01")), date_labels = "%B %Y",
               minor_breaks = "1 month", name = "", limits = as.Date(c("2020-01-01", "2022-07-01"))) +
  geom_xsidesegment(aes(x = firstDateRand, xend = lastDateRand, y = position, 
                        yend = position, group = trial, col = trial, 
                        linetype = "period1",
                        size = "period1"), 
                    data = resultsRecruitment) +
  geom_xsidesegment(aes(x = lastDateRand, xend = lastDateFup, y = position, 
                        yend = position, group = trial, col = trial, 
                        linetype = "period2",
                        size = "period2"),
                    data = resultsRecruitment) +
  geom_xsidepoint(aes(x = interimDates, y = position, col = trial, shape = "A"), size = 1, data = resultsDateCOV19) +
  scale_size_manual(values = c("period1" = 0.3, "period2" = 0.3), labels = c("recruitment", "follow-up")) +
scale_linetype_manual(values = c("period1" = 1, "period2" = 2), labels = c("recruitment", "follow-up")) +
scale_shape_manual(values = c("A" = 4), label = "events", name = "") +
labs(size = "Period", linetype = "Period", y = "estimated hazard ratio (HR)") +
  scale_xsidey_continuous(breaks = NULL, trans = "exp") + 
  ggside(x.pos = "bottom") +
  #facet_wrap(vars(meta), ncol = 1) +
  #facet_grid(rows = vars(meta)) +
  guides(shape = guide_legend(order = 2), size = guide_legend(order = 3), linetype = guide_legend(order = 3), color = guide_legend(order = 1)) +
  ggtitle("Typical effect COVID-19 infection hazard ratio (HR)", subtitle = "Anytime-valid 95%-confidence intervals (95%-confidence sequence)") +
  theme(#strip.text.y = element_blank(), 
    text = element_text(size = 12))


### If actual data
ggsave(filename = "./Data/confSeq.jpg",
       height   = 130,
       width    = 297 - 60, 
       units    = "mm")

### If synthetic data
#ggsave(filename = "./DataSynthetic/SYNTHETICconfSeq.jpg",
#       height   = 130,
#       width    = 297 - 60, 
#       units    = "mm")

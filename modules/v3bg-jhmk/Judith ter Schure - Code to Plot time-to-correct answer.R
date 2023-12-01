### Author: Judith ter Schure
### E-mail: j.a.terschure@amsterdamumc.nl
### Institute: Amsterdam UMC
###            AMC Location, Meibergdreef 9, 1105 AZ Amsterdam, NETHERLANDS
### Date: 11 October 2023 (niet opgeschoond!!)
### Licence: CC-BY 4.0

### This code is part of the supplementary material
### for the publication:
###   Klappe, Heijmans, Groen, Ter Schure, Cornet, De Keizer (2023) 
### Correctly structured problem lists lead to better and faster clinical 
### decision-making in electronic health records compared to non-curated 
### problem lists: a single-blinded crossover randomized controlled trial 

### Details on the OS, system and software that produced the results
### in the paper:

# Platform: x86_64-w64-mingw32/x64 (64-bit)
# OS: Microsoft Windows
# System: Windows 10 x64 (build 19042)
# R version: 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"

### The following packages were used:
# ggplot2 version 3.4.1
# dplyr version 1.1.0
# tidyr version 1.3.0

# stats version 4.2.2
# graphics version 4.2.2
# grDevices version 4.2.2
# utils version 4.2.2
# datasets version 4.2.2
# methods version 4.2.2
# base version 4.2.2

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


###### Figure time-to-correct-answer based on dataset_transformed_correct.xlsx
dataset_transformed_correct <- read_excel("dataset_transformed_correct.xlsx")

# Work on a copy called 'df' for 'dataframe'
df <- dataset_transformed_correct

# Function to calculate ranks per RECORDID
calcRank <- function(ID, rankAcc_of_Inacc, rankTijd) {
  timePerRECORDID <- subset(df, Acc_of_Inacc == rankAcc_of_Inacc, select = c("RECORDID", rankTijd))
  timePerRECORDID$rank <- rank(subset(timePerRECORDID, select = rankTijd), ties.method = "min")
  rankforID <- subset(timePerRECORDID, RECORDID == ID, select = rank)
  return(rankforID)
}
calcRankVectorized <- Vectorize(calcRank, vectorize.args = "ID")

# Function to calculate difference per RECORDID
calcDifference <- function(ID, tijdDiff) {
  tijdAccuraat <- subset(df, RECORDID == ID & Acc_of_Inacc == "Accuraat", tijdDiff)
  tijdInaccuraat <- subset(df, RECORDID == ID & Acc_of_Inacc == "Inaccuraat", tijdDiff)
  difference <- tijdAccuraat - tijdInaccuraat
  return(difference)
}

calcDifferenceVectorized <- Vectorize(calcDifference, vectorize.args = "ID")

# Function to calculate rank of difference per RECORDID
calcPairedRank <- function(ID, tijdDiff, absolute = FALSE) {
  timePerRECORDID <- subset(df, Acc_of_Inacc == "Accuraat", select = c("RECORDID", tijdDiff))
  timePerRECORDID$diff <- unlist(sapply(timePerRECORDID$RECORDID, function(ID) calcDifferenceVectorized(ID, tijdDiff = tijdDiff)))
  if(absolute) {
    time <- abs(timePerRECORDID$diff)
  } else {
    time <- timePerRECORDID$diff
  }
  timePerRECORDID$rank <- rank(time, ties.method = "min")
  rankforID <- subset(timePerRECORDID, RECORDID == ID, select = rank)
  return(rankforID)
}

calcPairedRankVectorized <- Vectorize(calcPairedRank, vectorize.args = "ID")

# Add ranks to df
df$rankAccuraat <- unlist(calcRankVectorized(df$RECORDID, rankAcc_of_Inacc = "Accuraat", rankTijd = "tijd_mutated"))
df$rankInaccuraat <- unlist(calcRankVectorized(df$RECORDID, rankAcc_of_Inacc = "Inaccuraat", rankTijd = "tijd_mutated"))
df$rankAccuraatIncorrect <- unlist(calcRankVectorized(df$RECORDID, rankAcc_of_Inacc = "Accuraat", rankTijd = "tijd"))
df$rankDiff <- unlist(calcPairedRankVectorized(df$RECORDID, tijdDiff = "tijd_mutated"))
df$rankDiffAbs <- unlist(calcPairedRankVectorized(df$RECORDID, tijdDiff = "tijd_mutated", absolute = TRUE))
df$rankDiffIncorrect <- unlist(calcPairedRankVectorized(df$RECORDID, tijdDiff = "tijd"))
df$rankDiffIncorrectAbs <- unlist(calcPairedRankVectorized(df$RECORDID, tijdDiff = "tijd", absolute = TRUE))

# Add leading zeros to the ranks for sorting as character (otherwise it will sort as "1" "10" "111" "2" etc)
df$rankAccuraat <- sprintf("%03d", df$rankAccuraat)
df$rankInaccuraat <- sprintf("%03d", df$rankInaccuraat)
df$rankAccuraatIncorrect <- sprintf("%03d", df$rankAccuraatIncorrect)
df$rankDiff <- sprintf("%03d", df$rankDiff)
df$rankDiffIncorrect <- sprintf("%03d", df$rankDiffIncorrect)
df$rankDiffAbs <- sprintf("%03d", df$rankDiffAbs)
df$rankDiffIncorrectAbs <- sprintf("%03d", df$rankDiffIncorrectAbs)

# Combine ranks into one identifying character string (that can be sorted)
df$rankID <- unite(df, rankID, c(rankAccuraat, rankInaccuraat, rankAccuraatIncorrect))$rankID
df$rankIDDiff <- unite(df, rankIDDiff, c(rankDiff, rankDiffIncorrect))$rankIDDiff
df$rankIDDiffAbs <- unite(df, rankIDDiffAbs, c(rankDiffAbs, rankDiffIncorrectAbs))$rankIDDiffAbs

# Function to calculate comparison per RECORDID: Is Inaccuraat faster or slower than Accuraat?
calcComparison <- function(ID) {
  tijdAccuraat <- subset(df, RECORDID == ID & Acc_of_Inacc == "Accuraat", tijd_mutated)
  tijdInaccuraat <- subset(df, RECORDID == ID & Acc_of_Inacc == "Inaccuraat", tijd_mutated)
  comparison <- ifelse(tijdAccuraat < tijdInaccuraat, "slower", ifelse(tijdAccuraat > tijdInaccuraat, "faster", "equal")) 
  return(comparison)
}

calcComparisonVectorized <- Vectorize(calcComparison, vectorize.args = "ID")

# Add comparison color to df
df$comparison <- NA
df$comparison[df$Acc_of_Inacc == "Accuraat"] <- "Accuraat"  # Show the comparison only in the colors of the 'Inaccuraat' times
df$comparison[df$Acc_of_Inacc == "Inaccuraat"] <- calcComparisonVectorized(df$RECORDID[df$Acc_of_Inacc == "Inaccuraat"])
  
# Change to factor to facilitate legends in ggplot
df$comparison <- as.factor(df$comparison)
df$correct_answer_medXY_for_time <- as.factor(df$correct_answer_medXY_for_time)

# Set the mutated values of 999999 to 730 for plotting
summary(df$tijd)  # 730 seconds is larger than any actual measured time
df$tijd_mutated[df$tijd_mutated > 730] <- 730  # so this only affects the mutated times set at 999999

#bestHalf <- ggplot(subset(df, rankIDDiffAbs %in% sort(df$rankIDDiffAbs)[1:158])) +
worstHalf <- ggplot(subset(df, rankIDDiffAbs %in% sort(df$rankIDDiffAbs)[159:316])) +  
  geom_point(aes(x = tijd_mutated, y = Acc_of_Inacc, shape = correct_answer_medXY_for_time, color = comparison), size = 2) +
  geom_linerange(aes(xmin = 0, xmax = tijd_mutated, color = comparison, y = Acc_of_Inacc), linewidth = 0.5, linetype = "dashed") +
  geom_linerange(aes(xmin = 0, xmax = tijd, color = comparison, y = Acc_of_Inacc), linewidth = 0.8, linetype = "solid") +
  scale_shape_manual(breaks = c("0", "1"), values = c(13, 16), 
                     labels = c("one of two incorrect \n (time-to both correct \n infinite)", "\n both correct \n"),  
                     name = "Prescription answers", drop = FALSE) +
  scale_color_manual(breaks = c("Accuraat", "equal", "slower", "faster"), 
                     values = c("grey20", "grey40", "orange", "blue"), 
                     labels = c("C: Correctly structured", 
                                "N: Non-curated",
                                "N: Non-curated slower \n      C faster",
                                "N: Non-curated faster \n      C slower"), name = "Paired comparison", drop = FALSE) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_x_continuous(name = "Time to both answers (minutes)", 
                     breaks = seq(from = 0, to = 60*11, by = 60),
                     labels = c("0", "", "2", "", "4", "", "6", "", "8", "", "10", ""),
                     sec.axis = sec_axis(function(x) x, 
                                         name = "Time to both answers (minutes)", 
                                         breaks = seq(from = 0, to = 60*11, by = 60),
                                         labels = c("0", "", "2", "", "4", "", "6", "", "8", "", "10", ""))) +
  scale_y_discrete(labels = c("   C", "N   "), name = "") +
  facet_grid(rows = vars(rankIDDiffAbs)) +
  theme(strip.text.y = element_text(angle = 360), axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 12), legend.text = element_text(size = 12), legend.title = element_text(size = 15),
        panel.grid.major.x = element_line(color = "grey50"), panel.background = element_rect(fill = NA))

ggsave("time-to-answer_bestHalfrankDiffAbs.jpg", plot = bestHalf, width = 30*(5/9), height = 30*(29.7/21), units = "cm")
ggsave("time-to-answer_worstHalfrankDiffAbs.jpg", plot = worstHalf, width = 30*(5/9), height = 30*(29.7/21), units = "cm")


inaccuraat_mutated <- subset(df,  Acc_of_Inacc == "Inaccuraat", tijd_mutated,
                             drop = TRUE)

accuraat_mutated <- subset(df,  Acc_of_Inacc == "Accuraat", tijd_mutated,
                           drop = TRUE)

# now combine them
pd_mutated <- paired(inaccuraat_mutated, accuraat_mutated)

test_mutated <- wilcox.test(inaccuraat_mutated, accuraat_mutated, paired=T, correct=F, alternative="greater")
test_mutated # it says: inaccurate (first variable) has a greater median than accurate (second variable). Changing them will give p = 1.
# but you can also change alternative to 'less'
# reported as p<.001 in 'Time to correct answers'

wilcox.exact(accuraat_mutated, inaccuraat_mutated, paired=T, exact=T, conf.int=T, conf.level=0.95, correct=F)

# beide zijn p=0000.2, dus dat maakt niet uit, maar volgens mij moet ik de 
# alternative p waarde gebruiken!

## data analysis for final project: Biostats Fall 2020
# by Anna Foster
# Topic: ADHD questionnaire over two separate periods in a study about working memory and reward in children
  # with and without ADHD

# some potential questions:
  # could see if the questions asked are a good measure of ADHD diagnosis
  # or if the "symptoms" decrease with medication or time
  # we can compare medications too
  # or look at the common traits of people with ADHD (if there are any: sex, age, etc.?)

rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(AICcmodavg)
library(PerformanceAnalytics)

participants <- read_excel("Final Project/participants.xlsx")
View(participants)                                                                                                          
phenotype1 <- read_excel("Final Project/phenotype_ses-T1_adhd-rs.xlsx")
View(phenotype1)                                                                                              
phenotype2 <- read_excel("Final Project/phenotype_ses-T2_adhd-rs.xlsx")
View(phenotype2)
phenotypes <- merge(phenotype1, phenotype2, by = "participant_id")
View(phenotypes)
ADHD <- merge(participants, phenotypes)
ADHD <- dplyr::select(ADHD, -(8:15))  # removed some columns we don't need

View(ADHD)

# first we have to fix the classes of the variables
ADHD[,3] <- as.numeric(ADHD[,3])
ADHD[,4] <- as.factor(ADHD[,4])
ADHD[,5] <- as.factor(ADHD[,5])
ADHD[,6] <- as.factor(ADHD[,6])
ADHD[,11:49] <- lapply(ADHD[,11:49], as.numeric)   # thank you stackoverflow.com

### Our Question:
# we want to see which questions best predict the total raw score

# check first if data follows normal distribution
autoplot(lm(ADHD_Total_Raw.x ~ Q1.x, data = ADHD))  ## low at bottom
autoplot(lm(ADHD_Total_Raw.x ~ Q2.x, data = ADHD))  ## kinda curvy at top
autoplot(lm(ADHD_Total_Raw.x ~ Q3.x, data = ADHD))  # more linear
autoplot(lm(ADHD_Total_Raw.x ~ Q4.x, data = ADHD))  # pretty linear
autoplot(lm(ADHD_Total_Raw.x ~ Q5.x, data = ADHD))  # more linear
autoplot(lm(ADHD_Total_Raw.x ~ Q6.x, data = ADHD))  ## curves at top
autoplot(lm(ADHD_Total_Raw.x ~ Q7.x, data = ADHD))  ## above at top 1/2
autoplot(lm(ADHD_Total_Raw.x ~ Q8.x, data = ADHD))  # pretty linear, bottom gets away
autoplot(lm(ADHD_Total_Raw.x ~ Q9.x, data = ADHD))  
autoplot(lm(ADHD_Total_Raw.x ~ Q10.x, data = ADHD))  # slopes below at top
autoplot(lm(ADHD_Total_Raw.x ~ Q11.x, data = ADHD))  ## messy at bottom
autoplot(lm(ADHD_Total_Raw.x ~ Q12.x, data = ADHD)) 
autoplot(lm(ADHD_Total_Raw.x ~ Q13.x, data = ADHD))  # more linear
autoplot(lm(ADHD_Total_Raw.x ~ Q14.x, data = ADHD))  # more linear
autoplot(lm(ADHD_Total_Raw.x ~ Q15.x, data = ADHD))  # low at bottom but pretty linear
autoplot(lm(ADHD_Total_Raw.x ~ Q16.x, data = ADHD))  # very linear
autoplot(lm(ADHD_Total_Raw.x ~ Q17.x, data = ADHD))  # pretty linear
autoplot(lm(ADHD_Total_Raw.x ~ Q18.x, data = ADHD))  # very linear
autoplot(lm(ADHD_Total_Raw.x ~ ADHD_diagnosis, data= ADHD))  # kinda linear, slopes away at top and bottom
## all questions appear fairly normally distributed when compared to the total score

# check relation between questions (likely will be)
pair_data <- ADHD[, 11:28]; pair_data
chart.Correlation(pair_data, histogram=TRUE, pchisq=19)
?chart
  # as expected, all the variables are fairly correlated
  # with the odd questions often together, and the even questions often together (expect Q2)
# after looking closely at which ones have a correlation over 0.7, we
# only need to look at Q1, 2, 4, 5, 6, 8, and 14
  # these 7 variables all have high correlations with the remaining variables:
    # 1: 3,7,9,11,13,15,17
    # 2: 3,7,15
    # 4 and 5: none
    # 6: 10
    # 8: 12
    # 14: 16,18

#####
# research question: what questions best predict the total raw score?
  # dependent variable is ADHD Total Raw Score
  # independent variables are the questions asked of participants (see ADHD-RS Description code)
    # after looking at correlations, we only will look at questions 1,2,4,5,6,8,14
    # as these variables can only have values 0 -> 3, there are no outliers
# all variables follow normality fairly well, 
  # but questions 1,2, and 6 taper off at the ends more than the others

# hypothesis: it is predicted that Questions 4 and 5 will be the best as they are the first in the order with the best normal distributions
  # we can reject or support this hypothesis through multiple regression and AIC
    # AIC tests how well each questions' linear model predicts total raw score
####

# our models:
Q1.model <- lm(ADHD_Total_Raw.x ~ Q1.x, data = ADHD)
Q2.model <- lm(ADHD_Total_Raw.x ~ Q2.x, data = ADHD)
Q4.model <- lm(ADHD_Total_Raw.x ~ Q4.x, data = ADHD)  
Q5.model <- lm(ADHD_Total_Raw.x ~ Q5.x, data = ADHD)  
Q6.model <- lm(ADHD_Total_Raw.x ~ Q6.x, data = ADHD)  
Q8.model <- lm(ADHD_Total_Raw.x ~ Q8.x, data = ADHD)
Q14.model <- lm(ADHD_Total_Raw.x ~ Q14.x, data = ADHD)
anova(Q1.model)
anova(Q2.model)
anova(Q4.model)
anova(Q5.model)
anova(Q6.model)
anova(Q8.model)
anova(Q14.model)  # they all have tiny p-values

# AIC analysis
questionModels <- list(Q1.model, Q2.model, Q4.model, Q5.model, Q6.model, Q8.model, Q14.model)
questionModels
aictab(cand.set = questionModels, second.ord = TRUE, sort = TRUE)
  # questions 1 and 2 the best (about the same)
  # then Q4 (within 7 points of the first two)
  # next 5 and 8 almost same, then 14, and lastly 6
# so we'll analyze the first 3 questions here
summary(Q1.model)  # size/slope is 10.669
summary(Q2.model)  # size/slope is 11.841
summary(Q4.model)  # size/slope of 11.614

# our plots of the best models (Q1, Q2 and Q4)
count <- seq(-2.7, 2.7, 0.1); count
q1Mean <- mean(ADHD$Q1.x, na.rm = TRUE)  # mean of Q1 is 1.716981
q1SD <- sd(ADHD$Q1.x, na.rm = TRUE)  # sd of Q1 is 1.166477
q2Mean <- mean(ADHD$Q2.x, na.rm = TRUE)  # mean of Q2 is 1.759259
q2SD <- sd(ADHD$Q2.x, na.rm = TRUE)  # sd of Q2 is 1.114826
q4Mean <- mean(ADHD$Q4.x, na.rm = TRUE)  # mean of Q4 is 1.132075
q4SD <- sd(ADHD$Q4.x, na.rm = TRUE)  # sd of Q4 is 1.038448

# calculate x-predicted values using mean and SD
q1Predicted <- (count * q1SD) + q1Mean; q1Predicted
q1Predicted <- data.frame("Q1.x" = q1Predicted)
colnames(q1Predicted) <- "Q1.x"
q2Predicted <- (count * q2SD) + q2Mean; q2Predicted
q2Predicted <- data.frame("Q2.x" = q2Predicted)
colnames(q2Predicted) <- "Q2.x"
q4Predicted <- (count * q4SD) + q4Mean; q4Predicted
q4Predicted <- data.frame("Q4.x" = q4Predicted)
colnames(q4Predicted) <- "Q4.x"

# calculate y-predicted values
Score1Pedicted <- predict(Q1.model, newdata = q1Predicted, se.fit = TRUE, level = 0.95); Score1Pedicted
Score2Pedicted <- predict(Q2.model, newdata = q2Predicted, se.fit = TRUE, level = 0.95); Score2Pedicted
Score4Pedicted <- predict(Q4.model, newdata = q4Predicted, se.fit = TRUE, level = 0.95); Score4Pedicted

# combine x and y into data table
PredictedQ1 <- cbind(q1Predicted, Score1Pedicted); PredictedQ1
PredictedQ2 <- cbind(q2Predicted, Score2Pedicted); PredictedQ2
PredictedQ4 <- cbind(q4Predicted, Score4Pedicted); PredictedQ4
# and plot
Q1PredictedScore <- ggplot(PredictedQ1, aes(Q1.x, fit)) + 
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), fill = "grey") +
  geom_line() +
  ylab("Predicted Total ADHD Raw Score") + xlab("Question 1: Frequency of Careless Mistakes in Schoolwork") +
  geom_line(aes(Q1.x, y = 20), color = "blue") +
  geom_label(aes(x = 4, y = 17), label = "Minimum for Diagnosis", color = "blue") +
  theme_bw() +
  ggtitle("Predicted Total ADHD-RS Score based-off Question 1 Response") +
  scale_x_continuous(breaks= c(0,1,2,3,4), labels= c("Never or rarely", "Sometimes", "Often", "Very often", "Always"))

Q2PredictedScore <- ggplot(PredictedQ2, aes(Q2.x, fit)) + 
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), fill = "gray") +
  geom_line() +
  ylab("Predicted Total ADHD Raw Score") + xlab("Question 2: Frequency of Fidgeting") +
  geom_line(aes(Q2.x, y = 20), color = "dark green") +
  geom_label(aes(x = 4, y = 17), label = "Minimum for Diagnosis", color = "dark green") +
  theme_bw() +
  ggtitle("Predicted Total ADHD-RS Score based-off Question 2 Response") +
  scale_x_continuous(breaks= c(0,1,2,3,4), labels= c("Never or rarely", "Sometimes", "Often", "Very often", "Always"))

Q4PredictedScore <- ggplot(PredictedQ4, aes(Q4.x, fit)) + 
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), fill = "gray") +
  geom_line() +
  ylab("Predicted Total ADHD Raw Score") + xlab("Question 4: Frequency of Leaving Seat") +
  geom_line(aes(Q4.x, y = 20), color = "dark red") +
  geom_label(aes(x = 3, y = 16), label = "Minimum for Diagnosis", color = "dark red") +
  theme_bw() +
  ggtitle("Predicted Total ADHD-RS Score based-off Question 4 Response") +
  scale_x_continuous(breaks= c(0,1,2,3,4), labels= c("Never or rarely", "Sometimes", "Often", "Very often", "Always"))

# some more plots (plot title is description)
Diagnosis_and_Score <- ggplot(ADHD, aes(ADHD_diagnosis, ADHD_Total_Raw.x)) + geom_point() +
  ggtitle("Participants Total ADHD-RS Score with Diagnosis") +
  scale_x_discrete(labels = c("No", "Yes")) +
  ylab("ADHD-RS Total Score") + xlab("ADHD Diagnosis")

Q1Score <- ggplot(ADHD, aes(Q1.x, ADHD_Total_Raw.x)) + geom_point() +
  ylab("Actual Total ADHD Raw Score") + xlab("Question 1: Frequency of Careless Mistakes in Schoolwork") +
  geom_line(aes(Q1.x, y = 20), color = "blue") +
  geom_label(aes(x = 0.5, y = 22), label = "Minimum for Diagnosis", color = "blue") +
  scale_x_continuous(breaks= c(0,1,2,3), labels= c("Never or rarely", "Sometimes", "Often", "Very often")) +
  ggtitle("Participant Total ADHD-RS Score with Question 1 Response")

Q2Score <- ggplot(ADHD, aes(Q2.x, ADHD_Total_Raw.x)) + geom_point() +
  ylab("Actual Total ADHD Raw Score") + xlab("Question 2: Frequency of Fidgeting") +
  geom_line(aes(Q1.x, y = 20), color = "dark green") +
  geom_label(aes(x = 0.5, y = 22), label = "Minimum for Diagnosis", color = "dark green") +
  scale_x_continuous(breaks= c(0,1,2,3), labels= c("Never or rarely", "Sometimes", "Often", "Very often")) +
  ggtitle("Participant Total ADHD-RS Score with Question 2 Response")

Q4Score <- ggplot(ADHD, aes(Q4.x, ADHD_Total_Raw.x)) + geom_point() +
  ylab("Actual Total ADHD Raw Score") + xlab("Question 4: Frequency of Leaving Seat") +
  geom_line(aes(Q1.x, y = 20), color = "dark red") +
  geom_label(aes(x = 0.5, y = 22.3), label = "Minimum for Diagnosis", color = "dark red") +
  scale_x_continuous(breaks= c(0,1,2,3), labels= c("Never or rarely", "Sometimes", "Often", "Very often")) +
  ggtitle("Participant Total ADHD-RS Score with Question 4 Response")

#### our seven plots are then: ####
Q1PredictedScore
Q2PredictedScore
Q4PredictedScore
Diagnosis_and_Score
Q1Score
Q2Score
Q4Score

# export our 7 main plots:
tiff(filename="Q1PredictedScore.tif", width = 7, height = 3, units = "in", res=300)
  Q1PredictedScore   # name of plot
  dev.off()
tiff(filename="Q2PredictedScore.tif", width = 7, height = 3, units = "in", res=300)
  Q2PredictedScore   # name of plot
  dev.off()
tiff(filename="Q4PredictedScore.tif", width = 7, height = 3, units = "in", res=300)
  Q4PredictedScore   # name of plot
  dev.off()
tiff(filename="Diagnosis_and_Score.tif", width = 5, height = 3, units = "in", res=300)
  Diagnosis_and_Score   # name of plot
  dev.off()
tiff(filename="Q1Score.tif", width = 6, height = 4, units = "in", res=300)
  Q1Score   # name of plot
  dev.off()
tiff(filename="Q2Score.tif", width = 6, height = 4, units = "in", res=300)
  Q2Score   # name of plot
  dev.off()
tiff(filename="Q4Score.tif", width = 6, height = 4, units = "in", res=300)
  Q4Score   # name of plot
  dev.off()
  
### some data tables:
# frequency of answers to questions for top 7 questions
  Q1freq <- dplyr::count(ADHD, Q1.x)
  Q2freq <- dplyr::count(ADHD, Q2.x)
  Q4freq <- dplyr::count(ADHD, Q4.x)
  Q5freq <- dplyr::count(ADHD, Q5.x)
  Q6freq <- dplyr::count(ADHD, Q6.x)
  Q8freq <- dplyr::count(ADHD, Q8.x)
  Q14freq <- dplyr::count(ADHD, Q14.x)
  names(Q1freq)[names(Q1freq) == "n"] <- "Q1 Frequency"
  names(Q1freq)[names(Q1freq) == "Q1.x"] <- "Score"
  names(Q2freq)[names(Q2freq) == "n"] <- "Q2 Frequency"
  names(Q4freq)[names(Q4freq) == "n"] <- "Q4 Frequency"
  names(Q5freq)[names(Q5freq) == "n"] <- "Q5 Frequency"
  names(Q6freq)[names(Q6freq) == "n"] <- "Q6 Frequency"
  names(Q8freq)[names(Q8freq) == "n"] <- "Q8 Frequency"
  names(Q14freq)[names(Q14freq) == "n"] <- "Q14 Frequency"
  freqTable <- cbind(Q1freq, Q2freq, Q4freq, Q5freq, Q6freq, Q8freq, Q14freq)  
  dplyr::select(freqTable, c(1,2,4,6,8,10,12,14)) -> frequencyTable
View(frequencyTable)

# AIC table
  AicTable <- aictab(cand.set = questionModels, second.ord = TRUE, sort = TRUE)
  names(AicTable)
  names(AicTable)[names(AicTable) == "Modnames"] <- "Model"
  names(AicTable)[names(AicTable) == "AICcWt"] <- "AIC_Weight"
  names(AicTable)[names(AicTable) == "Cum.Wt"] <- "Cumulative_Weight"
  class(AicTable)
  AicTable <- as.data.frame(AicTable)
  AICTable <- dplyr::select(AicTable, c(1,3,4,6,8))
  class(AICTable$`Model`) <- as.character(AICTable$`Model`)
  AICTable[1,1] <- "Q1"
  AICTable[2,1] <- "Q2"
  AICTable[3,1] <- "Q4"
  AICTable[5,1] <- "Q5"
  AICTable[7,1] <- "Q6"
  AICTable[4,1] <- "Q8"
  AICTable[6,1] <- "Q14"
  rownames(AICTable) <- c(1,2,3,4,5,6,7)
View(AICTable)

# export tables
write.table(frequencyTable, "frequencyTable.csv", quote = FALSE)
write.table(AICTable, "AICTable.csv", quote = FALSE)


#### IN SUMMARY ####

# our seven plots
Q1PredictedScore
Q2PredictedScore
Q4PredictedScore
Diagnosis_and_Score
Q1Score
Q2Score
Q4Score

# our two tables
frequencyTable
AICTable

# summary of the predictor variables
  mean(ADHD$ADHD_Total_Raw.x, na.rm = TRUE)  # mean of total score is 21.46835
  sd(ADHD$ADHD_Total_Raw.x, na.rm = TRUE)  # sd of total score is 16.12438
  dplyr::count(ADHD, ADHD_diagnosis)
#** For the 79 participant study, the average total ADHD-RS Score was 21.46835, 
  # with a standard deviation of 16.12438. The minimum score here for a participant with ADHD was 20, 
  # while the maximum for a participant without ADHD was 42. 
  # In total, 44 participants didn't have diagnosed ADHD, while 35 are diagnosed with it.

citation()
citation("dplyr")
citation("ggplot2")
citation("AICcmodavg")
citation("ggfortify")
citation("PerformanceAnalytics")

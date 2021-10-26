
#=============================================================================
# PROGRAMMER: Luis David Socarras
# PANTHER ID: 6243743
#
# CLASS: CAP4830
# SECTION: U01
# SEMESTER: Fall 2021
# CLASSTIME: T/TH 12:30-01:45 pm

# CERTIFICATION: I understand FIU’s academic policies, and I certify that this 
#                work is my own and that none of it is the work of any other person.

#=============================================================================



#1) Read the excel file  “CAP4830_HW2_Data.xlsx” data into R 
#  and store the imported data in a variable named “modelData”.

# Install xlsx package in R
#install.packages("xlsx")       

# Load xlsx package in R
library("xlsx")

getwd()                         # get the working directory checking it

setwd("~/Desktop/HW2/")         # set the working directory
getwd()


modelData <- read.xlsx(file.choose(), 1)  # read the the file and store it in a dataframe

###############################################################################
# 2) Output the names of the modelData dataframe.
names(modelData)                  # get the columns names of the dataframe


###############################################################################
# 3) Create a variable with name “model1” that stores the estimate of the linear model shown below

model1 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH 
             + DCOILWTICO_PCH + PCOPPUSDM_PCH + PCE_PCH 
             + WPU101_PCH + GPDIC1_PCH + RRVRUSQ156N_PCH , data = modelData)
summary(model1) # output model summary

# model1_Stats <- summary(model1)         # store the model information

###############################################################################
# 4) List all the estimate parameters from step 3 that are statistically significant 
#    for all "α ≤ 0.05" 


model2 <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH 
             + DCOILWTICO_PCH + PCE_PCH, data = modelData)
summary(model2) # output model summary

###############################################################################
# 5) Plot the model1’s residual Density Function 

plot(model1$residuals)

plot(density(model1$residuals), 
     main="Density Plot: Model 1 Residuals", 
     ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(model1$residuals), 2))) 

polygon(density(model1$residuals), col="red")

###############################################################################
# 6) Check the model1’s residual normality using the Sharpio test. 
 
shapiro.test(model1$residuals)
###############################################################################
# 7) Create model2 which is a refinement of model1 by removing all regressors 
#   that are statistically insignificant with a p < 0.55. Paste you model’s summary below:

summary(model1) # output model summary

model2 <- lm(UNRATE_PCH ~ CPILFESL_PCH + RRVRUSQ156N_PCH , data = modelData)

summary(model2) # output model summary

###############################################################################
# 9) Calculate prediction accuracy and error rates of model2. Look at 
#    the R-script in module 10.


finalData <- data.frame(modelData[ , c("UNRATE_PCH", "CPILFESL_PCH", "RRVRUSQ156N_PCH")])
names(finalData)
pairs(finalData)
names(model2)

# Create Training and Test data 
# 80-20 split

# setting seed to reproduce results of random sampling
set.seed(100)  

# row indices for training data
trainingRowIndex <- sample(1:nrow(finalData), 0.8*nrow(finalData)) 

# model training data
trainingData <- finalData[trainingRowIndex, ] 

# test data
testData  <- finalData[-trainingRowIndex, ]   


# build the model
trainingModel <- lm(UNRATE_PCH ~ CPILFESL_PCH + RRVRUSQ156N_PCH 
                    , data = trainingData)


summary(trainingModel)

# predict trainingModel on testing data
distPred <- predict(trainingModel, testData)  


# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(index = seq(1: nrow(testData)), 
                                  actuals= testData$UNRATE_PCH,
                                  predicteds=distPred))  

# A higher correlation accuracy implies that the actuals and predicted 
# values have similar directional movement,
cor(actuals_preds$actuals,actuals_preds$predicteds)  

library(ggplot2)


gg <- ggplot(data = actuals_preds, aes(index))  + 
  geom_point(aes(y = actuals), color = "red") + 
  geom_point(aes(y = predicteds), color = "blue") +
  labs( title = "Actual vs Predicted Values")
gg


###############################################################################
# 10) Create model3 which is a refinement of model2.
# A requirement for model3 it must only have three regressors. 
# How you pick the three regressor is up to you, but explain why you pick these three. 

summary(model1) # output model summary


model3 <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH + PCE_PCH , data = modelData)

summary(model3)

###############################################################################
# 11) Create model4 that uses a manual sampling technique with a training 
#    set of 60% of the data and a testing set of  40%. Paste the summary of the model below.
###############################################################################


model4 <- lm(UNRATE_PCH ~ CPILFESL_PCH + RRVRUSQ156N_PCH , data = modelData)

finalData <- data.frame(modelData[ , c("UNRATE_PCH", "CPILFESL_PCH", "RRVRUSQ156N_PCH")])
names(finalData)
pairs(finalData)
names(model4)

# Training and Test data 
#  60-40 split

# setting seed to reproduce results of random sampling
set.seed(100)  

# row indices for training data
trainingRowIndex <- sample(1:nrow(finalData), 0.6*nrow(finalData)) 

# model training data
trainingData <- finalData[trainingRowIndex, ] 

# test data
testData  <- finalData[-trainingRowIndex, ]   


# build the model
trainingModel <- lm(UNRATE_PCH ~ CPILFESL_PCH + RRVRUSQ156N_PCH 
                    , data = trainingData)


summary(trainingModel)

###############################################################################
#12) Use model4 to predict the values on the 40% testing set. 
# Store the results in  the distPred variable and paste beginning of variable data below.
###############################################################################
# predict trainingModel on testing data
distPred <- predict(trainingModel, testData)  
head(distPred)


###############################################################################
# 13) Using model4 calculate prediction accuracy and error rates then use ggplot 
#    that shows actual vs Predicted values. Paste your plot below.
###############################################################################

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(index = seq(1: nrow(testData)), 
                                  actuals= testData$UNRATE_PCH,
                                  predicteds=distPred))  

# A higher correlation accuracy implies that the actuals and predicted 
# values have similar directional movement,
cor(actuals_preds$actuals,actuals_preds$predicteds)  

library(ggplot2)


gg <- ggplot(data = actuals_preds, aes(index))  + 
  geom_point(aes(y = actuals), color = "red") + 
  geom_point(aes(y = predicteds), color = "blue") +
  labs( title = "Actual vs Predicted Values")
gg


#############################################################################
# 14) Run a k-fold cross validation with k=10. Paste the print of the model below.
#############################################################################
install.packages('caret')
library(caret)

controlled <- trainControl(method = "cv", number = 10)
control <- lm(UNRATE_PCH ~ CPILFESL_PCH + RRVRUSQ156N_PCH , data = modelData)
nrow(finalData)


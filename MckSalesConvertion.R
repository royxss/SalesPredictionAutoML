setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\Mckinsey\\Sales")
rm(list=ls())
seedVal = 17869
options(warn=-1)
options(scipen=999)

#Libraries
library(h2o)
library(dplyr)
library(lubridate)
library(dummies)
library(caret)
library(VIM)
# library(h2oEnsemble)
# library(cvAUC)
# library(SuperLearner)

train <- read.csv2("train.csv", header = TRUE, sep = ',')
test <- read.csv2("test.csv", header = TRUE, sep = ',')

#Trimws
#train <- data.frame(lapply(train, trimws), stringsAsFactors = FALSE)
#test <- data.frame(lapply(test, trimws), stringsAsFactors = FALSE)

#Format variables
train$Approved <- as.factor(train$Approved)
train$Var1 <- as.factor(train$Var1)
train$Employer_Category2 <- as.factor(train$Employer_Category2)
train$Monthly_Income <- as.numeric(as.character(train$Monthly_Income))
train$Existing_EMI <- as.numeric(as.character(train$Existing_EMI))
train$Interest_Rate <- as.numeric(as.character(train$Interest_Rate))
train$DOB <- dmy(train$DOB)
train$Age <- ifelse(year(train$DOB) > 2016, (2016 - (year(train$DOB) - 100)), (2016 - year(train$DOB)))
train$Lead_Creation_Date <- dmy(train$Lead_Creation_Date)
train$week_since_lead_creation <- round(time_length(interval(train$Lead_Creation_Date, dmy('30/10/16')), "week"))
train[is.na(train$Loan_Amount), 'Loan_Amount'] <- 0
train[is.na(train$Loan_Period), 'Loan_Period'] <- 0
train[train$City_Category == '', 'City_Category'] <- NA
train[train$Primary_Bank_Type == '', 'Primary_Bank_Type'] <- NA
train[train$Employer_Category1 == '', 'Employer_Category1'] <- NA

#Remove unwanted columns
excludeList <- c('DOB', 'Lead_Creation_Date', 'ID', 'City_Code', 'Employer_Code',
                 'Customer_Existing_Primary_Bank_Code', 'Source')#, 'Interest_Rate', 'EMI')
train <- train[,!names(train) %in% c(excludeList)]

#Scale
train$Monthly_Income <- log10(train$Monthly_Income + 1)
train$Existing_EMI <- log10(train$Existing_EMI + 1)
train$Loan_Amount <- log10(train$Loan_Amount + 1)
train$Age <- log10(train$Age + 1)
train$week_since_lead_creation <- log10(train$week_since_lead_creation + 1)

str(train)
apply(train, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))

#KNN Imputation
#train <- kNN(train, k=5)

#Format variables
test$Var1 <- as.factor(test$Var1)
test$Employer_Category2 <- as.factor(test$Employer_Category2)
test$Monthly_Income <- as.numeric(as.character(test$Monthly_Income))
test$Existing_EMI <- as.numeric(as.character(test$Existing_EMI))
test$Interest_Rate <- as.numeric(as.character(test$Interest_Rate))
test$DOB <- dmy(test$DOB)
test$Age <- ifelse(year(test$DOB) > 2016, (2016 - (year(test$DOB) - 100)), (2016 - year(test$DOB)))
test$Lead_Creation_Date <- dmy(test$Lead_Creation_Date)
test$week_since_lead_creation <- round(time_length(interval(test$Lead_Creation_Date, dmy('30/10/16')), "week"))
test[is.na(test$Loan_Amount), 'Loan_Amount'] <- 0
test[is.na(test$Loan_Period), 'Loan_Period'] <- 0
test[test$City_Category == '', 'City_Category'] <- NA
test[test$Primary_Bank_Type == '', 'Primary_Bank_Type'] <- NA
test[test$Employer_Category1 == '', 'Employer_Category1'] <- NA

#Remove unwanted columns
excludeList <- c('DOB', 'Lead_Creation_Date', 'City_Code', 'Employer_Code',
                 'Customer_Existing_Primary_Bank_Code', 'Source')#, 'Interest_Rate', 'EMI')
test <- test[,!names(test) %in% c(excludeList)]

#Scale
test$Monthly_Income <- log10(test$Monthly_Income + 1)
test$Existing_EMI <- log10(test$Existing_EMI + 1)
test$Loan_Amount <- log10(test$Loan_Amount + 1)
test$Age <- log10(test$Age + 1)
test$week_since_lead_creation <- log10(test$week_since_lead_creation + 1)

str(test)
apply(test, 2, function(x) length(which(x == "" | is.na(x) | x == "NA")))

#KNN Imputation
#test <- kNN(test, k=5)

# #Scale values
# #traindf <- train %>%
# #  mutate_if(~ as.numeric(.) %>% scale)
# 
# #Stratefied sampling
# trainPct <- .8
# testPct <- 1 - trainPct
# inTrain <- createDataPartition(y = train[,'Approved'], p = trainPct, list = FALSE)
# train_tbl <- train[inTrain, ]
# test_tbl <- train[-inTrain, ]
# stopifnot(nrow(train_tbl) + nrow(test_tbl) == nrow(train))
# 
# valid_tbl <- test_tbl[1:6000,]
# test_tbl <- test_tbl[6001:nrow(test_tbl),]
# 
# train_h2o <- as.h2o(train_tbl)
# valid_h2o <- as.h2o(valid_tbl)
# test_h2o  <- as.h2o(test_tbl)

# # Set names for h2o
# y <- "Approved"
# x <- setdiff(names(train_h2o), y)
# 
# 
# ##################### AutoML ######################
# automl_models_h2o <- h2o.automl(
#   x = x, 
#   y = y, 
#   training_frame = train_h2o, 
#   validation_frame = valid_h2o, 
#   leaderboard_frame = test_h2o, 
#   max_runtime_secs = 60, 
#   stopping_metric = "AUC")
# automl_models_h2o
# 
# # Extract leader model
# automl_leader <- automl_models_h2o@leader
# pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)
# perf <- h2o.performance(automl_leader, newdata = test_h2o)
# 

##################### Retrain on whole data ######################
h2o.init()
#Initialize Variables:
Accresult <- read.csv2("Sample_Solution_Yrq0zGh.csv", header = TRUE, sep = ',')
names(Accresult) <- c('ID', 'Approved')

#create models source wise.
modellist1 <- unique(train$Source_Category)

# #create models gender wise.
# modellist2 <- unique(train$Gender)

for (src1 in modellist1){
    print(paste0("Source Category: ", src1))
    dftrain <- train %>% filter(Source_Category == src1)  
    dftest <- test %>% filter(Source_Category == src1) 
    
    unqTarget <- unique(as.integer(as.character(dftrain$Approved)))
    if (length(unqTarget) == 1){
      print("Source Category will not be modelled...") 
      result <- NULL
      tempPred <- rep(unqTarget, nrow(dftest))
      result <- as.data.frame(dftest[,1])
      result <- cbind(result, tempPred)
      names(result) <- c('ID', 'Approved')
      names(Accresult) <- c('ID', 'Approved')
      Accresult <- rbind(Accresult, result)
      print("Results appended...")  
      next
    }
    
    # Retrain on whole data
    train_h2o <- as.h2o(dftrain)
    test_h2o <- as.h2o(dftest[,-1])
    
    print("Framing complete...")
    
    # Set names for h2o
    y <- "Approved"
    x <- setdiff(names(train_h2o), y)
    
    automl_models_h2o <- h2o.automl(
      x = x, 
      y = y, 
      training_frame = train_h2o, 
      max_runtime_secs = 60, 
      stopping_metric = "AUC", 
      seed = seedVal)
    automl_models_h2o
    print("Training complete...")
    print(automl_models_h2o)
    
    automl_leader <- automl_models_h2o@leader
    
    #Predict actual test
    pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)
    print("Prediction Complete...")
    
    #Append results
    result <- NULL
    tempPred <- as.data.frame(pred_h2o)
    result <- as.data.frame(dftest[,1])
    result <- cbind(result, tempPred[,1])
    names(result) <- c('ID', 'Approved')
    names(Accresult) <- c('ID', 'Approved')
    Accresult <- rbind(Accresult, result)
    print("Results appended...")
  
}

##################### Output ######################

names(Accresult) <- c('ID', 'Approved')
filename = 'result.csv'
write.table(Accresult, file = filename, quote = FALSE, row.names=FALSE, sep=",")

h2o.shutdown(prompt = FALSE)

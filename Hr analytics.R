library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)


in_time1<- read.csv("in_time.csv",stringsAsFactors = F)
out_time1<- read.csv("out_time.csv",stringsAsFactors = F)
#remove the company declared holidays
in_time1 <-in_time1[,-c(2,11,19,47,88,143,187,198,224,225,226,258)]
out_time1 <-out_time1[,-c(2,11,19,47,88,143,187,198,224,225,226,258)]

do_date <- function(in_time){
#get the date, We will use it as column name also
in_time <- separate(in_time,colnames(in_time), into = c("Date","Time"), sep = " ", remove = T)
in_time$Date <- ymd(in_time$Date)
NonNAindex <- which(!is.na(in_time$Date))
firstNonNA <- min(NonNAindex)
Date_part<- as.character(in_time[firstNonNA,which(colnames(in_time) == "Date")])
in_time[which(is.na(in_time$Date)),which(colnames(in_time) == "Date")]<-Date_part
in_time[which(is.na(in_time$Time)),which(colnames(in_time) == "Time")]<-"00:00:00"
in_time <- separate(in_time,Time, into = c("HH","MM","SS"), sep = ":", remove = T)
in_time[which(is.na(in_time$SS)),which(colnames(in_time) == "SS")]<-"00"
in_time <- unite(in_time,new_timestamp, c("HH","MM","SS"),sep = ":",remove = T)
in_time <- unite(in_time,Date_part, c("Date","new_timestamp"),sep = " ",remove = T)
in_time$Date_part <- as.POSIXct(in_time$Date_part)
colnames(in_time)[which(colnames(in_time) == "Date_part")] <- Date_part
return (in_time)
}
emp_id <- in_time1[,1]
in_time1 <- in_time1[,-1]
in_time2 <-data.frame(sapply(in_time1, function(x) do_date(data.frame(x))))
#a <- (out_time2$X2.Jan.15.2015.01.02)-(in_time2$X2.Jan.15.2015.01.02)
#took a long time to process

#process for out time
out_time1 <- out_time1[,-1]
out_time2 <-out_time1
out_time2 <-data.frame(sapply(out_time1, function(x) do_date(data.frame(x))))

typeof(out_time1[1,1])
logged_work_hrs<- out_time2-in_time2
logged_work_hrs<-logged_work_hrs/3600
timesheet<-cbind(emp_id,logged_work_hrs)
timesheet$avg_Workhours<- NA
timesheet$leaves<- NA


for(i in 1:nrow(timesheet)){
  row<- timesheet[i,c(-1,-251,-252)]
  row<- data.frame(as.numeric(row))
  meanWh<- mean(row[,1])
  timesheet[i,which(colnames(timesheet) == "avg_Workhours")]<- meanWh
  timesheet[i,which(colnames(timesheet) == "leaves")]<- length(which(row[,1] == 0))
}

workhrs_details <- timesheet[,c(1,251,252)]
##########################################################################################

#load general data

employee_data <- read.csv("general_data.csv",stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv",stringsAsFactors = F)

#adding survey information
setdiff(employee_data_final$EmployeeID,employee_survey_data$EmployeeID)
#no difference , we can directly bind instead of logical join
setdiff(employee_data_final$EmployeeID,manager_survey_data$EmployeeID)
#no difference , we can directly bind instead of logical join

sapply(employee_survey_data,FUN = function(x) ifelse((sum(is.na(x)))>0,sum(is.na(x)),"No NA"))
employee_survey_data<- na.omit(employee_survey_data)
sapply(manager_survey_data,FUN = function(x) ifelse((sum(is.na(x)))>0,sum(is.na(x)),"No NA"))


#but we have removed certain records from both so, have to join
employee_data <-employee_data%>%inner_join(employee_survey_data,by = c("EmployeeID" = "EmployeeID"))

employee_data <-employee_data%>%inner_join(manager_survey_data,by = c("EmployeeID" = "EmployeeID"))

employee_data <- employee_data%>%inner_join(workhrs_details, by = c("EmployeeID" = "emp_id"))

#Removing redundand columns
employee_data <- employee_data[,-c(8,16,18)]

sapply(employee_data,FUN = function(x) ifelse((sum(is.na(x)))>0,sum(is.na(x)),"No NA"))
#NumCompaniesWorked ,TotalWorkingYears has 19 and 9 NAs respectively
#Nas are very less in number, hence deleting

employee_data <- na.omit(employee_data)


################################################################
# Feature standardisation

# Normalising continuous features 
employee_data$Age<- scale(employee_data$Age)
employee_data$DistanceFromHome<- scale(employee_data$DistanceFromHome) # scale used: mean 64.8, sd 30.1
employee_data$MonthlyIncome<- scale(employee_data$MonthlyIncome) # scale used: mean 2280, sd 2267

# converting target variable telecom from No/Yes character to factorwith levels 0/1 
employee_data$Attrition <- ifelse(employee_data$Attrition=="Yes",1,0)

# Checking churn rate of prospect customer

attrition <- sum(employee_data$Attrition)/nrow(employee_data)
attrition # 16.08% attrition rate. 

# creating a dataframe of categorical features
employee_data_chr<- employee_data[,c(3,4,7,9,11,12)]

# converting categorical attributes to factor
employee_data_fact<- data.frame(sapply(employee_data_chr, function(x) factor(x)))
str(employee_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_data_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_data_fact))[,-1]))

#Following is the dummies assigned for >2 categories
#BusinessTravel: Non-Travel is the base line
#department: Human resource is the base
#Education: Human Resources is the base
#Gender: Female is 0, Male is 1
#Healthcare Rep is base
#Marital Status: Divorced is base



# Final dataset
employee_data_final<- cbind(employee_data[,c(-3,-4,-7,-9,-11,-12)],dummies) 
employee_data_final<- employee_data_final[,c(-5)] 

View(employee_data_final) #4382 obs. of  35 variables
#scaling the other ordered categorical variable

do_scale <- function(x){
  scale(x)
}

scale_var <- employee_data_final[,c(4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]
scaled_var<- data.frame(sapply(scale_var, do_scale))
employee_data_final<- employee_data_final[,-c(4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]
employee_data_final <- cbind(employee_data_final,scaled_var)
######################
#Ready for model
######################

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_data_final$Attrition, SplitRatio = 0.7)

train = employee_data_final[indices,]

test = employee_data_final[!(indices),]

########################################################################
# Logistic Regression: 

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC: 2283.3

# Stepwise selection
library("MASS")
library(car)
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

vif(model_2)

#Removing the insignificant variable 1 by 1
#Removing StockOptionLevel

model_3 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 avg_Workhours + leaves, family = "binomial", data = train)

summary(model_3)
vif(model_3)

#removing JobRole.xHuman.Resources

model_4 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 avg_Workhours + leaves, family = "binomial", data = train)
summary(model_4)

#Removing MaritalStatus.xMarried
model_5 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 JobLevel + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 avg_Workhours + leaves, family = "binomial", data = train)
summary(model_5)

#Removing Joblevel
model_6 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 avg_Workhours + leaves, family = "binomial", data = train)
summary(model_6)

#Removing Leaves

model_7 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 avg_Workhours, family = "binomial", data = train)
summary(model_7)

#Removing JobRole.xResearch.Director

model_8 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 avg_Workhours, family = "binomial", data = train)
summary(model_8)

#Removing JobRole.xSales.Executive
model_9 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + 
                 JobRole.xManager + JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 avg_Workhours, family = "binomial", data = train)
summary(model_9)

#Removing JobRole.xManager

model_10 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 EducationField.xTechnical.Degree + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + 
                 NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 avg_Workhours, family = "binomial", data = train)
summary(model_10)

#Removing BusinessTravel.xTravel_Rarely
model_11 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  EducationField.xLife.Sciences + 
                  EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_11)
vif(model_11)

#EducationField.xLife.Sciences and EducationField.xMedical has high vif
#removing 1 by 1

model_12 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_12)

#removing EducationField.xLife.Sciences made all EducationField cat values in-significant
#removing EducationField.xMarketing
model_13 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  EducationField.xMedical + EducationField.xOther + 
                  EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_13)

#removing EducationField.xMedical
model_14 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  EducationField.xOther + 
                  EducationField.xTechnical.Degree + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_14)

#removing EducationField.xTechnical.Degree
model_15 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  EducationField.xOther + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_15)

#removing EducationField.xOther
model_16 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_16)
vif(model_16)

#Removing JobRole.xManufacturing.Director
model_17 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_17)
vif(model_17)
#Removing TrainingTimesLastYear
model_18 <- glm(formula = Attrition ~ Age + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_18)
vif(model_17)
#all Vif values under threshold range

########################################################################
# With 11 significant variables in the model

final_model<- model_17

#######################################################################

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])
summary(test_pred)
test$prob <- test_pred
View(test[,c(2,42)])

# Let's use the probability cutoff of 50%.

test_pred_turnover <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_turnover <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_turnover,test_pred_turnover)

# Let's use the probability cutoff of 40%.

test_pred_turnover <- factor(ifelse(test_pred >= 0.35, "Yes", "No"))
test_actual_turnover <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_turnover,test_pred_turnover)


library(e1071)

test_conf <- confusionMatrix(test_pred_turnover, test_actual_turnover, positive = "Yes")
test_conf
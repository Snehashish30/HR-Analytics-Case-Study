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
library(gmodels)
library(corrplot)

# DATA files:-
# 1. general_data.csv  - File with general information which includes demographic informations             
#                      - information on Age, working experience educational background, department, gender 
#                         monthly income, salary hike recieved etc                             
# 2. employee_survey.csv  - information on survey done for employee regarding job statisfacton, enviroment
#                           statisfaction etc                                                             
# 3. manager_survey.csv   - information on survey given by manager regarding performance of the employee  
#                           job involvement level                                                         
# 4. in_time.csv          - information on in time of employee, helps for calculation of working hours    
# 5. out_time.csv         - information on out time of employee, helps for calcualtion of working hours of
#                           employee                                                                      


in_time1<- read.csv("in_time.csv",stringsAsFactors = F)
out_time1<- read.csv("out_time.csv",stringsAsFactors = F)
### Cleaning the In_time and Out_time files
#Remove the company's declared holidays
in_time1 <-in_time1[,-c(2,11,19,47,88,143,187,198,224,225,226,258)]
out_time1 <-out_time1[,-c(2,11,19,47,88,143,187,198,224,225,226,258)]


##################################
#do_date() function cleans and transforms the date
#data without transforming into long format.
#It processes and out puts the data in the same format 
#as given for 250+ working days, so that it is pleasing
#to eye and can be visualised in tabular form, even in excel
#gather() is NOT USED intentionally to improve performance
#of changing the formats 2 times

##################################
## Separating the date and time to calculate the working hrs.
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
in_time_emp_id <- in_time1[,1]
in_time1 <- in_time1[,-1]
in_time2 <-data.frame(sapply(in_time1, function(x) do_date(data.frame(x))))
#took a long time to process

#process for out time
out_time_emp_id <- out_time1[,1]
out_time1 <- out_time1[,-1]
out_time2 <-out_time1
out_time2 <-data.frame(sapply(out_time1, function(x) do_date(data.frame(x))))


# Calculating the logged working hours. We have to get the difference of outime and intime.
logged_work_hrs<- out_time2-in_time2
logged_work_hrs<-logged_work_hrs/3600
timesheet<-cbind(emp_id,logged_work_hrs)
timesheet$avg_Workhours<- NA
timesheet$leaves<- NA

## Calculating the Average working hrs of each employee

for(i in 1:nrow(timesheet)){
  row<- timesheet[i,c(-1,-251,-252)]
  row<- data.frame(as.numeric(row))
  meanWh<- mean(row[,1])
  timesheet[i,which(colnames(timesheet) == "avg_Workhours")]<- meanWh
  timesheet[i,which(colnames(timesheet) == "leaves")]<- length(which(row[,1] == 0))
}

# Avg working hr is stored as workhrs_details.
workhrs_details <- timesheet[,c(1,251,252)]
##########################################################################################
#Importing all the employee files into r ##
#load general data

employee_data <- read.csv("general_data.csv",stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv",stringsAsFactors = F)

#adding survey information
#Lets see whether all the employeeID from employee_data is  present in all the dataframe. With sediff function we can achieve this.
setdiff(employee_data_final$EmployeeID,employee_survey_data$EmployeeID)
#no difference , we can directly bind instead of logical join
setdiff(employee_data_final$EmployeeID,manager_survey_data$EmployeeID)
#no difference , we can directly bind instead of logical join

### Checking for duplicate data ##

##Employee data
if(length(unique(employee_data$EmployeeID))==length(employee_data$EmployeeID)){
  print("EmployeeID is Unique. Total Unique EmployeeID:")
  print(length(unique(employee_data$EmployeeID)))
}else {
  print("EmployeeID is not Unique in Employee_data")
}
##Employee_Survey
if(length(unique(employee_survey_data$EmployeeID))==length(employee_survey_data$EmployeeID)){
  print("EmployeeID is Unique. Total Unique EmployeeID:")
  print(length(unique(employee_survey_data$EmployeeID)))
}else {
  print("EmployeeID is not Unique in Employee_Survey_data")
}
##Manager_Survey
if(length(unique(manager_survey_data$EmployeeID))==length(manager_survey_data$EmployeeID)){
  print("EmployeeID is Unique. Total Unique EmployeeID:")
  print(length(unique(manager_survey_data$EmployeeID)))
}else {
  print("EmployeeID is not Unique in Manager_Survey_data")
}
## in_time, we have separated out the empids in in_time_emp_id
if(length(unique(in_time_emp_id))==length(in_time_emp_id)){
  print("EmployeeID is Unique. Total Unique EmployeeID:")
  print(length(unique(emp_id)))
}else {
  print("EmployeeID is not Unique in In_time_data")
}
## in_time, we have separated out the empids in out_time_emp_id
if(length(unique(out_time_emp_id))==length(out_time_emp_id)){
  print("EmployeeID is Unique. Total Unique EmployeeID:")
  print(length(unique(out_time_emp_id)))
}else {
  print("EmployeeID is not Unique in Out_time_data")
}

sapply(employee_survey_data,FUN = function(x) ifelse((sum(is.na(x)))>0,sum(is.na(x)),"No NA"))
employee_survey_data<- na.omit(employee_survey_data)
# EnvironmentSatisfaction has 25 NA. JobSatisfaction has 20 NA. WorkLifeBalancehas 38 NA. Removing the records with NA as the missing values are less.

sapply(manager_survey_data,FUN = function(x) ifelse((sum(is.na(x)))>0,sum(is.na(x)),"No NA"))
# No Missing data

#but we have removed certain records from both so, have to join
employee_data <-employee_data%>%inner_join(employee_survey_data,by = c("EmployeeID" = "EmployeeID"))

employee_data <-employee_data%>%inner_join(manager_survey_data,by = c("EmployeeID" = "EmployeeID"))

employee_data <- employee_data%>%inner_join(workhrs_details, by = c("EmployeeID" = "emp_id"))

#Removing redundant columns
employee_data <- employee_data[,-c(8,16,18)]

sapply(employee_data,FUN = function(x) ifelse((sum(is.na(x)))>0,sum(is.na(x)),"No NA"))
#NumCompaniesWorked ,TotalWorkingYears has 19 and 8 NAs respectively
#Nas are very less in number, hence deleting

employee_data <- na.omit(employee_data)


########################################################################################################
########################################################################################################
#Performing EDA and outlier treatment if required


box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(employee_data, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=Attrition,y=leaves, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
##Seems Younger people are more likely to leave the company
plot_grid(ggplot(employee_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
# Attrition rate decreases with increase in the number of years in the company.


plot_grid(ggplot(employee_data, aes(x=Attrition,y=avg_Workhours, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#person working more number of hours(ie. greater than standard working hours> 8 hours) are leaving
#Training time is Not an influencing factor.

plot_grid(ggplot(employee_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#employees who haven't got promoted for long time are likely to leave


plot_grid(ggplot(employee_data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          align = "v",nrow = 1)

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(employee_data, aes(x=BusinessTravel,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(employee_data, aes(x=Department,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme1, align = "h") 
#Frequent Travellers are bit high who are leaving the company. This may be due to they are not happy to travel or They are in more demand in market due to their global exposure.
#Human Resources Department is showing huge attrion rate

plot_grid(ggplot(employee_data, aes(x=Education,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(employee_data, aes(x=JobRole,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme1, align = "h")

plot_grid(ggplot(employee_data, aes(x=MaritalStatus,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(employee_data, aes(x=StockOptionLevel,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme1, align = "h")

#StockOptionLevel doesn't show much impact.Its not a driving factor
#singles are leaving the company

plot_grid(ggplot(employee_data, aes(x=EnvironmentSatisfaction,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(employee_data, aes(x=JobSatisfaction,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme1, align = "h")

plot_grid(ggplot(employee_data, aes(x=WorkLifeBalance,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(employee_data, aes(x=JobInvolvement,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme1, align = "h")

plot_grid(ggplot(employee_data, aes(x=PerformanceRating,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(employee_data, aes(x=EducationField,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme1, align = "h")

#Overall we can say, employees who are not satisfied with the Worklife balance, job and environment (employee survey parameters) are likely to leave 


########################################################################################################
########################################################################################################

#For Tabular data in proportion, below code is written 

prop<-CrossTable(employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#Attrition rate of 16.2%
boxplot(employee_data$Age)
#good to go
employee_data%>%group_by(Attrition)%>%summarise(average_age = mean(Age))
#seems Younger people are more likely to leave the company


prop<-CrossTable(employee_data$MaritalStatus ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#Singles are likely to leave


prop<-CrossTable(employee_data$BusinessTravel ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#Frequent Traveller are not happy ,hence leaving


prop<-CrossTable(employee_data$Department ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#Human Resources Department showing huge attrion rate


prop<-CrossTable(employee_data$StockOptionLevel ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#Not a driving factor

#spread of salary
boxplot(employee_data$MonthlyIncome)
#there are outliers in high income 
for(i in seq(0,1,0.1)){
  a<-quantile(employee_data$MonthlyIncome,i) 
  print(a)
}
#there is a sudden jump after 80%, hence capping the salary to 85%tile

employee_data[(which(employee_data$MonthlyIncome > quantile(employee_data$MonthlyIncome,0.85))),(which(colnames(employee_data) == "MonthlyIncome"))] <- quantile(employee_data$MonthlyIncome,0.85)
boxplot(employee_data$MonthlyIncome)
#looks fine

prop<-CrossTable(employee_data$TotalWorkingYears ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# Attrition rate decreases with increase in Workex

prop<-CrossTable(employee_data$YearsAtCompany ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# Attrition rate decreases with increase in Workex

prop<-CrossTable(employee_data$TotalWorkingYears ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# Attrition rate decreases with increase in Workex


prop<-CrossTable(employee_data$TotalWorkingYears ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# Attrition rate decreases with increase in Workex

options(scipen = 999)
ggplot(employee_data,aes(x = employee_data$MonthlyIncome, col = "black"))+geom_histogram(bins= 3,aes(fill = employee_data$Attrition))
# The Mid of the plot shows bit high attrition rate

prop<-CrossTable(employee_data$TrainingTimesLastYear ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#TrainingTimesLastYear not much an influencing factor

prop<-CrossTable(employee_data$PercentSalaryHike ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#Not an influencing factor

ggplot(employee_data,aes(x = employee_data$avg_Workhours, col = "black"))+geom_histogram(bins= 5,aes(fill = employee_data$Attrition))
#person working more number of hours(ie. greater than standard working hours> 8 hours) are leaving

prop<-CrossTable(employee_data$YearsSinceLastPromotion ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
#employees who haven't got promoted for long time are likely to leave

prop<-CrossTable(employee_data$EnvironmentSatisfaction ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

prop<-CrossTable(employee_data$JobSatisfaction ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

prop<-CrossTable(employee_data$WorkLifeBalance ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#Overall we can say, employees who are not satisfied with the Worklife balance, job and environment (employee survey parameters) are likely to leave 

prop<-CrossTable(employee_data$JobInvolvement ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)


prop<-CrossTable(employee_data$PerformanceRating ,employee_data$Attrition,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#Not much information is derived form Manegerial survey data.
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
attrition # 16.16% attrition rate. 

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

#checking for correlation between Attrition and other variables
correlation_matrix <- cor(employee_data_final)
corrplot(correlation_matrix, method = "number", title = "Correlation Map", mar=c(0,0,1,0),
         type = "lower", order = "FPC",col = c("red", "orange", "blue", "green"), number.cex = .5, tl.cex = 0.5)



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
summary(model_1) #AIC: 2141.9

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
vif(model_18)
#Total working years is correlated to AGe. Person with more Age can only have more workex.
#Hence removing the Age as it is less significant

model_19 <- glm(formula = Attrition ~ BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + 
                  NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  avg_Workhours, family = "binomial", data = train)
summary(model_19)
vif(model_19)
#all Vif values under threshold range


########################################################################
# With 10 significant variables in the model

final_model<- model_19

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

# Let's use the probability cutoff of 35%.

test_pred_turnover <- factor(ifelse(test_pred >= 0.35, "Yes", "No"))
test_actual_turnover <- factor(ifelse(test$Attrition==1,"Yes","No"))

a<-table(test_actual_turnover,test_pred_turnover)
acc<- (a[1,1]+a[2,2])/sum(a)
sens<- a[2,2]/sum(a[2,])
spec<- a[1,1]/sum(a[1,])
acc
sens
spec
#accuracy is 85.2%
#but sensitivity is poor

#test_conf <- confusionMatrix (test_pred_turnover, test_actual_turnover, positive = "Yes")
#test_conf
#confusionMatrix command not working, hence calculating the accuracy, specificity and sensitivity 
#manually as per logic  
#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_turnover <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  #conf <- confusionMatrix(predicted_churn, test_actual_turnover, positive = "Yes")
  
  a<-table(test_actual_turnover,predicted_turnover)
  acc<- (a[1,1]+a[2,2])/sum(a)
  sens<- a[2,2]/sum(a[2,])
  spec<- a[1,1]/sum(a[1,])
  
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)
OUT
s
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
min_diff <- min(abs(OUT[,1]-OUT[,2]))
cutoff<- s[which(abs(OUT[,1]-OUT[,2]) == min_diff)]
# Let's choose a cutoff value of 0.17757 for final model

test_cutoff_turnover <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

a<-table(test_actual_turnover,test_cutoff_turnover)
a
acc<- (a[1,1]+a[2,2])/sum(a)
sens<- a[2,2]/sum(a[2,])
spec<- a[1,1]/sum(a[1,])

acc
sens
spec
View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_turnover <- ifelse(test_cutoff_turnover=="Yes",1,0)
test_actual_turnover <- ifelse(test_actual_turnover=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_turnover, test_actual_turnover)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

ks_table_test
max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_turnover, test_pred, groups = 10)
Attrition_decile

#plotting gain chart
ggplot(Attrition_decile, aes(x = bucket, y = Gain/100))+geom_point()+geom_line()
plot(Attrition_decile$bucket, Attrition_decile$Gain, type="o", col="blue", pch="o", lty=1, ylim=c(0,110) )
points(Attrition_decile$bucket,Attrition_decile$bucket*10 , col="red", pch="*")
lines(Attrition_decile$bucket,Attrition_decile$bucket*10, col="red",lty=2)

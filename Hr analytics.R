library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
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

write.csv(timesheet,"timesheet.csv")

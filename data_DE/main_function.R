source("master_function.R")

df_master<-function_master(2017)

# rename df_master to df_master_save
df_master_save <- df_master

# categorize column "DateTime" in "year", "month", "day", "weekday", "hour"
df_master$DateTime<-as.character(df_master$DateTime)
df_master$year<-substr(df_master$DateTime,1,4)
df_master$month<-substr(df_master$DateTime,6,7)
df_master$day<-substr(df_master$DateTime,9,10)
df_master$weekday<-weekdays(as.Date(substr(df_master$DateTime,1,10)))
df_master$hour<-substr(df_master$DateTime,12,13)

#df_master<-df_master[,c(1,216:220,2:215)]

# create year_dummies
# merge columns of df_master and year_dummies
year_dummies<-dummy_cols(df_master$year)
colnames(year_dummies)<-c("year","2017")
year_dummies<-year_dummies[,-1]
df_master<-cbind(df_master,year_dummies)

# create month_dummies
# merge columns of df_master and month_dummies
month_dummies<-dummy_cols(df_master$month)
month_dummies<-month_dummies[,-1]
colnames(month_dummies)<-c("january","february","march","april","may","june","july","august","september","october","november","december")
df_master<-cbind(df_master,month_dummies)

# create weekday_dummies
# merge columns of df_master and weekday_dummies 
# start with sunday because the 01.01.2017 is a sunday !
weekday_dummies<-dummy_cols(df_master$weekday)
weekday_dummies<-weekday_dummies[,-1]
# Subject to change <- dynamically 
colnames(weekday_dummies)<-c("sunday","monday","tuesday","wednesday","thursday","friday","saturday")
df_master<-cbind(df_master,weekday_dummies)

# create hour_dummies
# merge columns of df_master and hour_dummies
hour_dummies<-dummy_cols(df_master$hour)
hour_dummies<-hour_dummies[,-1]
colnames(hour_dummies)<-sprintf("h%d", seq(0:23))
df_master<-cbind(df_master,hour_dummies)

setwd("..")
setwd("..")
setwd("./process_data/")
saveRDS(df_master, file = "df_master_final.rds")
setwd("..")




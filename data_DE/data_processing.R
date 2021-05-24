setwd("./process_data")

raw_data <- readRDS("df_master_final.rds")
not_all_na <- apply(raw_data, MARGIN=2, FUN=function(x){!all(is.na(x))})
raw_data <- raw_data[,not_all_na]

which_ <- is.na(raw_data)
plot(raw_data$`11WD7VOER1S--B-Z`)
raw_data <- na_interpolation(raw_data, na.rm=T)
replaced_ <- as.numeric(as.vector(raw_data[which_]))
summary(replaced_)

is_numeric_col <- as.numeric(which(sapply(raw_data, is.numeric)))
raw_data_chr <- raw_data[,-is_numeric_col]
raw_data_num <- raw_data[,is_numeric_col]
not_all_zero <- apply(raw_data_num, MARGIN=2, FUN=function(x){mean(x)!=0})
raw_data_num <- raw_data_num[,not_all_zero]
raw_data <- cbind(raw_data_chr,raw_data_num)

y_columns <- c(50:255)


# national holidays:
test <- GET('https://calendarific.com/api/v2/holidays?&api_key=c570ba667e041727f5ac390bec6354c59d810599&country=DE&year=2017&type=national')
json <- content(test,as="parsed") 

list_locat <- unlist(lapply(json$response$holidays, `[[`, 5))
list_descr <- lapply(json$response$holidays, `[[`, 2)
list_dates <- unlist(lapply(lapply(json$response$holidays, `[[`, 3), `[[`, 1))

df_nat <- as.data.frame(cbind("DATE"=list_dates, "WHERE"=list_locat, "DESCR"=list_descr))
df_nat <- df_nat[df_nat$DESCR!="NULL",]
raw_data$national_holiday <-as.numeric(substr(raw_data$DateTime, 1, 10) %in% c(df_nat$DATE, "2017-12-24", "2017-12-31"))


# religious holidays
# key needs to be added here
test <- GET('https://calendarific.com/api/v2/holidays?&api_key=INSERT_KEY_HERE&country=DE&year=2017&type=religious')
json <- content(test,as="parsed") 

list_locat <- unlist(lapply(json$response$holidays, `[[`, 5))
list_descr <- lapply(json$response$holidays, `[[`, 2)
list_dates <- unlist(lapply(lapply(json$response$holidays, `[[`, 3), `[[`, 1))

df_rel <- as.data.frame(cbind("DATE"=list_dates, "WHERE"=list_locat, "DESCR"=list_descr))
df_rel <- df_rel[df_rel$DESCR!="NULL",]

df_rel$DATE <- as.character(df_rel$DATE)
df_rel$WHERE <- as.character(df_rel$WHERE)
df_rel$DESCR <- as.character(df_rel$DESCR)

df_rel<-aggregate(.~DATE, df_rel, paste, collapse = ",")


df_rel$WHERE[df_rel$WHERE=="All"]<- "BW, BY, BE, BB, HB, HH, HE, MV, NI, NW, RP, SL, SN, ST, SH, TH"
df_rel$WHERE[df_rel$WHERE=="BB,All except BB"]<- "BW, BY, BE, BB, HB, HH, HE, MV, NI, NW, RP, SL, SN, ST, SH, TH"
df_rel$sum<-str_count(df_rel$WHERE, ',')+1 


raw_data$religious_holiday <-0
raw_data$religious_holiday[substr(raw_data$DateTime, 1, 10) %in% df_rel$DATE] <-rep(df_rel$sum, each=24)



# bridge day
raw_data$bridge<-0
raw_data$bridge_help<-0

for (h in c(1:nrow(df_nat))) {
     raw_data$bridge_help<-as.numeric(((round(as.numeric(difftime(strptime(substr(raw_data$DateTime,1,10), format="%Y-%m-%d"), 
                                                                strptime(df_nat$DATE[h], format="%Y-%m-%d"), units="days")),1)==1&raw_data$weekday=="Freitag")|
                                       (round(as.numeric(difftime(strptime(substr(raw_data$DateTime,1,10), format="%Y-%m-%d"), 
                                                                strptime(df_nat$DATE[h], format="%Y-%m-%d"), units="days")),1)==-1&raw_data$weekday=="Montag")) & 
                                       (raw_data$national_holiday+raw_data$religious_holiday==0))

     raw_data$bridge<-raw_data$bridge+raw_data$bridge_help

}
        
raw_data$bridge_help<-NULL

# German holidays (2017):

test2 <- GET('https://ferien-api.de/api/v1/holidays')
json2 <- content(test2,as="parsed")

list_locat_school <- unlist(lapply(json2, `[[`, 4))
list_descr_school <- lapply(json2, `[[`, 5)
list_start_school <- substr(unlist(lapply(json2, `[[`, 1)),1,10)
list_end_school <- substr(unlist(lapply(json2, `[[`, 2)),1,10)

df_school <- as.data.frame(cbind("START"=list_start_school, "END"=list_end_school, "WHERE"=list_locat_school, "DESCR"=list_descr_school))
df_school <- df_school[df_school$DESCR!="NULL",]

df_school<-as.data.frame(lapply(df_school, unlist))
df_school<-df_school[order(df_school$START),]

df_school$help<-substr(df_school$START,1,4)

df_school17<- df_school[df_school$help=='2017',]
df_school17<-df_school17[,-5]

df_school17$START <- as.character(df_school17$START)
df_school17$END <- as.character(df_school17$END)

df_school17$END[substr(df_school17$END,1,4)=="2018"] <- "2017-12-31"


# Filter by state

list_bl<-unique(df_school17$WHERE)
df_bl<-as.data.frame(list_bl)

df_school_bl<-data.frame(matrix(ncol = 18, nrow = nrow(raw_data)))
colnames(df_school_bl)<-c("DateTime",as.character(list_bl),"sum")

df_school_bl$DateTime<-raw_data$DateTime
df_school_bl[is.na(df_school_bl)]<-0


for (i in c(1:nrow(df_school17))) {
        
        df_school_bl[which(substr(df_school_bl$DateTime,1,10)==df_school17$START[i])[1]:which(substr(df_school_bl$DateTime,1,10)==df_school17$END[i])[24],
                     as.character(df_school17$WHERE[i])]<-1
        
}

df_school_bl$sum <- rowSums(df_school_bl[,2:17])

raw_data$holiday_sum<-df_school_bl$sum

rm(df_school, df_school17, df_school_bl,df_bl, df_nat, df_rel, list_descr, list_descr_school, json, json2, test, test2, raw_data_chr, raw_data_num)

drop_variables <- c("DateTime","december","h24","sunday","year_dummies","day","hour","month","year")


X_data <- raw_data[,-y_columns]
X_data <- X_data[, !names(X_data) %in% drop_variables]
y_data <- raw_data[,y_columns]

rm(raw_data)

which_zero <- which(sapply(y_data, max, na.rm=T)==0)
exclude_units <- colnames(y_data)[which_zero]
exclude_unavail <- sprintf("unavail-%s", exclude_units)

X_data <- X_data[,!names(X_data) %in% exclude_unavail]
y_data <- y_data[,!names(y_data) %in% exclude_units]

# Manual Adjustment
# Unit has only one observation
X_data <- X_data[,!names(X_data) %in% "unavail-11W0-0000-0044-W"]
y_data <- y_data[,!names(y_data) %in% "11W0-0000-0044-W"]

number_of_units <- ncol(y_data)

X_data$Netposition <- NULL

X_data$national_holiday <- X_data$national_holiday + X_data$bridge
X_data$bridge <- NULL

columns_numeric <- colnames(X_data)[2:44]
columns_dummies <- colnames(X_data)[c(186:226)]
X_data_numeric <- X_data[, names(X_data) %in% columns_numeric]
X_data_dummies <- X_data[, names(X_data) %in% columns_dummies]
X_data_rest <- X_data[, !names(X_data) %in% c(columns_numeric,columns_dummies)]

list_column_names_1 <- c("Load_DE_50HzT",
                       "Load_DE_Amprion",
                       "Load_DE_TenneT_GER",
                       "Load_DE_TransnetBW",
                       "Load_AT",
                       "generation_solar_DE_50HzT",             
                       "generation_wind_onshore_DE_50HzT",      
                       "generation_wind_offshore_DE_50HzT",     
                       "generation_solar_DE_Amprion",           
                       "generation_wind_onshore_DE_Amprion",    
                       "generation_solar_DE_TenneT_GER",        
                       "generation_wind_onshore_DE_TenneT_GER", 
                       "generation_wind_offshore_DE_TenneT_GER",
                       "generation_solar_DE_TransnetBW",        
                       "generation_wind_onshore_DE_TransnetBW",
                       "generation_solar_AT",                  
                       "generation_wind_onshore_AT")

list_column_names_2 <- list_column_names_1

for(col_1 in list_column_names_1){
     
     for(col_2 in list_column_names_2){
          col_temp <- X_data_numeric[,names(X_data_numeric)==col_1]*X_data_numeric[,names(X_data_numeric)==col_2]
          X_data_numeric <- cbind(X_data_numeric, col_temp)
          colnames(X_data_numeric)[ncol(X_data_numeric)] <- paste0(col_1,"_X_",col_2)
          print(paste0(col_1,"_X_",col_2))
     }
     
     list_column_names_2 <- list_column_names_2[list_column_names_2!=col_1]
}

X_data_poly <- cbind(X_data_dummies,X_data_numeric,X_data_rest)
X_data_poly_lag <- as.data.frame(cbind(X_data_poly[-c(1:96),], 
                                       y_data[-c(c(1:24),c((nrow(y_data)-71):nrow(y_data))),], 
                                       y_data[-c((nrow(y_data)-95):nrow(y_data)),]))

KW_names_lag72 <- sprintf("%s-lag72", colnames(y_data))
KW_names_lag96 <- sprintf("%s-lag96", colnames(y_data))

colnames(X_data_poly_lag) <- c(colnames(X_data_poly),KW_names_lag72,KW_names_lag96)

y_data_lag = y_data[-c(1:96),]

rm(X_data_poly, X_data, X_data_rest, X_data_numeric, KW_names_lag72, KW_names_lag96)

saveRDS(X_data_poly_lag,file="X_data_poly_lag_final.rds")
saveRDS(y_data_lag,file="y_data_poly_lag_final.rds")

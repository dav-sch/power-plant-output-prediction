setwd("./results")

for (sc in c(1:4)) {
     
     df_y_test <- readRDS(file=paste0("y_test_data_v6_SAMPLE_",sc,".rds"))
     df_y_test_predict <- readRDS(file=paste0("y_test_predict_data_v6_SAMPLE_",sc,".rds"))
     
     set.seed(list_of_i[sc])
     
     print(paste0("SAMPLE: ",sc," SEED: ",list_of_i[sc]))
     
     sample_days <- sort(sample.int(no_days, 24, replace=F))
     exclude_obs <- as.numeric()
     weekdays_list <- as.character()
     dates_list <- as.character()
     
     for (j in sample_days){
          exclude_obs <- c(exclude_obs, c((j*24-23):(j*24)))
          weekdays_list <- c(weekdays_list, as.character(unique(X_data_weekdays[((j*24-23)):(j*24)])))
          dates_list <- c(dates_list, substr(raw_data[j*24+96,1],6,10))
          print(paste0(j,": ",as.character(unique(X_data_weekdays[((j*24-23)):(j*24)]))))
     }

     weekdays_list[weekdays_list=="Montag"] <- "Mon"
     weekdays_list[weekdays_list=="Dienstag"] <- "Tue"
     weekdays_list[weekdays_list=="Mittwoch"] <- "Wed"
     weekdays_list[weekdays_list=="Donnerstag"] <- "Thu"
     weekdays_list[weekdays_list=="Freitag"] <- "Fri"
     weekdays_list[weekdays_list=="Samstag"] <- "Sat"
     weekdays_list[weekdays_list=="Sonntag"] <- "Sun"
     
     total_generation <- as.data.frame(cbind("Time"=as.numeric(c(1:nrow(df_y_test)))))
     total_generation_predict <- as.data.frame(cbind("Time"=as.numeric(c(1:nrow(df_y_test)))))

     for(type in c("Gas", "Hard Coal", "Lignite", "Nuclear", "PSP")){
          print(type)
          column_temp <- as.data.frame(rowSums(as.data.frame(cbind(df_y_test[,which(master_data$ProductionTypeName==type)]))))
          colnames(column_temp) <- type 
          total_generation <- cbind(total_generation, column_temp)
          column_temp <- as.data.frame(rowSums(as.data.frame(cbind(df_y_test_predict[,which(master_data$ProductionTypeName==type)]))))
          colnames(column_temp) <- type 
          total_generation_predict <- cbind(total_generation_predict, column_temp)
     }
     
     generation_errors <- total_generation_predict
     generation_errors[,-1] <- total_generation_predict[,-1] - total_generation[,-1]
     
}

setwd("..")

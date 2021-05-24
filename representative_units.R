for (sc in c(1:4)){
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
     
     setwd("./et_models")
     y_predict_binary <- readRDS(file=paste0("ET_model_data_v6_predict_all_binary_SAMPLE_",as.character(sc),".rds"))
     setwd("..")
     
     setwd("./results")
     df_y_test <- readRDS(file=paste0("y_test_data_v6_SAMPLE_",sc,".rds"))
     df_y_test_predict <- readRDS(file=paste0("y_test_predict_data_v6_SAMPLE_",sc,".rds"))
     
     df_actual <- data.frame(matrix(ncol = 0, nrow = nrow(df_y_test)))
     df_actual$Time <- c(1:nrow(df_y_test))
     df_predict <- data.frame(matrix(ncol = 0, nrow = nrow(df_y_test_predict)))
     df_predict$Time <- c(1:nrow(df_y_test_predict))
     
     # Repreentative units (cf. master_data)
     for (ui in c(76,153,114,51,136)){
          y_test_binary <- y_data_lag[exclude_obs,ui]
          y_test_binary[y_test_binary!=0] <- 1
          y_test_binary <- factor(y_test_binary, levels=c(0,1))
          
          confMat_test <- confusionMatrix(factor(y_predict_binary[exclude_obs,ui], levels=c(0,1)), 
                                          y_test_binary, mode="prec_recall")
          acc_test <- confMat_test$overall["Accuracy"]
          
          rmse_test <- sqrt(mean((df_y_test[,ui]-df_y_test_predict[,ui])^2))
          
          mae_test <- mean(abs(df_y_test[,ui]-df_y_test_predict[,ui]))
          
          col_temp <- as.data.frame(cbind(df_y_test[,ui]))
          colnames(col_temp) <- paste0(master_data$ProductionTypeName[ui],": ", master_data$PowerSystemResourceName[ui])
          df_actual <- cbind(df_actual,col_temp)
          col_temp <- as.data.frame(cbind(df_y_test_predict[,ui]))
          colnames(col_temp) <- paste0(master_data$ProductionTypeName[ui],": ", master_data$PowerSystemResourceName[ui])
          df_predict <- cbind(df_predict,col_temp)
           
          print(paste0("SAMPLE ",sc," UNIT ", ui,": ",round(acc_test, digits=2)))
          
          print(paste0("SAMPLE ",sc," UNIT ", ui,": ",round(rmse_test, digits=1)))
          
          print(paste0("SAMPLE ",sc," UNIT ", ui,": ",round(mae_test, digits=1)))
     }
 
}

setwd("..")

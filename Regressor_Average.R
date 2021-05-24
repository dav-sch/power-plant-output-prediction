setwd("./results")

for (sc in c(1:4)){
     set.seed(list_of_i[sc])
     
     print(paste0("SAMPLE: ",sc," SEED: ",list_of_i[sc]))
     
     sample_days <- sort(sample.int(no_days, 24, replace=F))
     print(sample_days)
     exclude_obs <- as.numeric()
     
     for (j in sample_days){
          exclude_obs <- c(exclude_obs, c((j*24-23):(j*24)))
          print(paste0(j,": ",as.character(unique(X_data_weekdays[(j*24-23):(j*24)]))))
     }
     
     summary(factor(X_data_weekdays[exclude_obs[rep(c(T,rep(F,24)),24)]]))
     
     results_df_RMSE <- data.frame(matrix(ncol = 2, nrow = 0))
     col_names_RMSE <- c("RMSE/Capacity Average Regression Train",
                    "RMSE/Capacity Average Regression Test")
     colnames(results_df_RMSE) <- col_names_RMSE
     
     results_df_MAE <- data.frame(matrix(ncol = 2, nrow = 0))
     col_names_MAE <- c("MAE/Capacity Average Regression Train",
                    "MAE/Capacity Average Regression Test")
     colnames(results_df_MAE) <- col_names_MAE
     
     #setwd("C:/Users/DaS/Documents/02_Base_Case/et_models")
     #y_predict_binary <- readRDS(file=paste0("ET_model_predict_all_binary_SAMPLE_",as.character(sc),".rds"))
     
     for (ui in c(1:ncol(y_data_lag))){
          
          print(paste0(ui," ",colnames(y_data_lag)[ui]))
          
          #max_temp <- max(y_data_lag[,ui])
          max_temp <- master_data$InstalledGenCapacity[ui]
          sd_temp <- sd(y_data_lag[,ui])
          
          y_test <- y_data_lag[exclude_obs,ui]
          y_train <- y_data_lag[-exclude_obs,ui]
          
          # NAIVE Predictor Average for all ones and zeroes
          y_train_predict <- rep(mean(y_train),nrow(X_train))
          RMSE_naive_train <- round(sqrt(mean((y_train_predict - y_train)^2))/max_temp, digits=3)
          MAE_naive_train <- round(mean(abs(y_train_predict - y_train))/max_temp, digits=3)
          
          # NAIVE Predictor Average for all ones and zeroes
          y_test_predict <- rep(mean(y_train),nrow(X_test))
          RMSE_naive <- round(sqrt(mean((y_test_predict - y_test)^2))/max_temp, digits=3)
          MAE_naive <- round(mean(abs(y_test_predict - y_test))/max_temp, digits=3)
          
          row_temp <- data.frame(rbind(c(round(RMSE_naive_train, digits=3),
                                         round(RMSE_naive, digits=3))))
          rownames(row_temp) <- colnames(y_data_lag)[ui]
          colnames(row_temp) <- col_names_RMSE
          results_df_RMSE <- rbind(results_df_RMSE, row_temp)
          
          row_temp <- data.frame(rbind(c(round(MAE_naive_train, digits=3),
                                         round(MAE_naive, digits=3))))
          rownames(row_temp) <- colnames(y_data_lag)[ui]
          colnames(row_temp) <- col_names_MAE
          results_df_MAE <- rbind(results_df_MAE, row_temp)
          
     }

     saveRDS(results_df_RMSE, file=paste0("Average_SAMPLE_",as.character(sc),"_data_v6_RMSE.rds"))
     saveRDS(results_df_MAE, file=paste0("Average_SAMPLE_",as.character(sc),"_data_v6_MAE.rds"))
}

setwd("..")
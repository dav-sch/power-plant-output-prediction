# --------------------------------------------------------------------------------------------
# KNN Regressor ------------------------------------------------------------------------------
# Self-implemented ---------------------------------------------------------------------------

k_list <- c(1,10,100,(361-24)*24)

setwd("./results")

# For each sample configuration sc:
for (sc in c(1:4)){
     set.seed(list_of_i[sc])
     
     print(paste0("SAMPLE: ",sc," SEED: ",list_of_i[sc]))
     
     sample_days <- sort(sample.int(no_days, 24, replace=F))
     exclude_obs <- as.numeric()
     
     for (j in sample_days){
          exclude_obs <- c(exclude_obs, c((j*24-23):(j*24)))
          print(paste0(j,": ",as.character(unique(X_data_weekdays[((j*24-23)):(j*24)]))))
     }
     
     X_train <- X_data_poly_lag[-exclude_obs,]
     X_test <- X_data_poly_lag[exclude_obs,]
     
     y_train <- y_data_lag[-exclude_obs,]
     y_test <- y_data_lag[exclude_obs,]
     
     results_train_df_RMSE <- data.frame(matrix(ncol = ncol(y_data_lag), nrow = 0))
     colnames(results_train_df_RMSE) <- colnames(y_data_lag)
     results_test_df_RMSE <- data.frame(matrix(ncol = ncol(y_data_lag), nrow = 0))
     colnames(results_test_df_RMSE) <- colnames(y_data_lag)
     
     results_train_df_MAE <- data.frame(matrix(ncol = ncol(y_data_lag), nrow = 0))
     colnames(results_train_df_MAE) <- colnames(y_data_lag)
     results_test_df_MAE <- data.frame(matrix(ncol = ncol(y_data_lag), nrow = 0))
     colnames(results_test_df_MAE) <- colnames(y_data_lag)
     
     
     setwd("..")
     setwd("./et_models")
     y_predict_binary <- readRDS(file=paste0("ET_model_data_v6_predict_all_binary_SAMPLE_",as.character(sc),".rds"))
     y_train_binary_predict <- y_predict_binary[-exclude_obs,]
     y_test_binary_predict <- y_predict_binary[exclude_obs,]
     
     # From KNN Classifier
     setwd("..")
     setwd("./results")
     df_closest_x_train <- readRDS(file=paste0("test_SAMPLE_",sc,"_df_closest_x_train_v6.rds"))
     df_closest_x <- readRDS(file=paste0("test_SAMPLE_",sc,"_df_closest_x_test_v6.rds"))
     
     print(paste0("Sum of first column of train-to-train dist. matrix: ", sum(rownames(df_closest_x_train)==df_closest_x_train[,1])))
     
     for (kk in k_list){
          
          print(kk)
          
          rmse_train_list <- as.numeric()
          rmse_test_list <- as.numeric()
          mae_train_list <- as.numeric()
          mae_test_list <- as.numeric()
          
          y_train_predict <- data.frame(matrix(ncol=ncol(y_data_lag), nrow = 0))
          colnames(y_train_predict) <- c(1:ncol(y_data_lag))      
          y_test_predict <- data.frame(matrix(ncol=ncol(y_data_lag), nrow = 0))
          colnames(y_test_predict) <- c(1:ncol(y_data_lag))
          
          for(obs in c(1:nrow(X_train))){
               print(obs)
               obs_temp <- as.numeric(df_closest_x_train[obs,])
               row_temp <- data.frame(matrix(ncol=ncol(y_data_lag), nrow = 1))
               for(ui in c(1:ncol(y_data_lag))){
                    y_train_ <- y_train[obs_temp,ui]
                    y_train_ <- y_train_[y_train_>0]
                    effective_k<- min(kk,length(y_train_))
                    row_temp[,ui] <- mean(y_train_[1:effective_k])
               }
               colnames(row_temp) <- c(1:ncol(y_data_lag))
               y_train_predict <- rbind(y_train_predict, row_temp)
          }
          
          for(obs in c(1:nrow(X_test))){
               print(obs)
               obs_temp <- as.numeric(df_closest_x[obs,])
               row_temp <- data.frame(matrix(ncol=ncol(y_data_lag), nrow = 1))
               for(ui in c(1:ncol(y_data_lag))){
                    y_train_ <- y_train[obs_temp,ui]
                    y_train_ <- y_train_[y_train_>0]
                    effective_k<- min(kk,length(y_train_))
                    row_temp[,ui] <- mean(y_train_[1:effective_k])
               }
               colnames(row_temp) <- c(1:ncol(y_data_lag))
               y_test_predict <- rbind(y_test_predict, row_temp)
          }
          
          y_train_predict[y_train_binary_predict==0] <- 0
          y_test_predict[y_test_binary_predict==0] <- 0
          
          for (ui in c(1:ncol(y_data_lag))){
               #max_temp <- max(y_data_lag[,ui])
               max_temp <- master_data$InstalledGenCapacity[ui]
               print(ui)
               rmse_train_temp <- sqrt(mean((y_train_predict[,ui]-y_train[,ui])^2))/max_temp
               rmse_train_list <- c(rmse_train_list,rmse_train_temp)
               rmse_test_temp <- sqrt(mean((y_test_predict[,ui]-y_test[,ui])^2))/max_temp
               rmse_test_list <- c(rmse_test_list,rmse_test_temp)
               
               mae_train_temp <- mean(abs(y_train_predict[,ui]-y_train[,ui]))/max_temp
               mae_train_list <- c(mae_train_list,mae_train_temp)
               mae_test_temp <- mean(abs(y_test_predict[,ui]-y_test[,ui]))/max_temp
               mae_test_list <- c(mae_test_list,mae_test_temp)
          }
          
          rmse_train_list <- as.data.frame(rbind(rmse_train_list))
          colnames(rmse_train_list) <- colnames(y_data_lag)
          results_train_df_RMSE <- rbind(results_train_df_RMSE, rmse_train_list)
          
          rmse_test_list <- as.data.frame(rbind(rmse_test_list))
          colnames(rmse_test_list) <- colnames(y_data_lag)
          results_test_df_RMSE <- rbind(results_test_df_RMSE, rmse_test_list)
          
          mae_train_list <- as.data.frame(rbind(mae_train_list))
          colnames(mae_train_list) <- colnames(y_data_lag)
          results_train_df_MAE <- rbind(results_train_df_MAE, mae_train_list)
          
          mae_test_list <- as.data.frame(rbind(mae_test_list))
          colnames(mae_test_list) <- colnames(y_data_lag)
          results_test_df_MAE <- rbind(results_test_df_MAE, mae_test_list)
          
     }
     
     saveRDS(results_train_df_RMSE,file=paste0("train_SAMPLE_",sc,"_KNN_Regressor_data_v6_df_RMSE.rds"))
     saveRDS(results_test_df_RMSE,file=paste0("test_SAMPLE_",sc,"_KNN_Regressor_data_v6_df_RMSE.rds"))
     saveRDS(results_train_df_MAE,file=paste0("train_SAMPLE_",sc,"_KNN_Regressor_data_v6_df_MAE.rds"))
     saveRDS(results_test_df_MAE,file=paste0("test_SAMPLE_",sc,"_KNN_Regressor_data_v6_df_MAE.rds"))
     
}

setwd("..")

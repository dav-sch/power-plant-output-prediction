setwd("./results")

lambda_list <- 10^seq(10,-4,length=8)

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
     
     results_train_df_RMSE <- data.frame(matrix(ncol = 0, nrow = length(lambda_list)))
     results_test_df_RMSE <- data.frame(matrix(ncol = 0, nrow = length(lambda_list)))
     
     results_train_df_MAE <- data.frame(matrix(ncol = 0, nrow = length(lambda_list)))
     results_test_df_MAE <- data.frame(matrix(ncol = 0, nrow = length(lambda_list)))
     
     setwd("..")
     setwd("./et_models")
     y_predict_binary <- readRDS(file=paste0("ET_model_data_v6_predict_all_binary_SAMPLE_",as.character(sc),".rds"))
     setwd("..")
     setwd("./results")
     
     for (ui in c(1:ncol(y_data_lag))){
          #for (ui in c(1:205)){     
          print(colnames(y_data_lag)[ui])
          
          print(ui)
          #max_temp <- max(y_data_lag[,ui])
          max_temp <- master_data$InstalledGenCapacity[ui]
          sd_temp <- sd(y_data_lag[,ui])
          
          y_test <- y_data_lag[exclude_obs,ui]
          y_train <- y_data_lag[-exclude_obs,ui]
          X_train_ <- X_train[y_train!=0,]
          y_train_ <- y_train[y_train!=0]
          
          sd_X <- sapply(X_train, sd)
          var_with_no_variance <- which(sd_X==0)
          
          if(length(var_with_no_variance)==0){
               var_with_no_variance <- 10000
          }
          
          RMSE_train_list <- numeric()
          RMSE_test_list <- numeric()
          
          MAE_train_list <- numeric()
          MAE_test_list <- numeric()
          
          y_train_binary_predict <- y_predict_binary[-exclude_obs,ui]
          y_test_binary_predict <- y_predict_binary[exclude_obs,ui]
          
          if(is.na(sd(y_train_))==T | sd(y_train_)==0){
               
               y_train_predict_temp <- rep(mean(y_train_),nrow(X_train))
               y_train_predict_temp[y_train_binary_predict==0] <- 0
               RMSE_train_list <- rep(sqrt(mean((y_train_predict_temp-y_train)^2))/max_temp, length(lambda_list))
               MAE_train_list <- rep(mean(abs(y_train_predict_temp-y_train))/max_temp, length(lambda_list))
               
               y_test_predict_temp <- rep(mean(y_train_),nrow(X_test))
               y_test_predict_temp[y_test_binary_predict==0] <- 0
               RMSE_test_list <- rep(sqrt(mean((y_test_predict_temp-y_test)^2))/max_temp, length(lambda_list))
               MAE_test_list <- rep(mean(abs(y_test_predict_temp-y_test))/max_temp, length(lambda_list))
               
          } else {
          
               ridge_lin_reg <- glmnet(data.matrix(X_train_[,-var_with_no_variance]), data.matrix(y_train_), 
                                       family="gaussian", standardize=F, lambda=lambda_list,
                                       alpha=0)
               
               y_train_predict <- predict(ridge_lin_reg, newx=as.matrix(X_train[,-var_with_no_variance]))
               y_test_predict <- predict(ridge_lin_reg, newx=as.matrix(X_test[,-var_with_no_variance]))
               
               for (l in c(1:length(ridge_lin_reg$lambda))){
                    y_train_predict_temp <- y_train_predict[,l]
                    y_train_predict_temp[y_train_binary_predict==0] <- 0
                    RMSE_train_list <- c(RMSE_train_list,sqrt(mean((y_train_predict_temp-y_train)^2))/max_temp)
                    MAE_train_list <- c(MAE_train_list,mean(abs(y_train_predict_temp-y_train))/max_temp)
                    
                    y_test_predict_temp <- y_test_predict[,l]
                    y_test_predict_temp[y_test_binary_predict==0] <- 0
                    RMSE_test_list <- c(RMSE_test_list,sqrt(mean((y_test_predict_temp-y_test)^2))/max_temp)
                    MAE_test_list <- c(MAE_test_list,mean(abs(y_test_predict_temp-y_test))/max_temp)
               }
               
               RMSE_train_list <- c(RMSE_train_list, rep(NA, length(lambda_list)-length(RMSE_train_list)))
               RMSE_test_list <- c(RMSE_test_list, rep(NA, length(lambda_list)-length(RMSE_test_list)))
               
               MAE_train_list <- c(MAE_train_list, rep(NA, length(lambda_list)-length(MAE_train_list)))
               MAE_test_list <- c(MAE_test_list, rep(NA, length(lambda_list)-length(MAE_test_list)))
          
          }
               
          col_temp <- data.frame(cbind(c(RMSE_train_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_train_df_RMSE <- cbind(results_train_df_RMSE, col_temp)
          print(col_temp[4,1])
          
          col_temp <- data.frame(cbind(c(RMSE_test_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_test_df_RMSE <- cbind(results_test_df_RMSE, col_temp)
          print(col_temp[4,1])
          
          col_temp <- data.frame(cbind(c(MAE_train_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_train_df_MAE <- cbind(results_train_df_MAE, col_temp)
          print(col_temp[4,1])
          
          col_temp <- data.frame(cbind(c(MAE_test_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_test_df_MAE <- cbind(results_test_df_MAE, col_temp)
          print(col_temp[4,1])
          
     }
     
     saveRDS(results_train_df_RMSE,file=paste0("train_SAMPLE_",sc,"_Ridge_Regressor_data_v6_df_RMSE.rds"))
     saveRDS(results_test_df_RMSE,file=paste0("test_SAMPLE_",sc,"_Ridge_Regressor_data_v6_df_RMSE.rds"))
     saveRDS(results_train_df_MAE,file=paste0("train_SAMPLE_",sc,"_Ridge_Regressor_data_v6_df_MAE.rds"))
     saveRDS(results_test_df_MAE,file=paste0("test_SAMPLE_",sc,"_Ridge_Regressor_data_v6_df_MAE.rds"))
}

setwd("..")

# --------------------------------------------------------------------------------------------
# KNN Classifier -----------------------------------------------------------------------------
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
     
     y_train_binary <- y_data_lag[-exclude_obs,]
     y_train_binary[y_train_binary!=0] <- 1
     y_test_binary <- y_data_lag[exclude_obs,]
     y_test_binary[y_test_binary!=0] <- 1
     
     
     results_train_df <- data.frame(matrix(ncol = ncol(y_data_lag), nrow = 0))
     colnames(results_train_df) <- colnames(y_data_lag)
     results_test_df <- data.frame(matrix(ncol = ncol(y_data_lag), nrow = 0))
     colnames(results_test_df) <- colnames(y_data_lag)
     
     
     df_closest_x <- data.frame(matrix(ncol=nrow(X_train), nrow = 0))
     colnames(df_closest_x) <- c(1:nrow(X_train))
     
     for(obs in c(1:nrow(X_test))){
          print(obs)
          test <- sqrt(rowMeans((as.matrix(X_train) - matrix(rep(as.numeric(X_test[obs,]),each=nrow(X_train)),
                                                             nrow=nrow(X_train),ncol=ncol(X_test)))^2))
          row_temp <- as.data.frame(rbind(order(test)))
          colnames(row_temp) <- c(1:nrow(X_train))
          df_closest_x <- rbind(df_closest_x, row_temp)
     }
     
     
     df_closest_x_train <- data.frame(matrix(ncol=nrow(X_train), nrow = 0))
     colnames(df_closest_x_train) <- c(1:nrow(X_train))
     
     for(obs in c(1:nrow(X_train))){
          print(obs)
          test <- sqrt(rowMeans((as.matrix(X_train) - matrix(rep(as.numeric(X_train[obs,]),each=nrow(X_train)),
                                                             nrow=nrow(X_train),ncol=ncol(X_train)))^2))
          row_temp <- as.data.frame(rbind(order(test)))
          colnames(row_temp) <- c(1:nrow(X_train))
          df_closest_x_train <- rbind(df_closest_x_train, row_temp)
     }
     
     print(paste0("Sum of first column of train-to-train dist. matrix: ", sum(rownames(df_closest_x_train)==df_closest_x_train[,1])))
     
     for (kk in k_list){
          
          print(kk)
          
          acc_train_list <- as.numeric()
          acc_test_list <- as.numeric()
    
          y_train_binary_predict <- data.frame(matrix(ncol=ncol(y_data_lag), nrow = 0))
          colnames(y_train_binary_predict) <- c(1:ncol(y_data_lag))      
          y_test_binary_predict <- data.frame(matrix(ncol=ncol(y_data_lag), nrow = 0))
          colnames(y_test_binary_predict) <- c(1:ncol(y_data_lag))
          
          for(obs in c(1:nrow(X_train))){
               print(obs)
               obs_temp <- as.numeric(df_closest_x_train[obs,1:kk])
               row_temp <- as.data.frame(rbind(round(colMeans(y_train_binary[obs_temp,]),0)))
               colnames(row_temp) <- c(1:ncol(y_data_lag))
               y_train_binary_predict <- rbind(y_train_binary_predict, row_temp)
          }
          
          for(obs in c(1:nrow(X_test))){
               print(obs)
               obs_temp <- as.numeric(df_closest_x[obs,1:kk])
               row_temp <- as.data.frame(rbind(round(colMeans(y_train_binary[obs_temp,]),0)))
               colnames(row_temp) <- c(1:ncol(y_data_lag))
               y_test_binary_predict <- rbind(y_test_binary_predict, row_temp)
          }
          
          for (ui in c(1:ncol(y_data_lag))){
               print(ui)
               confMat_train <- confusionMatrix(factor(y_train_binary_predict[,ui], levels=c(0,1)), factor(y_train_binary[,ui], levels=c(0,1)), mode="prec_recall")
               acc_train_list <- c(acc_train_list,as.numeric(confMat_train$overall["Accuracy"]))
               confMat_test <- confusionMatrix(factor(y_test_binary_predict[,ui], levels=c(0,1)), factor(y_test_binary[,ui], levels=c(0,1)), mode="prec_recall")
               acc_test_list <- c(acc_test_list,as.numeric(confMat_test$overall["Accuracy"]))
          }

          acc_train_list <- as.data.frame(rbind(acc_train_list))
          colnames(acc_train_list) <- colnames(y_data_lag)
          results_train_df <- rbind(results_train_df, acc_train_list)
                    
          acc_test_list <- as.data.frame(rbind(acc_test_list))
          colnames(acc_test_list) <- colnames(y_data_lag)
          results_test_df <- rbind(results_test_df, acc_test_list)

     }

     saveRDS(df_closest_x_train,file=paste0("test_SAMPLE_",sc,"_df_closest_x_train_v6.rds"))
     saveRDS(df_closest_x,file=paste0("test_SAMPLE_",sc,"_df_closest_x_test_v6.rds"))
     
     saveRDS(results_train_df,file=paste0("train_SAMPLE_",sc,"_KNN_Classifier_data_v6_df.rds"))
     saveRDS(results_test_df,file=paste0("test_SAMPLE_",sc,"_KNN_Classifier_data_v6_df.rds"))
     
}

setwd("..")

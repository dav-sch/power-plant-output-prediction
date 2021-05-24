node_size_list <- c(500,50,5,1)

setwd("./results")

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
     
     results_train_df_RMSE <- data.frame(matrix(ncol = 0, nrow = length(node_size_list)+1))
     results_test_df_RMSE <- data.frame(matrix(ncol = 0, nrow = length(node_size_list)+1))
     
     results_train_df_MAE <- data.frame(matrix(ncol = 0, nrow = length(node_size_list)+1))
     results_test_df_MAE <- data.frame(matrix(ncol = 0, nrow = length(node_size_list)+1))
     
     setwd("..")
     setwd("./et_models")
     y_predict_binary <- readRDS(file=paste0("ET_model_data_v6_predict_all_binary_SAMPLE_",as.character(sc),".rds"))
     setwd("..")
     setwd("./results")
     
     for (ui in c(1:ncol(y_data_lag))){
          #print(colnames(y_data_lag)[ui])
          max_temp <- master_data$InstalledGenCapacity[ui]
          
          print(ui)
          max_temp <- max(y_data_lag[,ui])
          sd_temp <- sd(y_data_lag[,ui])
          
          y_test <- y_data_lag[exclude_obs,ui]
          y_train <- y_data_lag[-exclude_obs,ui]
          X_train_ <- X_train[y_train!=0,]
          y_train_ <- y_train[y_train!=0]
          
          RMSE_train_list <- numeric()
          RMSE_test_list <- numeric()
          MAE_train_list <- numeric()
          MAE_test_list <- numeric()
          
          y_train_binary_predict <- y_predict_binary[-exclude_obs,ui]
          y_test_binary_predict <- y_predict_binary[exclude_obs,ui]
          
          if(is.na(sd(y_train_))==T | sd(y_train_)==0){
               
               y_train_predict_temp <- rep(mean(y_train_),nrow(X_train))
               y_train_predict_temp[y_train_binary_predict==0] <- 0
               RMSE_train_list <- rep(sqrt(mean((y_train_predict_temp-y_train)^2))/max_temp, length(node_size_list)+1)
               MAE_train_list <- rep(mean(abs(y_train_predict_temp-y_train))/max_temp, length(node_size_list)+1)
               
               y_test_predict_temp <- rep(mean(y_train_),nrow(X_test))
               y_test_predict_temp[y_test_binary_predict==0] <- 0
               RMSE_test_list <- rep(sqrt(mean((y_test_predict_temp-y_test)^2))/max_temp, length(node_size_list)+1)
               MAE_test_list <- rep(mean(abs(y_test_predict_temp-y_test))/max_temp, length(node_size_list)+1)
               
          } else {
               
               y_train_predict <- rep(mean(y_train_),nrow(X_train))
               y_train_predict[y_train_binary_predict==0] <- 0
               RMSE_naive_train <- round(sqrt(mean((y_train_predict - y_train)^2))/max_temp, digits=3)
               MAE_naive_train <- round(mean(abs(y_train_predict - y_train))/max_temp, digits=3)
               
               y_test_predict <- rep(mean(y_train_),nrow(X_test))
               y_test_predict[y_test_binary_predict==0] <- 0
               RMSE_naive_test <- round(sqrt(mean((y_test_predict - y_test)^2))/max_temp, digits=3)
               MAE_naive_test <- round(mean(abs(y_test_predict - y_test))/max_temp, digits=3)
               
               RMSE_train_list <- c(RMSE_train_list,RMSE_naive_train)
               RMSE_test_list <- c(RMSE_test_list,RMSE_naive_test)
               MAE_train_list <- c(MAE_train_list,MAE_naive_train)
               MAE_test_list <- c(MAE_test_list,MAE_naive_test)
               
               for (ns in node_size_list){
                    print(ns)
                    rf_temp <- extraTrees(x=X_train_, y=y_train_, 
                                          ntree=250, 
                                          nodesize=ns, 
                                          subsetSizes=ceiling((2/3)*length(y_train_)),
                                          numRandomCuts=5,
                                          numThreads=3)
                    
                    #smooth_temp <- smooth.spline(x=X_train_, y=y_train_,
                    #                             df=20)
                    
                    y_train_predict_temp <- predict(rf_temp, X_train)
                    y_train_predict_temp[y_train_binary_predict==0] <- 0
                    RMSE_train_list <- c(RMSE_train_list,sqrt(mean((y_train_predict_temp-y_train)^2))/max_temp)
                    MAE_train_list <- c(MAE_train_list,mean(abs(y_train_predict_temp-y_train))/max_temp)
                    
                    y_test_predict_temp <- predict(rf_temp, X_test)
                    y_test_predict_temp[y_test_binary_predict==0] <- 0
                    RMSE_test_list <- c(RMSE_test_list,sqrt(mean((y_test_predict_temp-y_test)^2))/max_temp)
                    MAE_test_list <- c(MAE_test_list,mean(abs(y_test_predict_temp-y_test))/max_temp)
                    
                    rm(rf_temp)
                    gc()
                    
               }
               
               #RMSE_train_list <- c(RMSE_train_list, rep(NA, length(lambda_list)-length(RMSE_train_list)))
               #RMSE_test_list <- c(RMSE_test_list, rep(NA, length(lambda_list)-length(RMSE_test_list)))
               
          }
          
          col_temp <- data.frame(cbind(c(RMSE_train_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_train_df_RMSE <- cbind(results_train_df_RMSE, col_temp)
          
          #print(col_temp[5,1])
          
          col_temp <- data.frame(cbind(c(RMSE_test_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_test_df_RMSE <- cbind(results_test_df_RMSE, col_temp)
          
          print(col_temp)
          
          col_temp <- data.frame(cbind(c(MAE_train_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_train_df_MAE <- cbind(results_train_df_MAE, col_temp)
          
          #print(col_temp[5,1])
          
          col_temp <- data.frame(cbind(c(MAE_test_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_test_df_MAE <- cbind(results_test_df_MAE, col_temp)
          
          print(col_temp)
          
     }

     saveRDS(results_train_df_RMSE,file=paste0("train_SAMPLE_",sc,"_ET_Regressor_data_v6_df_RMSE.rds"))
     saveRDS(results_test_df_RMSE,file=paste0("test_SAMPLE_",sc,"_ET_Regressor_data_v6_df_RMSE.rds"))
     saveRDS(results_train_df_MAE,file=paste0("train_SAMPLE_",sc,"_ET_Regressor_data_v6_df_MAE.rds"))
     saveRDS(results_test_df_MAE,file=paste0("test_SAMPLE_",sc,"_ET_Regressor_data_v6_df_MAE.rds"))

}

setwd("..")

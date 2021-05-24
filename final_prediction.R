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
     
     df_y_test <- data.frame(matrix(ncol = 0, nrow = nrow(X_test)))
     df_y_test_predict <- data.frame(matrix(ncol = 0, nrow = nrow(X_test)))
     
     setwd("..")
     setwd("./et_models")
     y_predict_binary <- readRDS(file=paste0("ET_model_data_v6_predict_all_binary_SAMPLE_",as.character(sc),".rds"))
     setwd("..")
     setwd("./results")
     
     for (ui in c(1:ncol(y_data_lag))){
          #for (ui in c(1:205)){     
          print(colnames(y_data_lag)[ui])
          
          print(ui)
          max_temp <- max(y_data_lag[,ui])
          sd_temp <- sd(y_data_lag[,ui])
          
          y_test <- y_data_lag[exclude_obs,ui]
          y_train <- y_data_lag[-exclude_obs,ui]
          X_train_ <- X_train[y_train!=0,]
          y_train_ <- y_train[y_train!=0]
          
          RMSE_train_list <- numeric()
          RMSE_test_list <- numeric()
          
          y_train_binary_predict <- y_predict_binary[-exclude_obs,ui]
          y_test_binary_predict <- y_predict_binary[exclude_obs,ui]
          
          if(is.na(sd(y_train_))==T | sd(y_train_)==0){
               
               y_test_predict_temp <- rep(mean(y_train_),nrow(X_test))
               y_test_predict_temp[y_test_binary_predict==0] <- 0
               
          } else {
               
               rf_temp <- extraTrees(x=X_train_, y=y_train_, 
                                     ntree=250, 
                                     nodesize=1, 
                                     subsetSizes=ceiling((2/3)*length(y_train_)),
                                     numRandomCuts=5,
                                     numThreads=3)
               
               y_test_predict_temp <- predict(rf_temp, X_test)
               y_test_predict_temp[y_test_binary_predict==0] <- 0
               
               rm(rf_temp)
               gc()
                    
          }
          
          col_temp <- data.frame(cbind(y_test))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          df_y_test <- cbind(df_y_test, col_temp)
          
          col_temp <- data.frame(cbind(y_test_predict_temp))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          df_y_test_predict <- cbind(df_y_test_predict, col_temp)
          
          df_to_plot <- as.data.frame(cbind(Output=y_test,
                                            Prediction=y_test_predict_temp,
                                            Time=c(0:(length(sample_days)*24-1))))
          df_to_plot_melt <- melt(df_to_plot, id='Time')
          
          maximum_temp <- max(c(y_test,y_test_predict_temp))
          name_temp <- name_temp <- master_data$PowerSystemResourceName[ui]
          rmse_temp <- sqrt(mean((y_test-y_test_predict_temp)^2))
          mae_temp <- mean(abs(y_test-y_test_predict_temp))
          
     }
     
     saveRDS(df_y_test,file=paste0("y_test_data_v6_SAMPLE_",sc,".rds"))
     saveRDS(df_y_test_predict,file=paste0("y_test_predict_data_v6_SAMPLE_",sc,".rds"))
     
}

setwd("..")

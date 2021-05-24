# --------------------------------------------------------------------------------------------
# Logistic Regression ------------------------------------------------------------------------

lambda_list <- 10^seq(4,-4,length=9)

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

     results_train_df <- data.frame(matrix(ncol = 0, nrow = length(lambda_list)))
     results_test_df <- data.frame(matrix(ncol = 0, nrow = length(lambda_list)))
     
     for (ui in c(1:ncol(y_data_lag))){
          #for (ui in c(1:9)){     
          print(ui)
          max_temp <- max(y_data_lag[ui])
          sd_temp <- sd(y_data_lag[,ui])
          
          y_train_binary <- y_data_lag[-exclude_obs,ui]
          y_train_binary[y_train_binary!=0] <- 1
          y_train_binary <- factor(y_train_binary, levels=c(0,1))
          
          y_test_binary <- y_data_lag[exclude_obs,ui]
          y_test_binary[y_test_binary!=0] <- 1
          y_test_binary <- factor(y_test_binary, levels=c(0,1))
          
          need_to_classify <- sum(as.numeric(as.character(y_train_binary)))<length(y_train_binary)-1 & sum(as.numeric(as.character(y_train_binary)))>1
          
          if(need_to_classify==T){
               
               sd_X <- sapply(X_train, sd)
               var_with_no_variance <- which(sd_X==0)
               
               if(length(var_with_no_variance)==0){
                    var_with_no_variance <- 10000
               }
               
               logistic_reg <- glmnet(data.matrix(X_train[,-var_with_no_variance]), data.matrix(y_train_binary), 
                                      family="binomial", standardize=F, lambda=lambda_list,
                                      alpha=0)
               
               acc_train_list <- numeric()
               acc_test_list <- numeric()
               
               for (l in c(1:length(logistic_reg$lambda))){
                    y_train_binary_predict <- factor(predict(logistic_reg, newx=as.matrix(X_train[,-var_with_no_variance]), 
                                                             type="class")[,l], levels=c(0,1))
                    confMat_train <- confusionMatrix(y_train_binary_predict, y_train_binary, mode="prec_recall") 
                    acc_train_list <- c(acc_train_list,as.numeric(confMat_train$overall["Accuracy"]))
                    y_test_binary_predict <- factor(predict(logistic_reg, newx=as.matrix(X_test[,-var_with_no_variance]), 
                                                            type="class")[,l], levels=c(0,1))
                    confMat_test <- confusionMatrix(y_test_binary_predict, y_test_binary, mode="prec_recall") 
                    acc_test_list <- c(acc_test_list,as.numeric(confMat_test$overall["Accuracy"]))
               }
               
               acc_train_list <- c(acc_train_list, rep(NA, length(lambda_list)-length(acc_train_list)))
               acc_test_list <- c(acc_test_list, rep(NA, length(lambda_list)-length(acc_test_list)))
               
          } else {
               if (sum(as.numeric(as.character(y_train_binary)))>=length(y_train_binary)-1){
                    #NIR_test <- sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary)
                    acc_train_list <- rep(sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary),length(lambda_list))
                    acc_test_list <- rep(sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary),length(lambda_list))
               } else {
                    #NIR_test <- 1-sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary)
                    acc_train_list <- rep(1-sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary),length(lambda_list))
                    acc_test_list <- rep(1-sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary),length(lambda_list))
               }
          }
          
          col_temp <- data.frame(cbind(c(acc_train_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_train_df <- cbind(results_train_df, col_temp)
          
          col_temp <- data.frame(cbind(c(acc_test_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_test_df <- cbind(results_test_df, col_temp)
          
          print(col_temp[length(lambda_list),])
     }
     
     results_train_df$Lambda <- lambda_list
     results_test_df$Lambda <- lambda_list
     
     saveRDS(results_train_df,file=paste0("train_SAMPLE_",sc,"_Ridge_Classifier_data_v6_df.rds"))
     saveRDS(results_test_df,file=paste0("test_SAMPLE_",sc,"_Ridge_Classifier_data_v6_df.rds"))
}

setwd("..")

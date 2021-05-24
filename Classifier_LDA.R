# --------------------------------------------------------------------------------------------
# LDA ----------------------------------------------------------------------------------------

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

     results_df <- data.frame(matrix(ncol = 5, nrow = 0))
     col_names <- c("RF Class. NIR Train",
                    "RF Class. Accuracy Train",
                    "RF Class. NIR Test",
                    "RF Class. Accuracy Test",
                    "Maximum output")
     colnames(results_df) <- col_names
     
     for (ui in c(1:ncol(y_data_lag))){
          
          print(ui)
          max_temp <- max(y_data_lag[ui])
          sd_temp <- sd(y_data_lag[,ui])
          
          y_train_binary <- y_data_lag[-exclude_obs,ui]
          y_train_binary[y_train_binary!=0] <- 1
          y_train_binary <- factor(y_train_binary, levels=c(0,1))
          
          y_test_binary <- y_data_lag[exclude_obs,ui]
          y_test_binary[y_test_binary!=0] <- 1
          y_test_binary <- factor(y_test_binary, levels=c(0,1))
          
          need_to_classify <- sum(as.numeric(as.character(y_train_binary)))!=length(y_train_binary) & sum(as.numeric(as.character(y_train_binary)))!=0
          
          if(need_to_classify==T){
               
               sd_X <- sapply(X_train, sd)
               var_with_no_variance <- which(sd_X==0)
               
               if(length(var_with_no_variance)==0){
                    var_with_no_variance <- 10000
               }
               
               # LDA - NO BANDWIDTH
               lda_temp <- lda(y_train_binary ~ ., data = X_train[,-var_with_no_variance])
               
               y_train_binary_predict <- predict(lda_temp, newdata = X_train[,-var_with_no_variance])
               confMat_train <- confusionMatrix(y_train_binary_predict$class, y_train_binary, mode="prec_recall")
               y_test_binary_predict <- predict(lda_temp, newdata = X_test[,-var_with_no_variance])
               confMat_test <- confusionMatrix(y_test_binary_predict$class, y_test_binary, mode="prec_recall")
               
               acc_train <- confMat_train$overall["Accuracy"]
               acc_test <- confMat_test$overall["Accuracy"]
               
               if(sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary)>0.5) {
                    NIR_train <- sum(confMat_train$table[,2])/sum(confMat_train$table)
                    NIR_test <- sum(confMat_test$table[,2])/sum(confMat_test$table)
               } else {
                    NIR_train <- 1-sum(confMat_train$table[,2])/sum(confMat_train$table)
                    NIR_test <- 1-sum(confMat_test$table[,2])/sum(confMat_test$table)               
               }
               
          } else {
               NIR_train <- 1
               acc_train <- 1  
               
               if(sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary)>0.5) {
                    NIR_test <- sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary)
                    acc_test <- NIR_test
               } else {
                    NIR_test <- 1-sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary)
                    acc_test <- NIR_test
               }
          }
          
          row_temp <- data.frame(rbind(c(round(NIR_train, digits=3),
                                         round(acc_train, digits=3),
                                         round(NIR_test, digits=3),
                                         round(acc_test, digits=3),
                                         round(max_temp, digits=3))))
          colnames(row_temp) <- col_names
          rownames(row_temp) <- colnames(y_data_lag)[ui]
          results_df <- rbind(results_df, row_temp)
          
          print(acc_test)
     }
     
     saveRDS(results_df,file=paste0("test_SAMPLE_",sc,"_LDA_Classifier_data_v6_df.rds"))
}

setwd("..")
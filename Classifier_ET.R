# --------------------------------------------------------------------------------------------
# RF Classifier -----------------------------------------------------------------------------

node_size_list <- c(500,50,5,1)

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
     
     results_train_df <- data.frame(matrix(ncol = 0, nrow = length(node_size_list)+1))
     results_test_df <- data.frame(matrix(ncol = 0, nrow = length(node_size_list)+1))
     
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
               
          acc_train_list <- numeric()
          acc_test_list <- numeric()

          need_to_classify <- sum(as.numeric(as.character(y_train_binary)))<length(y_train_binary)-1 & sum(as.numeric(as.character(y_train_binary)))>1
          
          if(need_to_classify==T){
               
               if(sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary)>0.5) {
                    NIR_train <- sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary)
                    NIR_test <- sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary)
               } else {
                    NIR_train <- 1-sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary)
                    NIR_test <- 1-sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary)              
               }
               
               acc_train_list <- c(acc_train_list, NIR_train)
               acc_test_list <- c(acc_test_list, NIR_test)
          
               for (ns in node_size_list){
                    print(ns)
                    rf_temp <- extraTrees(x=X_train, y=y_train_binary, 
                                          ntree=250, 
                                          nodesize=ns, 
                                          subsetSizes=ceiling((2/3)*length(y_train_binary)),
                                          numRandomCuts=5,
                                          #mtry=ncol(X_data_poly_lag)/5,
                                          numThreads=2)
                    
                    confMat_train <- confusionMatrix(factor(predict(rf_temp, X_train), levels=c(0,1)), y_train_binary, mode="prec_recall")
                    acc_train <- confMat_train$overall["Accuracy"]
                    acc_train_list <- c(acc_train_list,as.numeric(confMat_train$overall["Accuracy"]))
                    confMat_test <- confusionMatrix(factor(predict(rf_temp, X_test), levels=c(0,1)), y_test_binary, mode="prec_recall")
                    acc_test <- confMat_test$overall["Accuracy"]
                    acc_test_list <- c(acc_test_list,as.numeric(confMat_test$overall["Accuracy"]))
                    
                    rm(rf_temp)
                    gc()
                    
               }
          } else {
               if (sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary)>0.5){
                    #NIR_test <- sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary)
                    acc_train_list <- rep(sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary),length(node_size_list)+1)
                    acc_test_list <- rep(sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary),length(node_size_list)+1)
               } else {
                    #NIR_test <- 1-sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary)
                    acc_train_list <- rep(1-sum(as.numeric(as.character(y_train_binary)))/length(y_train_binary),length(node_size_list)+1)
                    acc_test_list <- rep(1-sum(as.numeric(as.character(y_test_binary)))/length(y_test_binary),length(node_size_list)+1)
               }
          }
          
          col_temp <- data.frame(cbind(c(acc_train_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_train_df <- cbind(results_train_df, col_temp)
          
          col_temp <- data.frame(cbind(c(acc_test_list)))
          colnames(col_temp) <- colnames(y_data_lag)[ui]
          results_test_df <- cbind(results_test_df, col_temp)
          
          print(NIR_test)
          print(col_temp)
          
     }
     
     saveRDS(results_train_df,file=paste0("train_SAMPLE_",sc,"_ET_Classifier_data_v6_df.rds"))
     saveRDS(results_test_df,file=paste0("test_SAMPLE_",sc,"_ET_Classifier_data_v6_df.rds"))
     

}

setwd("..")

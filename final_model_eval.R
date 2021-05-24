setwd("./results")

median_MAE <- numeric()

for (sc in c(1:4)) {
     master_data_ <- master_data
     df_y_test <- readRDS(file=paste0("y_test_data_v6_SAMPLE_",sc,".rds"))
     df_y_test_predict <- readRDS(file=paste0("y_test_predict_data_v6_SAMPLE_",sc,".rds"))
     
     master_data_$predict_RMSE <- sqrt(colMeans((df_y_test - df_y_test_predict)^2))/master_data$InstalledGenCapacity
     
     master_data_$predict_MAE <- colMeans(abs(df_y_test - df_y_test_predict))/master_data$InstalledGenCapacity
     
     median_MAE <- c(median_MAE, quantile(master_data_$predict_MAE,0.5))
     
     df_medoids<-master_data_[master_data_$medoids==T,]
     df_rest<-master_data_[master_data_$medoids==F,]

}

average_median_MAE_sc_1_4 <- mean(median_MAE)

setwd("..")

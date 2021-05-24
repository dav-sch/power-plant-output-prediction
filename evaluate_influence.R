setwd("./results")

#----------- Evaluation drivers (regression) -----------

df_y_test_all <- data.frame(matrix(ncol = 205, nrow = 0))
df_y_test_predict_all <- data.frame(matrix(ncol = 205, nrow = 0))
colnames(df_y_test_all) <- colnames(y_data_lag)
colnames(df_y_test_predict_all) <- colnames(y_data_lag)
X_test_all <- data.frame(matrix(ncol = ncol(X_data_poly_lag), nrow = 0))
colnames(X_test_all) <- colnames(X_data_poly_lag)

for (sc in c(1:4)) {
     df_y_test <- readRDS(file=paste0("y_test_data_v6_SAMPLE_",sc,".rds"))
     df_y_test_predict <- readRDS(file=paste0("y_test_predict_data_v6_SAMPLE_",sc,".rds"))
     
     df_y_test_all <- rbind(df_y_test_all, df_y_test)
     df_y_test_predict_all <- rbind(df_y_test_predict_all, df_y_test_predict)
     
     set.seed(list_of_i[sc])
     
     print(paste0("SAMPLE: ",sc," SEED: ",list_of_i[sc]))
     
     sample_days <- sort(sample.int(no_days, 24, replace=F))
     exclude_obs <- as.numeric()
     
     for (j in sample_days){
          exclude_obs <- c(exclude_obs, c((j*24-23):(j*24)))
          print(paste0(j,": ",as.character(unique(X_data_weekdays[((j*24-23)):(j*24)]))))
     }
     
     X_test <- X_data_poly_lag[exclude_obs,]
     X_test_all <- rbind(X_test_all, X_test)
}


df_evaluation <- data.frame(matrix(0, ncol = ncol(X_data_poly_lag), nrow = length(unique(master_data$ProductionTypeName))))
colnames(df_evaluation) <- colnames(X_data_poly_lag)
rownames(df_evaluation) <- unique(master_data$ProductionTypeName)

for (ui in c(1:205)) {
     print(ui)
     
     type_temp <- master_data$ProductionTypeName[ui]
     
     # Absolute errors
     residuals_temp <- abs(df_y_test_all[,ui] - df_y_test_predict_all[,ui])
     lm_temp <- lm(residuals_temp~., data=X_test_all)

     # ABSOLUTE
     most_relevant_variables <- names(sort(abs(lm_temp$coefficients), decreasing = T)[1:20])
     
     df_evaluation[rownames(df_evaluation)==type_temp, colnames(df_evaluation) %in% most_relevant_variables] <- df_evaluation[rownames(df_evaluation)==type_temp, colnames(df_evaluation) %in% most_relevant_variables] + 1
}

df_eval_short <- df_evaluation[,which(colSums(df_evaluation)>50)]
df_eval_short <- df_eval_short[,order(colSums(df_eval_short))]
df_eval_short$Type <- rownames(df_eval_short)

colnames(df_eval_short) <- gsub("_", " ", colnames(df_eval_short))
colnames(df_eval_short) <- gsub("generation", "Gen", colnames(df_eval_short))
colnames(df_eval_short) <- gsub("ConvGeneration", "Gen", colnames(df_eval_short))
colnames(df_eval_short) <- gsub("Amprion", "Amp", colnames(df_eval_short))
colnames(df_eval_short) <- gsub("TransnetBW", "TNG", colnames(df_eval_short))
colnames(df_eval_short) <- gsub("TenneT", "TTG", colnames(df_eval_short))
colnames(df_eval_short) <- gsub(" GER", "", colnames(df_eval_short))
colnames(df_eval_short) <- gsub(" X ", " * ", colnames(df_eval_short))

df_eval_short_melt <- melt(df_eval_short, id="Type")



#----------- Evaluation temporal -----------

order_temp <- order(master_data$ProductionTypeName)

master_data$ProductionTypeName[order(master_data$ProductionTypeName)]

for (sc in c(1:4)) {
     
     df_y_test <- readRDS(file=paste0("y_test_data_v6_SAMPLE_",sc,".rds"))
     df_y_test_predict <- readRDS(file=paste0("y_test_predict_data_v6_SAMPLE_",sc,".rds"))
     
     set.seed(list_of_i[sc])
     
     print(paste0("SAMPLE: ",sc," SEED: ",list_of_i[sc]))
     
     sample_days <- sort(sample.int(no_days, 24, replace=F))
     exclude_obs <- as.numeric()
     weekdays_list <- as.character()
     dates_list <- as.character()
     
     for (j in sample_days){
          exclude_obs <- c(exclude_obs, c((j*24-23):(j*24)))
          weekdays_list <- c(weekdays_list, as.character(unique(X_data_weekdays[((j*24-23)):(j*24)])))
          dates_list <- c(dates_list, substr(raw_data[j*24+96,1],1,10))
     }
     
     weekdays_list[weekdays_list=="Montag"] <- "Mon"
     weekdays_list[weekdays_list=="Dienstag"] <- "Tue"
     weekdays_list[weekdays_list=="Mittwoch"] <- "Wed"
     weekdays_list[weekdays_list=="Donnerstag"] <- "Thu"
     weekdays_list[weekdays_list=="Freitag"] <- "Fri"
     weekdays_list[weekdays_list=="Samstag"] <- "Sat"
     weekdays_list[weekdays_list=="Sonntag"] <- "Sun"
     
     for (j in sample_days){
          exclude_obs <- c(exclude_obs, c((j*24-23):(j*24)))
          print(paste0(j,": ",as.character(unique(X_data_weekdays[((j*24-23)):(j*24)]))))
     }
     
     cap_mat <- as.data.frame(matrix(rep(sapply(y_data_lag, max), each=nrow(df_y_test)), ncol=ncol(df_y_test), nrow=nrow(df_y_test)))
     colnames(cap_mat) <- colnames(df_y_test)
     errors_df <- (df_y_test_predict - df_y_test)/cap_mat
     avg_abs_errors <- rowMeans(abs(errors_df))
     avg_abs_errors_days <- as.numeric()
     for(i in 1:24){
          avg_abs_errors_days <- c(avg_abs_errors_days, round(mean(avg_abs_errors[((i-1)*24+1):(i*24)]), digits=3))
     }
     avg_abs_errors_days <- as.character(avg_abs_errors_days)
     
     errors_df <- errors_df[,order_temp]
     
     colnames(errors_df) <- c(1:205)
     errors_df$Observation <- c(1:576)
     errors_df_melt <- melt(errors_df, id="Observation")
     errors_df_melt$variable <- as.numeric(as.character(errors_df_melt$variable))
     
     types_ordered_df <- as.data.frame(cbind("Observation"=rep(1,205),
                                             "Unit"=c(1:205),
                                             "Type"=master_data$ProductionTypeName[order(master_data$ProductionTypeName)]))
     types_ordered_df$Unit <- as.numeric(as.character(types_ordered_df$Unit))
     types_ordered_df$Observation <- as.numeric(as.character(types_ordered_df$Observation))
     
     cuts <- as.numeric()
     
     for(i in unique(types_ordered_df$Type)){
          cuts <- c(cuts, max(which(types_ordered_df$Type==i))+0.5) 
     }
     
     mid_points <- as.numeric()
     
     for(i in unique(types_ordered_df$Type)){
          mid_points <- c(mid_points, (0.5*(max(which(types_ordered_df$Type==i))+min(which(types_ordered_df$Type==i)))+50))
     }

     
     types_ordered_df <- as.data.frame(cbind("Observation"=rep(c(1:5),each=305),
                                             "Unit"=rep(c(1:305),5),
                                             "Type"=c(c(rep(NA,50),
                                                    master_data$ProductionTypeName[order(master_data$ProductionTypeName)],
                                                    rep(NA,50)),
                                                    rep(NA,305*4))))
     types_ordered_df$Unit <- as.numeric(as.character(types_ordered_df$Unit))
     types_ordered_df$Observation <- as.numeric(as.character(types_ordered_df$Observation))
     
}

setwd("..")

# LIST USED:
library("extraTrees")
library("reshape2")
library("glmnet")
library("cluster")
library("class")
library("ggplot2")
library("RColorBrewer")
library("ggpubr")
library("imputeTS")
library("httr")
library("jsonlite")
library("stringr")
library("fastDummies")
library("plyr")
library("dplyr")
library("BBmisc")
library("factoextra")


# CREATE DATA FILES FROM RAW DATA
# PREPROCESSING OF DATA
# CLUSTERING OF DATA

setwd("./data_DE")

# only run if raw data available in \data_DE\raw_data
#source("main_function.R")

source("data_processing.R")
setwd("..")
setwd("..")

# only run if raw data available
#source("gen_unit_master_data_clustering.R")
#setwd("..")


# LOAD DATA
setwd("./data_DE/process_data")

master_data <- readRDS("gen_unit_master_data_cluster.rds")
X_data_poly_lag <- readRDS(file="X_data_poly_lag_final.rds")
y_data_lag <- readRDS(file="y_data_poly_lag_final.rds")
raw_data <- readRDS("df_master_final.rds")

# FURTHER PROCESSING OF DATA
# SPECIFICALLY ELIMINATION OF UNNECESSARY COLUMNS
X_data_weekdays <- X_data_poly_lag[,(colnames(X_data_poly_lag) %in% c("weekday"))]
X_data_poly_lag <- X_data_poly_lag[,!(colnames(X_data_poly_lag) %in% c("weekday"))]
X_data_poly_lag <- X_data_poly_lag[,!(colnames(X_data_poly_lag) %in% c("Load_BE","Load_NL","Load_FR","Load_PL",
                                                                       "Load_CZ","Load_DK1","Load_DK2","Load_CH",
                                                                       "generation_solar_BE",                                                            
                                                                       "generation_wind_onshore_BE",                                                     
                                                                       "generation_wind_offshore_BE",                                                    
                                                                       "generation_solar_NL",                                                            
                                                                       "generation_wind_onshore_NL",                                                     
                                                                       "generation_wind_offshore_NL",                                                    
                                                                       "generation_solar_FR",                                                            
                                                                       "generation_wind_onshore_FR",                                                     
                                                                       "generation_wind_onshore_PL",                                                     
                                                                       "generation_solar_CZ",                                                            
                                                                       "generation_solar_DK1",                                                           
                                                                       "generation_wind_onshore_DK1",                                                    
                                                                       "generation_wind_offshore_DK1",                                                   
                                                                       "generation_solar_DK2",                                                           
                                                                       "generation_wind_onshore_DK2",                                                    
                                                                       "generation_wind_offshore_DK2",                                                   
                                                                       "generation_solar_CH",                                                            
                                                                       "generation_wind_onshore_CH"))]

sd_X <- sapply(X_data_poly_lag, sd)
var_with_no_variance <- which(sd_X==0)
X_data_poly_lag <- X_data_poly_lag[,-var_with_no_variance]
X_data_poly_lag[,-c(1:41)] <- normalize(X_data_poly_lag[,-c(1:41)], method="standardize")


### START OF ANALYSIS
# Determine amount of days 
no_days <- nrow(y_data_lag)/24

# Find random seeds that select days with at least four days in between
# To ensure that lagged data is actually available and does not fall into the "test region"
i <- 0
list_of_i <- as.numeric()
no_random_samples <- 0
while(no_random_samples<5){
     set.seed(i)
     sample_days <- sort(sample.int(no_days, 24, replace=F))
     
     sample_days_diff <- sample_days[2:length(sample_days)]-sample_days[1:(length(sample_days)-1)]
     
     if(min(sample_days_diff)>4){
          list_of_i <- c(list_of_i,i)
     } 
     
     i <- i+1
     no_random_samples <- length(list_of_i)
}


## RUN ALL CLASSIFIER METHODS
# include files
setwd("..")
source("Classifier_LDA.R")
source("Classifier_Ridge_Logistic.R")
source("Classifier_KNN.R")
source("Classifier_ET.R")


## EVALUATE CLASSIFIER PERFORMANCE (in each sample for each best model)

setwd("./results")

df_to_boxplot <- data.frame(matrix(ncol = 4, nrow = 0))
col_names <- c("Model",
               "Parameter",
               "Accuracy",
               "Sample")
colnames(df_to_boxplot) <- col_names

lambda_list <- 10^seq(4,-4,length=9)
k_list <- c(1,10,100,"n")
max_leaf_nodes_list <- c("n",500,50,5,1)

for(sc in c(1:4)){
     
     set.seed(list_of_i[sc])
     sample_days <- sort(sample.int(no_days, 24, replace=F))
     exclude_obs <- as.numeric()
     
     for (j in sample_days){
          exclude_obs <- c(exclude_obs, c((j*24-23):(j*24)))
     }
     
     y_train_binary <- y_data_lag[-exclude_obs,]
     y_train_binary[y_train_binary!=0] <- 1
     
     y_test_binary <- y_data_lag[exclude_obs,]
     y_test_binary[y_test_binary!=0] <- 1
     
     NIR_train <- colSums(y_train_binary)/nrow(y_train_binary)
     NIR_test <- colSums(y_test_binary)/nrow(y_test_binary)
     NIR_test[NIR_train<0.5] <- 1-NIR_test[NIR_train<0.5]
     NIR_train[NIR_train<0.5] <- 1-NIR_train[NIR_train<0.5]
     
     df_temp <- data.frame(cbind("Model"=rep("NIR",205),
                                 "Parameter"=rep("",205),
                                 "Accuracy"=NIR_test,
                                 "Sample"=rep(sc,205)))
     
     df_to_boxplot <- rbind(df_to_boxplot,df_temp)
     
     df_all_possible_options <- as.data.frame(rbind(NIR_test))
     colnames(df_all_possible_options) <- colnames(y_data_lag)
     
     for(mod in c("LDA","Ridge","KNN","ET")){
          
          results_test_df <- readRDS(file=paste0("test_SAMPLE_",sc,"_",mod,"_Classifier_data_v6_df.rds"))
          
          if(mod=="Ridge"){
               index_best_model <- which.max(rowSums(results_test_df[,1:205], na.rm=T))
               value_column <- t(results_test_df[index_best_model,1:205])
               best_parameter <- lambda_list[index_best_model]
               model_column <- rep(paste0("RLR"),205)
               parameter_column <- rep(paste0(expression(lambda)," == ",best_parameter),205)
               df_all_possible_options <- rbind(df_all_possible_options,results_test_df[,1:205])
          } else if(mod=="KNN"){
               index_best_model <- which.max(rowSums(results_test_df[,1:205], na.rm=T))
               value_column <- t(results_test_df[index_best_model,1:205])
               best_parameter <- k_list[index_best_model]
               model_column <- rep(paste0("KNN"),205)
               parameter_column <- rep(paste0("k == ",best_parameter),205)
               df_all_possible_options <- rbind(df_all_possible_options,results_test_df[,1:205])
          } else if(mod=="ET"){
               index_best_model <- which.max(rowSums(results_test_df[,1:205], na.rm=T))
               value_column <- t(results_test_df[index_best_model,1:205])
               best_parameter <- max_leaf_nodes_list[index_best_model]
               model_column <- rep(paste0("ET"),205)
               parameter_column <- rep(paste0("s == ",best_parameter),205)
               df_all_possible_options <- rbind(df_all_possible_options,results_test_df[,1:205])
          } else {
               value_column <- results_test_df[,4]
               model_column <- rep(paste0("LDA"),205)
               parameter_column <- rep(paste0(""),205)
               df_all_possible_options <- rbind(df_all_possible_options,results_test_df[,4])
          }
          
          df_temp <- data.frame(cbind("Model"=model_column,
                                      "Parameter"=parameter_column,
                                      "Accuracy"=value_column,
                                      "Sample"=rep(sc,205)))
          
          colnames(df_temp) <- col_names
          df_to_boxplot <- rbind(df_to_boxplot,df_temp)
          
     }
     
     df_temp <- data.frame(cbind("Model"="Best",
                                 "Parameter"="",
                                 "Accuracy"=sapply(df_all_possible_options,max),
                                 "Sample"=rep(sc,205)))
     
     colnames(df_temp) <- col_names
     df_to_boxplot <- rbind(df_to_boxplot,df_temp)
     
}

df_to_boxplot$Accuracy <- as.numeric(as.character(df_to_boxplot$Accuracy))

df_to_boxplot$Type <- rep(master_data$ProductionTypeName,6*4)
#df_to_boxplot$Type <- factor(df_to_boxplot$Type)
df_to_boxplot$Capacity <- rep(master_data$InstalledGenCapacity,6*4)


df_eval <- data.frame(matrix(ncol = 0, nrow = 205))
rownames(df_eval) <- colnames(y_data_lag)

for(sc in c(1:4)){
     best_model <- as.character()
     for(ui in c(1:205)){
          df_temp <- df_to_boxplot[df_to_boxplot$Sample==sc,]
          df_temp_ui <- df_temp[rep(c(rep(F,(ui-1)), T, rep(F,(205-ui))),6),]
          df_temp_ui <- df_temp_ui[c(5,4,3,2,1),]
          best_model <- c(best_model, as.character(df_temp_ui$Model[which.max(df_temp_ui$Accuracy)]))
     }
     #best_model <- factor(best_model)
     df_eval <- cbind(df_eval, best_model)
     colnames(df_eval)[ncol(df_eval)] <- paste0("SC", sc)
}


df_eval <- data.frame(lapply(df_eval, as.character), stringsAsFactors=FALSE)

counter <- 0
most_frequent <- function(x){
     dd <- unique(as.character(x))
     if(max(tabulate(match(x,dd)))==1){
          return("ET-1")
     } else if(max(tabulate(match(x,dd)))==2 & length(tabulate(match(x,dd)))==2) {
          if (sum(as.character(x)=="ET")==2) {
               return("ET-2")
               counter <- counter + 1
          } else if (sum(as.character(x)=="KNN")==2) {
               return("KNN-2")
          } else if (sum(as.character(x)=="RLR")==2) {
               return("RLR-2")
          } else if (sum(as.character(x)=="LDA")==2) {
               return("LDA-2")
          } else {
               return("NIR-2")
          }
     } else {
          return(dd[which.max(tabulate(match(x,dd)))])
     }
}

df_eval$best_model <- apply(df_eval, 1, most_frequent)
df_eval$Type <- master_data$ProductionTypeName

summary(factor(df_eval$best_model))
length(which(rowSums(df_eval[,1:4]=="ET")==4&df_eval$best_model=="ET"))
length(which(rowSums(df_eval[,1:4]=="ET")==3&df_eval$best_model=="ET"))
length(which(rowSums(df_eval[,1:4]=="ET")==2&df_eval$best_model=="ET"))
length(which(rowSums(df_eval[,1:4]=="ET")==1&df_eval$best_model=="ET"))

df_eval$Type[df_eval$best_model=="KNN"]
df_eval$Type[df_eval$best_model=="RLR"]
df_eval$Type[df_eval$best_model=="NIR"]
df_eval$Type[df_eval$best_model=="LDA"]

df_to_boxplot$Accuracy <- as.numeric(as.character(df_to_boxplot$Accuracy))
df_to_boxplot$Type <- factor(df_to_boxplot$Type)



## TRAIN CHOSEN EXTRA TREE CLASSIFIERS ONCE FOR EACH SAMPLE 

setwd("..")
setwd("./et_models")

for (sc in c(1:4)){
     
     results_df <- data.frame(matrix(ncol = 0, nrow = 8664))
     
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
     
     acc_test_list <- numeric()
     
     #     for (ui in c(141:ncol(y_data_lag))){
     for (ui in c(1:205)){          
          print(ui)
          print(colnames(y_data_lag)[ui])
          
          y_train_binary <- y_data_lag[-exclude_obs,ui]
          y_train_binary[y_train_binary!=0] <- 1
          y_train_binary <- factor(y_train_binary, levels=c(0,1))
          
          y_test_binary <- y_data_lag[exclude_obs,ui]
          y_test_binary[y_test_binary!=0] <- 1
          y_test_binary <- factor(y_test_binary, levels=c(0,1))
          
          need_to_classify <- sum(as.numeric(as.character(y_train_binary)))!=length(y_train_binary) & sum(as.numeric(as.character(y_train_binary)))!=0
          
          if(need_to_classify==T){
               et_temp <- extraTrees(x=X_train, y=y_train_binary, 
                                     ntree=250, 
                                     nodesize=1, 
                                     subsetSizes=ceiling((2/3)*length(y_train_binary)),
                                     numRandomCuts=5,
                                     numThreads=3)
               
               y_binary_predict_temp <- predict(et_temp, X_data_poly_lag)
               
               confMat_test <- confusionMatrix(factor(predict(et_temp, X_test), levels=c(0,1)), y_test_binary, mode="prec_recall")
               acc_test <- confMat_test$overall["Accuracy"]
               acc_test_list <- c(acc_test_list, as.numeric(acc_test))
               print(as.numeric(acc_test))
               
               rm(et_temp)
               gc()
               
          } else {
               y_binary_predict_temp <- rep(unique(as.numeric(as.character(y_train_binary))), nrow(X_data_poly_lag))
               confMat_test <- confusionMatrix(factor(rep(unique(as.numeric(as.character(y_train_binary))), nrow(X_test)),
                                                      levels=c(0,1)), y_test_binary, mode="prec_recall")
               acc_test <- confMat_test$overall["Accuracy"]
               acc_test_list <- c(acc_test_list, as.numeric(acc_test))
               print(as.numeric(acc_test))
          }
          
          y_binary_predict_temp <- data.frame(cbind(as.numeric(as.character(y_binary_predict_temp))))
          colnames(y_binary_predict_temp) <- colnames(y_data_lag)[ui]
          
          results_df <- cbind(results_df, y_binary_predict_temp)
     }
     
     saveRDS(results_df,file=paste0("ET_model_data_v6_predict_all_binary_SAMPLE_",as.character(sc),".rds"))
     
     acc_df <- as.data.frame(rbind(acc_test_list))
     colnames(acc_df) <- colnames(y_data_lag)
     
     saveRDS(acc_df,file=paste0("ET_model_data_v6_accuracy_SAMPLE_",as.character(sc),".rds"))
}


## RUN ALL REGRESSOR METHODS
# include files
setwd("..")
source("Regressor_Average.R")
source("Regressor_Linear_Regression.R")
source("Regressor_Ridge_Regression.R")
source("Regressor_KNN.R")
source("Regressor_ET.R")



## EVALUATE REGRESSOR PERFORMANCE (in each sample for each best model)

setwd("./results")

df_to_boxplot <- data.frame(matrix(ncol = 4, nrow = 0))
col_names <- c("Model",
               "Parameter",
               "RMSE/Capacity",
               "Sample")
colnames(df_to_boxplot) <- col_names

lambda_list <- 10^seq(10,-4,length=8)
k_list <- c(1,10,100,"n")
node_size_list <- c("n",500,50,5,1)

# Choose MAE or RMSE
error_crit <- "MAE"

for(sc in c(1:4)){
     
     results_test_df <- readRDS(file=paste0("Average_SAMPLE_",as.character(sc),"_data_v6_",error_crit,".rds"))
     
     model_column <- rep(paste0("Avg."),205)
     value_column <- results_test_df[,2]
     parameter_column <- rep(paste0(""),205)
     
     df_temp <- data.frame(cbind("Model"=model_column,
                                 "Parameter"=parameter_column,
                                 "RMSE/Capacity"=value_column,
                                 "Sample"=rep(sc,205)))
     
     colnames(df_temp) <- col_names
     df_to_boxplot <- rbind(df_to_boxplot,df_temp)
     
     df_all_possible_options <- as.data.frame(rbind(value_column))
     colnames(df_all_possible_options) <- colnames(y_data_lag)
     
     results_test_df <- readRDS(file=paste0("Linear_Regression_SAMPLE_",sc,"_data_v6_",error_crit,".rds"))
     
     model_column <- rep(paste0("Naive"),205)
     value_column <- results_test_df[,4]
     parameter_column <- rep(paste0(""),205)
     
     df_temp <- data.frame(cbind("Model"=model_column,
                                 "Parameter"=parameter_column,
                                 "RMSE/Capacity"=value_column,
                                 "Sample"=rep(sc,205)))
     
     colnames(df_temp) <- col_names
     df_to_boxplot <- rbind(df_to_boxplot,df_temp)
     
     df_all_possible_options <- rbind(df_all_possible_options,value_column)
     
     model_column <- rep(paste0("LR"),205)
     value_column <- results_test_df[,3]
     parameter_column <- rep(paste0(""),205)
     
     df_temp <- data.frame(cbind("Model"=model_column,
                                 "Parameter"=parameter_column,
                                 "RMSE/Capacity"=value_column,
                                 "Sample"=rep(sc,205)))
     
     colnames(df_temp) <- col_names
     df_to_boxplot <- rbind(df_to_boxplot,df_temp)
     
     df_all_possible_options <- rbind(df_all_possible_options,value_column)
     
     for(mod in c("Ridge","KNN","ET")){
          
          results_test_df <- readRDS(file=paste0("test_SAMPLE_",sc,"_",mod,"_Regressor_data_v6_df_",error_crit,".rds"))
          
          if(mod=="Ridge"){
               index_best_model <- which.min(rowSums(results_test_df[,1:205], na.rm=T))
               value_column <- t(results_test_df[index_best_model,1:205])
               best_parameter <- lambda_list[index_best_model]
               model_column <- rep(paste0("RR"),205)
               parameter_column <- rep(paste0(expression(lambda)," == ",best_parameter),205)
               df_all_possible_options <- rbind(df_all_possible_options,results_test_df[,1:205])
          } else if(mod=="KNN"){
               index_best_model <- which.min(rowSums(results_test_df[,1:205], na.rm=T))
               value_column <- t(results_test_df[index_best_model,1:205])
               best_parameter <- k_list[index_best_model]
               model_column <- rep(paste0("KNN"),205)
               parameter_column <- rep(paste0("k == ",best_parameter),205)
               df_all_possible_options <- rbind(df_all_possible_options,results_test_df[,1:205])
          } else if(mod=="ET"){
               index_best_model <- which.min(rowSums(results_test_df[,1:205], na.rm=T))
               value_column <- t(results_test_df[index_best_model,1:205])
               best_parameter <- node_size_list[index_best_model]
               model_column <- rep(paste0("ET"),205)
               parameter_column <- rep(paste0("s == ",best_parameter),205)
               df_all_possible_options <- rbind(df_all_possible_options,results_test_df[,1:205])
          } 
          
          df_temp <- data.frame(cbind("Model"=model_column,
                                      "Parameter"=parameter_column,
                                      "RMSE/Capacity"=value_column,
                                      "Sample"=rep(sc,205)))
          
          colnames(df_temp) <- col_names
          
          df_to_boxplot <- rbind(df_to_boxplot,df_temp)
     }
     
     df_temp <- data.frame(cbind("Model"="Best",
                                 "Parameter"="",
                                 "Accuracy"=sapply(df_all_possible_options,min),
                                 "Sample"=rep(sc,205)))
     
     colnames(df_temp) <- col_names
     df_to_boxplot <- rbind(df_to_boxplot,df_temp)
}

df_to_boxplot$`RMSE/Capacity` <- as.numeric(as.character(df_to_boxplot$`RMSE/Capacity`))

df_to_boxplot$Type <- rep(master_data$ProductionTypeName,7*4)
#df_to_boxplot$Type <- factor(df_to_boxplot$Type)
df_to_boxplot$Capacity <- rep(master_data$InstalledGenCapacity,7*4)


df_to_boxplot_copy <- df_to_boxplot
df_eval <- data.frame(matrix(ncol = 0, nrow = 205))
rownames(df_eval) <- colnames(y_data_lag)

for(sc in c(1:4)){
     best_model <- as.character()
     for(ui in c(1:205)){
          df_temp <- df_to_boxplot_copy[df_to_boxplot_copy$Sample==sc,]
          df_temp_ui <- df_temp[rep(c(rep(F,(ui-1)), T, rep(F,(205-ui))),7),]
          df_temp_ui <- df_temp_ui[c(6,5,4,3,2,1),]
          best_model <- c(best_model, as.character(df_temp_ui$Model[which.min(df_temp_ui$`RMSE/Capacity`)]))
     }
     #best_model <- factor(best_model)
     df_eval <- cbind(df_eval, best_model)
     colnames(df_eval)[ncol(df_eval)] <- paste0("SC", sc)
}

df_eval <- data.frame(lapply(df_eval, as.character), stringsAsFactors=FALSE)

counter <- 0
most_frequent <- function(x){
     dd <- unique(as.character(x))
     if(max(tabulate(match(x,dd)))==1){
          return("ET-1")
     } else if(max(tabulate(match(x,dd)))==2 & length(tabulate(match(x,dd)))==2) {
          if (sum(as.character(x)=="ET")==2) {
               return("ET-2")
               counter <- counter + 1
          } else if (sum(as.character(x)=="KNN")==2) {
               return("KNN-2")
          } else if (sum(as.character(x)=="RR")==2) {
               return("RR-2")
          } else if (sum(as.character(x)=="LR")==2) {
               return("LR-2")
          } else if (sum(as.character(x)=="Naive")==2) {
               return("Naive-2")
          } else {
               return("Avg.-2")
          }
     } else {
          return(dd[which.max(tabulate(match(x,dd)))])
     }
}

df_eval$best_model <- apply(df_eval, 1, most_frequent)
df_eval$Type <- master_data$ProductionTypeName

summary(factor(df_eval$best_model))
length(which(rowSums(df_eval[,1:4]=="ET")==4&df_eval$best_model=="ET"))
length(which(rowSums(df_eval[,1:4]=="ET")==3&df_eval$best_model=="ET"))
length(which(rowSums(df_eval[,1:4]=="ET")==2&df_eval$best_model=="ET"))
length(which(rowSums(df_eval[,1:4]=="ET")==1&df_eval$best_model=="ET"))

df_eval$Type[df_eval$best_model=="KNN"]
df_eval$Type[df_eval$best_model=="RR"]
df_eval$Type[df_eval$best_model=="LR"]
df_eval$Type[df_eval$best_model=="Naive"]
df_eval$Type[df_eval$best_model=="Avg."]

df_to_boxplot$`RMSE/Capacity` <- as.numeric(as.character(df_to_boxplot$`RMSE/Capacity`))
df_to_boxplot$Type <- factor(df_to_boxplot$Type)


## MAKE FINAL PREDICTION
setwd("..")
source("final_prediction.R")

## OBTAIN FINAL MODEL RESULTS
source("final_model_eval.R")

## EVAL. INFLUENCE
source("evaluate_influence.R")

## GET PERFORMANCE OF SELECTED REPRESENTATIVE UNITS
source("representative_units.R")

## GET PERFORMANCE ON AGGREGATED FUEL TYPE-LEVEL
source("total_generation.R")


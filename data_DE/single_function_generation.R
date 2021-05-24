function_generation<-function(y) {
  
  # import load forecast data for January of the year y
  string_temp<-paste0(as.character(y),"_1_ActualGenerationOutputPerUnit.csv")
  df_temp<-read.table(string_temp, header = T, fileEncoding = "UTF-16", sep = "\t", quote = "")
  df_temp<-df_temp[,c(4,9,10,12,13,14)]
  
  # select all rows where MapCode is 'DE_50HzT','DE_Amprion','DE_TenneT_GER' or 'DE_TransnetBW'
  df_temp<-df_temp[df_temp$MapCode %in% c('DE_50HzT','DE_Amprion','DE_TenneT_GER','DE_TransnetBW'),]
  df_temp<-df_temp[,c(1,3,5,6)]  
  
  # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
  df_temp$DateTime<-as.character(df_temp$DateTime)
  df_temp$DateTime<-substr(df_temp$DateTime,1,13)
  
  # create vector plowerplant_names_v with every powerplant name (GenerationUnitEIC)
  plowerplant_names_v<-as.character(sort(unique(df_temp$GenerationUnitEIC)))
  
  
  # starting the for-loop with the first element of the powerplant_names_v
  df_powerplant<-df_temp[df_temp$GenerationUnitEIC==plowerplant_names_v[1],c(1,3,4)]
  df_powerplant<-df_powerplant[order(df_powerplant$DateTime),]
  # replace NAs with 0, otherwise -> Error
  df_powerplant$ActualConsumption[is.na(df_powerplant$ActualConsumption)==T] <- 0
  df_powerplant$ActualGenerationOutput <- df_powerplant$ActualGenerationOutput - df_powerplant$ActualConsumption
  df_powerplant <- df_powerplant[,-3]
  colnames(df_powerplant)[2]<-plowerplant_names_v[1]
  
  
  # for-loop: merging the second element of the powerplant_names_v to the last element of the powerplant_names_v
  # join df_powerplant and df_temp_u
  for (u in plowerplant_names_v[c(2:length(plowerplant_names_v))]) {
    df_temp_u<-df_temp[df_temp$GenerationUnitEIC==u,c(1,3,4)]
    df_temp_u$ActualConsumption[is.na(df_temp_u$ActualConsumption)==T] <- 0
    df_temp_u$ActualGenerationOutput <- df_temp_u$ActualGenerationOutput - df_temp_u$ActualConsumption
    df_temp_u <- df_temp_u[,-3]
    colnames(df_temp_u)[2] <- u
    df_powerplant<-join(df_powerplant,df_temp_u, by="DateTime", match="first")
  }
  
  
  # for-loop to import data for the months February - December of the year y
  # rename df_powerplant to df_powerplant_complete
  df_powerplant_complete<-df_powerplant
  
  
  for(i in c(2:12)) {
    string_temp<-paste0(as.character(y),"_", as.character(i),"_ActualGenerationOutputPerUnit.csv")
    df_temp<-read.table(string_temp, header = T, fileEncoding = "UTF-16", sep = "\t", quote = "")
    df_temp<-df_temp[,c(4,9,10,12,13,14)]
    
    # select all rows where MapCode is 'DE_50HzT','DE_Amprion','DE_TenneT_GER' or 'DE_TransnetBW'
    df_temp<-df_temp[df_temp$MapCode %in% c('DE_50HzT','DE_Amprion','DE_TenneT_GER','DE_TransnetBW'),]
    df_temp<-df_temp[,c(1,3,5,6)]  
    
    # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
    df_temp$DateTime<-as.character(df_temp$DateTime)
    df_temp$DateTime<-substr(df_temp$DateTime,1,13)
    
    # create vector plowerplant_names_v with every powerplant name (GenerationUnitEIC)
    plowerplant_names_v<-as.character(sort(unique(df_temp$GenerationUnitEIC)))
    
    # starting the for-loop with the first element of the powerplant_names_v
    df_powerplant<-df_temp[df_temp$GenerationUnitEIC==plowerplant_names_v[1],c(1,3,4)]
    df_powerplant<-df_powerplant[order(df_powerplant$DateTime),]
    df_powerplant$ActualConsumption[is.na(df_powerplant$ActualConsumption)==T] <- 0
    df_powerplant$ActualGenerationOutput <- df_powerplant$ActualGenerationOutput - df_powerplant$ActualConsumption
    df_powerplant <- df_powerplant[,-3]
    colnames(df_powerplant)[2]<-plowerplant_names_v[1]
    
    
    # for-loop: merging the second element of the powerplant_names_v to the last element of the powerplant_names_v
    # join df_powerplant and df_temp_u
    for (u in plowerplant_names_v[c(2:length(plowerplant_names_v))]) {
      df_temp_u<-df_temp[df_temp$GenerationUnitEIC==u,c(1,3,4)]
      df_temp_u$ActualConsumption[is.na(df_temp_u$ActualConsumption)==T] <- 0
      df_temp_u$ActualGenerationOutput <- df_temp_u$ActualGenerationOutput - df_temp_u$ActualConsumption
      df_temp_u <- df_temp_u[,-3]
      colnames(df_temp_u)[2] <- u
      df_powerplant<-join(df_powerplant,df_temp_u, by="DateTime", match="first")
    }
    # merge rows of df_powerplant and df_powerplant_complete -> df_powerplant_complete
    df_powerplant_complete<-rbind.fill(df_powerplant, df_powerplant_complete)
    print(i)
  }
  df_powerplant_complete<-df_powerplant_complete[order(df_powerplant_complete$DateTime),]
  return(df_powerplant_complete)
}
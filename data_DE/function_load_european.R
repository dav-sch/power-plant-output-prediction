function_load_european<-function(y) {
  
  # import load forecast data for January of the year y
  string_temp<-paste0(as.character(y),"_1_DayAheadTotalLoadForecast.csv")
  df_temp<-read.table(string_temp, header=T, fileEncoding="UTF-16", sep = "\t")
  
  # create euro_vector (includes all relevant European countries)
  # elements of the euro_vector must be equal to the designation in the load forecast data column "MapCode"
  # euro_vector is adjustable
  euro_vector <- unique(df_temp$MapCode)
  euro_vector<-c("DE_50HzT","DE_Amprion","DE_TenneT_GER","DE_TransnetBW", "BE", "NL", "FR", "AT", "PL", "CZ", "DK1", "DK2", "CH")
  
  # starting the for-loop with the first element of the euro_vector "DE_50HzT"
  # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
  l<-euro_vector[1]
  df_temp_loop<-df_temp[df_temp[,9]==l,c(4,10)]
  df_temp_loop$DateTime<-as.character(df_temp_loop$DateTime)
  df_temp_loop$DateTime<-substr(df_temp_loop$DateTime,1,13)
  df_temp_loop<-aggregate(.~DateTime, df_temp_loop, mean)
  
  # rename columns
  df_euro<-df_temp_loop
  colnames(df_euro)[2]<-paste0("Load_",l)
  
  
  # for-loop: merging the second element of the euro_vector to the last element of the euro_vector
  # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
  # merging all values of the euro_vector for January into a new dataframe "df_euro"
  for(c in c(2:length(euro_vector)))  {
    l<-euro_vector[c]
    df_temp_loop<-df_temp[df_temp[,9]==l,c(4,10)]
    df_temp_loop$DateTime<-as.character(df_temp_loop$DateTime)
    df_temp_loop$DateTime<-substr(df_temp_loop$DateTime,1,13)
    df_temp_loop<-aggregate(.~DateTime, df_temp_loop, mean)
    colnames(df_temp_loop)[2]<-paste0("Load_",l)
    df_euro<-merge(df_euro,df_temp_loop, by="DateTime", all.x = T)
  }
  
  
  # for-loop to import data for the months February - December of the year y
  for(i in c(2:12)){
    string_temp<-paste0(as.character(y),"_",as.character(i),"_DayAheadTotalLoadForecast.csv")
    df_temp<-read.table(string_temp, header=T, fileEncoding="UTF-16", sep = "\t")
      
      # starting the for-loop with the first element of the euro_vector "DE_50HzT"
      # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
      l<-euro_vector[1]
      df_temp_loop<-df_temp[df_temp[,9]==l,c(4,10)]
      df_temp_loop$DateTime<-as.character(df_temp_loop$DateTime)
      df_temp_loop$DateTime<-substr(df_temp_loop$DateTime,1,13)
      df_temp_loop<-aggregate(.~DateTime, df_temp_loop, mean)
      df_euro_month<-df_temp_loop
      colnames(df_euro_month)[2]<-paste0("Load_",l)
      
      
      # for-loop: merging the second element of the euro_vector to the last element of the euro_vector
      # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
      # merging all values of the euro_vector into a new dataframe "df_euro"
      for(c in c(2:length(euro_vector)))  {
        l<-euro_vector[c]
        df_temp_loop<-df_temp[df_temp[,9]==l,c(4,10)]
        df_temp_loop$DateTime<-as.character(df_temp_loop$DateTime)
        df_temp_loop$DateTime<-substr(df_temp_loop$DateTime,1,13)
        df_temp_loop<-aggregate(.~DateTime, df_temp_loop, mean)
        colnames(df_temp_loop)[2]<-paste0("Load_",l)
        df_euro_month<-merge(df_euro_month,df_temp_loop, by="DateTime", all.x = T)
        }
    
    # merging all months into a new dataframe "df_euro" (includes all elements of the euro_vector for each month of the year y)
    df_euro<-rbind(df_euro,df_euro_month)
    df_euro<-df_euro[order(df_euro$DateTime),]
    }
  return(df_euro)
}
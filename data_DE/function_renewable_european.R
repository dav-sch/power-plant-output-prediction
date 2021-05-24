function_renewable_european<-function(y) {
  
  # import generation data for January of the year y
  string_temp<-paste0(as.character(y),"_1_DayAheadGenerationForecastWindSolar.csv")
  df_temp<-read.table(string_temp, header=T, fileEncoding="UTF-16", sep = "\t")
  
  # create euro_vector (includes all relevant European countries)
  euro_vector <- unique(df_temp$MapCode)
  euro_vector<-c("DE_50HzT","DE_Amprion","DE_TenneT_GER","DE_TransnetBW", "BE", "NL", "FR", "AT", "PL", "CZ", "DK1", "DK2", "CH")
  
  # starting the for-loop with the first element of the euro_vector "DE_50HzT"
  # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
  l<-euro_vector[1]
  df_temp_loop<-df_temp[df_temp[,9]==l,c(4,10,11)]
  colnames(df_temp_loop)<-c("DateTime","Production Type","Aggregated Generation Forecast")
  
  df_temp_loop$DateTime<-as.character(df_temp_loop$DateTime)
  df_temp_loop$DateTime<-substr(df_temp_loop$DateTime,1,13)

      # select all rows where Production Type is "Solar" -> create new df "df_temp_solar"
      df_temp_solar<-df_temp_loop[df_temp_loop$`Production Type`=="Solar",]
      df_temp_solar<-df_temp_solar[,c(1,3)]
      df_temp_solar<-aggregate(.~DateTime, df_temp_solar, mean)
      
      # select all rows where Production Type is "Wind Onshore" -> create new df "df_temp_onshore"
      df_temp_onshore<-df_temp_loop[df_temp_loop$`Production Type`=="Wind Onshore",]
      df_temp_onshore<-df_temp_onshore[,c(1,3)]
      df_temp_onshore<-aggregate(.~DateTime, df_temp_onshore, mean)
      
      # select all rows where Production Type is "Wind offshore" -> create new df "df_temp_offshore"
      df_temp_offshore<-df_temp_loop[df_temp_loop$`Production Type`=="Wind Offshore",]
      df_temp_offshore<-df_temp_offshore[,c(1,3)]
      df_temp_offshore<-aggregate(.~DateTime, df_temp_offshore, mean)
      
      # create new df -> df_datetime
      df_datetime<-as.data.frame(cbind("DateTime"=df_temp_solar$DateTime))
      
      # merge df_datetime and df_temp_solar
      df_comb<-merge(df_datetime,df_temp_solar, by="DateTime", all.x = T)
      colnames(df_comb)<-c("DateTime","Generation Solar")
      
      # merge df_comb and df_temp_onshore
      df_comb<-merge(df_comb,df_temp_onshore, by="DateTime", all.x = T)
      colnames(df_comb)<-c("DateTime","Generation Solar","Generation Wind Onshore")
      
      # merge df_comb and df_temp_offshore -> df_comb with columns time, solar, onshore and offshore
      df_comb<-merge(df_comb,df_temp_offshore, by="DateTime", all.x = T)
      colnames(df_comb)<-c("DateTime","Generation Solar","Generation Wind Onshore","Generation Wind Offshore")
      
      df_euro<-df_comb
      
      # rename columns 
      colnames(df_euro)[c(2:4)]<-c(paste0("generation_solar_",l),paste0("generation_wind_onshore_",l),paste0("generation_wind_offshore_",l))
    
        
  # for-loop: merging the second element of the euro_vector to the last element of the euro_vector
  # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
  # merging all values of the euro_vector for January into a new dataframe "df_euro"
  
  for(c in c(2:length(euro_vector)))  {
    
    l<-euro_vector[c]
    df_temp_loop<-df_temp[df_temp[,9]==l,c(4,10,11)]
    colnames(df_temp_loop)<-c("DateTime","Production Type","Aggregated Generation Forecast")
    
    df_temp_loop$DateTime<-as.character(df_temp_loop$DateTime)
    df_temp_loop$DateTime<-substr(df_temp_loop$DateTime,1,13)
    
        # select all rows where Production Type is "Solar" -> create new df "df_temp_solar"
        df_temp_solar<-df_temp_loop[df_temp_loop$`Production Type`=="Solar",]
        df_temp_solar<-df_temp_solar[,c(1,3)]
        
        if(nrow(df_temp_solar)>0) {
          df_temp_solar<-aggregate(.~DateTime, df_temp_solar, mean) }
        
        # select all rows where Production Type is "Wind Onshore" -> create new df "df_temp_onshore"
        df_temp_onshore<-df_temp_loop[df_temp_loop$`Production Type`=="Wind Onshore",]
        df_temp_onshore<-df_temp_onshore[,c(1,3)]
        
        if(nrow(df_temp_onshore)>0) {
          df_temp_onshore<-aggregate(.~DateTime, df_temp_onshore, mean) }
        
        # select all rows where Production Type is "Wind offshore" -> create new df "df_temp_offshore"
        df_temp_offshore<-df_temp_loop[df_temp_loop$`Production Type`=="Wind Offshore",]
        df_temp_offshore<-df_temp_offshore[,c(1,3)]
        if(nrow(df_temp_offshore)>0) {
          df_temp_offshore<-aggregate(.~DateTime, df_temp_offshore, mean) }
        
        # merge df_datetime and df_temp_solar
        df_comb<-merge(df_datetime,df_temp_solar, by="DateTime", all.x = T)
        colnames(df_comb)<-c("DateTime","Generation Solar")
        
        # merge df_comb and df_temp_onshore
        df_comb<-merge(df_comb,df_temp_onshore, by="DateTime", all.x = T)
        colnames(df_comb)<-c("DateTime","Generation Solar","Generation Wind Onshore")
        
        # merge df_comb and df_temp_offshore -> df_comb with columns time, solar, onshore and offshore
        df_comb<-merge(df_comb,df_temp_offshore, by="DateTime", all.x = T)
        colnames(df_comb)<-c("DateTime","Generation Solar","Generation Wind Onshore","Generation Wind Offshore")
        
        colnames(df_comb)[c(2:4)]<-c(paste0("generation_solar_",l),paste0("generation_wind_onshore_",l),paste0("generation_wind_offshore_",l))
        
        df_euro<-merge(df_euro,df_comb, by="DateTime", all.x = T)
  }
  
      
  # for-loop to import data for the months February - December of the year y for every country existing in the euro_vector
  
  for(i in c(2:12)){
    string_temp<-paste0(as.character(y),"_",as.character(i),"_DayAheadGenerationForecastWindSolar.csv")
    df_temp<-read.table(string_temp, header=T, fileEncoding="UTF-16", sep = "\t")
    
    # starting the for-loop with the first element of the euro_vector "DE_50HzT"
    # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
    
    l<-euro_vector[1]
    df_temp_loop<-df_temp[df_temp[,9]==l,c(4,10,11)]
    colnames(df_temp_loop)<-c("DateTime","Production Type","Aggregated Generation Forecast")
    
    df_temp_loop$DateTime<-as.character(df_temp_loop$DateTime)
    df_temp_loop$DateTime<-substr(df_temp_loop$DateTime,1,13)
    
    # select all rows where Production Type is "Solar" -> create new df "df_temp_solar"
    df_temp_solar<-df_temp_loop[df_temp_loop$`Production Type`=="Solar",]
    df_temp_solar<-df_temp_solar[,c(1,3)]
    df_temp_solar<-aggregate(.~DateTime, df_temp_solar, mean)
    
    # select all rows where Production Type is "Wind Onshore" -> create new df "df_temp_onshore"
    df_temp_onshore<-df_temp_loop[df_temp_loop$`Production Type`=="Wind Onshore",]
    df_temp_onshore<-df_temp_onshore[,c(1,3)]
    df_temp_onshore<-aggregate(.~DateTime, df_temp_onshore, mean)
    
    # select all rows where Production Type is "Wind offshore" -> create new df "df_temp_offshore"
    df_temp_offshore<-df_temp_loop[df_temp_loop$`Production Type`=="Wind Offshore",]
    df_temp_offshore<-df_temp_offshore[,c(1,3)]
    df_temp_offshore<-aggregate(.~DateTime, df_temp_offshore, mean)
    
    
    df_datetime<-as.data.frame(cbind("DateTime"=df_temp_solar$DateTime))
    
    # merge df_datetime and df_temp_solar
    df_comb<-merge(df_datetime,df_temp_solar, by="DateTime", all.x = T)
    colnames(df_comb)<-c("DateTime","Generation Solar")
    
    # merge df_comb and df_temp_onshore
    df_comb<-merge(df_comb,df_temp_onshore, by="DateTime", all.x = T)
    colnames(df_comb)<-c("DateTime","Generation Solar","Generation Wind Onshore")
    
    # merge df_comb and df_temp_offshore -> df_comb with columns time, solar, onshore and offshore
    df_comb<-merge(df_comb,df_temp_offshore, by="DateTime", all.x = T)
    colnames(df_comb)<-c("DateTime","Generation Solar","Generation Wind Onshore","Generation Wind Offshore")

    # rename df_comb and columns
    df_euro_month<-df_comb
    colnames(df_euro_month)[c(2:4)]<-c(paste0("generation_solar_",l),paste0("generation_wind_onshore_",l),paste0("generation_wind_offshore_",l))
    
    
    # for-loop: merging the second element of the euro_vector to the last element of the euro_vector
    # reducing the column "DateTime" to "year-month-day hour" and calculate the average per hour to unify the DateTime-vector
    # merging all values of the euro_vector for January into a new dataframe "df_euro"
    
    for(c in c(2:length(euro_vector)))  {
      
      l<-euro_vector[c]
      df_temp_loop<-df_temp[df_temp[,9]==l,c(4,10,11)]
      colnames(df_temp_loop)<-c("DateTime","Production Type","Aggregated Generation Forecast")
      
      df_temp_loop$DateTime<-as.character(df_temp_loop$DateTime)
      df_temp_loop$DateTime<-substr(df_temp_loop$DateTime,1,13)
      
      # select all rows where Production Type is "Solar" -> create new df "df_temp_solar"
      df_temp_solar<-df_temp_loop[df_temp_loop$`Production Type`=="Solar",]
      df_temp_solar<-df_temp_solar[,c(1,3)]
      
      if(nrow(df_temp_solar)>0) {
        df_temp_solar<-aggregate(.~DateTime, df_temp_solar, mean) }
      
      # select all rows where Production Type is "Wind Onshore" -> create new df "df_temp_onshore"
      df_temp_onshore<-df_temp_loop[df_temp_loop$`Production Type`=="Wind Onshore",]
      df_temp_onshore<-df_temp_onshore[,c(1,3)]
      
      if(nrow(df_temp_onshore)>0) {
        df_temp_onshore<-aggregate(.~DateTime, df_temp_onshore, mean) }
      
      # select all rows where Production Type is "Wind offshore" -> create new df "df_temp_offshore"
      df_temp_offshore<-df_temp_loop[df_temp_loop$`Production Type`=="Wind Offshore",]
      df_temp_offshore<-df_temp_offshore[,c(1,3)]
      if(nrow(df_temp_offshore)>0) {
        df_temp_offshore<-aggregate(.~DateTime, df_temp_offshore, mean) }
      
      
      # merge df_datetime and df_temp_solar
      df_comb<-merge(df_datetime,df_temp_solar, by="DateTime", all.x = T)
      colnames(df_comb)<-c("DateTime","Generation Solar")
      
      # merge df_comb and df_temp_onshore
      df_comb<-merge(df_comb,df_temp_onshore, by="DateTime", all.x = T)
      colnames(df_comb)<-c("DateTime","Generation Solar","Generation Wind Onshore")
      
      # merge df_comb and df_temp_offshore -> df_comb with columns time, solar, onshore and offshore
      df_comb<-merge(df_comb,df_temp_offshore, by="DateTime", all.x = T)
      colnames(df_comb)<-c("DateTime","Generation Solar","Generation Wind Onshore","Generation Wind Offshore")
      
      # rename columns
      colnames(df_comb)[c(2:4)]<-c(paste0("generation_solar_",l),paste0("generation_wind_onshore_",l),paste0("generation_wind_offshore_",l))

      # merge df_euro_month and df_comb
      df_euro_month<-merge(df_euro_month,df_comb, by="DateTime", all.x = T)
      
    }
    
    # merge the rows of df_euro and df_euro_month -> df_euro
    df_euro<-rbind(df_euro,df_euro_month)
    df_euro<-df_euro[order(df_euro$DateTime),]
    
  }
  return(df_euro)
}
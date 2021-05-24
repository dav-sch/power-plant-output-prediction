function_out<-function(y) {
  
  setwd("./raw_data/Outages/")
  
  # import outages data for January of the year y
  # import only "GU" data, ignore "OG", "PU", "TG"
  
    string_temp_GU<-paste0(as.character(y),"_1_OutagesGU.csv")
    df_temp_GU<-read.table(string_temp_GU, header = T, fileEncoding = "UTF-16", sep = "\t", quote = "", fill=T)
    df_temp_GU<-df_temp_GU[,c(4,5,8,13,14,18)]
  
    # select all rows where Map Code is 'DE_50HzT','DE_Amprion','DE_TenneT_GER' or 'DE_TransnetBW' -> df_temp_GU
    # select all rows where Status is "Active"
    df_temp_GU<-df_temp_GU[df_temp_GU$MapCode %in% c('DE_50HzT','DE_Amprion','DE_TenneT_GER','DE_TransnetBW'),]  
    df_temp_GU<-df_temp_GU[df_temp_GU$Status=="Active",]
  
    # select all rows where the Unavailability Value is not 0
    df_temp_GU<-df_temp_GU[df_temp_GU$UnavailabilityValue!=0,]
    df_temp_GU<-df_temp_GU[,c(1,2,5,6)]  
  
    # reducing the column "df_temp_GU$StartTS" to "year-month-day hour"
    df_temp_GU$StartTS<-as.character(df_temp_GU$StartTS)
    df_temp_GU$StartTS<-substr(df_temp_GU$StartTS,1,13)  
    
    # reducing the column "df_temp_GU$EndTS" to "year-month-day hour"
    df_temp_GU$EndTS<-as.character(df_temp_GU$EndTS)
    df_temp_GU$EndTS<-substr(df_temp_GU$EndTS,1,13)  
    df_temp_GU<-df_temp_GU[order(df_temp_GU$StartTS),]

    
  # for-loop to import data for the months February - December of the year y
  # rename df_temp_GU to df_all
  df_all<-df_temp_GU
  
  for(i in c(2:12)) {
    
    # import only "GU" data, ignore "OG", "PU", "TG"
    string_temp_GU<-paste0(as.character(y),"_", as.character(i),"_OutagesGU.csv")
    df_temp_GU<-read.table(string_temp_GU, header = T, fileEncoding = "UTF-16", sep = "\t", quote = "", fill=T)
    df_temp_GU<-df_temp_GU[,c(4,5,8,13,14,18)]
    
    # select all rows where Map Code is 'DE_50HzT','DE_Amprion','DE_TenneT_GER' or 'DE_TransnetBW' -> df_temp_GU
    # select all rows where Status is "Active"
    df_temp_GU<-df_temp_GU[df_temp_GU$MapCode %in% c('DE_50HzT','DE_Amprion','DE_TenneT_GER','DE_TransnetBW'),]
    df_temp_GU<-df_temp_GU[df_temp_GU$Status=="Active",]
    
    # select all rows where the Unavailability Value is not 0
    df_temp_GU<-df_temp_GU[df_temp_GU$UnavailabilityValue!=0,]
    df_temp_GU<-df_temp_GU[,c(1,2,5,6)]  
    
    # reducing the column "df_temp_GU$StartTS" to "year-month-day hour"
    df_temp_GU$StartTS<-as.character(df_temp_GU$StartTS)
    df_temp_GU$StartTS<-substr(df_temp_GU$StartTS,1,13)  
    
    # reducing the column "df_temp_GU$EndTS" to "year-month-day hour"
    df_temp_GU$EndTS<-as.character(df_temp_GU$EndTS)
    df_temp_GU$EndTS<-substr(df_temp_GU$EndTS,1,13) 
    
    df_temp_GU<-df_temp_GU[order(df_temp_GU$StartTS),]
    
    # merge rows of df_all and df_temp_GU -> df_all
    df_all<-rbind(df_all, df_temp_GU)
    print(i)
  }
  df_all$PowerResourceEIC<-as.character(df_all$PowerResourceEIC)
  return(df_all)
}
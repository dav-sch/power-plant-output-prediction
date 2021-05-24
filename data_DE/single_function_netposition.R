function_netposition<-function(y) {
  
  # import netposition data for January of the year y
  string_temp<-paste0(as.character(y),"_1_NetPositionsDaily.csv")
  df_temp<-read.table(string_temp, header = T, fileEncoding = "UTF-16", sep = "\t")
  
  # select all rows where Map Code is "DE_AT_LU"
  df_all<-df_temp[df_temp$MapCode=="DE_AT_LU",]
  df_all<-df_all[,c(4,10)]
  
  # reducing the column "DateTime" to "year-month-day hour"
  df_all$DateTime<-substr(df_all$DateTime,1,13)
  
  
  # for-loop to import data for the months February - December of the year y
  for(i in c(2:12)) {
    string_temp<-paste0(as.character(y),"_", as.character(i),"_NetPositionsDaily.csv")
    df_temp<-read.table(string_temp, header = T, fileEncoding = "UTF-16", sep = "\t")
    
    # select all rows where Map Code is "DE_AT_LU"
    df_temp<-df_temp[df_temp$MapCode=="DE_AT_LU",]
    df_temp<-df_temp[,c(4,10)]
    
    # reducing the column "DateTime" to "year-month-day hour"
    df_temp$DateTime<-substr(df_temp$DateTime,1,13)
    
    # merge rows of df_temp and df_all
    df_all<-rbind(df_all, df_temp)
    print(i)
  }
  df_all<-df_all[order(df_all$DateTime),]
  colnames(df_all)[2]<-"Netposition"
  return(df_all)
}
library("httr")
library("jsonlite")

# raw_data aus data_porcessing_5.R laden!!!

test2 <- GET('https://ferien-api.de/api/v1/holidays')
json2 <- content(test2,as="parsed")

list_locat_school <- unlist(lapply(json2, `[[`, 4))
list_descr_school <- lapply(json2, `[[`, 5)
list_start_school <- substr(unlist(lapply(json2, `[[`, 1)),1,10)
list_end_school <- substr(unlist(lapply(json2, `[[`, 2)),1,10)

df_school <- as.data.frame(cbind("START"=list_start_school, "END"=list_end_school, "WHERE"=list_locat_school, "DESCR"=list_descr_school))
df_school <- df_school[df_school$DESCR!="NULL",]

df_school<-as.data.frame(lapply(df_school, unlist))
df_school<-df_school[order(df_school$START),]

df_school$help<-substr(df_school$START,1,4)

df_school17<- df_school[df_school$help=='2017',]
df_school17<-df_school17[,-5]

df_school17$START <- as.character(df_school17$START)
df_school17$END <- as.character(df_school17$END)

df_school17$END[substr(df_school17$END,1,4)=="2018"] <- "2017-12-31"
  

# Filtern nach Bundesland

list_bl<-unique(df_school17$WHERE)
df_bl<-as.data.frame(list_bl)

df_school_bl<-data.frame(matrix(ncol = 18, nrow = nrow(raw_data)))
colnames(df_school_bl)<-c("DateTime",as.character(list_bl),"sum")

df_school_bl$DateTime<-raw_data$DateTime
df_school_bl[is.na(df_school_bl)]<-0


for (i in c(1:nrow(df_school17))) {
  
  df_school_bl[which(substr(df_school_bl$DateTime,1,10)==df_school17$START[i])[1]:which(substr(df_school_bl$DateTime,1,10)==df_school17$END[i])[24],
               as.character(df_school17$WHERE[i])]<-1
  
}





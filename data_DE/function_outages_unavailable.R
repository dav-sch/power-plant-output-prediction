function_out_unavailable<-function(y,df_argument) {
    
    # df_ argument is function_generation(y) (coded in "master_function.R" row 22)
    # overwrite the generation values with 0
    df_dest<-df_argument
    df_dest[,-1]<-0
    
    source("single_function_outages.R")
    
    # create list_of_powerplants (list with every powerplant name)
    df_out<-function_out(y)
    list_of_powerplants<-colnames(df_dest)[-1]
    sum_unavailabilities<-0
    
    # for-loop: if the year of "row_temp$EndTS" is "2018" than replace with "2017-12-31 23", otherwise: Error
    
    # for-loop: if "row_temp$PowerResourceEIC" exists in "list_of_powerplants" then calculate unavailablility value
    # for-loop: overwrite the interval of unavailability with the calculated unavailability value
    
    for (j in c(1:nrow(df_out))) {
      row_temp<-df_out[j,]
      print(j)
      if(substr(row_temp$EndTS,1,4)=="2018"){
        row_temp$EndTS<-"2017-12-31 23"
      }
      
      if(row_temp$PowerResourceEIC %in% list_of_powerplants) {
      sum_unavailabilities<-sum_unavailabilities+(which(df_dest$DateTime==row_temp$EndTS)-which(df_dest$DateTime==row_temp$StartTS)+1)*row_temp$UnavailabilityValue
      df_dest[which(df_dest$DateTime==row_temp$StartTS):which(df_dest$DateTime==row_temp$EndTS),row_temp$PowerResourceEIC]<-row_temp$UnavailabilityValue
      }
      else {
        print(row_temp$PowerResourceEIC)
      }
    }
    # rename the columns "unavail-[name of powerplant]"
    colnames(df_dest)[-1]<-sprintf("unavail-%s", list_of_powerplants)
    return(df_dest)
}
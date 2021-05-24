source("function_load_european.R")
source("function_renewable_european.R")
source("single_function_generation.R")
source("single_function_netposition.R")
source("single_function_outages.R")
source("function_outages_unavailable.R")

function_master<-function(y)  {
     # import load forecast data
     setwd("./raw_data/Load Forecast/")
     part1<-function_load_european(y)
     
     # import renewable forecast data
     setwd("..")
     setwd("./Renewable Forecast/")
     part2<-function_renewable_european(y)
     master<-merge(part1, part2, by="DateTime")
     
     # import generation data
     setwd("..")
     setwd("./Generation/")
     part3<-function_generation(y)
     master<-merge(master, part3, by="DateTime")
     
     # import netposition data
     setwd("..")
     setwd("./Netposition/")
     part4<-function_netposition(y)
     master<-merge(master, part4, by="DateTime")
     
     # import outages data
     setwd("..")
     setwd("..")
     df_outage<-function_out_unavailable(y,part3)
     master<-merge(master, df_outage, by="DateTime")
     
     return(master)
}
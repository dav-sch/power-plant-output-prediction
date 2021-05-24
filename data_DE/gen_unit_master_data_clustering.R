source("x_data.R")
source("y_data.R")

function_powerplant_list<-function(y) {
  setwd("./raw_data/Generation")
  
  # import generation data for January of the year y
  # select all rows where Map Code is 'DE_50HzT','DE_Amprion','DE_TenneT_GER' or 'DE_TransnetBW'
  string_temp<-paste0(as.character(y),"_1_ActualGenerationOutputPerUnit.csv")
  df_temp<-read.table(string_temp, header = T, fileEncoding = "UTF-16", sep = "\t", quote = "")
  df_temp<-df_temp[,c(8,9,10,11,12,15)]
  df_temp<-df_temp[df_temp$MapCode %in% c('DE_50HzT','DE_Amprion','DE_TenneT_GER','DE_TransnetBW'),]
  df_temp<-df_temp[,c(3,1,2,4,5,6)]
  df_temp_distinct<-distinct(df_temp)
  
  # for-loop to import generation data for Feburary to December of the year y
  # select all rows where Map Code is 'DE_50HzT','DE_Amprion','DE_TenneT_GER' or 'DE_TransnetBW'
  df_powerplant_list_complete<-df_temp_distinct
  
  
  for(i in c(2:12)) {
    string_temp<-paste0(as.character(y),"_", as.character(i),"_ActualGenerationOutputPerUnit.csv")
    df_temp<-read.table(string_temp, header = T, fileEncoding = "UTF-16", sep = "\t", quote = "")
    df_temp<-df_temp[,c(8,9,10,11,12,15)]
    df_temp<-df_temp[df_temp$MapCode %in% c('DE_50HzT','DE_Amprion','DE_TenneT_GER','DE_TransnetBW'),]
    df_temp<-df_temp[,c(3,1,2,4,5,6)]
    df_temp_distinct<-distinct(df_temp)
    df_powerplant_list_complete<-rbind.fill(df_temp_distinct, df_powerplant_list_complete)
    print(i)
    df_powerplant_list_complete<-distinct(df_powerplant_list_complete)
}
  df_temp_distinct<-distinct(df_powerplant_list_complete)
  df_powerplant_list<-df_powerplant_list_complete
  df_powerplant_list<-df_powerplant_list[order(df_powerplant_list$GenerationUnitEIC),]
  df_powerplant_list<-distinct(df_powerplant_list, GenerationUnitEIC, .keep_all = TRUE)
  return(df_powerplant_list)
}

df_powerplant_list <- function_powerplant_list(2017)

getwd()

setwd("..")
setwd("..")
setwd("./process_data/")
saveRDS(df_powerplant_list, file="df_powerplant_list.rds")


# Part 2
# import df_master2017_v"..." as df_master2017 select all power plant names --> df_plants

df_master2017<-readRDS("df_master_final.rds")
df_plants<-df_master2017[,c(54:262)]

# interpolate df_plants (to fill non-existing values, otherwise: Error)
# Compute standard deviation
#df_powerplant_list$SD <- sapply(na_interpolation(df_plants, "linear"), sd)
# Replace all zeroes with NA, then compute sd while ignoring NAs
# equal to sd with only non-zeroes
df_plants_i <- na_interpolation(df_plants, "linear")
df_plants_i[df_plants_i==0] <- NA
df_powerplant_list$SD <- sapply(df_plants_i, sd, na.rm=T)

#ggplot(df_powerplant_list,aes(x=InstalledGenCapacity,y=SD,color=ProductionTypeName))+geom_point()
# create: No Infomation Rate (NIR)
# create binary dataframe --> if power plant is powered on --> 1
#                             if power plant is powered off --> 0
# compute average of df_plants_binary for every power plant --> if mean < 0.5 --> 1-mean 
df_plants_binary<-na_interpolation(df_plants, "linear")
df_plants_binary[df_plants_binary!=0]<-1
mean_binary<-colMeans(df_plants_binary)
mean_binary[mean_binary<0.5]<- 1-mean_binary[mean_binary<0.5]
df_powerplant_list$NIR <- mean_binary

#plot(df_powerplant_list$InstalledGenCapacity, df_powerplant_list$NIR)
#ggplot(df_powerplant_list,aes(x=InstalledGenCapacity,y=NIR,color=ProductionTypeName))+geom_point()

# devide standard deviation by capacity
df_powerplant_list$SD.Cap <- df_powerplant_list$SD/df_powerplant_list$InstalledGenCapacity

# standardization of capacity, NIR, sd/cap
df_num_stand<-df_powerplant_list[,c(6,8,9)]
df_num_stand<-normalize(df_num_stand, method = "standardize", margin=2)
colnames(df_num_stand) <- c("Cap.stand", "NIR.stand", "SD.Cap.stand")

df_num_stand_all<-as.data.frame(cbind(df_powerplant_list, df_num_stand))


#--------------------------
saveRDS(df_num_stand_all, file="gen_unit_master_data.rds")

# rename the ProductionTypeNames of df_num_stand_all
df_num_stand_all$ProductionTypeName <- as.character(df_num_stand_all$ProductionTypeName)
df_num_stand_all$ProductionTypeName[df_num_stand_all$ProductionTypeName %in% c("Fossil Gas", "Fossil Coal-derived gas")] <- "Gas"
df_num_stand_all$ProductionTypeName[df_num_stand_all$ProductionTypeName=="Fossil Brown coal/Lignite"] <- "Lignite"
df_num_stand_all$ProductionTypeName[df_num_stand_all$ProductionTypeName=="Fossil Hard coal"] <- "Hard Coal"
df_num_stand_all$ProductionTypeName[df_num_stand_all$ProductionTypeName=="Hydro Run-of-river and poundage"] <- "Run-of-river"
df_num_stand_all$ProductionTypeName[df_num_stand_all$ProductionTypeName=="Hydro Water Reservoir"] <- "Reservoir"
df_num_stand_all$ProductionTypeName[df_num_stand_all$ProductionTypeName=="Hydro Pumped Storage"] <- "PSP"
df_num_stand_all$ProductionTypeName[df_num_stand_all$ProductionTypeName=="Fossil Oil"] <- "Oil"

# need PROCESSED y_data_poly_lag for this. ("S:/LisaL/R-Daten/27.06.2019/data/y_data.R")
y_data_lag <- readRDS(file="y_data_poly_lag_final.rds")
df_num_stand_all <- df_num_stand_all[df_num_stand_all$GenerationUnitEIC %in% colnames(y_data_lag),]

# clustering by ProductionTypeName:
df_to_cluster <- df_num_stand_all[,c("ProductionTypeName", "Cap.stand", "NIR.stand", "SD.Cap.stand")]
rownames(df_to_cluster)<-df_num_stand_all$GenerationUnitEIC

# create type_vector for the unique production types
# create empty vectors: k_vector, medoid_vec, diss_vec
type_vector <- unique(df_to_cluster$ProductionTypeName)
#k_vector<-c(4,7,2,1,2,1,1,1,1,6)
k_vector <- as.numeric() 
medoid_vec <- as.character()
diss_vec <- as.numeric()


# for-loop for every production type existing in the type_vector where nrow > 2, otherwise: Error
for(i in c(1:length(type_vector)))  {
  t<-type_vector[i]
  df_temp<-df_to_cluster[df_to_cluster[,1]==t,c(2:4)]
  
  if(nrow(df_temp)>2) {
    
    # find ideal k
    ideal_k <- fviz_nbclust(df_temp, FUNcluster = cluster::pam, method = "silhouette", 
                            k.max = nrow(df_temp)-1, nboot = 100)
    k_temp <- ideal_k$data$clusters[which.max(ideal_k$data$y)]
    
    # use ideal k to cluster
    pam.res<-pam(df_temp, k_temp)
    medoid_temp<-rownames(pam.res$medoids)
    
    #names(medoid_temp)<-rep(t,length(medoid_temp))
    medoid_vec<-c(medoid_vec, medoid_temp)
    diss_temp<-pam.res$clusinfo[,3]
    
    #names(diss_temp)<-rep(t,length(diss_temp))
    #diss = dissimilarity
    diss_vec<-c(diss_vec, diss_temp)
    k_vector <- c(k_vector, k_temp)
  }
  
  # if nrow(df_temp)<=2: else (k=1)
  else {
    medoid_temp<-rownames(df_temp)[1]
    #names(medoid_temp)<-rep(t,length(medoid_temp))
    medoid_vec<-c(medoid_vec, medoid_temp)
    diss_temp<-0
    #names(diss_temp)<-rep(t,length(diss_temp))
    k_temp <- 1
    diss_vec<-c(diss_vec, diss_temp)
    k_vector <- c(k_vector, k_temp)
  }
  fullinfo_vector <- paste0(t," (",k_temp,"/",nrow(df_temp),")")
  df_num_stand_all$fullinfo[df_num_stand_all$ProductionTypeName==t] <- fullinfo_vector
}

# medoid column:
# overwrite medoid column with "F"
df_num_stand_all$medoids<-F

# overwrite medoid column with "T" if GenerationUnitEIC exists in medoid_vec
df_num_stand_all$medoids[df_num_stand_all$GenerationUnitEIC %in% medoid_vec]<-T

rownames(df_num_stand_all) <- c(1:nrow(df_num_stand_all))

saveRDS(df_num_stand_all, file = "gen_unit_master_data_cluster.rds")
#write.csv(df_powerplant_list, file = "df_powerplant_list.csv")
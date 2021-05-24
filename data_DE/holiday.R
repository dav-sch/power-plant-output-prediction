library("jsonlite")
feiertage<-fromJSON("https://feiertage-api.de/api/?jahr=2019")
feiertage_nat<-feiertage$NATIONAL

feiertage<-as.data.frame(feiertage_nat)



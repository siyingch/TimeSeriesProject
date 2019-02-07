rm(list=ls(all=T))
setwd("/home/bigdataadmin/RScripts/UseCase3/Demand_Forecasting_RScripts_Breakdown/US20")

library(date)

### Reading the Data from Sql or Flat file.

library('RODBC')

connectionString <- "Driver=ODBC Driver 13 for SQL Server;Server=tcp:10.100.0.12,51433;
Database=GEADataDiscoveryZone;Uid=GEADDZ;Pwd=bigdata;Encrypt=yes;TrustServerCertificate=no;
Connection Timeout=30;"
myconn <- odbcDriverConnect(connectionString)

data <- sqlQuery(myconn, "Select * from GEA_LPR_Transactions where substring(Plant_Name, 1, 4) = 'US20'")
close(myconn)
#data=read.csv("Data.csv",na.strings ="NULL" )

### Subset the data with "Plant_Name"
colnames(data)
#data=data[grep("4521",data$Plant_Name),]
colnames(data)[11:12] <- c('Calendar.month', 'Calendar.Year')

#data=data[grep("US20",data$Plant_Name),]

### Removing "NA" in Material_Name
data=data[!is.na(data$Material_Name),]

library(dplyr)
library(zoo)

###create a Year- Month column with "Calender.Year" & "Calender.month"
data$MONTH<-paste(as.character(data[,"Calendar.Year"]),as.character(data[,"Calendar.month"]),sep="-")
data$MONTH<-as.yearmon(data$MONTH)



###Replacing the negative values in the Qunatity with "0"
data$Quantity=ifelse(data$Quantity<0,0,data$Quantity)


###Aggregating the values with respect to "Material_Name,Plant_Name,Calendar.month,Calendar.Year,MONTH"
Datatmp=as.data.frame(data %>% group_by(Material_Name,Plant_Name,Calendar.month,Calendar.Year,MONTH) %>% summarise("qty"=sum(Quantity)))

rm(data)

sum(is.na(Datatmp$Material_Name))
sum(is.na(Datatmp$Plant_Name))

### Getting the Minimum date and Maximum date for each PMC(Plant Material Combination) in order to understand the type.

data123=as.data.frame(Datatmp %>% group_by(Material_Name,Plant_Name) %>% summarise("min"=min(MONTH),"max_data" = max(MONTH)))
sum(is.na(data123$Material_Name))
sum(is.na(data123$Plant_Name))

### Finding the differnce in months between Min and Max date
data123$diff1=((12*(data123$max_data-data123$min))+1)

### Recoding data to have max value as "Sep 2017"

data123$max=(as.character("2017-09"))
data123$max=as.yearmon(data123$max)

### Finding the differnce in months between Min and set up Max date date
data123$diff=((12*(data123$max-data123$min))+1)


### Create a new column  called "PMC_type" based on column "diff" created in above line.
data123$PMC_type=ifelse(data123$diff1>18,">18",ifelse(data123$diff1>=6,"6>=x>=18","<6"))
data123=data123[order(data123$diff1,decreasing = T),]
rownames(data123)=1:nrow(data123)

write.csv(data123,"data123.csv",row.names=FALSE)
write.csv(Datatmp,"Datatmp.csv",row.names=FALSE)


### Reading the Model_lookup file 

setwd("/home/bigdataadmin/RScripts/UseCase3/Demand_Forecasting_RScripts_Breakdown/US20")
library(zoo)
Model_Lookup=read.csv("Model_Lookup.csv")
data123=read.csv("data123.csv")

model_save_Path="/home/bigdataadmin/RScripts/UseCase3/Demand_Forecasting_RScripts_Breakdown/US20/Models/"

### Forecast function for forecasting next twelve months of a given PMC

Forecast_func<-function(ProductName,CHANNEL,Model_save_Path,model_name,noOfWeekstoForcast,data123){
  
  if(model_name!="Not-Applicable"){
    ##loading the model
    load(paste0(model_save_Path,gsub("[^A-Za-z0-9]", "", ProductName),"_",gsub("[^A-Za-z0-9]", "", CHANNEL),".RData"))
    
    ## Predicting for next 12 Months
    Test_pred <- data.frame(forecast(Model, h = noOfWeekstoForcast))
    
    ## Data Pre Processing to get the calender Month and Calender year for future forecasts
    y=data123[((data123$Material_Name==ProductName)&(data123$Plant_Name==CHANNEL)), ]
    
    t=as.data.frame(zooreg(1:13, as.yearmon(y$max[1]), freq = 12))
    t$MONTH=rownames(t)
    t=t[-1,]
    
    Test_pred$MONTH=t$MONTH
    Test_pred$Calendar.Year=format(as.yearmon(Test_pred$MONTH),"%Y")
    Test_pred$Calendar.month=format(as.yearmon(Test_pred$MONTH),"%m")
    
    Test_pred$Method<-model_name
    Test_pred$Material_Name=ProductName
    Test_pred$Plant_Name=CHANNEL
  } else{
    ## Predicting for next 12 Months
    Test_pred <- data.frame("Point.Forecast"=rep(0,12),"Lo.80"=rep(0,12),"Hi.80"=rep(0,12),"Lo.95"=rep(0,12),"Hi.95"=rep(0,12))
    
    ## Data Pre Processing to get the calender Month and Calender year for future forecasts
    y=data123[((data123$Material_Name==ProductName)&(data123$Plant_Name==CHANNEL)), ]
    
    t=as.data.frame(zooreg(1:13, as.yearmon(y$max[1]), freq = 12))
    t$MONTH=rownames(t)
    t=t[-1,]
    
    Test_pred$MONTH=t$MONTH
    Test_pred$Calendar.Year=format(as.yearmon(Test_pred$MONTH),"%Y")
    Test_pred$Calendar.month=format(as.yearmon(Test_pred$MONTH),"%m")
    
    Test_pred$Method<-model_name
    Test_pred$Material_Name=ProductName
    Test_pred$Plant_Name=CHANNEL
  }
  return(Test_pred)
}


### Crating a place holder for binding the forecasted values of all PMC

futureforecast=data.frame(Point.Forecast= numeric(0), Lo.80= numeric(0),Hi.80= numeric(0),Lo.95= numeric(0),Hi.95= numeric(0),MONTH= character(0),Calendar.Year= integer(0),Calendar.month= integer(0),Method = character(0),Material_Name= character(0), Plant_Name = character(0),Datype = character(0))


### Writing a forloop for doing this process for all PMC's

for(i in 1:nrow(Model_Lookup)){
  print(i)
  ProductName=Model_Lookup$Material_Name[i]
  CHANNEL=Model_Lookup$Plant_Name[i]
  model_name=as.character(Model_Lookup$Method[i])
  noOfWeekstoForcast=12
  Forecast_final=Forecast_func(ProductName,CHANNEL,Model_save_Path,model_name,noOfWeekstoForcast,data123)
  Forecast_final$Datype=as.character(Model_Lookup$Datype[i])
  futureforecast=rbind(futureforecast,Forecast_final)
}


write.csv(futureforecast,"Forecast_PMC.csv",row.names = F)

setwd("/home/bigdataadmin/RScripts/UseCase3/Demand_Forecasting_RScripts_Breakdown/US20")
start.time <- Sys.time()

### Reading Files required

data123=read.csv("data123.csv")
Datatmp=read.csv("Datatmp.csv")

library(smooth)
library(TTR)
library(zoo)

### "datafinal" is the Function written to find the missing values between Min date and Max date of PMC and impute it with "0" 

datafinal<-function(data,ProductName,CHANNEL,data123,k){
  
  ### Subsetting the Data on Plant and Material Name.
  data<-data[((data$Material_Name==ProductName)&(data$Plant_Name==CHANNEL)), ]
  data$MONTH=as.yearmon(data$MONTH)
  ### Subsetting the Data features (Datatmp) to create a sequence od dates from Min date and Max date.
  y=data123[k,]
  
  
  ### Logic to create sequence of the dates
  
  t=as.data.frame(zooreg(1:(y$diff[1]), as.yearmon(y$min[1]), freq = 12))
 
  t$MONTH=rownames(t)
  t$MONTH=as.yearmon(t$MONTH)
  t$Material_Name=ProductName
  t$Plant_Name=CHANNEL
  t$Calendar.month=as.integer(format(t$MONTH, "%m"))
  t$Calendar.Year=as.integer(format(t$MONTH, "%Y"))
  t=t[,setdiff(colnames(t),"zooreg(1:(y$diff[1]), y$min[1], freq = 12)")]
  ### Merge it with  data to populate missing values
  data=merge(t,data,all=T)
  data$qty=ifelse(is.na(data$qty),0,data$qty)
  
  
  rm(t,y)
  
  return(data)
}



### "mapefunc" is the Function written to set the upper cape of MAPE to "1" when actual values are zero, since acutal value zero results in INf mape and thus  being misleading 


mapefunc<-function(train,test){
  train=ifelse(train==0,0.01,train)
  mape=abs(train-test)/train
  mape=ifelse(mape>1,1,mape)
  finalmape=mean(mape)
  return(finalmape)
}



### "Model_Building" is the Function written to build various model and select the best model for each PMC model

Model_Building<-function(New_data,ProductName,CHANNEL,t,model_save_Path){
  
  ### Split the Data into train and test depending on the type of the Model
  ### For PMC of type ">18" : Test set is last 6 Points of the data available
  ### For PMC of type "6<=x<=18" : Train is 70% of the data points and Test is 30% of the data points.
  
  if(t==3){
    test_points=6
    start=1
    end=nrow(New_data)-6
    traindata<-New_data[start:end,]
    testdata<-New_data[((end+1):(end+test_points)),]
  }else{
    test_points=as.integer(0.3*nrow(New_data))
    start=1
    end=nrow(New_data)-test_points
    traindata<-New_data[start:end,]
    testdata<-New_data[((end+1):(end+test_points)),]
  }
  
  ### Preparing the data as time series object
  dataTimeSeries <- ts(traindata$qty,frequency = 12,start = c(as.numeric(traindata$Calendar.Year[1]),as.numeric(traindata$Calendar.month[1])))
  library(forecast)
  library(DMwR)
  
  
  ### Building various Time series models
  
  
  noOfWeekstoForcast <- nrow(testdata)
  
  ### "count" is intialized in order to find if all the models failed for particular PMc ( It is written just to have a check)
  count=0
  
  ### Create a Place Holders for the Outputs of the Function. 
  
  Train_Forecast <- data.frame(MONTH= character(0), Material_Name= character(0), Plant_Name = character(0),Calendar.month= integer(0),Calendar.Year= integer(0),qty= numeric(0),Point.Forecast= numeric(0),Lo.80= numeric(0),Hi.80= numeric(0),Lo.95= numeric(0),Hi.95= numeric(0),Method= character(0))
  Test_Forecast <- data.frame(MONTH= character(0), Material_Name= character(0), Plant_Name = character(0),Calendar.month= integer(0),Calendar.Year= integer(0),qty= numeric(0),Point.Forecast= numeric(0),Lo.80= numeric(0),Hi.80= numeric(0),Lo.95= numeric(0),Hi.95= numeric(0),Method= character(0))
  Error_Matrix<-data.frame(mae= numeric(0), mse= numeric(0),rmse= numeric(0),mape= numeric(0),msae= numeric(0),MAPE= numeric(0),Material_Name= character(0), Plant_Name = character(0),Dataset = character(0),Method = character(0))
  
  ### Building different Timeseries models starts here.
  
  ### Auto arima model with trycatch so that other models run incase of error with this model
  
  tryCatch({
    
    ### Model Building 
    Model = auto.arima(dataTimeSeries)
    
    ### Getting the fitted values 
    Train_pred=as.data.frame(as.vector(fitted(Model)))
    colnames(Train_pred)=c("Point.Forecast")
    
    ### Binding it with the global Train_Forecast data frame which we created in line 91 to 93
    
    Train_pred=cbind(traindata,Train_pred)
    Train_pred$"Lo.80"="NA"
    Train_pred$"Hi.80"="NA"
    Train_pred$"Lo.95"="NA"
    Train_pred$"Hi.95"="NA"
    Train_pred$Method<-"AutoArima"
    
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    Test_pred <- data.frame(forecast(Model, h = noOfWeekstoForcast))
    Test_pred=cbind(testdata,Test_pred)
    Test_pred$Method<-"AutoArima"
    Train_Forecast=rbind(Train_Forecast,Train_pred)
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    
    Train_Error=as.data.frame(t(regr.eval(Train_pred$qty,Train_pred$Point.Forecast)))
    Train_Error$msae=MASE(Train_pred$qty,Train_pred$Point.Forecast,mean(abs(Train_pred$qty)))
    Train_Error$MAPE=mapefunc(Train_pred$qty,Train_pred$Point.Forecast)
    Train_Error$Material_Name=ProductName
    Train_Error$Plant_Name=CHANNEL
    Train_Error$Dataset="TrainSet"
    Train_Error$Method<-"AutoArima"
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(Train_pred$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"AutoArima"
    Error_Matrix=rbind(Error_Matrix,Train_Error,Test_Error)
    rm(Train_pred,Model,Test_pred,Train_Error,Test_Error)},
    
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  ### TSLM model with trycatch so that other models run incase of error with this model
  
  tryCatch({
    ### Model Building 
    Model <- tslm(dataTimeSeries ~ trend + season)
    
    ### Getting the fitted values
    Train_pred=as.data.frame(as.vector(fitted(Model)))
    colnames(Train_pred)=c("Point.Forecast")
    
    
    ### Binding it with the global Train_Forecast data frame which we created in line 91 to 93
    Train_pred=cbind(traindata,Train_pred)
    Train_pred$"Lo.80"="NA"
    Train_pred$"Hi.80"="NA"
    Train_pred$"Lo.95"="NA"
    Train_pred$"Hi.95"="NA"
    Train_pred$Method<-"TSLM"
    
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    Test_pred <- data.frame(forecast(Model, h = noOfWeekstoForcast))
    Test_pred=cbind(testdata,Test_pred)
    Test_pred$Method<-"TSLM"
    Train_Forecast=rbind(Train_Forecast,Train_pred)
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Train_Error=as.data.frame(t(regr.eval(Train_pred$qty,Train_pred$Point.Forecast)))
    Train_Error$msae=MASE(Train_pred$qty,Train_pred$Point.Forecast,mean(abs(Train_pred$qty)))
    Train_Error$MAPE=mapefunc(Train_pred$qty,Train_pred$Point.Forecast)
    Train_Error$Material_Name=ProductName
    Train_Error$Plant_Name=CHANNEL
    Train_Error$Dataset="TrainSet"
    Train_Error$Method<-"TSLM"
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(Train_pred$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"TSLM"
    Error_Matrix=rbind(Error_Matrix,Train_Error,Test_Error)
    
    rm(Train_pred,Model,Test_pred,Train_Error,Test_Error)},
    error = function(e) {
      print(e)
      count=count+1})
  
  
  
  ####HOLT
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    Test_pred <- as.data.frame(holt(dataTimeSeries,h=6))
    colnames(Test_pred)=c("Point.Forecast","Lo.80","Hi.80" ,"Lo.95", "Hi.95")
    Test_pred=cbind(testdata,Test_pred)
    Test_pred$Method<-"HOLT"
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"HOLT"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    
    rm(Test_pred,Test_Error)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  ####SES
  tryCatch({
    
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    Test_pred <- as.data.frame(ses(dataTimeSeries,h=6))
    colnames(Test_pred)=c("Point.Forecast","Lo.80","Hi.80" ,"Lo.95", "Hi.95")
    Test_pred=cbind(testdata,Test_pred)
    Test_pred$Method<-"SES"
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"SES"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    
    rm(Test_pred,Test_Error)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  ####Movinga average 
  ###SMA3
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    tforecast=as.data.frame(lag(SMA(New_data$qty,n=3),k=1))
    tforecast$Method<-"sma3"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"SMA3"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  ###SMA6
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    tforecast=as.data.frame(lag(SMA(New_data$qty,n=6),k=1))
    tforecast$Method<-"sma6"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"SMA6"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  
  ###SMA9
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    
    tforecast=as.data.frame(lag(SMA(New_data$qty,n=9),k=1))
    tforecast$Method<-"sma9"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"SMA9"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  
  
  ###WMA3
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    
    tforecast=as.data.frame(lag(WMA(New_data$qty,n=3),k=1))
    tforecast$Method<-"wma3"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"WMA3"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  ###WMA6
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    tforecast=as.data.frame(lag(WMA(New_data$qty,n=6),k=1))
    tforecast$Method<-"WMA6"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"WMA6"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  
  ###WMA9
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    tforecast=as.data.frame(lag(WMA(New_data$qty,n=9),k=1))
    tforecast$Method<-"WMA9"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"WMA9"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  ###EMA3
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    tforecast=as.data.frame(lag(EMA(New_data$qty,n=3),k=1))
    tforecast$Method<-"EMA3"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"EMA3"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  ###EMA6
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    tforecast=as.data.frame(lag(EMA(New_data$qty,n=6),k=1))
    tforecast$Method<-"EMA6"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"EMA6"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  
  ###EMA9
  tryCatch({
    
    ### Holt, Ses and Moving averages models have no train forecasting and model building since they are averaging techniques
    ### Binding it with the global Test_Forecast data frame which we created in line 91 to 93
    
    tforecast=as.data.frame(lag(EMA(New_data$qty,n=9),k=1))
    tforecast$Method<-"EMA9"
    colnames(tforecast)=c("Point.Forecast","Method")
    
    
    Test_pred <- tforecast[((end+1):(end+test_points)),]
    
    Test_pred$"Lo.80"="NA"
    Test_pred$"Hi.80"="NA"
    Test_pred$"Lo.95"="NA"
    Test_pred$"Hi.95"="NA"
    
    Test_pred=cbind(testdata,Test_pred)
    
    Test_Forecast=rbind(Test_Forecast,Test_pred)
    
    ### Binding it with the global Error_Matrix data frame which we created in line 91 to 93
    Test_Error=as.data.frame(t(regr.eval(Test_pred$qty,Test_pred$Point.Forecast)))
    Test_Error$msae=MASE(Test_pred$qty,Test_pred$Point.Forecast,mean(abs(traindata$qty)))
    Test_Error$MAPE=mapefunc(Test_pred$qty,Test_pred$Point.Forecast)
    Test_Error$Material_Name=ProductName
    Test_Error$Plant_Name=CHANNEL
    Test_Error$Dataset="TestSet"
    Test_Error$Method<-"EMA9"
    Error_Matrix=rbind(Error_Matrix,Test_Error)
    rm(Test_pred,Test_Error,tforecast)},
    error = function(e) {
      print(e)
      count=count+1}
  )
  
  ### Code for selecting best model starts here 
  
  ### evalauting test rmse and picking up the final model 
  library(reshape)
  Result_RMSE=cast(Error_Matrix,Material_Name+Plant_Name+Dataset~Method,value="rmse")
  Result_RMSE=Result_RMSE[Result_RMSE$Dataset=="TestSet",]
  y=as.data.frame(t(colnames(sort(Result_RMSE[1,4:ncol(Result_RMSE)]))[1]))
  y$Material_Name=Result_RMSE$Material_Name[1]
  y$Plant_Name=Result_RMSE$Plant_Name[1]
  
  
  if(t==3){
    y$Datype=">18"
  }else if(t==2){
    y$Datype="6=>x=>18"
  }else{
    y$Datype="<=5"
  }
  
  model_name=as.character(y$V1[1])
  
  ### Storing Model Name for further reference
  Final_result1=data.frame(Material_Name= ProductName, Plant_Name = CHANNEL,Method=model_name)
  Final_result1$Datype=y$Datype[1]
  
  ### Building final model and saving it by using Model_Building_Final function
  Model_Building_Final(New_data,model_name,ProductName,CHANNEL,model_save_Path)
  
  
  return(list(Train_Forecast,Test_Forecast,Error_Matrix,count,Final_result1))
}


### "Model_Building_Final" is the Function written to get the best modelname and retrain it on entire data and save the model with Plant_Material name. 

Model_Building_Final<-function(New_data,model_name,ProductName,CHANNEL,model_save_Path){
  
  # Preparing the data as time series object
  dataTimeSeries <- ts(New_data$qty,frequency = 12,start = c(as.numeric(New_data$Calendar.Year[1]),as.numeric(New_data$Calendar.month[1])))
  library(forecast)
  library(DMwR)
  
  ### Written to save the error of "/"
  ProductName=gsub("[^A-Za-z0-9]", "", ProductName)
  CHANNEL=gsub("[^A-Za-z0-9]", "", CHANNEL)
  
  # Building model on entire data and saving it in the form of "RData"
  
  ###autoarima
  if(model_name=="AutoArima"){
    Model = auto.arima(dataTimeSeries)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="TSLM"){
    Model <- tslm(dataTimeSeries ~ trend + season)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="HOLT"){
    Model = holt(dataTimeSeries,12)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="SES"){
    Model = ses(dataTimeSeries,12)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
    
  }else if(model_name=="SMA3"){
    Model <- SMA(New_data$qty,n=3)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="SMA6"){
    Model <- SMA(New_data$qty,n=6)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="SMA9"){
    Model <- SMA(New_data$qty,n=9)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="WMA3"){
    Model <- WMA(New_data$qty,n=3)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="WMA6"){
    Model <- WMA(New_data$qty,n=6)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="WMA9"){
    Model <- WMA(New_data$qty,n=9)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="EMA3"){
    Model <- EMA(New_data$qty,n=3)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="EMA6"){
    Model <- EMA(New_data$qty,n=6)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
    
  }else if(model_name=="EMA9"){
    Model <- EMA(New_data$qty,n=9)
    save(Model,file=paste0(model_save_Path,ProductName,"_",CHANNEL,".RData"))
}
  
}


### Creating Place holders for the final Data files

### Error Handler file for further processing of PMC
temp=data.frame(Material_Name=character(0),Plant_Name=character(0))

### Final File which contains fitted values for trained points
Train_F <- data.frame(MONTH= character(0), Material_Name= character(0), Plant_Name = character(0),Calendar.month= integer(0),Calendar.Year= integer(0),qty= numeric(0),Point.Forecast= numeric(0),Lo.80= numeric(0),Hi.80= numeric(0),Lo.95= numeric(0),Hi.95= numeric(0),Method= character(0))

### Final File which contains fitted values for test points

Test_F <- data.frame(MONTH= character(0), Material_Name= character(0), Plant_Name = character(0),Calendar.month= integer(0),Calendar.Year= integer(0),qty= numeric(0),Point.Forecast= numeric(0),Lo.80= numeric(0),Hi.80= numeric(0),Lo.95= numeric(0),Hi.95= numeric(0),Method= character(0))

### Final File which contains Error metrics for test and train points
Error_M<-data.frame(mae= numeric(0), mse= numeric(0),rmse= numeric(0),mape= numeric(0),msae= numeric(0),MAPE= numeric(0),Material_Name= character(0), Plant_Name = character(0),Dataset = character(0),Method = character(0))

### Final File which contains best picked model againts each PMC 
futureforecast<-data.frame(Material_Name= character(0), Plant_Name = character(0),Method=character(0),Datype = character(0))

model_save_Path="/home/bigdataadmin/RScripts/UseCase3/Demand_Forecasting_RScripts_Breakdown/US20/Models/"

for(k in 1:nrow(data123)){
  pr<-tryCatch({
    print(k)
    ProductName<- as.character(data123$Material_Name[k])
    CHANNEL=as.character(data123$Plant_Name[k])
    differnce=data123$diff1[k]
    datapr<-datafinal(Datatmp,ProductName,CHANNEL,data123,k)
    row.names(datapr)<-seq(1,nrow(datapr))
    if(differnce>18){
      t=3
    }else if(differnce<6){t=1}else{t=2}
    
    ### Models run only for data points greater than 6 
    if(t!=1){
      Final_result=Model_Building(datapr,ProductName,CHANNEL,t,model_save_Path)
      Train=Final_result[[1]]
      Test=Final_result[[2]]
      error=Final_result[[3]]
      Final_Forecast=Final_result[[5]]
      count=Final_result[[4]]
    }else{
      Train=data.frame(character(0))
      Test=data.frame(character(0))
      error=data.frame(character(0))
      count=-1
      Final_Forecast=data.frame(Material_Name= ProductName, Plant_Name = CHANNEL,Method="Not-Applicable",Datype="<6")
    }
    
    
    ### Error handling checks 
    if(count==13 ){
      temp1=data.frame(Material_Name=ProductName,Plant_Name=CHANNEL)
      temp=rbind(temp,temp1)
    }else{
      if(nrow(Train)>0){
        Train_F=rbind(Train_F,Train)
        }
      
      if(nrow(Test)>0){
        Test_F=rbind(Test_F,Test)
        }
      
      if(nrow(error)>0){
        Error_M=rbind(Error_M,error)
        }
      if(nrow(Final_Forecast)>0){futureforecast=rbind(futureforecast,Final_Forecast)}
    }
    
    
    
    },
    
    error = function(e) {print(e)
      temp1=data.frame(Material_Name=ProductName,Plant_Nam=CHANNEL)
      temp=rbind(temp,temp1)}
  )
}






#rm(list= ls()[!(ls() %in% c('Train_F','Test_F','Error_M','futureforecast','temp','start.time'))])

write.csv(Train_F,"Train_Forecast.csv",row.names = F)
write.csv(Test_F,"Test_Forecast.csv",row.names = F)
write.csv(Error_M,"Error_Metrics.csv",row.names = F)
write.csv(futureforecast,"Model_Lookup.csv",row.names = F)
write.csv(temp,"Error_Handlers.csv",row.names = F)


end.time <- Sys.time()
end.time - start.time
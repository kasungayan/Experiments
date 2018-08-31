library(pracma)
library(gtools)
library(forecast)
library(smooth)

file <-read.csv(file="NN5_FINAL_DATASET_WITH_TEST_DATA_2.txt",sep=',',header = FALSE) 
nn5_dataset <-as.data.frame(t(file[,-1]))

sunday = vector()
monday = vector()
tuesday = vector()
wednesday = vector()
thursday= vector()
friday = vector()
saturday = vector()
wrong= vector()

#replacing missing values
for (idr in 1: nrow(nn5_dataset)) {
  oneLine_df=nn5_dataset[idr,]
  numericvalue<-as.numeric(oneLine_df)
  for(i in 1:length(numericvalue)){
    if(i%%7==0){
      sunday = append(sunday,numericvalue[i])
    }else if(i%%7==1){
      monday = append(monday,numericvalue[i])
    }else if(i%%7==2){
      tuesday = append(tuesday,numericvalue[i])
    }else if(i%%7==3){
      wednesday  = append(wednesday,numericvalue[i])
    }else if(i%%7==4){
      thursday= append(thursday,numericvalue[i])
    }else if(i%%7==5){
      friday= append(friday,numericvalue[i])
    }else if(i%%7==6){
      saturday= append(saturday,numericvalue[i]) 
    }else{
      wrong= append(wrong,numericvalue[i]) 
    }
  }
  print(idr)
} 

sunday_median<-median(sunday,na.rm = TRUE)
monday_median <-median(monday,na.rm = TRUE)
tuesday_median <- median(tuesday,na.rm = TRUE)
wednesday_median <-median(wednesday,na.rm = TRUE)
thursday_median<-median(thursday,na.rm = TRUE)
friday_median<-median(friday,na.rm = TRUE)
saturday_median<-median(saturday,na.rm = TRUE)

#replacing missing values
for (idr in 1: nrow(nn5_dataset)) {
  oneLine_df=nn5_dataset[idr,]
  numericvalue<-as.numeric(oneLine_df)
  for(i in 1:length(numericvalue)){
    if(is.na(oneLine_df[i])){
      if(i%%7==0){
        nn5_dataset[idr,i] =sunday_median
      }else if(i%%7==1){
        nn5_dataset[idr,i]= monday_median
      }else if(i%%7==2){
        nn5_dataset[idr,i]= tuesday_median
      }else if(i%%7==3){
        nn5_dataset[idr,i] =wednesday_median
      }else if(i%%7==4){
        nn5_dataset[idr,i] =thursday_median
      }else if(i%%7==5){
        nn5_dataset[idr,i]= friday_median
      }else if(i%%7==6){
        nn5_dataset[idr,i]= saturday_median
      }
    }
  }
}


cif_df=read.csv(file="NN5_results_1.txt",sep=',',header = FALSE)
results_56 = cif_df

predicion_horizon <- 57

sMAPE_mean_12 = NULL
sMAPE_median_12 = NULL
MASE_mean_12 = NULL
MASE_median_12 = NULL

for (idr in 1:nrow(nn5_dataset)) {
  
  final_rolling_prediction_vector = NULL
  final_rolling_actual_vector = NULL
  final_rolling_naive_vector = NULL
  
  insample_error = NULL
  
  print(paste0("Timeseries-->", idr ) )
  oneLine_training_df = nn5_dataset[idr, ]
  y_train = as.numeric(oneLine_training_df)
  y_train =  y_train[!is.na(y_train)]
  train_length = length(y_train)
  
  oneLine_test_df = results_56[idr, ]
  y_test = as.numeric(oneLine_test_df[1:(ncol(oneLine_test_df))])
  test_length = length(y_test)
  
  y_series = c(y_train,y_test)
  
  for(index in 1 : (predicion_horizon-1) ){
    if(index ==1){
      time_seres <- y_series[1:((train_length+(index-1)))]
      ets_model_12 = baggedETS(ts(time_seres, frequency = 7))
      ets_forecast = forecast(ets_model_12, h= 56)
      ets_predict = as.numeric(ets_forecast$mean)
      ets_predict = ets_predict[1:(predicion_horizon-index)]
      ets_naive = (naive(time_seres, h=(predicion_horizon-index)))
      ets_naive = as.numeric(ets_naive$mean)
      ets_actual = tail(y_series,(predicion_horizon-index))
      insample_error = mean(abs(diff(time_seres, lag=7,differences=1)))
      
    }else{
      time_seres <- y_series[1:((train_length+(index-1)))]
      #ets_model = ets(ts(time_seres, frequency = 12), model = ets_model_12, use.initial.values = TRUE)
      ets_model = baggedETS(ts(time_seres, frequency = 7))
      ets_forecast = forecast(ets_model, h= 56)
      ets_predict = as.numeric(ets_forecast$mean)
      ets_predict = ets_predict[1:(predicion_horizon-index)]
      ets_naive = (naive(time_seres, h=(predicion_horizon-index)))
      ets_naive = as.numeric(ets_naive$mean)
      ets_actual = tail(y_series,(predicion_horizon-index))
    }
    
    if(is.null(final_rolling_prediction_vector)){
      final_rolling_prediction_vector <- ets_predict
    }else{
      final_rolling_prediction_vector <- c(final_rolling_prediction_vector, ets_predict)
    }
    if(is.null(final_rolling_actual_vector)){
      final_rolling_actual_vector <- ets_actual
    }else{
      final_rolling_actual_vector <-c(final_rolling_actual_vector,ets_actual)
    }
    if(is.null(final_rolling_naive_vector)){
      final_rolling_naive_vector <- ets_naive
    }else{
      final_rolling_naive_vector <-c(final_rolling_naive_vector,ets_naive)
    }
  }
  
  sMAPE_error <- 2*abs(final_rolling_prediction_vector-final_rolling_actual_vector)/(abs(final_rolling_prediction_vector)+abs(final_rolling_actual_vector))
  sMAPE_mean_12[idr] <- mean(sMAPE_error)
  
  MASE_error <- MASE(final_rolling_actual_vector,final_rolling_prediction_vector,insample_error)
  MASE_mean_12[idr] <- (MASE_error)
  
}

file_write <- cbind(sMAPE_mean_12,MASE_mean_12)
write.table(file_write, file="sMAPE_baggedets.csv", row.names = F, col.names=c('sMAPE','MASE'), sep=",", quote=F)
# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}
subm_url = 'http://167.172.183.67'

u_name = "Group17"
p_word = "MuH3NvpH8z8G2Z4l"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

library("modeest")
library("forecast")
library("ggfortify")
library("ggplot2")
library("GGally")
#splitting the data by row
la_roche<-data[product_content_id==85004]
sleepy<-data[product_content_id==4066298]
xiaomi_kulaklik<-data[product_content_id==6676673]
fakir_supurge<-data[product_content_id==7061886]
trend_tayt<-data[product_content_id==31515569]
oralb<-data[product_content_id==32939029]
trend_bikini<-data[product_content_id==5926527]
koton_mont<-data[product_content_id==3904356]

#making the new dataframes ts objects
ts_la_roche<-ts(la_roche$sold_count)
ts_sleepy<-ts(sleepy$sold_count)
ts_xiaomi<-ts(xiaomi_kulaklik$sold_count)
ts_fakir<-ts(fakir_supurge$sold_count)
ts_trend_tayt<-ts(trend_tayt$sold_count)
ts_oralb<-ts(oralb$sold_count)
ts_trend_bikini<-ts(trend_bikini$sold_count)
ts_koton<-ts(koton_mont$sold_count)

##plotting ts objects
autoplot(ts_la_roche)
autoplot(ts_sleepy)
autoplot(ts_xiaomi)
autoplot(ts_fakir)
autoplot(ts_trend_tayt)
autoplot(ts_oralb)
autoplot(ts_trend_bikini)
autoplot(ts_koton)

####Fakir supurge
##Building test and train sets
ts_fakir_train<-window(ts_fakir,start=300,end=412)
ts_fakir_test<-window(ts_fakir,start=413,end=432)

##holt
fakir_holt<-holt(ts_fakir_train,20)
summary(fakir_holt)
autoplot(fakir_holt)
accuracy(fakir_holt,ts_fakir_test)
checkresiduals(fakir_holt)

##ses
fakir_ses<-ses(ts_fakir_train,20)
summary(fakir_ses)
autoplot(fakir_ses)+autolayer(fitted(fakir_ses))
accuracy(fakir_ses,ts_fakir_test)

##naive
fakir_naive<-naive(ts_fakir_train,20)
summary(fakir_naive)
accuracy(fakir_naive,ts_fakir_test)

##mean
fakir_mean<-meanf(ts_fakir_train,20)
summary(fakir_mean)
accuracy(fakir_mean,ts_fakir_test)

##ets
fakir_ets<-ets(ts_fakir_train)
checkresiduals(fakir_ets)
fakir_ets_fc<-forecast(fakir_ets,20)
autoplot(fakir_ets_fc)
summary(fakir_ets)
accuracy(fakir_ets_fc,ts_fakir_test)

##arima
fakir_arima<-auto.arima(ts_fakir_train)
checkresiduals(fakir_arima)
fakir_arima_fc<-forecast(fakir_arima,20)
autoplot(fakir_arima_fc)
accuracy(fakir_arima_fc,ts_fakir_test)

##tbats
fakir_tbats<-tbats(ts_fakir_train)
fakir_tbats_fc<-forecast(fakir_tbats,20)
autoplot(fakir_tbats_fc)
accuracy(fakir_tbats_fc,ts_fakir_test)

##linear regression
autoplot(ts_fakir)
fakir_supurge<-fakir_supurge[,-"product_content_id"]
train.fakir<-fakir_supurge[300:412,]
test.fakir<-fakir_supurge[413:432,]
test.fakir2<-fakir_supurge[413:414,]

fakir_linear<-lm(sold_count~.,data=train.fakir)
fakir.predict<-predict(fakir_linear,test.fakir)
accuracy(fakir.predict,test.fakir$sold_count)
plot(fakir.predict,type="l",xlab="Days",ylab="Fakir Vacuum")

##linear according to correlation
ggcorr(train.fakir,label=TRUE)
fakir_linear_corr<-lm(sold_count~basket_count+favored_count+visit_count+
                        category_visits,train.fakir)
fakir.predict.corr<-predict(fakir_linear_corr,test.fakir)
accuracy(fakir.predict.corr,test.fakir$sold_count)

####Xiaomi Kulaklik
##Building test and train sets
ts_xiaomi_train<-window(ts_xiaomi,start=300,end=412)
ts_xiaomi_test<-window(ts_xiaomi,start=413,end=432)

##holt
xiaomi_holt<-holt(ts_xiaomi_train,20)
summary(xiaomi_holt)
autoplot(xiaomi_holt)
accuracy(xiaomi_holt,ts_xiaomi_test)
checkresiduals(xiaomi_holt)

##ses
xiaomi_ses<-ses(ts_xiaomi_train,20)
summary(xiaomi_ses)
autoplot(xiaomi_ses)+autolayer(fitted(xiaomi_ses))
accuracy(xiaomi_ses,ts_xiaomi_test)

##naive
xiaomi_naive<-naive(ts_xiaomi_train,20)
summary(xiaomi_naive)
accuracy(xiaomi_naive,ts_xiaomi_test)

##mean
xiaomi_mean<-meanf(ts_xiaomi_train,20)
summary(xiaomi_mean)
accuracy(xiaomi_mean,ts_xiaomi_test)

##ets
xiaomi_ets<-ets(ts_xiaomi_train)
checkresiduals(xiaomi_ets)
xiaomi_ets_fc<-forecast(xiaomi_ets,20)
autoplot(xiaomi_ets_fc)
summary(xiaomi_ets)
accuracy(xiaomi_ets_fc,ts_xiaomi_test)

##arima
xiaomi_arima<-auto.arima(ts_xiaomi_train)
checkresiduals(xiaomi_arima)
xiaomi_arima_fc<-forecast(xiaomi_arima,20)
autoplot(xiaomi_arima_fc)
accuracy(xiaomi_arima_fc,ts_xiaomi_test)

##tbats

xiaomi_tbats<-tbats(ts_xiaomi_train)
xiaomi_tbats_fc<-forecast(xiaomi_tbats,20)
autoplot(xiaomi_tbats_fc)
accuracy(xiaomi_tbats_fc,ts_xiaomi_test)

##linear regression
autoplot(ts_xiaomi)
xiaomi_kulaklik<-xiaomi_kulaklik[,-"product_content_id"]
train.xiaomi<-xiaomi_kulaklik[300:412,]
test.xiaomi<-xiaomi_kulaklik[413:432,]
xiaomi_linear<-lm(sold_count~.,data=train.xiaomi)
xiaomi.predict<-predict(xiaomi_linear,test.xiaomi)
accuracy(xiaomi.predict,test.xiaomi$sold_count)
plot(xiaomi.predict,type="l",xlab="Days",ylab="Xiaomi Earbuds")

##linear according to correlation
ggcorr(train.xiaomi,label=TRUE)
xiaomi_linear_corr<-lm(sold_count~basket_count+favored_count+visit_count
                        ,train.xiaomi)
xiaomi.predict.corr<-predict(xiaomi_linear_corr,test.xiaomi)
accuracy(xiaomi.predict.corr,test.xiaomi$sold_count)

####Sleepy
##Building test and train sets
ts_sleepy_train<-window(ts_sleepy,start=300,end=412)
ts_sleepy_test<-window(ts_sleepy,start=413,end=432)

##holt
sleepy_holt<-holt(ts_sleepy_train,20)
summary(sleepy_holt)
autoplot(sleepy_holt)
accuracy(sleepy_holt,ts_sleepy_test)
checkresiduals(sleepy_holt)

##ses
sleepy_ses<-ses(ts_sleepy_train,20)
summary(sleepy_ses)
autoplot(sleepy_ses)+autolayer(fitted(sleepy_ses))
accuracy(sleepy_ses,ts_sleepy_test)

##naive
sleepy_naive<-naive(ts_sleepy_train,20)
summary(sleepy_naive)
accuracy(sleepy_naive,ts_sleepy_test)

##mean
sleepy_mean<-meanf(ts_sleepy_train,20)
summary(sleepy_mean)
accuracy(sleepy_mean,ts_sleepy_test)

##ets
sleepy_ets<-ets(ts_sleepy_train)
checkresiduals(sleepy_ets)
sleepy_ets_fc<-forecast(sleepy_ets,20)
autoplot(sleepy_ets_fc)
summary(sleepy_ets)
accuracy(sleepy_ets_fc,ts_sleepy_test)

##arima
sleepy_arima<-auto.arima(ts_sleepy_train)
checkresiduals(sleepy_arima)
sleepy_arima_fc<-forecast(sleepy_arima,20)
autoplot(sleepy_arima_fc)
accuracy(sleepy_arima_fc,ts_sleepy_test)

sleepy_arima_fc2<-forecast(sleepy_arima,2)
ts_sleepy_test2<-window(ts_sleepy_test,end=414)
accuracy(sleepy_arima_fc,ts_sleepy_test[1:2])
##tbats

sleepy_tbats<-tbats(ts_sleepy_train)
sleepy_tbats_fc<-forecast(sleepy_tbats,20)
autoplot(sleepy_tbats_fc)
accuracy(sleepy_tbats_fc,ts_sleepy_test)

##linear regression
autoplot(ts_sleepy)
sleepy<-sleepy[,-"product_content_id"]
train.sleepy<-sleepy[300:412,]
test.sleepy<-sleepy[413:432,]
sleepy_linear<-lm(sold_count~.,data=train.sleepy)
sleepy.predict<-predict(sleepy_linear,test.sleepy)
accuracy(sleepy.predict,test.sleepy$sold_count)
plot(sleepy.predict)

##linear according to correlation
ggcorr(train.sleepy,label=TRUE)
sleepy_linear_corr<-lm(sold_count~basket_count+favored_count+visit_count+
                        category_visits,train.sleepy)
sleepy.predict.corr<-predict(sleepy_linear_corr,test.sleepy)
accuracy(sleepy.predict.corr,test.sleepy$sold_count)
plot(sleepy.predict.corr,type="l",xlab="Days",ylab="Sleepy Wet Towel")


####La Roche
##Building test and train sets
ts_la_roche_train<-window(ts_la_roche,start=300,end=412)
ts_la_roche_test<-window(ts_la_roche,start=413,end=432)

##holt
ts_la_roche_train<-window(ts_la_roche,start=300,end=412)
ts_la_roche_test<-window(ts_la_roche,start=413,end=432)
la_roche_holt<-holt(ts_la_roche_train,20)
summary(la_roche_holt)
autoplot(la_roche_holt)
accuracy(la_roche_holt,ts_la_roche_test)
checkresiduals(la_roche_holt)

##ses
la_roche_ses<-ses(ts_la_roche_train,20)
summary(la_roche_ses)
autoplot(la_roche_ses)+autolayer(fitted(la_roche_ses))
accuracy(la_roche_ses,ts_la_roche_test)

##naive
la_roche_naive<-naive(ts_la_roche_train,20)
summary(la_roche_naive)
accuracy(la_roche_naive,ts_la_roche_test)

##mean
la_roche_mean<-meanf(ts_la_roche_train,20)
summary(la_roche_mean)
accuracy(la_roche_mean,ts_la_roche_test)

##ets
la_roche_ets<-ets(ts_la_roche_train)
checkresiduals(la_roche_ets)
la_roche_ets_fc<-forecast(la_roche_ets,20)
autoplot(la_roche_ets_fc)
summary(la_roche_ets)
accuracy(la_roche_ets_fc,ts_la_roche_test)

##arima
la_roche_arima<-auto.arima(ts_la_roche_train)
checkresiduals(la_roche_arima)
la_roche_arima_fc<-forecast(la_roche_arima,20)
autoplot(la_roche_arima_fc)
accuracy(la_roche_arima_fc,ts_la_roche_test)

##tbats

la_roche_tbats<-tbats(ts_la_roche_train)
la_roche_tbats_fc<-forecast(la_roche_tbats,20)
autoplot(la_roche_tbats_fc)
accuracy(la_roche_tbats_fc,ts_la_roche_test)

##linear regression
autoplot(ts_la_roche)
la_roche<-la_roche[,-"product_content_id"]
train.la_roche<-la_roche[300:412,]
test.la_roche<-la_roche[413:432,]
la_roche_linear<-lm(sold_count~.,data=train.la_roche)
la_roche.predict<-predict(la_roche_linear,test.la_roche)
accuracy(la_roche.predict,test.la_roche$sold_count)
plot(la_roche.predict)

##linear according to correlation
ggcorr(train.la_roche,label=TRUE)
la_roche_linear_corr<-lm(sold_count~basket_count+favored_count+visit_count+
                         category_visits+ty_visits,train.la_roche)
la_roche.predict.corr<-predict(la_roche_linear_corr,test.la_roche)
accuracy(la_roche.predict.corr,test.la_roche$sold_count)
plot(la_roche.predict.corr,type="l",xlab="Days",ylab="La Roche Cleanser")

####Trendyol Tayt
##Building test and train sets
ts_trend_tayt_train<-window(ts_trend_tayt,start=300,end=412)
ts_trend_tayt_test<-window(ts_trend_tayt,start=413,end=432)

##holt
trend_tayt_holt<-holt(ts_trend_tayt_train,20)
summary(trend_tayt_holt)
autoplot(trend_tayt_holt)
accuracy(trend_tayt_holt,ts_trend_tayt_test)
checkresiduals(trend_tayt_holt)

##ses
trend_tayt_ses<-ses(ts_trend_tayt_train,20)
summary(trend_tayt_ses)
autoplot(trend_tayt_ses)+autolayer(fitted(trend_tayt_ses))
accuracy(trend_tayt_ses,ts_trend_tayt_test)

##naive
trend_tayt_naive<-naive(ts_trend_tayt_train,20)
summary(trend_tayt_naive)
accuracy(trend_tayt_naive,ts_trend_tayt_test)

##mean
trend_tayt_mean<-meanf(ts_trend_tayt_train,20)
summary(trend_tayt_mean)
accuracy(trend_tayt_mean,ts_trend_tayt_test)

##ets
trend_tayt_ets<-ets(ts_trend_tayt_train)
checkresiduals(trend_tayt_ets)
trend_tayt_ets_fc<-forecast(trend_tayt_ets,20)
autoplot(trend_tayt_ets_fc)
summary(trend_tayt_ets)
accuracy(trend_tayt_ets_fc,ts_trend_tayt_test)

##arima
trend_tayt_arima<-auto.arima(ts_trend_tayt_train)
checkresiduals(trend_tayt_arima)
trend_tayt_arima_fc<-forecast(trend_tayt_arima,20)
autoplot(trend_tayt_arima_fc)
accuracy(trend_tayt_arima_fc,ts_trend_tayt_test)

##tbats

trend_tayt_tbats<-tbats(ts_trend_tayt_train)
trend_tayt_tbats_fc<-forecast(trend_tayt_tbats,20)
autoplot(trend_tayt_tbats_fc)
accuracy(trend_tayt_tbats_fc,ts_trend_tayt_test)

##linear regression
autoplot(ts_trend_tayt)
trend_tayt<-trend_tayt[,-"product_content_id"]
train.trend_tayt<-trend_tayt[300:412,]
test.trend_tayt<-trend_tayt[413:432,]
trend_tayt_linear<-lm(sold_count~.,data=train.trend_tayt)
trend_tayt.predict<-predict(trend_tayt_linear,test.trend_tayt)
accuracy(trend_tayt.predict,test.trend_tayt$sold_count)
plot(trend_tayt.predict)

##linear according to correlation
ggcorr(train.trend_tayt,label=TRUE)
trend_tayt_linear_corr<-lm(sold_count~basket_count+favored_count+visit_count
                           ,train.trend_tayt)
trend_tayt.predict.corr<-predict(trend_tayt_linear_corr,test.trend_tayt)
accuracy(trend_tayt.predict.corr,test.trend_tayt$sold_count)
plot(trend_tayt.predict.corr)
plot(trend_tayt.predict.corr,type="l",xlab="Days",ylab="Trendyol Tights")

####Oralb
##Building test and train sets
ts_oralb_train<-window(ts_oralb,start=280,end=378)
ts_oralb_test<-window(ts_oralb,start=379,end=398)

##holt
oralb_holt<-holt(ts_oralb_train,20)
summary(oralb_holt)
autoplot(oralb_holt)
accuracy(oralb_holt,ts_oralb_test)
checkresiduals(oralb_holt)

##ses
oralb_ses<-ses(ts_oralb_train,20)
summary(oralb_ses)
autoplot(oralb_ses)+autolayer(fitted(oralb_ses))
accuracy(oralb_ses,ts_oralb_test)

##naive
oralb_naive<-naive(ts_oralb_train,20)
summary(oralb_naive)
accuracy(oralb_naive,ts_oralb_test)

##mean
oralb_mean<-meanf(ts_oralb_train,20)
summary(oralb_mean)
accuracy(oralb_mean,ts_oralb_test)

##ets
oralb_ets<-ets(ts_oralb_train)
checkresiduals(oralb_ets)
oralb_ets_fc<-forecast(oralb_ets,20)
autoplot(oralb_ets_fc)
summary(oralb_ets)
accuracy(oralb_ets_fc,ts_oralb_test)

##arima
oralb_arima<-auto.arima(ts_oralb_train)
checkresiduals(oralb_arima)
oralb_arima_fc<-forecast(oralb_arima,20)
autoplot(oralb_arima_fc)
accuracy(oralb_arima_fc,ts_oralb_test)

##tbats
oralb_tbats<-tbats(ts_oralb_train)
oralb_tbats_fc<-forecast(oralb_tbats,20)
autoplot(oralb_tbats_fc)
accuracy(oralb_tbats_fc,ts_oralb_test)

##linear regression
autoplot(ts_oralb)
oralb<-oralb[,-"product_content_id"]
train.oralb<-oralb[280:378,]
test.oralb<-oralb[379:388,]
oralb_linear<-lm(sold_count~.,data=train.oralb)
oralb.predict<-predict(oralb_linear,test.oralb)
accuracy(oralb.predict,test.oralb$sold_count)
plot(oralb.predict)

##linear according to correlation
ggcorr(train.oralb,label=TRUE)
oralb_linear_corr<-lm(sold_count~basket_count+favored_count+visit_count+
                           category_visits,train.oralb)
oralb.predict.corr<-predict(oralb_linear_corr,test.oralb)
accuracy(oralb.predict.corr,test.oralb$sold_count)
plot(oralb.predict.corr,type="l",xlab="Days",ylab="Oral-B Toothbrush")

####Koton Mont
##Building test and train sets
ts_koton_train<-window(ts_koton,start=300,end=395)
ts_koton_test<-window(ts_koton,start=396,end=415)

##naive
koton_naive<-naive(ts_koton_train,20)
summary(koton_naive)
accuracy(koton_naive,ts_koton_test)

####Trendyol bikini
##Building test and train sets
ts_trend_bikini_train<-window(ts_trend_bikini,start=300,end=395)
ts_trend_bikini_test<-window(ts_trend_bikini,start=396,end=415)

##holt
trend_bikini_holt<-holt(ts_trend_bikini_train,20)
accuracy(trend_bikini_holt,ts_trend_bikini_test)

##mean
trend_bikini_mean<-meanf(ts_trend_bikini_train,20)
summary(trend_bikini_mean)
accuracy(trend_bikini_mean,ts_trend_bikini_test)

##seasonal naive
trend_bikini_snaive<-snaive(ts_trend_bikini_train,20)
summary(trend_bikini_snaive)
accuracy(trend_bikini_snaive,ts_trend_bikini_test)
autoplot(trend_bikini_snaive)

##linear regression
trend_bikini<-trend_bikini[,-"product_content_id"]
train.trend_bikini<-trend_bikini[300:395,]
test.trend_bikini<-trend_bikini[396:415,]
trend_bikini_linear<-lm(sold_count~.,data=train.trend_bikini)
trend_bikini.predict<-predict(trend_bikini_linear,test.trend_bikini)
accuracy(trend_bikini.predict,test.trend_bikini$sold_count)
plot(trend_bikini.predict)

##linear according to correlation
ggcorr(train.trend_bikini,label=TRUE)
trend_bikini_linear_corr<-lm(sold_count~price,data=train.trend_bikini)
trend_bikini.predict.corr<-predict(trend_bikini_linear_corr,test.trend_bikini)
accuracy(trend_bikini.predict.corr,test.trend_bikini$sold_count)

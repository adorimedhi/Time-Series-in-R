#Library
library(vars)
library(ggplot2)
library(forecast)
library(Metrics)
#Dataset Preprocessing
data=read.csv('E:/Internships/UEMS Project/VAR/Occupancy.csv')
View(data)
data=data[,2:3]
data=as.ts(data)
ts.plot(data)
#Splitting the data
N = nrow(data)
n = round(N *0.999, digits = 0)
train = data[1:n, ]
test  = data[(n+1):N,  ]
test=data.frame(test)
ntest=nrow(test)
#Information criteria and FPE for different VAR(p)
VARselect(train,type = "const")[["selection"]]
#Model
var1= VAR(train,p=1,type='const')
serial.test(var1, lags.pt=10, type="PT.asymptotic")
yhat = var1 %>% predict(test)
yhat
plot(yhat)
Yhatval = yhat$endog
Yhatval=data.frame(Yhatval)
#Model accuracy
rmse(test$Temperature,Yhatval$Temperature)
rmse(test$Humidity,Yhatval$Humidity)

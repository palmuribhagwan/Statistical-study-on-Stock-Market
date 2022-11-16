library(readxl)
library(Quandl)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(tseries)
library(forecast)

library(lubridate)
library(openxlsx)

 NSE<- read_excel("COMPLETE/EXCEL DATA/20 yr NSE BSE.xlsx", 
                  sheet = "NSE")


a<-NSE$Close

 b<-ts(NSE$Close,start = c(2001,4),end=c(2021,3),frequency =249.05)
 b<-b[1:4974]
 plot.ts(b)
 boxplot(b~cycle(b))

p <- ggplot(NSE, aes(x=Date, y=Close)) +
  geom_line(color="blue", size=0.8) +
  theme_minimal() +
  xlab("") +
  ylab("NSE") 

p##Increasing trend observed



####To check stationarity
adf.test(a, alternative="stationary", k=0)


#IF p-value is less than 0.05 so this implies that ts is stationary

b1<-acf(b, plot = T, main = "ACF Plot of NSE")
b2<-pacf(b, plot = T, main = "ACF Plot of NSE")
# ACF loooking good,As lag increases-fast decrease in ACF value- all in Confidence Interval


#We apply auto arima to the dataset 
modelfit <- auto.arima(b, lambda = "auto");modelfit
plot(modelfit$residuals)


Box.test(modelfit$residuals,  type="Ljung-Box")

f<-forecast(modelfit, h=365)
f= data.frame(f)
q <- plot(f)
head(f$lower)
head(f$upper)
head(f$mean)



###Below work is optional######
#Dividing the data into train and test, applying the model
N = length(a)
n = 0.7*N
train =a[1:n]
test  = a[(n+1):N]
trainarimafit <- auto.arima(train, lambda = "auto")
predlen=length(test)
trainarimafit <- forecast(trainarimafit, h=predlen)
#Plotting mean predicted values vs real data
meanvalues <- as.vector(trainarimafit$mean)
precios <- as.vector(test)
plot(meanvalues, type= "l", col= "red")
lines(precios, type = "l")


####NOW Exporting data

a=write.xlsx(f,file="bhu1.xlsx");a
  
  
  
  


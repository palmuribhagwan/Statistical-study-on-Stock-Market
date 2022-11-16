library(readxl)
library(Quandl)
library(Quandl)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(tseries)
library(forecast)
library(lubridate)



BSE <-read_excel("D:/project/Data P.xlsx", 
                  sheet = "FMCG")


a<-BSE$Close

#b<-ts(FMCG$Close,start = c(2011,4),end=c(2021,3),frequency =249)
# b<-b[1:2478]
# plot.ts(b)
# boxplot(b~cycle(b))

b<-ts(BSE$Close,start = c(201,4),end=c(2021,3),frequency =249)
b<-b[1:2477]
 plot.ts(b)
 boxplot(b~cycle(b))

p <- ggplot(FMCG, aes(x=Date, y=Close)) +
  geom_line(color="blue", size=0.8) +
  theme_minimal() +
  xlab("") +
  ylab("FMCG") 
p##Increasing trend observed


####To check stationarity
adf.test(a, alternative="stationary", k=0)
#IF p-value is less than 0.05 so this implies that ts is stationary

b1<-acf(b, plot = T, main = "ACF Plot of FMCG")
b2<-pacf(b, plot = T, main = "ACF Plot of FMCG")
# ACF loooking good,As lag increases-fast decrease in ACF value- all in Confidence Interval


#We apply auto arima to the dataset 
modelfit <- auto.arima(b, lambda = "auto");modelfit
plot(modelfit$residuals)


Box.test(modelfit$residuals,  type="Ljung-Box")

f<-forecast(modelfit, h="365");f
options("max.print")
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


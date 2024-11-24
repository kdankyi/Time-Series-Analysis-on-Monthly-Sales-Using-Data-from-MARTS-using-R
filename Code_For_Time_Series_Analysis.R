library(magrittr)
library(ggplot2)
library(tseries)
library(dplyr)



#loading data
df <- read.csv("/Users/jay/Desktop/Data Mining Assesment 2/series.csv",header = TRUE)
head(df,5)
tail(df,5)


#keeping only the sales data
df[,'over_res'] <- df$over_res/1000 #converting to billion
(df <- df[,'over_res'])


#from 1992 to 2019 - 326 observations
length(df)

plot(df,type = 'l',lwd=1.5)


#The starting year of the data is from Jan 1992 and ends at Feb 2019, 326 months.
data <- df %>% ts(start = 1992,frequency = 12)
TIME = seq(from=1992, by=1/frequency(data), length.out=length(data))



ggplot()+
  geom_line(aes(x=TIME,y=data),linewidth=0.8,col='black')+
  xlab("Time")+
  ylab("Billions of $US")+
  labs(title = "Retail Trade and Food Services US Total")+
  theme_classic(base_family = "serif",base_size = 10)



#creating a time series to plot data
linear.fit.us <- lm(data~.,data.frame(TIME = poly(TIME,degree = 1,raw = TRUE)))
summary(linear.fit.us)

ggplot()+
  geom_line(aes(x=TIME,y=data),size=0.6,col='tomato1')+
  ylab('Billions of $US')+
  labs(title = "Retail Trade and Food Services US Total")+
  geom_line(aes(x=TIME,y=fitted(linear.fit.us)),lwd=0.7,col='green')+
  geom_abline(slope=0,intercept = mean(data),col='black')


#quadratic
quadratic.fit.us <- lm(data~.,data.frame(TIME = poly(TIME,degree = 2,raw = TRUE)))
summary(quadratic.fit.us)
ggplot()+
  geom_line(aes(x=TIME,y=data),size=0.6,col='black')+
  ylab('Billions of $US')+
  labs(title = "Retail Trade and Food Services US Total")+
  geom_line(aes(x=TIME,y=fitted(quadratic.fit.us)),lwd=0.7,col='blue')


#BEST MODEL
#cubic
cubic.fit.us <- lm(data~.,data.frame(TIME = poly(TIME,degree = 3,raw = TRUE)))
summary(cubic.fit.us)
ggplot()+
  geom_line(aes(x=TIME,y=data,color='Trend'),size=0.7)+
  geom_line(aes(x=TIME,y=fitted(cubic.fit.us),color='Fitted trend'),lwd=0.8)+
  xlab("Time")+
  ylab("Billions of $US")+
  labs(title = "Retail Trade and Food Services US Total")+
  labs(color='')+
  scale_color_manual(values = c('Trend'='black','Fitted trend'='blue'))+
  theme_classic(base_family = "serif",base_size = 10)


AIC(linear.fit.us)
AIC(quadratic.fit.us) 
AIC(cubic.fit.us) #this is chosen because it has a lower AIC


#Removing the trend
data.no.trend.us <- data-fitted(cubic.fit.us)
ggplot()+
  geom_line(aes(x=tail(TIME,120),y=tail(data.no.trend.us,120)),lwd=0.8,col='black')+
  xlab("Time")+
  ylab("Billions of $US")+
  labs(title = "Seasonality of Retail Trade and Food Services US Total")+
  theme_classic(base_family = "serif",base_size = 10)


SIN <- COS <- matrix(nrow = length(TIME), ncol = 6)

for(i in 1:6) {
  SIN[ ,i] <- sin(2 * pi * i * TIME)
  COS[ ,i] <- cos(2 * pi * i * TIME)
}

seas.1 <- lm(data.no.trend.us ~.-1, data.frame(SIN = SIN[,1:1], COS = COS[, 1:1]))
seas.2 <- lm(data.no.trend.us ~.-1, data.frame(SIN = SIN[,1:2], COS = COS[, 1:2]))
seas.3 <- lm(data.no.trend.us ~.-1, data.frame(SIN = SIN[,1:3], COS = COS[, 1:3]))
seas.4 <- lm(data.no.trend.us ~.-1, data.frame(SIN = SIN[,1:4], COS = COS[, 1:4]))
seas.5 <- lm(data.no.trend.us ~.-1, data.frame(SIN = SIN[,1:5], COS = COS[, 1:5]))
seas.6 <- lm(data.no.trend.us ~.-1, data.frame(SIN = SIN[,1:6], COS = COS[, 1:6]))


plot(tail(TIME, 180), tail(data.no.trend.us, 180), type = "l")
lines(tail(TIME, 180), tail(fitted(seas.1),180), col = "red", lwd = 0.5)

plot(tail(TIME, 180), tail(data.no.trend.us, 180), type = "l")
lines(tail(TIME, 180), tail(fitted(seas.2),180), col = "red", lwd = 0.5)

plot(tail(TIME, 180), tail(data.no.trend.us, 180), type = "l")
lines(tail(TIME, 180), tail(fitted(seas.3),180), col = "red", lwd = 0.5)

plot(tail(TIME, 180), tail(data.no.trend.us, 180), type = "l")
lines(tail(TIME, 180), tail(fitted(seas.4),180), col = "red", lwd = 0.5)

plot(tail(TIME, 180), tail(data.no.trend.us, 180), type = "l")
lines(tail(TIME, 180), tail(fitted(seas.5),180), col = "red", lwd = 0.5)

plot(tail(TIME, 180), tail(data.no.trend.us, 180), type = "l")
lines(tail(TIME, 180), tail(fitted(seas.6),180), col = "red", lwd = 0.5)

  
#BEST FITTED MODEL FOR SEASONALITY
plot(tail(TIME, 90), tail(data.no.trend.us, 90), type = "l",main='Fitted seasonality',
     xlab = 'time',ylab='Change in retail sales',lwd=3)
lines(tail(TIME, 90), tail(fitted(seas.6),90), col = "red", lwd = 2)


AIC(seas.1)
AIC(seas.2)
AIC(seas.3)
AIC(seas.4)
AIC(seas.5) 
AIC(seas.6) #The best model


#residuals stationary
residuals.us <- data.no.trend.us-fitted(seas.6)
  
#plots of acf before applying ARIMA
plot(acf(residuals.us),main = 'Plot of ACF of residuals')
plot(pacf(residuals.us), main='Plot of PACF of residuals')


## Order selection -- AIC
n <- length(residuals.us)

norder <- 7


p <- c(1:norder)-1
q <- c(1:norder)-1

aic <- matrix(0, norder, norder)

for(i in 1:norder){
  
  for(j in 1:norder){
    
    modij <- arima(residuals.us, order = c(p[i],0,q[j]), method='ML')
    
    aic[i,j] <- modij$aic-2*(p[i]+q[j]+1)+2*(p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
  }
}

aicv <- as.vector(aic)

plot(aicv, ylab="AIC values")

indexaic <- which(aic == min(aic), arr.ind = TRUE)
indexaic

porder <- indexaic[1] - 1
qorder <- indexaic[2] - 1
porder
qorder

#create a table
aic

# Final residuals model
residuals.model <- arima(residuals.us, order = c(porder, 0, qorder), method = "ML")


#Plot of residuals
plot(residuals.us,main='Plot of Residuals before ARIMA',ylab='Residuals')
plot(residuals(residuals.model),main='Plot of Residuals after ARIMA',ylab='Residuals')

#plots of acf after applying arima
plot(acf(residuals(residuals.model)),main='ACF after ARIMA(5,0,6)')
plot(pacf(residuals(residuals.model)),main='PACF after ARIMA(5,0,6)')


#Prediction of 
ahead <- 24

pred.res <- predict(residuals.model, n.ahead = ahead)$pred


TIME.NEW <- seq(from = 2019.0, by = 1/12, length = ahead)

SIN.NEW <- COS.NEW <- matrix(nrow = length(TIME.NEW), ncol = 6)

for(i in 1:6){
  SIN.NEW[,i] <- sin(2 * pi * i * TIME.NEW)
  COS.NEW[,i] <- cos(2 * pi * i * TIME.NEW)
}


trend.pred <- predict(cubic.fit.us, newdata = data.frame(TIME = poly(TIME.NEW,degree = 3,
                                                                     raw = TRUE)))

seasonality.pred <- predict(seas.6, newdata = data.frame(SIN = SIN.NEW[, 1:6],
                                                              COS = COS.NEW[, 1:6]))

ggplot()+
  geom_line(aes(x=tail(TIME,120),y=tail(data,120),color='Actual values'),size=0.8)+
  geom_line(aes(x=TIME.NEW,y=trend.pred+seasonality.pred+pred.res,color='Predictions'),
            size=0.8)+
  xlab('Time')+
  ylab("Billions of $US")+
  labs(title = 'Prediction of Retail Trade and Food Services US Total')+
  labs(color='')+
  scale_color_manual(values = c('Actual values'='black','Predictions'='red'))


#
existing <- 26
pred.existing.correct <- predict(residuals.model, n.ahead = existing)$pred


TIME.existing<- seq(from = 2017, by = 1/12, length = existing)

SIN.existing <- COS.existing <- matrix(nrow = length(TIME.existing), ncol = 6)

for(i in 1:6){
  SIN.existing[,i] <- sin(2 * pi * i * TIME.existing)
  COS.existing[,i] <- cos(2 * pi * i * TIME.existing)
}

trend.existing <- predict(cubic.fit.us, newdata = data.frame(TIME = poly(TIME.existing,
                                                                     degree = 3,
                                                                     raw = TRUE)))

seasonality.existing <- predict(seas.6, 
                                newdata = data.frame(SIN = SIN.existing[, 1:6],
                                                         COS = COS.existing[, 1:6]))
ggplot()+
  geom_line(aes(x=tail(TIME,130),y=tail(data,130),color='Observed'),size=0.8)+
  geom_line(aes(x=TIME.existing,y=trend.existing+seasonality.existing+pred.existing.correct,
                color='Predictions'),size=0.8)+
  xlab('Time')+
  labs(color='')+
  ylab("Billions of $US")+
  labs(title = 'Prediction of of Exisiting Retail Trade and Food Services US Total')+
  scale_color_manual(values = c('Observed'='black',
                                'Predictions'='red'))



#ask chat to compare the figures using RMSE, MSE, MAPE
(last.26.months <- tail(data,26))

(forecast.correct <- trend.existing+seasonality.existing+pred.existing.correct %>% 
    as.vector())











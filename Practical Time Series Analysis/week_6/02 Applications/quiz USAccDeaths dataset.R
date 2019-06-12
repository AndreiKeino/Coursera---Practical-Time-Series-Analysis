data <- USAccDeaths
plot(data)

plot(diff(USAccDeaths, 12))

par(mfrow=c(2,1))

# obtain acf and pacf below
acData <- diff(USAccDeaths)
acf(acData)
pacf(acData)

library(astsa)

sarima(USAccDeaths, 0,1,1,0,1,1,12)

library(astsa)

model<-sarima(USAccDeaths, 0,1,1,0,1,1,12)
model$ttable


a<-sarima.for(USAccDeaths, 3, 0,1,1,0,1,1,12)
a
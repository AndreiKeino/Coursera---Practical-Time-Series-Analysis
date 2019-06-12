# Plot time series 'BJsales'
plot.ts(BJsales)

plot(diff(BJsales))

plot(diff(diff(BJsales)))

pacf(diff(diff(BJsales)))

acf(diff(diff(BJsales)))

d=2
for(p in 1:4){
    for(q in 1:2){
        if(p+d+q<=6){
            model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
            pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
            sse<-sum(model$residuals^2)
            cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
    }
}

d=2
for(p in 1:4){
    for(q in 1:2){
        if(p+d+q<=8){
            model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
            pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
            sse<-sum(model$residuals^2)
            cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
    }
}

model<-arima(BJsales, order=c(0,2,1))

par(mfrow=c(2,2))

plot(model$residuals)
acf(model$residuals)
pacf(model$residuals)
qqnorm(model$residuals)

model


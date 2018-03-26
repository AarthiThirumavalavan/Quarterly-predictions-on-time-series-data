expense_data <-read.csv('C:/Users/thirumav/Desktop/New folder/expense_data.csv')
expense_data
total_expense_amount<- expense_data$E
total_expense_amount
total_expense_amount.ts<- ts(data= total_expense_amount, frequency= 4, start=c(2007,1))
total_expense_amount.ts
plot(x=total_expense_amount.ts,type='l', xlim=c(2007,2014), main= "Total expense amount time series", ylab="Total expense amount", xlab='Transaction quarter')
abline(reg = lm(total_expense_amount.ts ~ time(total_expense_amount.ts)))
plot(x=log(total_expense_amount.ts),type='l', xlim=c(2007,2014), main= "Log of Total expense amount time series", ylab="Total expense amount", xlab='Transaction quarter')
plot(x=diff(log(total_expense_amount.ts)),type='l', xlim=c(2007,2014), main= "Diff(Log) of Total expense amount time series", ylab="Total expense amount", xlab='Transaction quarter')
abline(reg = lm(diff(log(total_expense_amount.ts)) ~ time(diff(log(total_expense_amount.ts)))))
plot(x=diff(diff(log(total_expense_amount.ts))),type='l', xlim=c(2007,2014), main= "Diff(Diff(Log))) of Total expense amount time series", ylab="Total expense amount", xlab='Transaction quarter')
abline(reg = lm(diff(diff(log(total_expense_amount.ts))) ~ time(diff(diff(log(total_expense_amount.ts))))))
acf(diff(diff(log(total_expense_amount.ts))), main="ACF of diff(diff(log(Total expense amount)))")       
pacf(diff(diff(log(total_expense_amount.ts))), main="PACF of diff(diff(log(Total expense amount)))")       

d=2
DD=2
per=4
for(p in 1:2){
  for(q in 1:2){
    for(P in 1:2){
      for(Q in 1:2){
        if(p+d+q+P+DD+Q <= 10){
          model <- arima(log(total_expense_amount.ts), order=c((p-1),d,(q-1)), seasonal = list(order=c((P-1),DD,(Q-1)), period=per))
          pval <-Box.test(model$residuals, lag = log(length(total_expense_amount.ts)))
          sse <- sum(model$residuals ^ 2)
          cat((p-1),d,(q-1),(P-1),DD,(Q-1), "AIC=", model$aic, "SSE=", sse, "pvalue=", pval$p.value, '\n')
        }
      }
    }
  }
}
fit<- arima(log(total_expense_amount.ts), c(0,2,1), seasonal = list(order=c(0,2,1),period=4))
pred <- predict(fit, n.ahead=1*4)
ts.plot(total_expense_amount.ts,type='l', 2.718 ^ pred$pred, log="y", lty=c(1,3), main="Predicted total expense amounts for quarters of 2015", xlim=c(2007,2015), ylab="Total expense amount", xlab='Transaction quarter')
pred_2015<- 2.718 ^ pred$pred
pred_2015<-round(pred_2015, digits=2)
pred_2015
class(pred_2015)
pred_2015_q <- ts(data=pred_2015, frequency = 4, start=c(2015,1))
quarterly_totals<- timetk::tk_tbl(quarters_2015)
quarterly_totals
write.csv(quarterly_totals, file="Quarterly Predictions for 2015.csv")


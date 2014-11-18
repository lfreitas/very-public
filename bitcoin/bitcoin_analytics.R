library(RCurl)
library(rjson)
library(forecast)

latestblock <- fromJSON(getURL("http://blockchain.info/latestblock"))
mktcap <- fromJSON(getURL("http://blockchain.info/q/marketcap"))

# fetch latest BTC historic price data
dat <- unlist(fromJSON(getURL("http://blockchain.info/charts/market-price?format=json")))
date <- dat[seq(1,length(dat),2)]
date <- as.Date(as.POSIXct(date, origin="1970-01-01"))
price <- dat[seq(0,length(dat),2)]
df <- data.frame(date,price)

# fit an ARIMA model to th eprice data
fit <- auto.arima(df$price)
plot(forecast(fit,h=5))
plot(seq.Date(from=df$date[1],by=1,length.out=370),forecast(fit,h=5))
# zoom into the forward-period forecasted values
plot(seq.Date(from=df$date[length(df)],by=1,length.out=10),forecast(fit,h=10)$mean)

x<-data.frame(Time=character(0),Count=numeric())

time<-c()
count<-c()

for (i in 0:5){
  count <- append(count,fromJSON(getURL("http://blockchain.info/q/unconfirmedcount")))
  time <- append(time,Sys.time())
  cat(paste(time[i]),count[i],"\n")
  Sys.sleep(2)
}

df <- data.frame(time,count)

# i=1
# while (i==1){
#   count <- append(count,fromJSON(getURL("http://blockchain.info/q/unconfirmedcount")))
#   time <- append(time,Sys.time())
#   cat(paste(time[i]),count[i],"\n")
#   Sys.sleep(2)
#   
# }
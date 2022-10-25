library(devtools)
library(quantmod)

library(devtools)

install.packages("statsr")
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)
library(statsr)



library(RCurl)
library(jsonlite)
library(plyr)



fixJSON <- function(json){
  gsub('([^,{:]+):', '"\\1":', json)
}

URL1 = 'https://www.google.com/finance/option_chain?q=%s%s&output=json'
URL2 = 'https://www.google.com/finance/option_chain?q=%s%s&output=json&expy=%d&expm=%d&expd=%d'




getOptionQuotes <- function(symbol, exchange = NA) {
  exchange = ifelse(is.na(exchange), "", paste0(exchange, ":"))
  #
  url = sprintf(URL1, exchange, symbol)
  #
  chain = tryCatch(fromJSON(fixJSON(getURL(url))), error = function(e) NULL)
  #
  if (is.null(chain)) stop(sprintf("retrieved document is not JSON. Try opening %s in your browser.", url))
  #
  # Iterate over the expiry dates
  #
  options = mlply(chain$expirations, function(y, m, d) {
    url = sprintf(URL2, exchange, symbol, y, m, d)
    expiry = fromJSON(fixJSON(getURL(url)))
    #
    expiry$calls$type = "Call"
    expiry$puts$type  = "Put"
    #
    prices = rbind(expiry$calls, expiry$puts)
    #
    prices$expiry = sprintf("%4d-%02d-%02d", y, m, d)
    prices$underlying.price = expiry$underlying_price
    #
    prices$retrieved = Sys.time()
    #
    prices
  })
  #
  options = options[sapply(options, class) == "data.frame"]
  #
  # Concatenate data for all expiration dates and add in symbol column
  #
  options = cbind(data.frame(symbol), rbind.fill(options))
  #
  options = rename(options, c("p" = "premium", "b" = "bid", "a" = "ask", "oi" = "open.interest"))
  #
  for (col in c("strike", "premium", "bid", "ask")) options[, col] = suppressWarnings(as.numeric(options[, col]))
  options[, "open.interest"] = suppressWarnings(as.integer(options[, "open.interest"]))
  #
  options[, c("symbol", "type", "expiry", "strike", "premium", "bid", "ask", "open.interest", "retrieved")]
}

TUR = getOptionQuotes("TUR")
TUR
na.omit(TUR)

summary(TUR)
> stem(calls); hist(calls)

APPL$currency


#install.packages("ggplot2")                                   
#library("ggplot2")
#install.packages("reshape2")
#require(ggplot2)
#require(reshape2)
#oi <- melt(oi ,  id.vars = 'time', variable.name = 'series')
#ggplot(oi, aes(time,value)) + geom_line(aes(colour = series))
#ggplot(oi, aes(time,value)) + geom_line() + facet_grid(series ~ .)
#require(zoo)
#set.seed(1)
#x = "prices"
#y = "ask"
#graph1 = "x"
#grap2 <- zoo(graph1)
#plot(grap2)


qqnorm(TUR)


summary(x)

callgreek(greek = c("delta", "gamma", "theta", "vega", "rho", "premium"),
          s, x, sigma, t, r, d = 0)



names(cls)
summary(TUR)

x1 = cls = TUR$puts

y1 = cls = TUR$calls

x$OI
y$OI


x$OI
plot(x$OI, x$OI)

x$OI
y$OI

plot(x$LastTradeTime, x$OI)
plot(y$LastTradeTime, y$OI)

typeof()



cls$Bid


dim(x)

names(x)



ggplot(x = OP, aes(t = lastTradeTime, O = OI)) +
  geom_point(





cls$OI
plot(cls$OI, cls$Strike)






hist(cls$Strike)



y1 %>% ggplot(aes(x=y1$LastTradeTime)) + geom_histogram()

y1 %>% ggplot(aes(x=y1$LastTradeTime)) + geom_histogram(binwidth = 3)

y1 %>% ggplot(aes(x=Strike, y=OI)) + geom_()










cls %>% ggplot(aes(x=cls$Strike)) + geom_histogram()
cls %>% ggplot(aes(x=cls$Strike)) + geom_histogram(binwidth = 5)
cls %>% ggplot(aes(x=Strike, y=OI)) %>% geom_point()











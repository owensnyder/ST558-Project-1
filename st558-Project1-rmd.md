ST558 Project1
================
Owen Snyder
2022-06-16

-   [Necessary Packages](#necessary-packages)
-   [Functions to Query Data](#functions-to-query-data)
    -   [Function for Daily Open/Close for
        Stocks](#function-for-daily-openclose-for-stocks)
    -   [Function for Daily Open/Close for
        Crypto](#function-for-daily-openclose-for-crypto)
    -   [Function for Aggregates(Bars) Stock
        Data](#function-for-aggregatesbars-stock-data)
    -   [Function for Aggregates(Bars) Crypto
        Data](#function-for-aggregatesbars-crypto-data)
    -   [Function to find all Exchanges the API
        carries](#function-to-find-all-exchanges-the-api-carries)
    -   [Function to find Grouped Daily(Bars) for All
        Stocks](#function-to-find-grouped-dailybars-for-all-stocks)
-   [Exploratory Data Analysis](#exploratory-data-analysis)
    -   [Extract S&P 500 (SPY) Data](#extract-sp-500-spy-data)
    -   [Extract Vanguard 500 Index Fund (VOO)
        Data](#extract-vanguard-500-index-fund-voo-data)
    -   [Extract Bitcoin (BTC) Data](#extract-bitcoin-btc-data)
    -   [Extract Ethereum (ETH) Data](#extract-ethereum-eth-data)
    -   [Contingency Tables](#contingency-tables)
    -   [Numerical Summaries](#numerical-summaries)
    -   [Histograms: BTC, ETH, SPY, VOO](#histograms-btc-eth-spy-voo)
    -   [Boxplots: BTC, ETH](#boxplots-btc-eth)
    -   [Time Series Graph for BTC, ETH](#time-series-graph-for-btc-eth)

Create render function

``` r
rmarkdown::render('README.md',
 output_format = "github_document",
 #output_dir = "_posts",
  output_options = list(
  html_preview = TRUE
)
)
 

######
rmarkdown::render('README.md',
                  output_format = "github_document",
                  output_dir = "images",
                  output_options = list(
                    html_preview = FALSE, toc = TRUE, toc_depth = 2, toc_float = TRUE)
)


######
 output_format = "github_document"
  output_dir = "_posts"
  output_options = list(
  html_preview = FALSE
  )
```

``` r
knitr::opts_chunk$set(fig.path = "../images/")
```

# Necessary Packages

``` r
library(tidyverse)
library(ggplot2)
library(httr)
library(jsonlite)
library(lubridate)
library(bizdays)
```

dont need?, i.e. dont need to include, echo = F

``` r
fin.data <- GET("https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2020-06-01/2020-06-17?apiKey=O5bhHpJ7TFFBgq7bUJp8TA7Yi7uDLMIe")

str(fin.data, max.level = 1)
```

    ## List of 10
    ##  $ url        : chr "https://api.polygon.io/v2/aggs/ticker/AAPL/range/1/day/2020-06-01/2020-06-17?apiKey=O5bhHpJ7TFFBgq7bUJp8TA7Yi7uDLMIe"
    ##  $ status_code: int 200
    ##  $ headers    :List of 8
    ##   ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ all_headers:List of 1
    ##  $ cookies    :'data.frame': 0 obs. of  7 variables:
    ##  $ content    : raw [1:127] 7b 22 74 69 ...
    ##  $ date       : POSIXct[1:1], format: "2022-06-25 14:34:12"
    ##  $ times      : Named num [1:6] 0 0.0593 0.0995 0.194 0.3071 ...
    ##   ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
    ##  $ request    :List of 7
    ##   ..- attr(*, "class")= chr "request"
    ##  $ handle     :Class 'curl_handle' <externalptr> 
    ##  - attr(*, "class")= chr "response"

``` r
parsed <- fromJSON(rawToChar(fin.data$content))
str(parsed, max.level = 1)
```

    ## List of 6
    ##  $ ticker      : chr "AAPL"
    ##  $ queryCount  : int 0
    ##  $ resultsCount: int 0
    ##  $ adjusted    : logi TRUE
    ##  $ status      : chr "OK"
    ##  $ request_id  : chr "7bebbd41fa3ad9fe6a99f7e8f7a52199"

    ## [1] "{\"ticker\":\"AAPL\",\"queryCount\":0,\"resultsCount\":0,\"adjusted\":true,\"status\":\"OK\",\"request_id\":\"7bebbd41fa3ad9fe6a99f7e8f7a52199\"}"

    ## $status
    ## [1] "OK"
    ## 
    ## $from
    ## [1] "2020-10-14"
    ## 
    ## $symbol
    ## [1] "AAPL"
    ## 
    ## $open
    ## [1] 121
    ## 
    ## $high
    ## [1] 123.03
    ## 
    ## $low
    ## [1] 119.62
    ## 
    ## $close
    ## [1] 121.19
    ## 
    ## $volume
    ## [1] 151057198
    ## 
    ## $afterHours
    ## [1] 120.81
    ## 
    ## $preMarket
    ## [1] 121.55

# Functions to Query Data

## Function for Daily Open/Close for Stocks

``` r
## This function will take in values of the ticker of a given stock, the date (day) you want to 
## look at, and whether or not you want adjusted splits

getDailyOC <- function(stocksTicker,date,adjusted){
  adj <- ifelse(adjusted, "true", "false") ##setting adjusted to be only true or false
  link <- paste0("https://api.polygon.io/v1/open-close/",stocksTicker,"/",date,"?adjusted=",adj,
                 "&apiKey=O5bhHpJ7TFFBgq7bUJp8TA7Yi7uDLMIe")
  ## the link to connect to the API endpoint
  fromJSON(readLines(link,warn = FALSE))
 ## here we are converting a JSON object into a usable R object and suppressing warnings
}
## test the function with a ticker
## getDailyOC(stocksTicker = "SPY", date = "2021-10-12", adjusted = TRUE)
```

## Function for Daily Open/Close for Crypto

``` r
## This function will take in values for the symbol of a given cryptocurrency, the currency you want 
## the crypto to be converted into, the date (day) you want to look at, and  and whether or not you want 
## adjusted splits

getCryptoOC <- function(symbol, to, date, adjusted,...){
  adj <- ifelse(adjusted, "true", "false") ##setting adjusted to be only true or false
  link <-  paste0("https://api.polygon.io/v1/open-close/crypto/",symbol,"/",to,"/",date,"?adjusted=",adj,
                  "&apiKey=O5bhHpJ7TFFBgq7bUJp8TA7Yi7uDLMIe")
  ## the link to connect to the API endpoint
  fromJSON(readLines(link,warn = FALSE))
  ## here we are converting a JSON object into a usable R object and suppressing warnings
}

## test the function with a crypto ticker
##getCryptoOC(symbol = "ETH", to = "USD", date = "2021-10-12", adjusted = TRUE)
```

## Function for Aggregates(Bars) Stock Data

``` r
## this function will take in a stocks ticker, multiplier, time span, date range values (from and to),
## adjusted splits, a sort option, and a limit value
## Note: the pre-inputted arguments are for common defaults. they can be changed per the users request

getAgg <- function(stocksTicker,multiplier,timespan="day",from,to,adjusted,sort="desc",limit=5000){
  
  adj <- ifelse(adjusted, "true", "false") ## setting adjusted to be only true or false
  multiplier <- as.integer(multiplier) ## multiplier must be an integer 
  timespan <- match.arg(timespan,choices = c("minute", "hour", "day", "week", "month", "quarter", "year"))
  ## provide a list of time span choices the URL and function will be able to include
  sort <- match.arg(sort, choices = c("asc", "desc"))
  ## establish the sort of how you want data to be printed 
  limit <- as.integer(limit)
  if (limit > 50000)
    stop("limit must be less than 50000")
  if(limit < 1)
    stop("limit must be greater than or equal to 1")
  ## set limit conditions and stop the function if they fail
    
  link <-  paste0("https://api.polygon.io/v2/aggs/ticker/",stocksTicker,"/range/",multiplier,"/"
                  ,timespan,"/",from,"/",to,"?adjusted=",adj,"&sort=",sort,"&limit=",limit,
                  "&apiKey=O5bhHpJ7TFFBgq7bUJp8TA7Yi7uDLMIe")
  ## the link to connect to the API endpoint
  output <- fromJSON(readLines(link,warn = FALSE))
  ## here we are converting a JSON object into a usable R object and suppressing warnings
  
  return(output$results)
  ## return only the results 
  
}

## test the function
## get.appl <- getAgg("SPY", multiplier = 1, timespan = "day", from = "2020-10-01", to= "2022-06-15",adjusted = TRUE, sort = 'desc', limit = 6000)
```

## Function for Aggregates(Bars) Crypto Data

``` r
## this function will take in a crypto ticker, a multiplier value, a time span, date ranges (from and to)
## adjusted splits, a sort option, and limit value
getAggCryp <- function(cryptoTicker,multiplier,timespan="day",from,to,adjusted,
                       sort="desc", limit=5000){
  
  adj <- ifelse(adjusted, "true", "false") ## setting adjusted to be only true or false
  multiplier <- as.integer(multiplier) ## multiplier must be an integer
  timespan <- match.arg(timespan,choices = c("minute", "hour", "day", "week", "month", "quarter", "year"))
  ## provide a list of time span choices the URL and function will be able to include
  sort <- match.arg(sort, choices = c("asc", "desc"))
  ## establish the sort of how you want data to be printed
  limit <- as.integer(limit)
  if (limit > 50000)
    stop("limit must be less than 50000")
  if(limit < 1)
    stop("limit must be greater than or equal to 1")
  
  
  link <- paste0("https://api.polygon.io/v2/aggs/ticker/",cryptoTicker,"/range/",multiplier,"/"
                 ,timespan,"/",from,"/",to,"?adjusted=",adj,"&sort",sort,"&limit=",limit,
                 "&apiKey=O5bhHpJ7TFFBgq7bUJp8TA7Yi7uDLMIe")
  ## the link to connect to the API endpoint
  output <- fromJSON(readLines(link,warn = FALSE))
  return(output$results) ## return only the results 
}

## test function
## get.eth <- getAgg("X:ETHUSD", multiplier = 1, timespan = "day", from = "2021-10-01", to= "2022-12-31",
##                  adjusted = TRUE, sort = 'desc', limit = 5000)
```

## Function to find all Exchanges the API carries

``` r
## this function will take in an asset class (options specified below) and a locale (location)

getEXCH <- function(asset_class,locale){
  
  asset_class <- match.arg(asset_class, choices = c("stocks","options","crypto","fx"))
  ## choices for asset_class must be these four
  locale <- match.arg(locale, choices = c("us","global"))
  ## locale must be US or global
  link <- paste0("https://api.polygon.io/v3/reference/exchanges?asset_class=",asset_class,
                 "&apiKey=O5bhHpJ7TFFBgq7bUJp8TA7Yi7uDLMIe")
  ## the link to connect to the API endpoint
  output <- fromJSON(readLines(link,warn = FALSE))
  return(output$results) ## return only the results section
  
}

## test the function
## get.all <- getEXCH(asset_class = "stocks", locale = "us")
```

## Function to find Grouped Daily(Bars) for All Stocks

``` r
## Get the daily open, high, low, and close (OHLC) for the entire stocks/equities markets

getGroupBars <- function(date, adjusted, include_otc){
  adj <- ifelse(adjusted, "true", "false") ## setting adjusted to be only true or false
  otc <- ifelse(include_otc, "true", "false") ## setting include_otc to be only true or false
  link <- paste0("https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/",date,"?adjusted=",adj,
                 "&apiKey=O5bhHpJ7TFFBgq7bUJp8TA7Yi7uDLMIe")
  ## the link to connect to the API endpoint
  output <- fromJSON(readLines(link,warn = FALSE))
  return(output$results) 
  ## return only the results
}

## test function
## getGroupBars(date = "2021-10-12", adjusted = TRUE, include_otc = TRUE)
```

LETS COMPARE CRYTO AND STOCK PRICES FOR A GIVEN TIME PERIOD

“Bars” refer to a data representation that contains the most basic
information about price movements of a financial asset.

NEW VARIABLE SHOULD BE RETURNS and LOG RETURNS

# Exploratory Data Analysis

Modeling stock and crypto performance can be a challenging task as there
are many different measures we can assess. You may want to analyze one
stock or a portfolio of stocks. and You may want to look at how risky
these positions are.

In this EDA, I will pull data from the S&P 500 and different
cryptocurrencies such as Bitcoin, Etherieum and Solana to see how they
perform against my chosen date window. I want to analyze if there is any
similarity between stock market trends and crypto trends

NOTE: It is important to recogtnize the major market differences between
the tradional stock market and crypto currencies. Crypto is a 24/7
market

First, I will extract the necessary data i am going to work with via my
created functions from above.

## Extract S&P 500 (SPY) Data

``` r
## use the getAgg function to pull S&P 500 data 

data.spy <- getAgg("SPY", multiplier = 1, timespan = "day", from = "2021-06-15", to= "2022-06-15",adjusted = FALSE, sort = 'asc', limit = 5000)

## Now i am renaming column names 
colnames(data.spy) <- c('Volume', 'WeightedVolume', "Open", "Close", "HighestPrice",
                        "LowestPrice", "UnixTimes(msec)", "NumTransactions")

## create a new variable for Log Returns 
#data.spy %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0)
SPY <- data.spy %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0) %>% 
       mutate(Symbol = 'SPY')# %>% mutate(Date = range)
## Note: set na = 0 because there are no returns on the first day/observation
```

## Extract Vanguard 500 Index Fund (VOO) Data

``` r
data.voo <- getAgg("VOO", multiplier = 1, timespan = "day", from = "2021-06-15", to= "2022-06-15",adjusted = FALSE, sort = 'asc', limit = 5000)

## Now i am renaming column names 
colnames(data.voo) <- c('Volume', 'WeightedVolume', "Open", "Close", "HighestPrice",
                        "LowestPrice", "UnixTimes(msec)", "NumTransactions")

## create a new variable for Log Returns 
VOO <- data.voo %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0) %>% 
       mutate(Symbol = "VOO")
```

Date formatting

## Extract Bitcoin (BTC) Data

``` r
## use the getAggCryp function to pull Bitcoin Data

start.date <- ymd("2021-06-15")
end.date <- ymd("2022-06-15")
range <- seq(start.date, end.date,"days")
fin.quarters <- quarters(range)

data.btc <- getAggCryp("X:BTCUSD", multiplier = 1, timespan = "day", from = "2021-06-15", to= "2022-06-15",                           adjusted = FALSE, sort = 'asc', limit = 5000)
## Now change column names again
colnames(data.btc) <- c('Volume', 'WeightedVolume', "Open", "Close", "HighestPrice",
                        "LowestPrice", "UnixTimes(msec)", "NumTransactions")

BTC <- data.btc %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0) %>% 
       mutate(Symbol = "BTC") %>% mutate(Date = range) %>% mutate(Quarters = fin.quarters)
```

## Extract Ethereum (ETH) Data

``` r
data.eth <- getAggCryp("X:ETHUSD", multiplier = 1, timespan = "day", from = "2021-06-15", to= "2022-06-15",                           adjusted = FALSE, sort = 'asc', limit = 5000)
colnames(data.eth) <- c('Volume', 'WeightedVolume', "Open", "Close", "HighestPrice",
                        "LowestPrice", "UnixTimes(msec)", "NumTransactions")

ETH <- data.eth %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0) %>% 
       mutate(Symbol = "ETH") %>% mutate(Date = range, Quarters = fin.quarters) %>% 
       mutate(TransCategory = ifelse(NumTransactions %in% 800000:1852104, "Many",
                              ifelse(NumTransactions %in% 500000:800000, "Average",
                              ifelse(NumTransactions %in% 42510:500000, "Few", "NONE"))))
```

Combine Crypto

## Contingency Tables

``` r
#hist(ETH$NumTransactions)
table(ETH$TransCategory, ETH$Quarters)
```

    ##          
    ##           Q1 Q2 Q3 Q4
    ##   Average 55 45 16 38
    ##   Few     22 33 73 51
    ##   Many    13 14  3  3

## Numerical Summaries

I first want to display some simple summary statistics for the stocks
and the cryptocurrecnies.

``` r
## Summary for SPY data 
SPY %>% summarise(meanVol = mean(Volume), meanOpen = mean(Open), meanClose = mean(Close))
```

    ##    meanVol meanOpen meanClose
    ## 1 88238884 440.7512   440.638

Graphs:

## Histograms: BTC, ETH, SPY, VOO

``` r
p <- ggplot(data = BTC, aes(x = BTC$LogRet))
p+geom_histogram() + geom_density()
```

    ## Warning: Use of `BTC$LogRet` is discouraged. Use `LogRet` instead.
    ## Use of `BTC$LogRet` is discouraged. Use `LogRet` instead.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../images/unnamed-chunk-20-1.png)<!-- -->

``` r
p2 <- ggplot(data = ETH, aes(x = LogRet))
p2+geom_histogram() + geom_density()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../images/unnamed-chunk-20-2.png)<!-- -->

``` r
p3 <- ggplot(data = SPY, aes(x = LogRet))
p3+geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../images/unnamed-chunk-20-3.png)<!-- -->

``` r
p4 <- ggplot(data = VOO, aes(x = LogRet))
p4+geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../images/unnamed-chunk-20-4.png)<!-- -->

## Boxplots: BTC, ETH

``` r
#bp1 <- ggplot(data = all.c, x= all.c$NumTransactions.y, y = all.c$Open.y )
#bp1 + geom_boxplot()

## boxplot for quarters and open prices
bp1 <- ggplot(BTC, aes(x = Quarters, y = Close))
bp1 + geom_boxplot()
```

![](../images/unnamed-chunk-21-1.png)<!-- -->

``` r
bp2 <- ggplot(ETH, aes(x = Quarters, y = Close))
bp2 + geom_boxplot()
```

![](../images/unnamed-chunk-21-2.png)<!-- -->

## Time Series Graph for BTC, ETH

``` r
## time series graph for BTC
ggplot(data = BTC, aes(x=Date)) + 
  geom_line(aes(y=LogRet)) + 
  labs(title="Time Series Plot on Log Returns for Bitcoin", 
       subtitle="Returns Based on Close Price", 
       y="Log Returns") +  # title and caption
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid 
```

![](../images/unnamed-chunk-22-1.png)<!-- -->

``` r
## time series graph for ETH
ggplot(data = ETH, aes(x=Date)) + 
  geom_line(aes(y=LogRet)) + 
  labs(title="Time Series Plot on Log Returns for Ethereum", 
       subtitle="Returns Based on Close Price", 
       y="Log Returns") +  
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))  
```

![](../images/unnamed-chunk-22-2.png)<!-- -->

``` r
        #panel.grid.minor = element_blank())  # turn off minor grid 

## CHECK GRIDS
```

Here, we notice that ETH is much nosier than Bitcoin.

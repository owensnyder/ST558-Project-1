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
    -   [Function for All Exchanges the API
        carries](#function-for-all-exchanges-the-api-carries)
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
    -   [Boxplots: SPY, VOO, BTC, ETH](#boxplots-spy-voo-btc-eth)
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
    ##  $ date       : POSIXct[1:1], format: "2022-06-26 01:35:08"
    ##  $ times      : Named num [1:6] 0 0.0579 0.0959 0.192 0.2384 ...
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
    ##  $ request_id  : chr "0f4c60318a4154bf823a915c368e763d"

    ## [1] "{\"ticker\":\"AAPL\",\"queryCount\":0,\"resultsCount\":0,\"adjusted\":true,\"status\":\"OK\",\"request_id\":\"0f4c60318a4154bf823a915c368e763d\"}"

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

## Function for All Exchanges the API carries

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

# Exploratory Data Analysis

Modeling stock and crypto currency performance can be a challenging task
as there are many different measures we can assess. You may want to
analyze one stock or a portfolio of stocks and you may want to look at
how risky these positions are.

In this Exploratory Data Analysis, I will pull data from the S&P 500,
the Vanguard 500 Index Fund as my two traditional stock market
exchange-traded funds. I will also pull data from Bitcoin and Ethereum
from the same date window as my tradiational stocks.

The big question will be: How do these stocks and crypto currencies
compare against each other? In the past year, the stock market has been
extremely volatile and the talk and usage of crypto currencies has
skyrocketed. I will be analyzing the S&P 500 against the Vanguard 500
based on different market variables. I well then do the same analysis on
Bitcoin and Ethereum to see how well they perform against each other. I
will be looking at variables such as the “Number of Transactions” in a
given day for a specific position and the Open and Close Price on a
given day in my time window.

I will also be creating a very important new variable that is used
throughout finance, that being “Log Returns”. Log returns are extremely
important in financial analysis because it gives us the ability to
assume a Normal distribution. This assumption becomes extremely powerful
when risk analytics are performed (Value at Risk, Expected Shortfall).
However, in the scope of this project, Log Returns will still be useful
as it will give use the opportunity to visualize and quantify returns in
a more consistent manner.

Another important variable I will be creating is that of a “Quarters”
variable. Quarterly performances in finance are referenced often
throughout a fiscal year and they will give us a better understanding on
which quarters have been performing best or worse. By doing this, we
will be able to know what was the best time to have invested into a
stock or crypto based on quarterly Close Prices. Along with the
“Quarters” variable, another variable will be created to achieve a more
categorically pleasing way to look at quarter performance. I will be
creating a variable, “TransCategory” to categorize how many transactions
were made throughot the given time period.

By the end of this EDA, I will offer a brief conclusion on which stock
and crypt currency I would have preferred to have invested into during
the given time period.

NOTE: It is important to recognize the major market differences between
the traditional stock market and crypto currencies. Crypto currency is a
24/7 market whereas the stock market excludes weekends and holidays.

NOTE: Other variables will be created below but will not be analyzed
directly. Specifically, they will be part of the data wrangling process
and will be explained as they arise.

## Extract S&P 500 (SPY) Data

``` r
## use the getAgg function to pull S&P 500 data 

data.spy <- getAgg("SPY", multiplier = 1, timespan = "day", from = "2021-06-15", to= "2022-06-15",adjusted = FALSE, sort = 'asc', limit = 5000)

## Now i am renaming column names 
colnames(data.spy) <- c('Volume', 'WeightedVolume', "Open", "Close", "HighestPrice",
                        "LowestPrice", "UnixTimes(msec)", "NumTransactions")

## adding a LogReturn variable (note: we set the first value to be zero because there are no 
## returns on the first trading day). Also adding a NumberObs variable to be able to graph 
## time series in the plots below. And adding a Symbol variable to assign to each data value.
## Also adding a Quarters variable and Transaction Category variable

SPY <- data.spy %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0) %>% 
       mutate(Symbol = 'SPY') %>% mutate(Nobs = row_number()) %>% mutate(Quarters = 
                           ifelse(Nobs %in% 1:12, "Q2",
                           ifelse(Nobs %in% 13:87, "Q3",
                           ifelse(Nobs %in% 88:144, "Q4",
                           ifelse(Nobs %in% 155:208, "Q1", "Q2"))))) %>%
    mutate(TransCategory = ifelse(NumTransactions %in% 1000000:3263011, "Many",
                           ifelse(NumTransactions %in% 650000:1000000, "Average",
                           ifelse(NumTransactions %in% 0:650000, "Few", "NONE"))))

summary(SPY$NumTransactions)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  234209  408476  604976  712788  895168 3263011

## Extract Vanguard 500 Index Fund (VOO) Data

``` r
## use the getAgg function to pull Vanguard 500 data 
data.voo <- getAgg("VOO", multiplier = 1, timespan = "day", from = "2021-06-15", to= "2022-06-15",adjusted = FALSE, sort = 'asc', limit = 5000)

## Now i am renaming column names 
colnames(data.voo) <- c('Volume', 'WeightedVolume', "Open", "Close", "HighestPrice",
                        "LowestPrice", "UnixTimes(msec)", "NumTransactions")

## adding a LogReturn variable (note: we set the first value to be zero because there are no 
## returns on the first trading day). Also adding a NumberObs variable to be able to graph 
## time series in the plots below. And adding a Symbol variable to assign to each data value. 
## Also adding a Quarters variable and a Transaction Category variable
VOO <- data.voo %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0) %>% 
       mutate(Symbol = "VOO") %>% mutate(Nobs = row_number()) %>% mutate(Quarters = 
                           ifelse(Nobs %in% 1:12, "Q2",
                           ifelse(Nobs %in% 13:87, "Q3",
                           ifelse(Nobs %in% 88:144, "Q4",
                           ifelse(Nobs %in% 155:208, "Q1", "Q2"))))) %>% 
    mutate(TransCategory = ifelse(NumTransactions %in% 200000:488939, "Many",
                           ifelse(NumTransactions %in% 90000:200000, "Average",
                           ifelse(NumTransactions %in% 0:90000, "Few", "NONE"))))
```

date formatting

## Extract Bitcoin (BTC) Data

``` r
## use the getAggCryp function to pull Bitcoin Data
data.btc <- getAggCryp("X:BTCUSD", multiplier = 1, timespan = "day", from = "2021-06-15", to= "2022-06-15",                           adjusted = FALSE, sort = 'asc', limit = 5000)
## Now change column names again
colnames(data.btc) <- c('Volume', 'WeightedVolume', "Open", "Close", "HighestPrice",
                        "LowestPrice", "UnixTimes(msec)", "NumTransactions")

## adding a LogReturn variable (note: we set the first value to be zero because there are no 
## returns on the first trading day). Also adding a Date variable , Quarters variable, a 
## TransCategory variable, and a Symbol variable
BTC <- data.btc %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0) %>% 
       mutate(Symbol = "BTC") %>% mutate(Date = range) %>% mutate(Quarters = fin.quarters) %>%
       mutate(TransCategory = ifelse(NumTransactions %in% 800000:2240555, "Many",
                              ifelse(NumTransactions %in% 500000:800000, "Average",
                              ifelse(NumTransactions %in% 0:500000, "Few", "NONE"))))
```

## Extract Ethereum (ETH) Data

``` r
data.eth <- getAggCryp("X:ETHUSD", multiplier = 1, timespan = "day", from = "2021-06-15", to= "2022-06-15",                           adjusted = FALSE, sort = 'asc', limit = 5000)
## now change column names
colnames(data.eth) <- c('Volume', 'WeightedVolume', "Open", "Close", "HighestPrice",
                        "LowestPrice", "UnixTimes(msec)", "NumTransactions")

## adding a LogReturn variable (note: we set the first value to be zero because there are no 
## returns on the first trading day). Also adding a Date variable , Quarters variable, a 
## TransCategory variable and a Symbol variable
ETH <- data.eth %>% mutate(LogRet = log(Close) - log(lag(Close))) %>% replace(is.na(.), 0) %>% 
       mutate(Symbol = "ETH") %>% mutate(Date = range, Quarters = fin.quarters) %>% 
       mutate(TransCategory = ifelse(NumTransactions %in% 800000:1852104, "Many",
                              ifelse(NumTransactions %in% 500000:800000, "Average",
                              ifelse(NumTransactions %in% 0:500000, "Few", "NONE"))))
```

## Contingency Tables

I will no create contingency tables for the Number of Transactions based
on Quarters. I have grouped the Number of Transactions into three main
categories: Average, Few, or Many. This way, we will be able to identify
which quarters seemed to have the what category of transaction. For
example, A quarter with a high count of “Many” transactions may indicate
a surging market. This could be an indication of a good time to buy or
sell your given position. Below, I will summarize my findings:

For the S&P 500 data, there is a high count of “Few” transactions in Q3,
this may be an indication of a slow market period for Q3 and in turn,
there are more people holding onto their stock in the S&P 500. We can
also see that there was a very low count of “Many”, indicating that
transaction numbers were down considerably.

For the Vanguard 500 data, there is also a high count of “Few”
transactions in Q3. This is similar to the S&P data and this gives us a
better idea of the stock market conditions in Q3.

For the Ethereum data, the trend is surprisingly the same. Q3 has the
highest count fo “Few” transactions. This is important to note that the
quarterly transaction performance may be similar between the traditional
stock market and the crypto market.

Finally, for the Bitcoin data also follows the same trend. Q3 has the
highest count fo “Few” transactions.

Between the four contingency tables, I believe that it is safe to
conclude that an investor was best to hold their shares if the had any,
or not buy into the market during a slow Quarter 3 across the board of
the stock market and the crypto market.

``` r
SPY %>% group_by(TransCategory,Quarters) %>% summarize(counts = n()) %>% 
    pivot_wider(values_from = counts, names_from = Quarters)
```

    ## # A tibble: 3 × 5
    ## # Groups:   TransCategory [3]
    ##   TransCategory    Q1    Q2    Q3    Q4
    ##   <chr>         <int> <int> <int> <int>
    ## 1 Average          19    26     9     8
    ## 2 Few               9    26    64    45
    ## 3 Many             26    16     2     4

``` r
VOO %>% group_by(TransCategory,Quarters) %>% summarize(counts = n()) %>% 
    pivot_wider(values_from = counts, names_from = Quarters)
```

    ## # A tibble: 3 × 5
    ## # Groups:   TransCategory [3]
    ##   TransCategory    Q1    Q2    Q3    Q4
    ##   <chr>         <int> <int> <int> <int>
    ## 1 Average          32    31    22    27
    ## 2 Few               6    27    51    28
    ## 3 Many             16    10     2     2

``` r
ETH %>% group_by(TransCategory,Quarters) %>% summarize(counts = n()) %>% 
    pivot_wider(values_from = counts, names_from = Quarters)
```

    ## # A tibble: 3 × 5
    ## # Groups:   TransCategory [3]
    ##   TransCategory    Q1    Q2    Q3    Q4
    ##   <chr>         <int> <int> <int> <int>
    ## 1 Average          55    45    16    38
    ## 2 Few              22    33    73    51
    ## 3 Many             13    14     3     3

``` r
BTC %>% group_by(TransCategory,Quarters) %>% summarize(counts = n()) %>% 
    pivot_wider(values_from = counts, names_from = Quarters)
```

    ## # A tibble: 3 × 5
    ## # Groups:   TransCategory [3]
    ##   TransCategory    Q1    Q2    Q3    Q4
    ##   <chr>         <int> <int> <int> <int>
    ## 1 Average          56    51    16    62
    ## 2 Few              21    22    72    17
    ## 3 Many             13    19     4    13

## Numerical Summaries

Next, I will display some quantitative summaries for all four positions.
The goal of these summaries is to show the success of each position
based on fiscal quarters and we hope to identify quarters where the
average value of returns is positive. When investing, your main goal is
to maximize your return on investment. It is also important to note the
average value of the Open and Close Price, standard deviation of Log
Returns and the average Number of Transactions. These will help paint a
better picture of what goes into a strong return for each quarter.

Starting with S&P 500 summaries, the quarter with the worst performance
on average Log Returns was Q2. It also had the lowest average Close
Price which solidifies that this was a poor quarter for investments into
the S&P 500. On the other hand, Q4 produced the highest average Log
Returns.

As expected, the Vanguard 500 stock also had the worst performance of
average Log Returns and average Close Price. Again, Q4 produced the
highest average Log Returns.

Switching into the crypto-realm, Bitcoin produced poor average Log
Returns for both Q1 and Q2. Conversely, Q3 had the highest average Log
Returns.

For Ethereum, we see a similar trend in that Q1 and Q2 produced the
worst average Log Returns. Again, Q3 produced the best average Log
Returns.

``` r
## Simple quantitative summaries for each position data 

## SPY data
SPY %>% group_by(Quarters) %>% summarise(avgOpen = mean(Open), avgClose = mean(Close), 
                                         avgLogRet = mean(LogRet), sdLogRet = sd(LogRet),
                                         avgTrans = mean(NumTransactions))
```

    ## # A tibble: 4 × 6
    ##   Quarters avgOpen avgClose avgLogRet sdLogRet avgTrans
    ##   <chr>      <dbl>    <dbl>     <dbl>    <dbl>    <dbl>
    ## 1 Q1          441.     441.  0.000401  0.0137  1101395.
    ## 2 Q2          422.     421. -0.00329   0.0163   816281.
    ## 3 Q3          440.     440.  0.000544  0.00739  470895.
    ## 4 Q4          464.     464.  0.000848  0.00894  539448.

``` r
## VOO data
VOO %>% group_by(Quarters) %>% summarise(avgOpen = mean(Open), avgClose = mean(Close), 
                                         avgLogRet = mean(LogRet), sdLogRet = sd(LogRet),
                                         avgTrans = mean(NumTransactions))
```

    ## # A tibble: 4 × 6
    ##   Quarters avgOpen avgClose avgLogRet sdLogRet avgTrans
    ##   <chr>      <dbl>    <dbl>     <dbl>    <dbl>    <dbl>
    ## 1 Q1          405.     406.  0.000402  0.0136   163707.
    ## 2 Q2          388.     387. -0.00329   0.0162   123707.
    ## 3 Q3          405.     405.  0.000545  0.00727   89236.
    ## 4 Q4          427.     427.  0.000842  0.00872  105276.

``` r
## BTC data
BTC %>% group_by(Quarters) %>% summarise(avgOpen = mean(Open), avgClose = mean(Close), 
                                         avgLogRet = mean(LogRet),sdLogRet = sd(LogRet),
                                         avgTrans = mean(NumTransactions))
```

    ## # A tibble: 4 × 6
    ##   Quarters avgOpen avgClose avgLogRet sdLogRet avgTrans
    ##   <chr>      <dbl>    <dbl>     <dbl>    <dbl>    <dbl>
    ## 1 Q1        41302.   41292. -0.000165   0.0351  662756.
    ## 2 Q2        35181.   34872. -0.00911    0.0406  706141.
    ## 3 Q3        41888.   41982.  0.00241    0.0348  444181.
    ## 4 Q4        55896.   55925.  0.000590   0.0342  639951.

``` r
## ETH data
ETH %>% group_by(Quarters) %>% summarise(avgOpen = mean(Open), avgClose = mean(Close), 
                                         avgLogRet = mean(LogRet),sdLogRet = sd(LogRet),
                                         avgTrans = mean(NumTransactions))
```

    ## # A tibble: 4 × 6
    ##   Quarters avgOpen avgClose avgLogRet sdLogRet avgTrans
    ##   <chr>      <dbl>    <dbl>     <dbl>    <dbl>    <dbl>
    ## 1 Q1         2939.    2934.  -0.00126   0.0409  621339.
    ## 2 Q2         2405.    2381.  -0.0118    0.0499  607405.
    ## 3 Q3         2842.    2850.   0.00300   0.0469  399008.
    ## 4 Q4         4092.    4100.   0.00221   0.0387  509313.

Graphs:

## Histograms: BTC, ETH, SPY, VOO

``` r
p <- ggplot(data = BTC, aes(x = BTC$LogRet))
p+geom_histogram() + geom_density()
```

    ## Warning: Use of `BTC$LogRet` is discouraged. Use `LogRet` instead.
    ## Use of `BTC$LogRet` is discouraged. Use `LogRet` instead.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../images/unnamed-chunk-19-1.png)<!-- -->

``` r
p2 <- ggplot(data = ETH, aes(x = LogRet))
p2+geom_histogram() + geom_density()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../images/unnamed-chunk-19-2.png)<!-- -->

``` r
p3 <- ggplot(data = SPY, aes(x = LogRet))
p3+geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../images/unnamed-chunk-19-3.png)<!-- -->

``` r
p4 <- ggplot(data = VOO, aes(x = LogRet))
p4+geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../images/unnamed-chunk-19-4.png)<!-- -->

## Boxplots: SPY, VOO, BTC, ETH

Below are boxplots that assess quarterly performance of Close Prices.
The closing price of a stock is extremely useful because it allows
investors to assess the changes in stock prices over time. This will
provide a visual on which quarters are deemed the most “expensive” in
terms of the Close Price.

First, the boxplot for the S&P 500 shows how volatile Q2 was for the
Close Price which can be related to how poor the average Log Returns
were for that quarter. That plot shows a very large spread compared to
the others. Conversely, the boxplot for Q3 has a tight amount of spread,
indicating that this quarter’s Close Price did not vary much.

Second, the boxplot for the Vanguard 500 shows the volatility in Close
Price for Q2 and Q3 where the plots depict large amounts of spread.
Also, we see that Q4 shows the highest Close Prices like we analyzed
above.

For Bitcoin, Q3 and Q4 shows large amounts of sprad and skew in terms of
Close Price. Whereas Q1 has a very tight plot with little spread.

For Ethereum, Q2 and Q3 show large amonts of spread and some skew.
Whereas Q1 and Q4 are quite similar and more consistent

``` r
bp1 <- ggplot(SPY, aes(x = Quarters, y = Close, fill = Quarters))
bp1 + geom_boxplot() + geom_jitter() + ggtitle("SPY Boxplot")
```

![](../images/unnamed-chunk-20-1.png)<!-- -->

``` r
bp2 <- ggplot(ETH, aes(x = Quarters, y = Close, fill = Quarters))
bp2 + geom_boxplot() + geom_jitter() + ggtitle("VOO Boxplot")
```

![](../images/unnamed-chunk-20-2.png)<!-- -->

``` r
bp3 <- ggplot(BTC, aes(x = Quarters, y = Close, fill = Quarters))
bp3 + geom_boxplot() + geom_jitter() + ggtitle("Bitcoin Boxplot")
```

![](../images/unnamed-chunk-20-3.png)<!-- -->

``` r
bp4 <- ggplot(ETH, aes(x = Quarters, y = Close, fill = Quarters))
bp4 + geom_boxplot() + geom_jitter() + ggtitle("Ethereum Boxplot")
```

![](../images/unnamed-chunk-20-4.png)<!-- -->

## Time Series Graph for BTC, ETH

``` r
## time series graph for SPY
ts.spy <- ggplot(data = SPY, aes(x=Nobs))
ts.spy + geom_line(aes(y=LogRet), color = "purple") + 
         ggtitle("Time Series Plot on Log Returns for S&P 500") +
        theme(axis.text.x = element_text(vjust=0.5)) +
        theme(legend.position="none")
```

![](../images/unnamed-chunk-21-1.png)<!-- -->

``` r
## time series graph for VOO
ts.voo <- ggplot(data = VOO, aes(x = Nobs))
ts.voo + geom_line(aes(y=LogRet), color = "darkblue") + 
        ggtitle("Time Series Plot on Log Returns for Vanguard 500") +
        theme(axis.text.x = element_text(vjust=0.5)) +
        theme(legend.position="none")
```

![](../images/unnamed-chunk-21-2.png)<!-- -->

``` r
## time series graph for BTC
ts.btc <- ggplot(data = BTC, aes(x=Date)) 
ts.btc + geom_line(aes(y=LogRet), color = "red") + ggtitle("Time Series Plot on Log Returns for Bitcoin") +
        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
        theme(legend.position="none")
```

![](../images/unnamed-chunk-21-3.png)<!-- -->

``` r
## time series graph for ETH
ts.eth <- ggplot(data = ETH, aes(x=Date)) 
ts.eth + geom_line(aes(y=LogRet), color = "blue") + ggtitle("Time Series Plot on Log Returns for Ethereum") +
        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
        theme(legend.position="none")
```

![](../images/unnamed-chunk-21-4.png)<!-- -->

Here, we notice that ETH is much nosier than Bitcoin.

spy and voo graphs

``` r
t <- ggplot(data = SPY, aes(x = SPY$Nobs))
t + geom_point(aes(y = SPY$Close)) 
```

    ## Warning: Use of `SPY$Close` is discouraged. Use `Close` instead.

    ## Warning: Use of `SPY$Nobs` is discouraged. Use `Nobs` instead.

![](../images/unnamed-chunk-22-1.png)<!-- -->

``` r
SPY
```

    ##        Volume WeightedVolume    Open  Close HighestPrice LowestPrice
    ## 1    51671109       424.5186 425.420 424.48     425.4600    423.5400
    ## 2    79931518       422.4890 424.630 422.11     424.8700    419.9200
    ## 3    91117086       421.8919 421.670 421.97     423.0200    419.3200
    ## 4   118528545       416.8030 417.090 414.92     417.8281    414.7000
    ## 5    72920873       419.2599 416.800 420.86     421.0600    415.9300
    ## 6    56977011       422.2165 420.850 423.11     424.0000    420.0800
    ## 7    49240348       423.1750 423.190 422.60     424.0500    422.5100
    ## 8    45246597       424.8872 424.890 425.10     425.5500    424.6200
    ## 9    58240132       426.2099 425.900 426.61     427.0943    425.5500
    ## 10   53354729       426.8197 427.170 427.47     427.6500    425.8900
    ## 11   35873804       427.8444 427.880 427.70     428.5600    427.1300
    ## 12   64517207       428.0337 427.210 428.06     428.7800    427.1800
    ## 13   53657782       429.7116 428.870 430.43     430.6000    428.8000
    ## 14   57852713       432.8409 431.670 433.72     434.1000    431.3300
    ## 15   68729063       432.1618 433.780 432.93     434.0100    430.0100
    ## 16   63714583       433.7315 433.660 434.46     434.7600    431.5100
    ## 17   97804861       430.1577 428.780 430.92     431.7300    427.5200
    ## 18   76378977       434.4412 432.530 435.52     435.8400    432.3100
    ## 19   52449266       436.5282 435.430 437.08     437.3500    434.9700
    ## 20   52790461       436.3838 436.240 435.59     437.8400    435.3100
    ## 21   64286151       436.3153 437.400 436.24     437.9200    434.9100
    ## 22   55212984       434.4532 434.810 434.75     435.5300    432.7200
    ## 23   72752672       433.2107 436.010 431.34     436.0600    430.9200
    ## 24  147932133       424.4345 426.190 424.97     431.4089    421.9700
    ## 25   99784841       429.6423 425.680 431.06     432.4200    424.8300
    ## 26   64904513       433.4252 432.340 434.55     434.7000    431.0111
    ## 27   47963026       434.8862 434.740 435.46     435.7200    433.6900
    ## 28   63950573       438.5210 437.520 439.94     440.3000    436.7900
    ## 29   43875088       440.1950 439.310 441.02     441.0300    439.2600
    ## 30   67539148       438.2311 439.910 439.01     439.9400    435.9900
    ## 31   50894998       438.9868 439.680 438.83     440.3000    437.3100
    ## 32   47397842       440.8157 439.815 440.65     441.8000    439.8100
    ## 33   68951202       438.8069 437.910 438.51     440.0600    437.7700
    ## 34   58399297       438.9469 440.340 437.59     440.9300    437.2100
    ## 35   57738850       439.1748 438.440 441.15     441.2800    436.1000
    ## 36   46706212       439.7050 439.780 438.98     441.1243    438.7300
    ## 37   38964559       440.6863 440.220 441.76     441.8500    439.8800
    ## 38   45812008       442.3052 442.100 442.49     442.9400    441.8000
    ## 39   41222585       442.2061 442.460 442.13     442.7950    441.3100
    ## 40   42975851       442.5943 442.610 442.68     443.4400    441.8800
    ## 41   43749190       443.3916 443.820 443.78     443.8820    442.6200
    ## 42   38942418       444.1853 443.620 445.11     445.2572    442.6600
    ## 43   39441451       445.5441 445.590 445.92     445.9400    445.0700
    ## 44   73930864       445.2783 444.530 446.97     447.1100    442.8700
    ## 45   92651433       443.5632 444.240 444.04     444.9600    440.8500
    ## 46   89351931       441.7939 442.960 439.18     444.6250    438.9200
    ## 47   92689386       439.0990 436.270 439.86     441.1400    436.1200
    ## 48   71819112       442.2624 440.230 443.36     443.7100    439.7100
    ## 49   54968538       446.7581 445.160 447.26     448.2300    443.4355
    ## 50   38721714       447.9647 447.970 447.97     448.5400    447.4200
    ## 51   40529711       448.7852 448.170 448.91     449.4570    447.7700
    ## 52   57710772       447.2380 448.610 446.26     448.8600    446.1600
    ## 53   77056613       449.4451 447.120 450.25     450.6500    447.0600
    ## 54   48357355       452.0586 450.970 452.23     453.0700    450.7100
    ## 55   58987985       451.7938 452.130 451.56     452.4900    450.9200
    ## 56   48580378       452.2969 452.560 451.80     453.1100    451.5450
    ## 57   40912367       452.9232 453.320 453.19     454.0500    451.9100
    ## 58   47231364       452.9032 451.980 453.08     453.6300    451.5500
    ## 59   51921318       451.8342 452.710 451.46     452.8100    450.7423
    ## 60   56147907       450.5382 450.890 450.91     451.6700    448.8600
    ## 61   57935207       450.2357 450.700 448.98     452.5700    448.7200
    ## 62   89465874       447.6791 451.040 445.44     451.4900    445.3100
    ## 63   83969944       446.0005 448.640 446.58     448.9200    444.1100
    ## 64   79344873       444.9748 448.120 444.17     448.3400    443.2200
    ## 65   78937824       446.2478 444.620 447.88     448.4100    443.4400
    ## 66   77209185       446.5427 447.320 447.17     448.3600    444.0200
    ## 67  118047787       442.6771 444.920 441.40     445.3700    441.0200
    ## 68  165478034       433.6052 434.880 434.04     436.5600    428.8600
    ## 69   92357112       435.1409 436.530 433.63     437.9100    433.0700
    ## 70  101480143       437.8969 436.050 437.86     440.0300    433.7470
    ## 71   76365981       442.9113 439.850 443.18     444.8900    439.6000
    ## 72   61994837       443.3184 441.440 443.91     444.6700    441.2100
    ## 73   60867583       443.0258 442.810 442.64     444.0500    441.9000
    ## 74  130350306       435.6609 439.690 433.72     440.0400    432.9400
    ## 75   82058963       435.3033 435.190 434.45     437.0400    433.8500
    ## 76  140598198       432.0754 436.020 429.14     436.7700    428.7800
    ## 77  129107301       431.7327 430.980 434.24     436.0329    427.2300
    ## 78  128346956       429.0805 433.000 428.64     433.9600    426.3600
    ## 79   90682519       433.1342 430.240 433.10     435.4900    429.3900
    ## 80  112977345       431.7015 429.270 434.90     435.1199    427.5400
    ## 81   72335949       439.6760 438.390 438.66     441.6800    438.2000
    ## 82   74665304       438.3975 439.480 437.86     439.8900    437.1900
    ## 83   65233283       437.1387 437.160 434.69     440.2600    434.6200
    ## 84   71170063       434.3746 435.670 433.62     436.1000    432.7800
    ## 85   72761979       434.0766 434.710 435.18     436.0500    431.5400
    ## 86   67509325       441.0021 439.080 442.50     442.6600    438.5800
    ## 87   66145960       445.1634 444.750 445.87     446.2600    444.0900
    ## 88   62213228       446.2843 443.970 447.19     447.5500    443.2700
    ## 89   46957827       449.8321 448.920 450.64     450.7100    448.2700
    ## 90   49571569       451.9120 451.130 452.41     452.7320    451.0100
    ## 91   41055438       452.4889 451.770 453.59     453.8290    451.3100
    ## 92   58745088       453.0362 453.130 453.12     454.6700    451.0500
    ## 93   45173607       454.6282 454.280 455.55     455.9000    452.3900
    ## 94   55858916       456.8677 457.200 455.96     458.4900    455.5600
    ## 95   72215997       455.2170 456.450 453.94     457.1600    453.8600
    ## 96   51412959       456.8443 455.460 458.32     458.4000    455.4500
    ## 97   70147024       458.3068 455.870 459.25     459.5600    455.5600
    ## 98   48433642       459.4617 460.300 460.04     460.7021    458.2000
    ## 99   48504417       461.3643 460.220 461.90     462.2300    460.0800
    ## 100  52507426       463.0092 461.300 464.72     465.1500    460.8300
    ## 101  52842687       465.9631 465.360 466.91     467.0000    464.9900
    ## 102  66100363       468.8804 469.280 468.53     470.6500    466.9200
    ## 103  50192592       468.9905 469.700 468.93     470.2300    468.2031
    ## 104  51132147       467.3029 469.320 467.38     469.5700    465.8800
    ## 105  69429644       464.8782 465.580 463.62     467.3800    462.0400
    ## 106  34848494       464.3668 465.210 463.77     465.2900    463.7500
    ## 107  53466654       466.3091 465.120 467.27     467.8600    464.1100
    ## 108  46967500       467.3737 468.640 467.43     468.8100    466.2300
    ## 109  48857502       469.0457 467.150 469.28     470.4850    467.0700
    ## 110  47158289       468.3700 469.000 468.14     469.1900    467.4800
    ## 111  50625607       468.7785 469.240 469.73     470.0100    466.3400
    ## 112  54378573       469.4411 469.610 468.89     470.9400    468.5000
    ## 113  72491954       470.4350 470.890 467.57     473.5400    467.3500
    ## 114  73206538       466.9913 467.220 468.19     469.0950    464.4500
    ## 115  61508813       468.0542 466.060 469.44     469.5700    465.1900
    ## 116 112669635       461.2786 462.340 458.97     463.9000    457.7700
    ## 117  86240818       464.0689 464.070 464.60     466.5600    461.7300
    ## 118 148559598       458.8431 462.000 455.56     464.0300    455.3000
    ## 119 132414435       457.4152 461.640 450.50     464.6700    450.2900
    ## 120 127637758       455.7043 450.730 457.40     459.0700    450.3100
    ## 121 137331647       453.0439 459.170 453.42     460.3000    448.9200
    ## 122  97477532       457.5764 456.130 458.79     460.7900    453.5600
    ## 123  95467688       466.8130 464.410 468.28     468.8800    458.6546
    ## 124  71609277       468.5873 468.700 469.52     470.0000    466.8300
    ## 125  61272568       467.9605 468.150 466.35     469.6291    466.1400
    ## 126  77128507       468.9296 469.230 470.74     470.9000    466.5100
    ## 127  87724680       468.0599 470.190 466.57     470.5600    466.2700
    ## 128  97159128       463.1988 463.090 463.36     465.7400    460.2500
    ## 129 116899251       465.8393 463.420 470.60     470.8600    460.7400
    ## 130 116494626       468.7097 472.570 466.45     472.8700    464.8000
    ## 131 135583150       461.5026 461.550 459.87     464.7400    458.0600
    ## 132 106788817       454.1844 454.480 454.98     455.4000    451.1400
    ## 133  69806260       460.2953 458.610 463.06     463.2100    456.3100
    ## 134  58790223       465.8010 462.790 467.69     467.8100    462.5800
    ## 135  56139745       470.6108 468.750 470.60     472.1900    468.6400
    ## 136  56808619       475.2790 472.060 477.26     477.3100    472.0100
    ## 137  46974585       477.2276 477.720 476.87     478.8100    476.0600
    ## 138  54091464       477.2659 476.980 477.48     478.5600    475.9200
    ## 139  55329041       477.4587 477.930 476.16     479.0000    475.6700
    ## 140  64917431       475.6196 475.640 474.96     476.8600    474.6700
    ## 141  72668233       476.5270 476.300 477.71     477.8500    473.8500
    ## 142  71070678       477.8703 479.220 477.55     479.9800    475.5800
    ## 143 104494940       473.2328 477.160 468.38     477.9800    468.2801
    ## 144  86498500       468.4813 467.890 467.94     470.8200    465.4300
    ## 145  85111593       466.9390 467.950 466.09     469.2000    464.6500
    ## 146 119361988       461.5904 462.700 465.51     465.7400    456.5973
    ## 147  74189562       466.4853 465.230 469.75     469.8500    462.0500
    ## 148  67602444       470.9955 471.590 471.02     473.2000    468.9400
    ## 149  91137601       467.9368 472.190 464.53     472.8800    463.4400
    ## 150  95890948       462.9488 461.190 464.72     465.0900    459.9000
    ## 151 109851226       457.9682 459.740 456.49     459.9600    455.3100
    ## 152 108347069       455.5262 458.130 451.75     459.6124    451.4600
    ## 153 122001796       452.3944 453.750 446.75     458.7400    444.5000
    ## 154 202025141       442.5437 445.560 437.98     448.0600    437.9500
    ## 155 252248938       430.7247 432.030 439.84     440.3800    420.7600
    ## 156 167685150       433.6947 433.060 434.47     439.7200    427.1500
    ## 157 186240418       437.1371 440.720 433.38     444.0400    428.8600
    ## 158 149846107       434.5969 438.260 431.24     441.5900    429.4500
    ## 159 164188170       435.3435 432.680 441.95     442.0000    427.8200
    ## 160 152106025       445.9021 441.240 449.91     450.2800    439.8100
    ## 161 123155381       450.7136 450.680 452.95     453.6300    446.9384
    ## 162 116776953       455.6624 455.500 457.35     458.1200    453.0500
    ## 163 118024442       449.8270 450.950 446.60     452.9700    445.7100
    ## 164 118436652       448.3226 446.350 448.70     452.7800    443.8300
    ## 165  84472859       448.6198 449.510 447.26     450.9900    445.8500
    ## 166  80981286       449.0816 446.730 450.94     451.9200    445.2200
    ## 167  92495929       455.9303 455.220 457.54     457.8800    455.0050
    ## 168 140103712       452.9121 451.340 449.32     457.7100    447.2000
    ## 169 153214597       444.1699 449.410 440.46     451.6050    438.9400
    ## 170 122934262       438.9049 439.920 439.02     441.6000    435.3400
    ## 171  88641503       444.6794 443.730 446.10     446.2800    443.1800
    ## 172  84815085       444.9697 443.930 446.60     448.0550    441.9400
    ## 173 102259108       439.9789 443.220 437.06     446.5652    436.4200
    ## 174 132502896       435.2278 437.330 434.23     438.6600    431.8200
    ## 175 124379870       430.7070 431.890 429.57     435.5000    425.8600
    ## 176 132374005       425.9146 432.660 421.95     433.2600    421.3500
    ## 177 213923446       419.7499 411.020 428.30     428.7600    410.6400
    ## 178 121785659       434.3466 429.610 437.75     437.8400    427.8600
    ## 179 145594529       434.5719 432.030 436.63     438.2000    430.7000
    ## 180 137720158       431.6669 435.040 429.98     437.1700    427.1100
    ## 181 117711825       436.2299 432.370 437.89     439.7200    431.5700
    ## 182 105501715       436.9085 440.470 435.71     441.1100    433.8000
    ## 183 114083256       430.9209 431.750 432.17     433.3700    427.8800
    ## 184 137811760       423.9601 431.550 419.43     432.3018    419.3600
    ## 185 164739744       419.7630 419.620 416.25     427.2100    415.1200
    ## 186 116832265       426.1810 425.140 427.41     429.5100    422.8200
    ## 187  93972655       423.8766 422.520 425.48     426.4300    420.4400
    ## 188  95360277       423.9476 428.120 420.07     428.7700    419.5300
    ## 189  95729188       419.1776 420.890 417.00     424.5500    415.7900
    ## 190 106134117       422.7610 419.770 426.17     426.8400    418.4200
    ## 191 144704805       431.3891 429.890 435.62     435.6800    424.8000
    ## 192 102651870       437.6986 433.590 441.07     441.0700    433.1900
    ## 193 106332044       441.5188 438.000 444.52     444.8600    437.2200
    ## 194  88249756       443.8006 444.340 444.39     446.4600    440.6800
    ## 195  74280394       448.5657 445.860 449.59     450.5800    445.8600
    ## 196  79367033       446.0537 446.910 443.80     448.4900    443.7100
    ## 197  64736892       447.7172 445.940 450.49     450.5000    444.7600
    ## 198  76738724       451.0456 451.160 452.69     452.9800    448.4300
    ## 199  68289867       453.2845 452.060 455.91     455.9100    450.0600
    ## 200  86556542       459.6370 460.020 461.55     462.0700    457.1800
    ## 201  79155242       459.1689 460.340 458.70     461.1950    456.4650
    ## 202 121083948       455.3757 457.890 451.64     458.7600    451.1600
    ## 203  88973773       451.8507 453.310 452.92     453.4600    449.1400
    ## 204  59504500       455.3110 453.130 456.80     456.9100    452.2600
    ## 205  74213133       453.4101 455.220 451.03     457.8300    449.8200
    ## 206 106844970       446.3477 446.890 446.52     448.9300    443.4700
    ## 207  78097214       447.1117 445.590 448.77     450.6900    443.5300
    ## 208  79205711       448.2497 447.970 447.57     450.6300    445.9400
    ## 209  89770538       442.2088 444.110 439.92     445.0000    439.3900
    ## 210  84363635       440.8142 443.080 438.29     445.7500    436.6501
    ## 211  73950154       441.2623 438.030 443.31     444.1100    437.8400
    ## 212  97869451       440.2705 443.550 437.79     444.7301    437.6800
    ## 213  64950498       437.8417 436.810 437.97     439.7500    435.6100
    ## 214  77784013       443.0733 437.860 445.04     445.8000    437.6800
    ## 215  65210049       445.6116 446.920 444.71     447.5700    443.4800
    ## 216  85417327       442.7637 448.540 438.06     450.0100    437.1000
    ## 217 132371772       429.8197 436.910 426.04     438.0825    425.4400
    ## 218 119636648       423.9628 423.670 428.51     428.6900    418.8400
    ## 219 103996312       420.0383 425.830 416.10     426.0400    416.0700
    ## 220 121980990       418.8355 417.240 417.27     422.9200    415.0100
    ## 221 105375338       424.0568 422.290 427.81     429.6400    417.6000
    ## 222 145311088       416.8611 423.590 412.00     425.8700    411.2100
    ## 223 158312526       411.1530 412.070 414.48     415.9200    405.0200
    ## 224 100013490       416.1059 415.010 416.38     418.9300    413.3600
    ## 225 144247895       421.2666 417.080 429.06     429.6600    413.7099
    ## 226 172929106       415.4351 424.550 413.81     425.0000    409.4400
    ## 227 151746311       410.5178 411.100 411.34     414.8000    405.7300
    ## 228 155571067       401.9209 405.100 398.17     406.4100    396.5000
    ## 229 132497200       399.8401 404.490 399.09     406.0800    394.8200
    ## 230 142360992       397.4380 398.070 392.75     404.0400    391.9600
    ## 231 125090753       389.9946 389.370 392.34     395.8000    385.1500
    ## 232 104060921       399.8724 396.710 401.72     403.1800    395.6100
    ## 233  78582794       400.6442 399.980 400.09     403.9700    397.6000
    ## 234  82720710       405.7484 406.530 408.32     408.5700    402.5824
    ## 235 117669645       396.0277 403.500 391.86     403.8000    390.5500
    ## 236  98477715       390.3546 388.620 389.46     394.1400    387.1100
    ## 237 131137197       387.0485 393.250 389.63     397.0300    380.5400
    ## 238  76371878       395.1774 392.830 396.92     397.7300    390.3800
    ## 239  91380131       391.5536 392.560 393.89     395.1500    386.9600
    ## 240  91397866       395.7817 392.310 397.37     399.4500    391.8900
    ## 241  82123839       404.2641 398.670 405.31     407.0400    398.4500
    ## 242  84769855       412.4828 407.910 415.26     415.3801    407.7000
    ## 243  95936980       413.3975 413.550 412.93     416.4600    410.0300
    ## 244  86565813       410.7886 415.170 409.59     416.2400    406.9300
    ## 245  79409633       413.3725 409.420 417.39     417.4400    407.0400
    ## 246  71874281       411.8587 412.400 410.54     414.0400    409.5100
    ## 247  57503038       413.0568 414.780 411.79     416.6090    410.5523
    ## 248  59259373       412.9883 408.100 415.74     416.2200    407.6100
    ## 249  64349966       412.7375 413.930 411.22     415.8200    410.3800
    ## 250  86249796       406.8426 409.340 401.44     411.7400    401.4400
    ## 251 131723864       392.8831 394.880 389.80     395.7777    389.7500
    ## 252 169975861       378.6775 379.850 375.00     381.8100    373.3000
    ## 253 104007431       374.3226 376.850 373.87     377.9400    370.5900
    ## 254 125666797       378.0522 377.360 379.20     383.9000    372.1200
    ##     UnixTimes(msec) NumTransactions        LogRet Symbol Nobs Quarters
    ## 1      1.623730e+12          301990  0.0000000000    SPY    1       Q2
    ## 2      1.623816e+12          649279 -0.0055989468    SPY    2       Q2
    ## 3      1.623902e+12          571245 -0.0003317221    SPY    3       Q2
    ## 4      1.623989e+12          718016 -0.0168484909    SPY    4       Q2
    ## 5      1.624248e+12          453658  0.0142145062    SPY    5       Q2
    ## 6      1.624334e+12          351117  0.0053319557    SPY    6       Q2
    ## 7      1.624421e+12          291116 -0.0012060873    SPY    7       Q2
    ## 8      1.624507e+12          292709  0.0058983302    SPY    8       Q2
    ## 9      1.624594e+12          330093  0.0035458116    SPY    9       Q2
    ## 10     1.624853e+12          296632  0.0020138636    SPY   10       Q2
    ## 11     1.624939e+12          249144  0.0005379048    SPY   11       Q2
    ## 12     1.625026e+12          316131  0.0008413574    SPY   12       Q2
    ## 13     1.625112e+12          316827  0.0055213363    SPY   13       Q3
    ## 14     1.625198e+12          313148  0.0076144556    SPY   14       Q3
    ## 15     1.625544e+12          399106 -0.0018231125    SPY   15       Q3
    ## 16     1.625630e+12          390379  0.0035278285    SPY   16       Q3
    ## 17     1.625717e+12          648986 -0.0081814226    SPY   17       Q3
    ## 18     1.625803e+12          394502  0.0106182614    SPY   18       Q3
    ## 19     1.626062e+12          345868  0.0035755252    SPY   19       Q3
    ## 20     1.626149e+12          365362 -0.0034148107    SPY   20       Q3
    ## 21     1.626235e+12          357507  0.0014911167    SPY   21       Q3
    ## 22     1.626322e+12          421957 -0.0034213974    SPY   22       Q3
    ## 23     1.626408e+12          457243 -0.0078745110    SPY   23       Q3
    ## 24     1.626667e+12         1020165 -0.0148780640    SPY   24       Q3
    ## 25     1.626754e+12          591627  0.0142287134    SPY   25       Q3
    ## 26     1.626840e+12          346365  0.0080637213    SPY   26       Q3
    ## 27     1.626926e+12          315207  0.0020919307    SPY   27       Q3
    ## 28     1.627013e+12          376771  0.0102354104    SPY   28       Q3
    ## 29     1.627272e+12          283529  0.0024518719    SPY   29       Q3
    ## 30     1.627358e+12          492000 -0.0045680340    SPY   30       Q3
    ## 31     1.627445e+12          391860 -0.0004100975    SPY   31       Q3
    ## 32     1.627531e+12          303009  0.0041388152    SPY   32       Q3
    ## 33     1.627618e+12          427130 -0.0048682930    SPY   33       Q3
    ## 34     1.627877e+12          409572 -0.0021002176    SPY   34       Q3
    ## 35     1.627963e+12          412417  0.0081025547    SPY   35       Q3
    ## 36     1.628050e+12          337471 -0.0049310997    SPY   36       Q3
    ## 37     1.628136e+12          259461  0.0063128942    SPY   37       Q3
    ## 38     1.628222e+12          283934  0.0016511171    SPY   38       Q3
    ## 39     1.628482e+12          242109 -0.0008139088    SPY   39       Q3
    ## 40     1.628568e+12          266361  0.0012432049    SPY   40       Q3
    ## 41     1.628654e+12          274051  0.0024817827    SPY   41       Q3
    ## 42     1.628741e+12          261155  0.0029924985    SPY   42       Q3
    ## 43     1.628827e+12          234209  0.0018181211    SPY   43       Q3
    ## 44     1.629086e+12          407360  0.0023519145    SPY   44       Q3
    ## 45     1.629173e+12          594093 -0.0065768298    SPY   45       Q3
    ## 46     1.629259e+12          510458 -0.0110052966    SPY   46       Q3
    ## 47     1.629346e+12          682355  0.0015471426    SPY   47       Q3
    ## 48     1.629432e+12          410210  0.0079255867    SPY   48       Q3
    ## 49     1.629691e+12          322046  0.0087579999    SPY   49       Q3
    ## 50     1.629778e+12          251170  0.0015861849    SPY   50       Q3
    ## 51     1.629864e+12          251660  0.0020961563    SPY   51       Q3
    ## 52     1.629950e+12          441246 -0.0059206804    SPY   52       Q3
    ## 53     1.630037e+12          395689  0.0089012422    SPY   53       Q3
    ## 54     1.630296e+12          297857  0.0043879159    SPY   54       Q3
    ## 55     1.630382e+12          336320 -0.0014826456    SPY   55       Q3
    ## 56     1.630469e+12          334755  0.0005313496    SPY   56       Q3
    ## 57     1.630555e+12          290617  0.0030718596    SPY   57       Q3
    ## 58     1.630642e+12          309749 -0.0002427533    SPY   58       Q3
    ## 59     1.630987e+12          352112 -0.0035819350    SPY   59       Q3
    ## 60     1.631074e+12          414394 -0.0012190123    SPY   60       Q3
    ## 61     1.631160e+12          411806 -0.0042894197    SPY   61       Q3
    ## 62     1.631246e+12          528437 -0.0079157856    SPY   62       Q3
    ## 63     1.631506e+12          595851  0.0025559979    SPY   63       Q3
    ## 64     1.631592e+12          513360 -0.0054111836    SPY   64       Q3
    ## 65     1.631678e+12          478421  0.0083179674    SPY   65       Q3
    ## 66     1.631765e+12          494053 -0.0015865039    SPY   66       Q3
    ## 67     1.631851e+12          719238 -0.0129873417    SPY   67       Q3
    ## 68     1.632110e+12         1162776 -0.0168147981    SPY   68       Q3
    ## 69     1.632197e+12          688757 -0.0009450598    SPY   69       Q3
    ## 70     1.632283e+12          775245  0.0097075887    SPY   70       Q3
    ## 71     1.632370e+12          560385  0.0120767835    SPY   71       Q3
    ## 72     1.632456e+12          407156  0.0016458311    SPY   72       Q3
    ## 73     1.632715e+12          426386 -0.0028650406    SPY   73       Q3
    ## 74     1.632802e+12          948210 -0.0203576340    SPY   74       Q3
    ## 75     1.632888e+12          598061  0.0016816987    SPY   75       Q3
    ## 76     1.632974e+12          928225 -0.0122976573    SPY   76       Q3
    ## 77     1.633061e+12          852551  0.0118141706    SPY   77       Q3
    ## 78     1.633320e+12          940502 -0.0129799709    SPY   78       Q3
    ## 79     1.633406e+12          595529  0.0103512424    SPY   79       Q3
    ## 80     1.633493e+12          828088  0.0041474714    SPY   80       Q3
    ## 81     1.633579e+12          524649  0.0086085059    SPY   81       Q3
    ## 82     1.633666e+12          488344 -0.0018254010    SPY   82       Q3
    ## 83     1.633925e+12          435654 -0.0072660912    SPY   83       Q3
    ## 84     1.634011e+12          492212 -0.0024645588    SPY   84       Q3
    ## 85     1.634098e+12          513022  0.0035911641    SPY   85       Q3
    ## 86     1.634184e+12          451179  0.0166807258    SPY   86       Q3
    ## 87     1.634270e+12          417630  0.0075869653    SPY   87       Q3
    ## 88     1.634530e+12          390540  0.0029561305    SPY   88       Q4
    ## 89     1.634616e+12          376977  0.0076852344    SPY   89       Q4
    ## 90     1.634702e+12          371072  0.0039200537    SPY   90       Q4
    ## 91     1.634789e+12          315667  0.0026048580    SPY   91       Q4
    ## 92     1.634875e+12          494968 -0.0010367152    SPY   92       Q4
    ## 93     1.635134e+12          346367  0.0053484891    SPY   93       Q4
    ## 94     1.635221e+12          459682  0.0008996062    SPY   94       Q4
    ## 95     1.635307e+12          442869 -0.0044400557    SPY   95       Q4
    ## 96     1.635394e+12          361417  0.0096025994    SPY   96       Q4
    ## 97     1.635480e+12          436007  0.0020270940    SPY   97       Q4
    ## 98     1.635739e+12          408110  0.0017187181    SPY   98       Q4
    ## 99     1.635826e+12          342315  0.0040349752    SPY   99       Q4
    ## 100    1.635912e+12          427380  0.0060866562    SPY  100       Q4
    ## 101    1.635998e+12          386856  0.0047014459    SPY  101       Q4
    ## 102    1.636085e+12          494032  0.0034636142    SPY  102       Q4
    ## 103    1.636348e+12          354640  0.0008533698    SPY  103       Q4
    ## 104    1.636434e+12          461988 -0.0033108723    SPY  104       Q4
    ## 105    1.636520e+12          556473 -0.0080773801    SPY  105       Q4
    ## 106    1.636607e+12          282338  0.0003234885    SPY  106       Q4
    ## 107    1.636693e+12          344554  0.0075185094    SPY  107       Q4
    ## 108    1.636952e+12          372696  0.0003423558    SPY  108       Q4
    ## 109    1.637039e+12          328010  0.0039500003    SPY  109       Q4
    ## 110    1.637125e+12          327913 -0.0024322087    SPY  110       Q4
    ## 111    1.637212e+12          368768  0.0033906651    SPY  111       Q4
    ## 112    1.637298e+12          355189 -0.0017898622    SPY  112       Q4
    ## 113    1.637557e+12          542126 -0.0028191292    SPY  113       Q4
    ## 114    1.637644e+12          563874  0.0013251263    SPY  114       Q4
    ## 115    1.637730e+12          436744  0.0026662985    SPY  115       Q4
    ## 116    1.637903e+12          785687 -0.0225556465    SPY  116       Q4
    ## 117    1.638162e+12          571696  0.0121919719    SPY  117       Q4
    ## 118    1.638248e+12         1145807 -0.0196493889    SPY  118       Q4
    ## 119    1.638335e+12         1039069 -0.0111693544    SPY  119       Q4
    ## 120    1.638421e+12         1022777  0.0152002045    SPY  120       Q4
    ## 121    1.638508e+12         1234209 -0.0087394333    SPY  121       Q4
    ## 122    1.638767e+12          765876  0.0117737408    SPY  122       Q4
    ## 123    1.638853e+12          640751  0.0204738186    SPY  123       Q4
    ## 124    1.638940e+12          507901  0.0026444886    SPY  124       Q4
    ## 125    1.639026e+12          489126 -0.0067744711    SPY  125       Q4
    ## 126    1.639112e+12          513526  0.0093694994    SPY  126       Q4
    ## 127    1.639372e+12          615392 -0.0088978620    SPY  127       Q4
    ## 128    1.639458e+12          627255 -0.0069037729    SPY  128       Q4
    ## 129    1.639544e+12          882924  0.0155041865    SPY  129       Q4
    ## 130    1.639631e+12          820646 -0.0088576429    SPY  130       Q4
    ## 131    1.639717e+12          889475 -0.0142069926    SPY  131       Q4
    ## 132    1.639976e+12          807669 -0.0106903789    SPY  132       Q4
    ## 133    1.640063e+12          619585  0.0176031734    SPY  133       Q4
    ## 134    1.640149e+12          454693  0.0099490480    SPY  134       Q4
    ## 135    1.640236e+12          381386  0.0062027930    SPY  135       Q4
    ## 136    1.640581e+12          380197  0.0140529395    SPY  136       Q4
    ## 137    1.640668e+12          372331 -0.0008174987    SPY  137       Q4
    ## 138    1.640754e+12          345712  0.0012783572    SPY  138       Q4
    ## 139    1.640840e+12          353567 -0.0027683420    SPY  139       Q4
    ## 140    1.640927e+12          435448 -0.0025233422    SPY  140       Q4
    ## 141    1.641186e+12          535421  0.0057732639    SPY  141       Q4
    ## 142    1.641272e+12          565655 -0.0003349873    SPY  142       Q4
    ## 143    1.641359e+12          788712 -0.0193889342    SPY  143       Q4
    ## 144    1.641445e+12          806488 -0.0009398497    SPY  144       Q4
    ## 145    1.641532e+12          625067 -0.0039613340    SPY  145       Q2
    ## 146    1.641791e+12          963294 -0.0012451698    SPY  146       Q2
    ## 147    1.641877e+12          626792  0.0090670595    SPY  147       Q2
    ## 148    1.641964e+12          632057  0.0026999177    SPY  148       Q2
    ## 149    1.642050e+12          724347 -0.0138744142    SPY  149       Q2
    ## 150    1.642136e+12          855904  0.0004089319    SPY  150       Q2
    ## 151    1.642482e+12          911795 -0.0178682797    SPY  151       Q2
    ## 152    1.642568e+12          880787 -0.0104378645    SPY  152       Q2
    ## 153    1.642655e+12          986090 -0.0111297754    SPY  153       Q2
    ## 154    1.642741e+12         1885192 -0.0198259068    SPY  154       Q2
    ## 155    1.643000e+12         3263011  0.0042377772    SPY  155       Q1
    ## 156    1.643087e+12         1832295 -0.0122841270    SPY  156       Q1
    ## 157    1.643173e+12         2620794 -0.0025119562    SPY  157       Q1
    ## 158    1.643260e+12         1837412 -0.0049501616    SPY  158       Q1
    ## 159    1.643346e+12         1759648  0.0245319738    SPY  159       Q1
    ## 160    1.643605e+12         1132217  0.0178508093    SPY  160       Q1
    ## 161    1.643692e+12         1094848  0.0067341814    SPY  161       Q1
    ## 162    1.643778e+12          998599  0.0096672180    SPY  162       Q1
    ## 163    1.643864e+12         1106476 -0.0237856227    SPY  163       Q1
    ## 164    1.643951e+12         1162845  0.0046911736    SPY  164       Q1
    ## 165    1.644210e+12          759934 -0.0032144320    SPY  165       Q1
    ## 166    1.644296e+12          685659  0.0081942120    SPY  166       Q1
    ## 167    1.644383e+12          670714  0.0145300196    SPY  167       Q1
    ## 168    1.644469e+12         1217132 -0.0181289838    SPY  168       Q1
    ## 169    1.644556e+12         1467422 -0.0199156934    SPY  169       Q1
    ## 170    1.644815e+12         1148020 -0.0032746652    SPY  170       Q1
    ## 171    1.644901e+12          751040  0.0159981720    SPY  171       Q1
    ## 172    1.644988e+12          784456  0.0011201973    SPY  172       Q1
    ## 173    1.645074e+12          850969 -0.0215928540    SPY  173       Q1
    ## 174    1.645160e+12         1137286 -0.0064961378    SPY  174       Q1
    ## 175    1.645506e+12         1213511 -0.0107896393    SPY  175       Q1
    ## 176    1.645592e+12         1166037 -0.0178978848    SPY  176       Q1
    ## 177    1.645679e+12         2316981  0.0149370610    SPY  177       Q1
    ## 178    1.645765e+12         1141900  0.0218240865    SPY  178       Q1
    ## 179    1.646024e+12         1305973 -0.0025618166    SPY  179       Q1
    ## 180    1.646111e+12         1422063 -0.0153474586    SPY  180       Q1
    ## 181    1.646197e+12         1252755  0.0182290413    SPY  181       Q1
    ## 182    1.646284e+12         1039161 -0.0049908529    SPY  182       Q1
    ## 183    1.646370e+12         1085108 -0.0081578551    SPY  183       Q1
    ## 184    1.646629e+12         1448619 -0.0299223827    SPY  184       Q1
    ## 185    1.646716e+12         1950899 -0.0076106054    SPY  185       Q1
    ## 186    1.646802e+12         1133569  0.0264576986    SPY  186       Q1
    ## 187    1.646888e+12          917457 -0.0045257965    SPY  187       Q1
    ## 188    1.646975e+12          837760 -0.0127965793    SPY  188       Q1
    ## 189    1.647230e+12          897065 -0.0073351423    SPY  189       Q1
    ## 190    1.647317e+12          949202  0.0217521059    SPY  190       Q1
    ## 191    1.647403e+12         1405236  0.0219319760    SPY  191       Q1
    ## 192    1.647490e+12          812423  0.0124332893    SPY  192       Q1
    ## 193    1.647576e+12          814468  0.0077914553    SPY  193       Q1
    ## 194    1.647835e+12          937033 -0.0002924931    SPY  194       Q1
    ## 195    1.647922e+12          637073  0.0116335011    SPY  195       Q1
    ## 196    1.648008e+12          641633 -0.0129620458    SPY  196       Q1
    ## 197    1.648094e+12          538727  0.0149618687    SPY  197       Q1
    ## 198    1.648181e+12          662816  0.0048716853    SPY  198       Q1
    ## 199    1.648440e+12          624220  0.0070878571    SPY  199       Q1
    ## 200    1.648526e+12          738517  0.0122949688    SPY  200       Q1
    ## 201    1.648613e+12          659195 -0.0061939888    SPY  201       Q1
    ## 202    1.648699e+12          755684 -0.0155109993    SPY  202       Q1
    ## 203    1.648786e+12          627039  0.0028301071    SPY  203       Q1
    ## 204    1.649045e+12          504721  0.0085301489    SPY  204       Q1
    ## 205    1.649131e+12          646030 -0.0127118022    SPY  205       Q1
    ## 206    1.649218e+12          903937 -0.0100496640    SPY  206       Q1
    ## 207    1.649304e+12          632862  0.0050263149    SPY  207       Q1
    ## 208    1.649390e+12          574887 -0.0026775570    SPY  208       Q1
    ## 209    1.649650e+12          781085 -0.0172400579    SPY  209       Q2
    ## 210    1.649736e+12          696834 -0.0037121005    SPY  210       Q2
    ## 211    1.649822e+12          567443  0.0113885078    SPY  211       Q2
    ## 212    1.649909e+12          705563 -0.0125299562    SPY  212       Q2
    ## 213    1.650254e+12          575881  0.0004110715    SPY  213       Q2
    ## 214    1.650341e+12          599070  0.0160137509    SPY  214       Q2
    ## 215    1.650427e+12          595910 -0.0007417814    SPY  215       Q2
    ## 216    1.650514e+12          722878 -0.0150664970    SPY  216       Q2
    ## 217    1.650600e+12          974880 -0.0278226487    SPY  217       Q2
    ## 218    1.650859e+12         1146068  0.0057808364    SPY  218       Q2
    ## 219    1.650946e+12         1007658 -0.0293884590    SPY  219       Q2
    ## 220    1.651032e+12         1010488  0.0028078783    SPY  220       Q2
    ## 221    1.651118e+12          807518  0.0249456775    SPY  221       Q2
    ## 222    1.651205e+12         1148898 -0.0376558224    SPY  222       Q2
    ## 223    1.651464e+12         1466164  0.0060013732    SPY  223       Q2
    ## 224    1.651550e+12          854781  0.0045735823    SPY  224       Q2
    ## 225    1.651637e+12         1465732  0.0299984644    SPY  225       Q2
    ## 226    1.651723e+12         1586336 -0.0361898380    SPY  226       Q2
    ## 227    1.651810e+12         1531002 -0.0059868082    SPY  227       Q2
    ## 228    1.652069e+12         1422451 -0.0325410734    SPY  228       Q2
    ## 229    1.652155e+12         1467481  0.0023079056    SPY  229       Q2
    ## 230    1.652242e+12         1530065 -0.0160136782    SPY  230       Q2
    ## 231    1.652328e+12         1485883 -0.0010444663    SPY  231       Q2
    ## 232    1.652414e+12          859058  0.0236265177    SPY  232       Q2
    ## 233    1.652674e+12          734539 -0.0040658067    SPY  233       Q2
    ## 234    1.652760e+12          743971  0.0203616589    SPY  234       Q2
    ## 235    1.652846e+12          967497 -0.0411465476    SPY  235       Q2
    ## 236    1.652933e+12          937782 -0.0061434689    SPY  236       Q2
    ## 237    1.653019e+12         1100341  0.0004364066    SPY  237       Q2
    ## 238    1.653278e+12          714909  0.0185371782    SPY  238       Q2
    ## 239    1.653365e+12          804223 -0.0076630665    SPY  239       Q2
    ## 240    1.653451e+12          768574  0.0087961541    SPY  240       Q2
    ## 241    1.653538e+12          606434  0.0197843698    SPY  241       Q2
    ## 242    1.653624e+12          603517  0.0242526237    SPY  242       Q2
    ## 243    1.653970e+12          713844 -0.0056267430    SPY  243       Q2
    ## 244    1.654056e+12          697746 -0.0081214277    SPY  244       Q2
    ## 245    1.654142e+12          616249  0.0188643772    SPY  245       Q2
    ## 246    1.654229e+12          577277 -0.0165476702    SPY  246       Q2
    ## 247    1.654488e+12          473552  0.0030401444    SPY  247       Q2
    ## 248    1.654574e+12          481780  0.0095465542    SPY  248       Q2
    ## 249    1.654661e+12          505047 -0.0109317138    SPY  249       Q2
    ## 250    1.654747e+12          580040 -0.0240702685    SPY  250       Q2
    ## 251    1.654834e+12          914118 -0.0294242955    SPY  251       Q2
    ## 252    1.655093e+12         1138048 -0.0387077611    SPY  252       Q2
    ## 253    1.655179e+12          826153 -0.0030178826    SPY  253       Q2
    ## 254    1.655266e+12         1079873  0.0141556270    SPY  254       Q2
    ##     TransCategory
    ## 1             Few
    ## 2             Few
    ## 3             Few
    ## 4         Average
    ## 5             Few
    ## 6             Few
    ## 7             Few
    ## 8             Few
    ## 9             Few
    ## 10            Few
    ## 11            Few
    ## 12            Few
    ## 13            Few
    ## 14            Few
    ## 15            Few
    ## 16            Few
    ## 17            Few
    ## 18            Few
    ## 19            Few
    ## 20            Few
    ## 21            Few
    ## 22            Few
    ## 23            Few
    ## 24           Many
    ## 25            Few
    ## 26            Few
    ## 27            Few
    ## 28            Few
    ## 29            Few
    ## 30            Few
    ## 31            Few
    ## 32            Few
    ## 33            Few
    ## 34            Few
    ## 35            Few
    ## 36            Few
    ## 37            Few
    ## 38            Few
    ## 39            Few
    ## 40            Few
    ## 41            Few
    ## 42            Few
    ## 43            Few
    ## 44            Few
    ## 45            Few
    ## 46            Few
    ## 47        Average
    ## 48            Few
    ## 49            Few
    ## 50            Few
    ## 51            Few
    ## 52            Few
    ## 53            Few
    ## 54            Few
    ## 55            Few
    ## 56            Few
    ## 57            Few
    ## 58            Few
    ## 59            Few
    ## 60            Few
    ## 61            Few
    ## 62            Few
    ## 63            Few
    ## 64            Few
    ## 65            Few
    ## 66            Few
    ## 67        Average
    ## 68           Many
    ## 69        Average
    ## 70        Average
    ## 71            Few
    ## 72            Few
    ## 73            Few
    ## 74        Average
    ## 75            Few
    ## 76        Average
    ## 77        Average
    ## 78        Average
    ## 79            Few
    ## 80        Average
    ## 81            Few
    ## 82            Few
    ## 83            Few
    ## 84            Few
    ## 85            Few
    ## 86            Few
    ## 87            Few
    ## 88            Few
    ## 89            Few
    ## 90            Few
    ## 91            Few
    ## 92            Few
    ## 93            Few
    ## 94            Few
    ## 95            Few
    ## 96            Few
    ## 97            Few
    ## 98            Few
    ## 99            Few
    ## 100           Few
    ## 101           Few
    ## 102           Few
    ## 103           Few
    ## 104           Few
    ## 105           Few
    ## 106           Few
    ## 107           Few
    ## 108           Few
    ## 109           Few
    ## 110           Few
    ## 111           Few
    ## 112           Few
    ## 113           Few
    ## 114           Few
    ## 115           Few
    ## 116       Average
    ## 117           Few
    ## 118          Many
    ## 119          Many
    ## 120          Many
    ## 121          Many
    ## 122       Average
    ## 123           Few
    ## 124           Few
    ## 125           Few
    ## 126           Few
    ## 127           Few
    ## 128           Few
    ## 129       Average
    ## 130       Average
    ## 131       Average
    ## 132       Average
    ## 133           Few
    ## 134           Few
    ## 135           Few
    ## 136           Few
    ## 137           Few
    ## 138           Few
    ## 139           Few
    ## 140           Few
    ## 141           Few
    ## 142           Few
    ## 143       Average
    ## 144       Average
    ## 145           Few
    ## 146       Average
    ## 147           Few
    ## 148           Few
    ## 149       Average
    ## 150       Average
    ## 151       Average
    ## 152       Average
    ## 153       Average
    ## 154          Many
    ## 155          Many
    ## 156          Many
    ## 157          Many
    ## 158          Many
    ## 159          Many
    ## 160          Many
    ## 161          Many
    ## 162       Average
    ## 163          Many
    ## 164          Many
    ## 165       Average
    ## 166       Average
    ## 167       Average
    ## 168          Many
    ## 169          Many
    ## 170          Many
    ## 171       Average
    ## 172       Average
    ## 173       Average
    ## 174          Many
    ## 175          Many
    ## 176          Many
    ## 177          Many
    ## 178          Many
    ## 179          Many
    ## 180          Many
    ## 181          Many
    ## 182          Many
    ## 183          Many
    ## 184          Many
    ## 185          Many
    ## 186          Many
    ## 187       Average
    ## 188       Average
    ## 189       Average
    ## 190       Average
    ## 191          Many
    ## 192       Average
    ## 193       Average
    ## 194       Average
    ## 195           Few
    ## 196           Few
    ## 197           Few
    ## 198       Average
    ## 199           Few
    ## 200       Average
    ## 201       Average
    ## 202       Average
    ## 203           Few
    ## 204           Few
    ## 205           Few
    ## 206       Average
    ## 207           Few
    ## 208           Few
    ## 209       Average
    ## 210       Average
    ## 211           Few
    ## 212       Average
    ## 213           Few
    ## 214           Few
    ## 215           Few
    ## 216       Average
    ## 217       Average
    ## 218          Many
    ## 219          Many
    ## 220          Many
    ## 221       Average
    ## 222          Many
    ## 223          Many
    ## 224       Average
    ## 225          Many
    ## 226          Many
    ## 227          Many
    ## 228          Many
    ## 229          Many
    ## 230          Many
    ## 231          Many
    ## 232       Average
    ## 233       Average
    ## 234       Average
    ## 235       Average
    ## 236       Average
    ## 237          Many
    ## 238       Average
    ## 239       Average
    ## 240       Average
    ## 241           Few
    ## 242           Few
    ## 243       Average
    ## 244       Average
    ## 245           Few
    ## 246           Few
    ## 247           Few
    ## 248           Few
    ## 249           Few
    ## 250           Few
    ## 251       Average
    ## 252          Many
    ## 253       Average
    ## 254          Many

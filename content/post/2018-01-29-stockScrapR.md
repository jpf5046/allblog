---
title: "StockScrapeR"
author: "John Friel"
date: '2018-01-29'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(rvest)
library(plyr)
#devtools::install_github("wch/webshot")
library(webshot)
```

## Scan Stock Market With Rvest and Webshot problem

I wanted a way to look at stocks charts that quickly fit the range of investment I was looking to make. 

# Let's Grab all Stocks on the NYSE

The following script will run rvest on a loop, each loop opens a new webpage. so the first run through opens eoddata.com/stocklist/NYSE/a.htm then grabs all stock information for Stock Tickers beginning with the letter A. 

```{r}

# creating a shell for putting scrapped info into
allnyse <- data.frame()

# createing results array for refined table after completeing the scrape
resultsarraynyse <- data.frame()

# creating a list that allows the loop to be completed
abcs <- c("a", "b", "c", "d", "e", "f", "g", "h", "i",
          "j", "k", "l", "m", "n", "o", "p", "q", "r",
          "s", "t", "u", "v", "w", "x", "y", "z")

# first loop for grabbing stock data
for (i in abcs) {
  
  # this is the website that the loop is getting information from
  # this is the highlighted part in the picture below
  url <- paste("http://eoddata.com/stocklist/NYSE/", i, ".htm", sep="")
  
  url <- read_html(url)
  
  # results arrary is stacking the peicing of information needed into a dtafram
  # notice I define 'stockticker' 'stockname' and 'closeprice' 
  # html_nodes is from slector gadget 
  resultsarraynyse <- data.frame(stockticker = url %>% 
  html_nodes(xpath =
  '//*[(@id = "ctl00_cph1_divSymbols")]//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]')
  %>%
 
  html_text(), 
 
  stockname = url %>% 
  
  html_nodes(xpath = 
  '//*[(@id = "ctl00_cph1_divSymbols")]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]') %>%
                               html_text(),
                             closeprice =  url %>% 
                               html_nodes(xpath = '//*[(@id = "ctl00_cph1_divSymbols")]//td[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]') %>%
                               html_text(),
                              stringsAsFactors = FALSE)
  
  # compile all information into dataframe
  allnyse <- rbind.fill(allnyse, resultsarraynyse)
  
}

head(allnyse)

tail(allnyse)


```

The loop changes the highlighted part below and then grabs the tickers circled in blue. After the loop runs, we will have all stock tickers. 

 ![](/images/ChangeURLStockScrapR.PNG)
 
 And this is the outout we get running the `head(allnyse)` and `tail(allnyse)`
 
 ![](/images/allnyseHeadandTail.PNG)

# Starting NASDAQ

Now  have a data frame with the Ticker, close price and stock name. Let's do the same thing with NASDAQ:

```{r}


allnasdaq <- data.frame()

resultsarraynasdaq <- data.frame()

# this is the same looping action described above
abcs <- c("a", "b", "c", "d", "e", "f", "g", "h", "i",
          "j", "k", "l", "m", "n", "o", "p", "q", "r",
          "s", "t", "u", "v", "w", "x", "y", "z")


for (i in abcs) {
  
  url <- paste("http://eoddata.com/stocklist/NASDAQ.htm", i, ".htm", sep="")
  
  url <- read_html(url)
  
  resultsarraynasdaq <- data.frame(stockticker = url %>% 
                               html_nodes(xpath = '//*[(@id = "ctl00_cph1_divSymbols")]//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
                               html_text(), 
                             stockname = url %>% 
                               html_nodes(xpath = '//*[(@id = "ctl00_cph1_divSymbols")]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]') %>%
                               html_text(),
                             closeprice =  url %>% 
                               html_nodes(xpath = '//*[(@id = "ctl00_cph1_divSymbols")]//td[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]') %>%
                               html_text(),
                             stringsAsFactors = FALSE)
  
  
  allnasdaq <- rbind.fill(allnasdaq, resultsarraynasdaq)
  
}

head(allnasdaq)

tail(allnasdaq)

```

Here is the output we are expecting:

 ![](/images/allnasdaqHeadandTail.PNG)

# Comebine both Datasets

Now let's combine the two 

```{r}
# combine the stocks
allstocks <- rbind(allnyse, allnasdaq)

# make sure close price is numeric
allstocks <- transform(allstocks, closeprice = as.numeric(closeprice))

allstocks <- unique(allstocks)

```

Okay, I'm a risky investor, so let's look at stocks that cost between 2.50 and 2.70 :

```{r}
StockstoInvest <- allstocks %>% filter(closeprice >= 2.50 & closeprice < 2.70)

StockstoInvest <- unique(StockstoInvest)

head(StockstoInvest)

```

Quick look at the stocks. I have a total of 16, let's look at the top 6:

 ![](/images/headStockstoInvest.PNG)


I have 12 stocks I can look at, but let's use `webshot` to do the heavy lifting for us:

```{r}


for (i in StockstoInvest$stockticker) {
 
  wurlShot <- paste("http://www.nasdaq.com/symbol/",i, "/stock-chart", sep = '')
 
  
  savepngname <- paste('"', i, ".png")
 
  webshot(wurlShot, file = paste('testfolder/',i, '.png', sep =''),  selector = ".marginT5px img")
 
}


```

The loop above runs each of the 16 tickers we narrowed to in the chunk above and takes a screen shot of the year price chart. The picture then saves to a folder named `testfolder`. 

Then we can look at each of the stocks one by one, check out this gif:

 ![](/images/stockScrapR.gif.gif)


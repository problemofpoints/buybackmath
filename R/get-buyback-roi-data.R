
# get data needed for buyback roi analysis

library(tidyverse)
library(readxl)
library(tibbletime)
library(lubridate)
library(tidyquant)


# input variables ---------------------------------------------------------

from_date <- as.Date("2014-01-01")
to_date <- as.Date("2018-12-31")

# import buyback data from spreadsheet ------------------------------------

data <- readxl::read_xlsx("data-raw/insurer_buybacks_2018.xlsx", sheet = "import") %>%
  gather(key = "statistic", value = "value", -ticker, -company) %>%
  separate(statistic, c("yr_qtr","metric"), sep = "_") %>%
  separate(yr_qtr, c("year","qtr"), sep = 4, remove = FALSE) %>%
  mutate(value = as.double(value), 
         year = as.double(year),
         date = lubridate::yq(yr_qtr), 
         qtr_ = lubridate::quarter(date)) %>%
  replace_na(list(value = 0)) %>% 
  spread(key = metric, value = value) %>%
  mutate(dollarrepurchase = avgpricepershare * sharesrepurchased)

# import stock price data -------------------------------------------------

# get a list of all the Property-Casualty Insurers from the major US stock exchanges
stock_tickers <- list('AMEX','NASDAQ','NYSE') %>% map_df(tq_exchange) %>% 
  filter((industry == 'Property-Casualty Insurers' | symbol %in% c("AGII", "ERIE", "AIZ")) & !is.na(market.cap)) %>%
  mutate(market_cap = if_else(stringr::str_sub(market.cap,start = -1)=="M",1e6,1e9) *
           as.numeric(stringr::str_sub(market.cap,start = 2, end = -2))) %>%
  dplyr::select(symbol, company, sector, industry, market_cap) %>%
  arrange(desc(market_cap))

stock_tickers <- stock_tickers %>% 
  inner_join(data %>% distinct(ticker), by = c("symbol"="ticker"))

# now get the historical stock prices for all of the tickers; get quarter close stock prices
stock_prices <- stock_tickers %>%
  tq_get(get = "stock.prices", from = from_date, to = to_date) %>% 
  filter(!is.na(adjusted)) %>% 
  select(symbol, company, market_cap, date, close, adjusted) 

stock_prices_qtr <- stock_prices %>%
  group_by(symbol) %>%
  as_tbl_time(index = date) %>%
  as_period("quarterly", side = "end") %>%
  mutate(year = lubridate::year(date), qtr_ = lubridate::quarter(date))


# combine input data ------------------------------------------------------

data_to_use <- data %>% 
  select(-date) %>% 
  left_join(stock_prices_qtr %>% 
              select(symbol, market_cap, year, date, qtr_, close, adjusted), 
            by = c("ticker"="symbol", "year", "qtr_")) %>%
  drop_na() %>%
  group_by(ticker) %>%
  as_tbl_time(index = date) %>%
  filter_time(as.character(lubridate::year(from_date)) ~ as.character(lubridate::year(to_date)))

# save data -------------------------------------------------------------

saveRDS(data_to_use, "data/buyback-roi-data-2018.rds")
saveRDS(stock_prices, "data/stock-prices-2018.rds")


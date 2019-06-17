library(tidyquant)
library(timetk)

#1
rm(list=ls())
stock_day_year<-read_tsv("data_wrangle_practice/tej_day_price_2017_2018.txt")
glimpse(stock_day_year)

#2
price_day_year <- stock_day_year %>% 
  rename(id    = 證券代碼, 
         name  = 簡稱, 
         date  = 年月日, 
         price = `收盤價(元)`,
         cap   = `市值(百萬元)`
  )
dim(price_day_year)
glimpse(price_day_year)

#3
price_day_year1 <- stock_day_year %>% 
  rename(id    = 證券代碼, 
         name  = 簡稱, 
         date  = 年月日, 
         price = `收盤價(元)`,
         cap   = `市值(百萬元)`
  ) %>% 
  mutate(id = as.character(id)) %>%
  mutate(date = as.Date(as.character(date),'%Y%m%d')) %>%
  select(id,date,price) %>%
  spread(key = id,value = price)
dim(price_day_year1)
glimpse(price_day_year1)

#4
price_day_year_na <- price_day_year1 %>% 
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day_year_na

price_day_year_na.1 <- price_day_year1 %>% 
  # last observation carried forward
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day_year_na.1

#5
price_day_year_clear <-  price_day_year1 %>% 
  na.locf(fromLast = TRUE, na.rm=FALSE) %>%
  select(-c("2025", "6131"))
head(price_day_year_clear)

#6
dim(price_day_year_clear)

#7
ret_day_year <- price_day_year_clear %>% 
  select(1:6) %>%  
  tk_xts(select = -date, date_var = date) %>% 
  Return.calculate(method = "log") %>%
  na.omit()
dim(ret_day_year)

head(ret_day_year,5)

#8
price_day_year.xts <- price_day_year_clear %>%
  select(1:6) %>%
  tk_xts(select = -date, date_var = date)  

ret_mon_year.xts <- price_day_year.xts %>% 
  to.period(period = "months", 
            indexAt = "lastof", 
            OHLC= FALSE) %>% 
  Return.calculate(method = "log") %>% 
  na.omit()
dim(ret_mon_year.xts)

head(ret_mon_year.xts,5)

#9
price_day_year %>%
  select(id, price) %>%
  filter(price > mean(price))  

price_day_year %>%
  select(id, price) %>%
  filter((price > 000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  View()








#11
ret_day_year.tibble <- ret_day_year %>% 
  select(1:6) %>%  
  tk_tbl(select = ) %>% 
  Return.calculate(method = "log") %>%
  gather(key = id,value = price)

dim(ret_day_year2)

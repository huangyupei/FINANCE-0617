rm(list=ls())
stock_day_year<-read_tsv("price2019_UTF8.txt")
glimpse(stock_day_year)

price_day_year <- stock_day_year %>% 
  rename(id    = 證券代碼, 
         name  = 簡稱, 
         date  = 年月日, 
         price = `收盤價(元)`,
         cap   = `市值(百萬元)`
  ) %>% 
  mutate(id = as.character(id)) %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
  select(id, date, price) %>% 
  spread(key = id, value = price)
dim(price_day_year)

price_day_year_na <- price_day_year %>% 
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day_year_na
#
price_day_year_na.1 <- price_day_year %>% 
  na.locf(fromLast = TRUE, na.rm=FALSE) %>% 
  map_df(~sum(is.na(.))) %>% 
  gather() %>% 
  filter(value!=0)
price_day_year_na.1
#
price_day_year_clear <-  price_day_year %>% 
  na.locf(fromLast = TRUE, na.rm=FALSE) %>%
  select(-c("2025", "6131"))
dim(price_day_year_clear)
#
ret_day_year <- price_day_year %>%
  # convert from tibble into xts
  tk_xts(select = -date, date_var = date) %>% 
  Return.calculate(method = "log")
dim(ret_day_year)
#
price_day_year.xts <- price_day_year_clear %>%
  tk_xts(select = -date, date_var = date)  

ret_mon_year.xts <- price_day_year.xts %>% 
  to.period(period = "months", 
            indexAt = "lastof", 
            OHLC= FALSE) %>% 
  Return.calculate(method = "log")

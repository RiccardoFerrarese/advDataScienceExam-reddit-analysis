over_time_sentiment <- doge_per_com_sentiment %>%
   count(bing, date) %>%
   # each (bing's) sentiment is a column with value equal to the count for date
   spread(bing, n, fill = 0) 

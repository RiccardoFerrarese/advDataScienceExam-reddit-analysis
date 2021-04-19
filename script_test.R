sentiment_bing <- function(x, y){
   if({{x}} == 'positive' & {{y}} == 'positive'){
      return('positive')
   } else if( {{x}} == 'negative' & {{y}} == 'negative' ){
      return('negative')
   } else {
      return('neutral')
   }
}

sentiment_df <- doge_bigrams_counts %>% 
   inner_join(get_sentiments("afinn"), by=c('word1'= 'word')) %>%
   inner_join(get_sentiments("bing"), by=c('word1'= 'word')) %>%
   inner_join(get_sentiments("afinn"), by=c('word2'= 'word')) %>%
   inner_join(get_sentiments("bing"), by=c('word2'= 'word')) %>%
   mutate( affin = (value.x + value.y) / 2) %>%
   mutate( bing = case_when(sentiment.x == 'positive' & sentiment.y == 'positive' ~ 'positive', 
                            sentiment.x == 'negative' & sentiment.y == 'negative' ~ 'negative',
                            TRUE ~ 'neutral')) %>%
   unite(words, c("word1", "word2"), sep = " ") 

View(sentiment_df)
## with affin make mean between value.x & value.y
## with bing 
   ## positive - positive --> positive
   ## positive - negative --> neutral
   ## negative - positive --> neutral
   ## negative - negative --> negative



## tokenize corpus and make document-feature matrix
# data :: text corpus 
# stop :: bool for remove stop word
# ngrams :: {1,2,3} for compute corpus on ngrams
create_tidy_corpus <- function(data, stop=TRUE, ngrams=1){
   
   # if stopword .. 
   # build toks  
   if(stop){
      toks <- quanteda::tokens_select(
         quanteda::tokens(data,
                          remove_punct = TRUE, 
                          remove_symbols = TRUE, 
                          remove_numbers = TRUE, 
                          remove_url = TRUE),
         pattern = stopwords("en"), selection = "remove")
   } else {
      toks <- quanteda::tokens(data,
                               remove_punct = TRUE, 
                               remove_symbols = TRUE, 
                               remove_numbers = TRUE, 
                               remove_url = TRUE)
   }
   
   # if ngrams -- compute toks for bigrams or trigrams 
   if(ngrams==2){
      dfm <- quanteda::dfm(quanteda::tokens_ngrams(toks, n = 2, concatenator = " "))
   } else if(ngrams==3){
      dfm <- quanteda::dfm(quanteda::tokens_ngrams(toks, n = 3, concatenator = " "))
   } else {
      dfm <- quanteda::dfm(toks)
   }
   
   return(tidy(dfm))
}

## function for clean data and separate ngrams 
# tidy :: dataframe compute by create corpus ( tidy dfm )
# ngrams :: {1,2,3} for compute corpus on ngrams
compute_clean_corpus_tidy <- function(tidy, ngrams=1, stemming=FALSE, lemma=TRUE){
   
   if(stemming & lemma){stop("Please choose only one between stemming vs lemmatization")}
   # check ngrams and build clean dataframe
   if(ngrams == 2){
      clean_df <- tidy %>%
         ## mutate term in word
         unnest_tokens(words, term, token = "ngrams", n=2) %>% 
         ## divide bigrams in words
         tidyr::separate(words, c("word1", "word2"), sep = " " )   %>%       
         filter( word1 != word2 & word2 != word1 ) %>%
         ## extract word without weird char
         mutate(word1 = str_extract(word1,  "[a-z]+"))  %>%                   
         mutate(word2 = str_extract(word2,  "[a-z]+")) %>% 
         ## remove word with 1-2 letter
         mutate(word1 = gsub(" *\\b[[:alpha:]]{1,2}\\b *", '', word1)) %>%
         mutate(word2 = gsub(" *\\b[[:alpha:]]{1,2}\\b *", '', word2)) %>%
         filter(word1 != "", 
                word2 != "")
      
      ## lemmatize words
      if(lemma){
         clean_df <- clean_df %>%
            mutate(word1 = textstem::lemmatize_words(word1)) %>%
            mutate(word2 = textstem::lemmatize_words(word2))
         ## stemming words
      } else if(stemming){
         clean_df <- clean_df %>%
            mutate(word1 = SnowballC::wordStem(word1, language = "english")) %>%
            mutate(word2 = SnowballC::wordStem(word2, language = "english"))
      }
      
   } else if (ngrams == 3){
      clean_df <- tidy %>%
         unnest_tokens(words, term, token = "ngrams", n=3) %>%                     
         tidyr::separate(words, c("word1", "word2", "word3"), sep = " " ) %>%   
         filter( word1 != word2 & word2 != word3 & word1 != word3 ) %>%
         mutate(word1 = str_extract(word1,  "[a-z]+"))  %>%                         
         mutate(word2 = str_extract(word2,  "[a-z]+")) %>% 
         mutate(word3 = str_extract(word3,  "[a-z]+")) %>% 
         mutate(word1 = gsub(" *\\b[[:alpha:]]{1,2}\\b *", '', word1)) %>%
         mutate(word2 = gsub(" *\\b[[:alpha:]]{1,2}\\b *", '', word2)) %>%
         mutate(word3 = gsub(" *\\b[[:alpha:]]{1,2}\\b *", '', word3)) %>%
         filter(word1 != "", 
                word2 != "", 
                word3 != "")
      
      ## lemmatize words
      if(lemma){
         clean_df <- clean_df %>%
            mutate(word1 = textstem::lemmatize_words(word1)) %>%
            mutate(word2 = textstem::lemmatize_words(word2)) %>%
            mutate(word3 = textstem::lemmatize_words(word3)) 
         ## stemming words
      } else if(stemming){
         clean_df <- clean_df %>%
            mutate(word1 = SnowballC::wordStem(word1, language = "english")) %>%
            mutate(word2 = SnowballC::wordStem(word2, language = "english")) %>%
            mutate(word3 = SnowballC::wordStem(word3, language = "english"))
      }
      
   } else {
      clean_df <- tidy %>%
         unnest_tokens(words, term)  %>%                      
         mutate( words = str_extract(words,  "[a-z]+")) %>%   
         mutate( words = gsub(" *\\b[[:alpha:]]{1,2}\\b *", '', words)) %>%     
         #mutate( words = gsub("^(?!\b(.)\1+\b)", '', words)) %>%
         filter( words != "" ) 
      ## lemmatize words
      if(lemma){
         clean_df <- clean_df %>%
            mutate(words = textstem::lemmatize_words(words)) 
         ## stemming words
      } else if(stemming){
         clean_df <- clean_df %>%
            mutate(words = SnowballC::wordStem(words, language = "english"))
         # with porter buy -> bui 
      }
      
   }
   
   return(clean_df)
}

## function for word (or ngrams) counts and plot 
# data :: clean dataframe compute by compute clean corpus ( tidy dfm )
# threshold_count :: value to filter items by count for pritty plot
# threshold_freq :: value to filter items by frequency for pritty plot
# ngrams :: {1,2,3} for compute corpus on ngrams
# bool_plot_count :: boolean to indicate whether to plot word counts or not 
# bool_plot_freq :: boolean to indicate whether to plot word frequencies or not
compute_plot_count_and_freq <- function(data, threshold_count=1000, threshold_freq=0.001, ngrams=1, 
                                        bool_plot_count=TRUE, bool_plot_frequency=TRUE){
   
   if(ngrams == 2){
      # merge single word
      words_unite <- data %>%
         unite(words, c("word1", "word2"), sep = " ") 
      # sum count col for each word
      word_counts <- aggregate(cbind(count) ~ words, data=words_unite, FUN=sum) 
      
   } else if(ngrams == 3){
      words_unite <- data %>%
         unite(words, c("word1", "word2", "word3"), sep = " ") 
      word_counts <- aggregate(cbind(count) ~ words, data=words_unite, FUN=sum) 
      
   } else {
      word_counts <- aggregate(cbind(count) ~ words, data=data, FUN=sum)
   }
   
   total_of_word <- sum( word_counts$count )
   word_counts <- word_counts %>%
      mutate( count = as.integer(count)) %>%
      mutate( total_of_word = total_of_word ) %>%
      mutate( frequency = count/total_of_word)
   
   # plot for count col
   plot_freq <- word_counts %>%
      filter( frequency > threshold_freq) %>%
      ggplot(aes(words, frequency)) + 
      geom_point(alpha = 0.3, size = 1.5, width = 0.25, height = 0.1) + 
      geom_text(aes(label = words), check_overlap = TRUE, vjust = 1) + 
      theme_minimal() + 
      theme(axis.text.x=element_blank()) 
   
   # plot for frequency col
   plot_count <- word_counts %>%
      filter( count > threshold_count) %>%   
      ggplot(aes(words, count)) + 
      geom_point(alpha = 0.3, size = 1.5, width = 0.25, height = 0.1) + 
      geom_text(aes(label = words), check_overlap = TRUE, vjust = 1) + 
      scale_y_log10() + 
      theme_minimal() + 
      theme(axis.text.x=element_blank()) 
   
   
   # print selected plot 
   if(bool_plot_count & bool_plot_frequency){
      print(plot_freq + plot_count + plot_layout(ncol=1, heights = c(4,4)) )
   } else if(bool_plot_count){
      print(plot_count)
   } else if(bool_plot_frequency){
      print(plot_freq)
   }
   
   # split words for sentiment analysis 
   if(ngrams == 3 ){
      word_counts <- word_counts %>%
         tidyr::separate(words, c("word1", "word2", "word3"), sep = " " )
   } else if(ngrams == 2){
      word_counts <- word_counts %>%
         tidyr::separate(words, c("word1", "word2"), sep = " " )
   }
   
   return(word_counts)
   
}

plot_class_sentiment <- function(data, n_filter_sentiment, ngrams=1){
   # inner join with nrc sentiment
   if(ngrams == 3 ){
      sentiment_class <- data %>%
         # join sentiment with each word 
         inner_join(get_sentiments("nrc"), by=c('word1' = 'word')) %>%
         inner_join(get_sentiments("nrc"),by=c('word2' = 'word')) %>%
         inner_join(get_sentiments("nrc"),by=c('word3' = 'word')) %>%
         # merge sentiment
         unite(sentiment, c("sentiment.x", "sentiment.y", "sentiment"), sep="-") %>%
         # group and count 
         group_by(sentiment) %>% 
         summarise(word_4_sentiment = n()) %>% 
         arrange(-word_4_sentiment, sentiment)
      
      # plot number of word for each sentiment class
      plot <- sentiment_class %>% 
         filter(word_4_sentiment > n_filter_sentiment ) %>%
         ggplot(aes(word_4_sentiment, sentiment, fill=sentiment)) + 
         geom_col( show.legend = FALSE) + 
         xlab("") +
         theme_minimal() 
      
      
   } else if(ngrams == 2 ){
      sentiment_class <- data %>%
         inner_join(get_sentiments("nrc"), by=c('word1' = 'word')) %>%
         inner_join(get_sentiments("nrc"),by=c('word2' = 'word')) %>%
         unite(sentiment, c("sentiment.x", "sentiment.y"), sep="-") %>%
         group_by(sentiment) %>% 
         summarise(word_4_sentiment = n()) %>% 
         arrange(-word_4_sentiment, sentiment) 
      
      
      plot <- sentiment_class %>% 
         filter(word_4_sentiment > n_filter_sentiment ) %>%
         ggplot(aes(word_4_sentiment, sentiment, fill=sentiment)) + 
         geom_col( show.legend = FALSE) + 
         xlab("") +
         theme_minimal() 
      
      
   } else {
      sentiment_class <- data %>%
         inner_join(get_sentiments("nrc"), by=c('words'= 'word')) %>%
         group_by(sentiment) %>%
         summarise(word_4_sentiment = n()) %>% 
         arrange(-word_4_sentiment, sentiment) 
      
      plot <- sentiment_class %>% 
         #filter(word_4_sentiment > 2500 ) %>%
         ggplot(aes(word_4_sentiment, sentiment, fill=sentiment)) + 
         geom_col( show.legend = FALSE) + 
         xlab("") +
         theme_minimal() 
   }
   
   
   print(plot)
   return(sentiment_class)
}

# function for normalize value between (0-1]
# usefull for afinn sentiment who have value [-4, 4]
range01 <- function(x){
   min <- min({{x}})
   #print(min)
   max <- max({{x}})
   #print(max)
   (x-min+1)/(max-min+1)
}


## function for print words splited by sentiment
# data :: word_frequency 
# n_filter :: value to filter words by count for pritty plot
# ngrams :: {1,2,3} for compute corpus on ngrams
plot_word_per_sentiment <- function(data, n_filter=20, ngrams=1){
   
   if(ngrams == 2){
      ## with affin --> mean between value.x & value.y
      ## with bing 
      ## positive - positive --> positive
      ## positive - negative --> neutral
      ## negative - positive --> neutral
      ## negative - negative --> negative
      
      sentiment_df <- data %>% 
         inner_join(get_sentiments("afinn"), by=c('word1'= 'word')) %>%
         inner_join(get_sentiments("bing"), by=c('word1'= 'word')) %>%
         inner_join(get_sentiments("afinn"), by=c('word2'= 'word')) %>%
         inner_join(get_sentiments("bing"), by=c('word2'= 'word')) %>%
         mutate( affin = (value.x + value.y) / 2) %>%
         mutate( bing = case_when(sentiment.x == 'positive' & 
                                     sentiment.y == 'positive' ~ 'positive', 
                                  sentiment.x == 'negative' & 
                                     sentiment.y == 'negative' ~ 'negative',
                                  TRUE ~ 'neutral')) %>%
         unite(words, c("word1", "word2"), sep = " ") 
      
      plot <- sentiment_df %>%
         dplyr::filter( count > n_filter ) %>%  
         # compute normalize value of sentiment 
         mutate( affin_nrm = range01(affin)) %>%
         ggplot( aes(words, count,  color = affin_nrm)) +
         geom_jitter(alpha = 0.2, width=0.2, height = 0.1) + 
         geom_text(aes(label = words), check_overlap = TRUE, vjust = 1.5) + 
         scale_y_log10() + 
         # split positive - negative 
         facet_wrap(bing~.) +
         theme_minimal() + 
         theme(axis.text.x=element_blank(), 
               legend.position = "bottom") + 
         labs( color = "Sentiment degree")
      
      
      
   } else if(ngrams == 3){
      
      sentiment_df <- data %>% 
         inner_join(get_sentiments("afinn"), by=c('word1'= 'word')) %>%
         inner_join(get_sentiments("bing"), by=c('word1'= 'word')) %>%
         inner_join(get_sentiments("afinn"), by=c('word2'= 'word')) %>%
         inner_join(get_sentiments("bing"), by=c('word2'= 'word')) %>%
         inner_join(get_sentiments("afinn"), by=c('word3'= 'word')) %>%
         inner_join(get_sentiments("bing"), by=c('word3'= 'word')) %>%
         mutate( affin = (value.x + value.y + value) / 3) %>%
         mutate( bing = case_when(sentiment.x == 'positive' & 
                                     sentiment.y == 'positive' & 
                                     sentiment == 'positive' ~ 'positive', 
                                  sentiment.x == 'negative' & 
                                     sentiment.y == 'negative' & 
                                     sentiment == 'negative' ~ 'negative',
                                  
                                  sentiment.x == 'positive' & 
                                     sentiment.y == 'positive' & 
                                     sentiment == 'negative' ~ 'neutral-positive',
                                  sentiment.x == 'positive' & 
                                     sentiment.y == 'negative' & 
                                     sentiment == 'positive' ~ 'neutral-positive',
                                  sentiment.x == 'negative' & 
                                     sentiment.y == 'positive' & 
                                     sentiment == 'positive' ~ 'neutral-positive',
                                  
                                  TRUE ~ 'neutral-negative')) %>%
         unite(words, c("word1", "word2", "word3"), sep = " ") 
      
      plot <- sentiment_df %>%
         dplyr::filter( count > n_filter ) %>%  
         mutate( affin_nrm = range01(affin)) %>%
         ggplot( aes(words, count,  color = affin_nrm)) +
         geom_jitter(alpha = 0.2, width=0.2, height = 0.1) + 
         geom_text(aes(label = words), check_overlap = TRUE, vjust = 1.5) + 
         scale_y_log10() + 
         facet_wrap(bing~.) +
         theme_minimal() + 
         theme(axis.text.x=element_blank(), 
               legend.position = "bottom") + 
         labs( color = "Sentiment degree")
   } else {
      # plot positive-negative -- color: how much positive/negative is a word
      sentiment_df <- data %>% 
         inner_join(get_sentiments("afinn"), by=c('words'= 'word')) %>%
         inner_join(get_sentiments("bing"), by=c("words" = "word"))
      
      plot <- sentiment_df %>%
         dplyr::filter( count > n_filter ) %>%  
         mutate( value_std = range01(value)) %>%
         ggplot( aes(words, count,  color = value_std)) +
         geom_jitter(alpha = 0.2, width=0.2, height = 0.1) +
         geom_text(aes(label = words), check_overlap = TRUE, vjust = 1.5) + 
         facet_wrap(sentiment~.) +
         scale_y_log10() + 
         theme_minimal() + 
         theme(axis.text.x=element_blank(), 
               legend.position = "bottom") + 
         labs( color = "Sentiment degree")
   }
   print(plot)
   return(sentiment_df)
}



word_network <- function(data, n_filter=1000){
   
   word_graph <- data %>%
      filter(count > n_filter) %>%
      as_tbl_graph()
   
   a <- grid::arrow(type = "open", length = unit(.1, "inches"))
   
   graph <- ggraph(word_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = count), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "lightblue", size = 1) +
      geom_node_text(aes(label = name), vjust = 1.5, hjust = 1) +
      theme_void()
   
   print(graph)
}

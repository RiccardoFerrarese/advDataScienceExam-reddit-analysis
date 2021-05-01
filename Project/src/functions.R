source("./lib.R")

dfm.frequency <- function(.dfm, n = 50){
   
   features_dfm <- textstat_frequency(.dfm, n)
   
   head(features_dfm, 10)
   
   # Sort by reverse frequency order
   features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))


   plot <- features_dfm %>%
      ggplot(aes(x = feature, y = frequency)) +
         geom_point(size=0.1) + 
         scale_y_log10() + 
         theme_classic() +
         theme(axis.text.x = element_blank())
   
   #theme(axis.text.x = element_text(angle = 90, hjust = 1))
   
   plot2 <- features_dfm %>%
      filter(frequency > 10000 ) %>%
      ggplot(aes(x = feature, y = frequency)) +
      geom_point(size=0.01) + 
      scale_y_log10() + 
      theme_classic() +
      theme(axis.text.x = element_blank())
   
   print(plot + plot2)
   
   return(features_dfm)
}

## data must have cols : (created_utc, author, body )
# data is pushshift's returned csv
df.create <- function(data, dir, subR, filter_post=5){
   
   # compute type datef
   # add unique id for each comment 
   # filter removed comment
   data_clean <- data %>%
      mutate(date = as.Date(as_datetime(created_utc))) %>%
      filter(author != '[deleted]' & author != 'AutoModerator') %>% 
      mutate(id = row_number()) %>%
      distinct( author, date, body, parent_id, link_id, .keep_all= TRUE) %>%
      select(id, date, author, body, parent_id, link_id) 
   
   ## remove source data for save space
   remove(data)
   
   # DF for Corpus by POST 
   df_post <- data_clean %>% 
      select(link_id, body) %>%
      # aggregate comment whith same postID
      aggregate(by=list(data_clean$link_id), paste) %>% 
      # compute num of element in body list for each postID
      mutate( n_com = unlist(map(link_id, length))) %>%
      rename("postID" = "Group.1") %>% 
      select(postID, body, n_com) %>% 
      # group by post ID and merge senteces in body
      group_by(postID) %>%
      mutate( body = paste( do.call( c , body), collapse = "\n\n ") ) %>%
      ungroup() %>%
      # filtra post con meno di 5 commenti 
      filter(n_com > filter_post)
   
   
   # BUILD CORPUS
   post_corpus <- corpus(df_post$body, 
                         docnames = df_post$postID, 
                         docvars = data.frame(n = df_post$n_com))
   
   # for comment + date, id-post, usr, 
   corpus <- corpus( as.character(data_clean$body), 
                     docnames = data_clean$id, 
                     docvars = data.frame(author = data_clean$author, 
                                          data = data_clean$date, 
                                          link_id = data_clean$link_id))
   
   #print(summary(data_clean[ c("id", "date", "author")] ))
   
   rtn <- list("df_comm" = data_clean, 
               "df_post" = df_post, 
               "corpus_comm" = corpus, 
               "corpus_post" = post_corpus ) 
   saveRDS(rtn, paste0(dir, subR, ".rds", sep = ""))
   
   return(rtn)
}


## function for normalize value between (0-1]
# x = data.frame(n = c(1,2,3,4,5,6,7))
# x %>% mutate(minmax = range01(n))
range01 <- function(x){
   min <- min({{x}})
   max <- max({{x}})
   (x-min+1)/(max-min+1)
}

## func for clean a little bit the words
# data for dyplr pipe -- 
#          curly-curly impl.  := to assign, 
#                             {{}} to ref a cols in data, 
#                             !!! access to the value
# data:: tidy text object 
# term:: ref to cols in data 
word.apply_regexs <- function(data, term){
   
   ## use curly-curly for term cols
   #data[term] <- str_extract(data[term], "[a-z]+")
   data %>% 
      mutate( {{term}} := str_extract({{term}},  "[a-z]+")) %>%
      # rimuove spazi ecc
      mutate( {{term}} := gsub("^[[:digit:]]", "", {{term}})) %>%
      mutate( {{term}} := gsub("[[:punct:]]", "", {{term}})) %>%
      # term with all same char
      mutate( {{term}} := gsub("a+(h)+", "", {{term}})) %>%
      mutate( {{term}} := gsub("^([:alpha:])\1+", "", {{term}})) %>%
      mutate( {{term}} := gsub("^(a){3}[a-z]*", "", {{term}})) %>%
      # term with 1 or 2 char
      mutate( {{term}} := gsub("^[a-z]{1,2}\\b", "", {{term}})) %>%
   
      
      # laughing term 
      mutate( {{term}} := gsub("\\b(?:a*(?:ha)+h?|h*ha+h[ha]*|(?:l+o+)+l+|o?l+o+l+[ol]*)\\b", "", {{term}}) ) %>%
      filter( {{term}} != "" )
   
}

## function for lemmatization, stemming or POS for words, bi-grams and tri-grams
# data:: tidy text object 
# mode:: {"lemma", "stem", "other"} default is "none" 
# words{1,2,3}:: ref to cols in data 
word.manipulation <- function(data, word1, word2, word3, mode = 0, ngrams = 1){
   
   
   if(mode == 1){
      if(ngrams == 2){
         data %>%
            mutate({{word1}} := textstem::lemmatize_words({{word1}})) %>%
            mutate({{word2}} := textstem::lemmatize_words({{word2}}))
      } else if(ngrams == 3){
         data %>%
            mutate({{word1}} := textstem::lemmatize_words({{word1}})) %>%
            mutate({{word2}} := textstem::lemmatize_words({{word2}})) %>%
            mutate({{word3}} := textstem::lemmatize_words({{word3}})) 
         
      }else {
         data %>%
            mutate( {{word1}} := textstem::lemmatize_words({{word1}})) 
         
      }
   } else if(mode == 2){
      if(ngrams == 2){
         data %>%
            mutate({{word1}} := SnowballC::wordStem({{word1}}, language = "english")) %>%
            mutate({{word2}} := SnowballC::wordStem({{word2}}, language = "english"))
      } else if(ngrams == 3){
         data %>%
            mutate({{word1}} := SnowballC::wordStem({{word1}}, language = "english")) %>%
            mutate({{word2}} := SnowballC::wordStem({{word2}}, language = "english")) %>%
            mutate({{word3}} := SnowballC::wordStem({{word3}}, language = "english"))
      } else {
         data %>%
            mutate({{word1}} := SnowballC::wordStem({{word1}}, language = "english"))
         
      }
      
   } else {
      data
   }
   
}

## tokenize corpus and make document-feature matrix (DFM + TIDY)
# data :: text corpus 
# stop :: bool for remove stop word
# ngrams :: {1,2,3} for compute corpus on ngrams
corpus.tokenize_dfmTidy <- function(data, stop=TRUE, eng_word = FALSE, ngrams=1){
   
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
   
   if( eng_word ){
      # take a dictionary with engish word for clean data 
      #toks <- tokens_select(toks,)
   }
   
   # if ngrams -- compute toks for bigrams or trigrams 
   if(ngrams==2){
      dfm <- quanteda::dfm(quanteda::tokens_ngrams(toks, n = 2, concatenator = " "))
   } else if(ngrams==3){
      dfm <- quanteda::dfm(quanteda::tokens_ngrams(toks, n = 3, concatenator = " "))
   } else {
      dfm <- quanteda::dfm(toks)
   }
   
   # return dfm and tidy 
   return(list(dfm = dfm, tidy = tidy(dfm)))
}

## function for clean data and separate ngrams 
# tidy :: dataframe compute by create corpus ( tidy dfm )
# ngrams :: {1,2,3} for compute corpus on ngrams
corpus.clean_tidy <- function(tidy, ngrams=1, mode = "none"){
   
   if(mode == "lemma"){ .mode <- 1 }
   else if(mode == "stem" ){ .mode <- 2 }
   else{ .mode <- 0 }
   #### check ngrams and build clean dataframe
   if(ngrams == 2){
      clean_df <- tidy %>%
         ## mutate term in word
         unnest_tokens(words, term, token = "ngrams", n=2) %>% 
         ## divide bigrams in words
         tidyr::separate(words, c("word1", "word2"), sep = " ", remove = FALSE) %>%       
         filter( word1 != word2 & word2 != word1 ) %>%
         # apply some regex
         word.apply_regexs(word1)  %>%                   
         word.apply_regexs(word2) %>%
         # apply stemming or lemmatiz ..
         word.manipulation(word1, word2, mode = .mode , ngrams = ngrams)
      
   } else if (ngrams == 3){
      clean_df <- tidy %>%
         unnest_tokens(words, term, token = "ngrams", n=3) %>%                     
         tidyr::separate(words, c("word1", "word2", "word3"), sep = " ", remove = FALSE) %>%   
         filter( word1 != word2 & word2 != word3 & word1 != word3 ) %>%
         word.apply_regexs(word1) %>%                   
         word.apply_regexs(word2) %>%
         word.apply_regexs(word3) %>%
         word.manipulation(word1, word2, word3, mode = .mode , ngrams = ngrams)
      
      
   } else {
      clean_df <- tidy %>%
         unnest_tokens(words, term)  %>%                      
         word.apply_regexs(words)   %>%
         word.manipulation(words, mode = .mode, ngrams = ngrams)
   }
   
   return(clean_df)
}

## function for word (or ngrams) counts and plot 
# data :: clean df TIDY DFM 

# ngrams :: {1,2,3} for compute corpus on ngrams
# threshold_count :: value to filter items by count for pritty plot
# threshold_freq :: value to filter items by frequency for pritty plot
# bool_plot_count :: boolean to indicate whether to plot word counts or not 
# bool_plot_freq :: boolean to indicate whether to plot word frequencies or not
corpus.countPlot_tidy <- function(data, threshold_count=1000, threshold_freq=0.001, 
                                  ngrams=1, 
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
      mutate( frequency = (count/total_of_word)*100)
   
   # plot for count col
   plot_freq <- word_counts %>%
      filter( frequency > threshold_freq) %>%
      ggplot(aes(words, frequency)) + 
      geom_point(alpha = 0.3, size = 1.5, width = 0.25, height = 0.1) + 
      geom_text(aes(label = words), check_overlap = TRUE, vjust = 1) + 
      theme_classic() + 
      theme(axis.text.x=element_blank()) 
   
   # plot for frequency col
   plot_count <- word_counts %>%
      filter( count > threshold_count) %>%   
      ggplot(aes(words, count)) + 
      geom_point(alpha = 0.3, size = 1.5, width = 0.25, height = 0.1) + 
      geom_text(aes(label = words), check_overlap = TRUE, vjust = 1) + 
      scale_y_log10() + 
      theme_classic() + 
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


## function for print number of word for each sentiment in nrc df 
# data :: word_frequency
# n_filter_sentiment :: value to filter number of sentiment for pritty plot
# ngrams :: {1,2,3} for compute corpus on ngrams
words.classSentiment <- function(data, n_filter_sentiment, ngrams=1){
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





## function for print words splited by sentiment
# data :: word_frequency 
# n_filter :: value to filter words by count for pritty plot
# ngrams :: {1,2,3} for compute corpus on ngrams
words.computeSentiment <- function(data, n_filter=20, ngrams=1, plot = TRUE){
   
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
      
      if(plot){
         p <- sentiment_df %>%
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
      }
      
      
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
      
      if(plot){
         p <- sentiment_df %>%
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
      }
   } else {
      # plot positive-negative -- color: how much positive/negative is a word
      sentiment_df <- data %>% 
         inner_join(get_sentiments("afinn"), by=c('words'= 'word')) %>%
         inner_join(get_sentiments("bing"), by=c("words" = "word"))
      
      if(plot){
         p <- sentiment_df %>%
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
   }
   
   if(plot){ print(p) }
   return(sentiment_df)
}


words.network <- function(data, n_filter=1000){
   
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


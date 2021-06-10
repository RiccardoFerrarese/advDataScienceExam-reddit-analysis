print_ <- function(string){ paste0("\n", string, "\n") %>% cat()}

dfm.frequency <- function(.dfm, n = 50, plot = TRUE){
   
   # n :: top n features to be returned 
   features_dfm <- textstat_frequency(.dfm, n)
   
   head(features_dfm, 10)
   
   # Sort by reverse frequency order
   features_dfm$feature <- with(features_dfm, reorder(feature, -frequency))
   features_dfm$rank <- as.numeric(features_dfm$rank)
   if(plot){
      plot <- features_dfm %>%
         ggplot(aes(x = rank, y = frequency)) +
         geom_point(size=0.1) + 
         scale_y_log10() + 
         scale_x_log10() + 
         theme_classic() +
         theme(axis.text.x = element_blank(), 
               axis.title.x = element_text("log( rank )"), 
               axis.title.y = element_text("log( frequency )"))
      
      features_dfm$feature <- with(features_dfm, reorder(feature, -rank))
      #theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      mod = lm(log10(features_dfm$frequency) 
               ~ log10(features_dfm$rank), data = features_dfm)
      #print(mod$coefficients[1])
      plot2 <- features_dfm %>%
         ggplot(aes(x = rank, y = frequency)) +
         geom_abline(intercept = mod$coefficients[1], 
                     slope = mod$coefficients[2], 
                     color = 'red', 
                     linetype = 3
         ) +
         geom_line(size=0.5) + 
         scale_y_log10() + 
         scale_x_log10() + 
         theme_classic() +
         theme(axis.text.x = element_blank(), 
               axis.title.x = element_text("log( rank )"), 
               axis.title.y = element_text("log( frequency )"))
      
      print(plot + plot2)
   }
   return(features_dfm)
}

## data must have cols : (created_utc, author, body )
# data is pushshift's returned csv
df.create <- function(data, dir, subR, filter_post=2){
   
   # compute type date
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
      mutate( body = paste( do.call( c , body), collapse = "\n ") ) %>%
      ungroup() %>%
      # filtra post con meno di 5 commenti 
      filter(n_com > filter_post)
   
   
   
   post_corpus <- corpus(df_post$body, 
                         docnames = df_post$postID, 
                         docvars = data.frame(n = df_post$n_com))
   
   # quanteda corpus function 
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
# word:: ref to cols in data 
word.apply_regexs <- function(data, word){
   
   ## use curly-curly for word cols
   data %>% 
      mutate( {{word}} := str_extract({{word}},  "[a-z]+")) %>%
      # rimuove spazi ecc
      mutate( {{word}} := gsub("[[:punct:]]", "", {{word}})) %>%
      # word with all same char
      mutate( {{word}} := gsub("^([:alpha:])\1+", "", {{word}})) %>%
      mutate( {{word}} := gsub("^(a){5}[a-z]*", "", {{word}})) %>%
      # word with 1 or 2 char
      mutate( {{word}} := gsub("^[a-z]{1,2}\\b", "", {{word}})) %>%
      
      # word with numeber
      # mutate( word = gsub("^(\\d)+", "", word)) %>%
      
      # laughing word 
      mutate( {{word}} := gsub("\\b(?:a*(?:ha)+h?|h*ha+h[ha]*|(?:l+o+)+l+|o?l+o+l+[ol]*)\\b", "", {{word}}) ) %>%
      filter( {{word}} != "" ) 

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
corpus.tokenize_dfmTidy <- function(data, remove_stop=TRUE, spell_checking = FALSE, mode_correction = 0, ngrams=1, dfm_b=TRUE){
   
   # if stopword .. 
   # build toks  
   if(remove_stop){
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
   
   if( spell_checking ){
      
      # correct words with my dictionary 
      if( mode_correction == 0){
         dict <- readRDS("../Data/dictionary.rds")
         
         dict <- dict %>% drop_na()
         dic_words <- dict$term
         correct <- dict$correction
         
         toks <- tokens_replace(toks, dic_words, correct, case_insensitive = TRUE)
         
      } else if( mode_correction == 1){
         # correct all words with first hunspell's suggestion
      } else if( mode_correction == 2){
         # vader 
         
      }
   }
   
   # if ngrams -- compute toks for bigrams or trigrams 
   if(ngrams==2){
      dfm <- quanteda::dfm(quanteda::tokens_ngrams(toks, n = 2, 
                                                   concatenator = " "))
   } else if(ngrams==3){
      dfm <- quanteda::dfm(quanteda::tokens_ngrams(toks, n = 3, 
                                                   concatenator = " "))
   } else {
      dfm <- quanteda::dfm(toks)
   }
   
   # return dfm and tidy 
   
   rtn <- tidytext::tidy(dfm)
   if(dfm_b){
      rtn <- list(tidy = tidytext::tidy(dfm), dfm=dfm)
   }
   return(rtn)
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
         tidyr::separate(words, c("word1", "word2"), sep = " ", 
                         remove = FALSE) %>%       
         filter( word1 != word2 & word2 != word1 ) %>%
         # apply some regex
         word.apply_regexs(word1)  %>%                   
         word.apply_regexs(word2) %>%
         # apply stemming or lemmatiz ..
         word.manipulation(word1, word2, mode = .mode , ngrams = ngrams)
      
   } else if (ngrams == 3){
      clean_df <- tidy %>%
         unnest_tokens(words, term, token = "ngrams", n=3) %>%                     
         tidyr::separate(words, c("word1", "word2", "word3"), sep = " ",
                         remove = FALSE) %>%   
         filter( word1 != word2 & word2 != word3 & word1 != word3 ) %>%
         word.apply_regexs(word1) %>%                   
         word.apply_regexs(word2) %>%
         word.apply_regexs(word3) %>%
         word.manipulation(word1, word2, word3, 
                           mode = .mode , 
                           ngrams = ngrams)
      
      
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
corpus.countPlot_tidy <- function(data, 
                                  threshold_count=1000, 
                                  threshold_freq=0.001, 
                                  ngrams=1, plot = TRUE, 
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
   
   if(plot){
      # plot for count col
      plot_freq <- word_counts %>%
         filter( frequency > threshold_freq) %>%
         ggplot(aes(words, frequency)) + 
         geom_point(alpha = 0.3, size = 1.5) + 
         geom_text(aes(label = words), check_overlap = TRUE, vjust = 1) + 
         theme_classic() + 
         theme(axis.text.x=element_blank()) 
      
      # plot for frequency col
      plot_count <- word_counts %>%
         filter( count > threshold_count) %>%   
         ggplot(aes(words, count)) + 
         geom_point(alpha = 0.3, size = 1.5) + 
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

words.check_correct_word <- function(data, word){
   # counts word
   com_counts <- corpus.countPlot_tidy(data, bool_plot_count = FALSE, 
                                       bool_plot_frequency = FALSE)
   #print(com_counts)
   # check words
   com_correct <- com_counts %>%
      mutate.(check = hunspell_check({{word}})) 
   
   print(paste0("RAW WORDS: ", dim(com_correct)[1], sep = " "))
   
   # aggregate for words
   correct_unique <- aggregate(count ~ {{word}}+check, data=com_correct, FUN=sum)
   
   # extract correct 
   correct <- correct_unique %>%
      filter(check == TRUE) 
   
   print_(paste0("CORRECTS WORDS: ", dim(correct)[1],  "   ", 
                 round((dim(correct)[1]/dim(correct_unique)[1])*100, 2),
                 "%", sep = " "))
   
   
   # extract uncorrect 
   uncorrect <- com_correct %>%
      filter(check == FALSE) 
   
   
   print_(paste0("UNCORRECTS WORDS: ", dim(uncorrect)[1], "   ",  
                 round(dim(uncorrect)[1]/dim(correct_unique)[1]*100, 2),
                 "%", sep = " "))
   
   return(list(correct=correct, uncorrect=uncorrect))
}

words.spell_checking <- function(data, input) {
   data %>%
      mutate( suggest = 
                 case_when(
                    # any manual corrections
                    {{input}} == 'gbp' ~ 'gbp',
                    {{input}} == 'halving' ~ 'halving',
                    {{input}} == 'aways' ~ 'away',
                    {{input}} == 'covid' ~ 'covid',
                    {{input}} == 'binance' ~ 'binance',
                    {{input}} == 'stonks' ~ 'stonks',
                    {{input}} == 'cryptocurrency' ~ 'cryptocurrency',
                    {{input}} == 'cryptocurrencies' ~ 'cryptocurrency',
                    
                    {{input}} == "didn" ~ "did not", 
                    {{input}} == "doesn" ~ "does not", 
                    {{input}} == "aren" ~ "are not", 
                    {{input}} == "isn" ~ "is not", 
                    {{input}} == "isnt" ~ "is not",
                    {{input}} == "wasn" ~ "was not", 
                    {{input}} == "weren" ~ "were not", 
                    {{input}} == "couldn" ~ "could not",
                    {{input}} == "wasnt" ~ "was not", 
                    {{input}} == "wouldn" ~ "would not", 
                    {{input}} == "wouldnt" ~ "would not",
                    {{input}} == 'hadn' ~ 'had not', 
                    
                    {{input}} == "ive" ~ "i have",
                    {{input}} == "youre" ~ "you are",
                    
                    # check and (if required) correct spelling
                    !hunspell_check({{input}}) ~
                       hunspell_suggest({{input}}) %>%
                       # get first suggestion, or NA if suggestions list is empty
                       map(1, .default = NA) %>%
                       unlist() %>%
                       tolower(),
                    TRUE ~ {{input}} # if word is correct
                 ))
   # if input incorrectly spelled but no suggestions, return input word
   # ifelse(is.na(output), {{input}}, output)
}

word.little_correction <- function(input){
   output <- case_when(
      input == 'hodl' ~ 'hold',
      input == 'obis' ~ 'boys', 
      input == 'fuckin' ~ 'fucking', 
      input == 'ios' ~ 'ios', 
      input == 'gpu' ~ 'gpu', 
      input == 'gme' ~ 'gme',
      input == 'font' ~ 'dont',
      input == 'stonks' ~ 'stonks',
      grepl("^hold[d+]", input) ~ 'hold' ,
      grepl("^s[o+]", input) ~ 'so', 
      grepl("^g[o+]", input) ~ 'go',
      grepl("^usa", input) ~ 'usa',
      TRUE ~ input
   )
   ifelse(is.na(output), input, output)
}

lab.dizCompare<- function(correction = TRUE, stop=FALSE){
   
   if(correction){
      raw_data <- readRDS("../Data/post_doge_myCorrection.rds") # is a dfm
      # transform from tidy table to tidy object 
      raw_data <- tidytext::tidy(raw_data)
   }
   
   # clean tokens
   corpus_stem <- corpus.clean_tidy(raw_data,  mode = "stem")
   corpus_lem  <- corpus.clean_tidy(raw_data, mode = "lemma")
   corpus_raw <- corpus.clean_tidy(raw_data, mode = "none")
   
   # counts tokens
   count_stem <- corpus.countPlot_tidy(corpus_stem, 
                                       plot = FALSE)
   count_lem <- corpus.countPlot_tidy(corpus_lem, 
                                      plot = FALSE)
   count_raw <- corpus.countPlot_tidy(corpus_raw, 
                                      plot = FALSE)
   
   remove(corpus_stem)
   remove(corpus_lem)
   remove(corpus_raw)
   
   # build sentiment
   sentiment_stem <- words.computeSentiment(count_stem, plot=FALSE)
   sentiment_lem <- words.computeSentiment(count_lem, plot=FALSE)
   sentiment_raw <- words.computeSentiment(count_raw, plot=FALSE)
   
   
   word_raw.source <- as.set(count_raw$words)
   word_lem.source <- as.set(count_lem$words)
   word_stem.source <- as.set(count_stem$words)
   
   remove(count_stem)
   remove(count_lem)
   remove(count_raw)
   
   word_lem.sentiment <- as.set(sentiment_lem$words)
   word_raw.sentiment <- as.set(sentiment_raw$words)
   word_stem.sentiment <- as.set(sentiment_stem$words)
   
   s_raw <- set_cardinality(word_raw.source)
   r_raw <- set_cardinality(word_raw.sentiment)
   
   s_lem <- set_cardinality(word_lem.source)
   r_lem <- set_cardinality(word_lem.sentiment)
   
   s_stem <- set_cardinality(word_stem.source)
   r_stem <- set_cardinality(word_stem.sentiment)
   
   print( paste("Source total words: ", s_raw, '--', "Words used raw x sentiment: ", r_raw))
   print( paste("Source total words Lemmatization: ", s_lem, '--', "Words used lem x sentiment: ", r_lem))
   print( paste("Source total words Stemming: ", s_stem, '--', "Words used stem x sentiment: ", r_stem))
   
}

plot_com_per_date <- function(dataset){
   
   df_date <- dataset %>%
      select(date, link_id) 
   
   plot <- 
      df_date%>%
      group_by(date) %>%
      # retrieve comments for day 
      summarise(n_com = n()) %>%
      arrange(date) %>%
      # plot..
      ggplot(aes(date, n_com)) +
      geom_line(size = 0.3) +
      xlab("Date") + 
      ylab("Number of Comment") + 
      scale_y_log10() + 
      geom_vline(xintercept = as.Date('2020-07-01'), color = 'green3', size = 0.2) +
      geom_vline(xintercept = as.Date('2021-01-01'), color = 'orangered1', size = 0.2) +
      geom_vline(xintercept = as.Date('2021-02-01'), color = 'orangered1', size = 0.2) +
      geom_vline(xintercept = as.Date('2021-03-01'), color = 'orangered1', size = 0.2) +
      geom_vline(xintercept = as.Date('2021-04-01'), color = 'orangered1', size = 0.2) +
      geom_vline(xintercept = as.Date('2021-05-01'), color = 'grey', size = 0.2) +
      theme_minimal()
   
   print(plot)
}

corpus.compute_sentiment_per_comment <- function(tidy_clean_bigrams, data_txt){
   
   sentiment_per_doc <- tidy_clean_bigrams  %>% 
      inner_join(get_sentiments("afinn"), by=c('word1'= 'word')) %>%
      inner_join(get_sentiments("bing"), by=c('word1'= 'word')) %>%
      inner_join(get_sentiments("afinn"), by=c('word2'= 'word')) %>%
      inner_join(get_sentiments("bing"), by=c('word2'= 'word')) %>%
      mutate( affin = (value.x + value.y) / 2) %>%
      mutate( bing = case_when(sentiment.x == 'positive' & sentiment.y == 'positive' ~ 'positive', 
                               sentiment.x == 'negative' & sentiment.y == 'negative' ~ 'negative',
                               TRUE ~ 'neutral')) %>%
      
      
      mutate( affin_nrm = range01(affin)) %>%
      unite(words, c("word1", "word2"), sep = " ") %>%
      #  working with this task, each ID is a comment in the corpus, 
      # the same of the tidy dataset 
      mutate(id = as.integer(document)) %>%
      select(id, words, affin, affin_nrm, bing)
   
   
   sentiment_per_comment <- sentiment_per_doc %>%
      inner_join(data_txt, by='id') %>%
      select(date, author, id, words, affin, affin_nrm, bing)
   
   return(sentiment_per_comment)
}

plot_year_sentiment<- function(data, in_year, pol){
   
   df <- data %>%
      mutate(year_ = year(date)) %>%
      filter(year_ == in_year) %>%
      mutate(month = month(date)) %>%
      mutate(dayMonth = as.numeric(day(date))) %>%
      group_by(dayMonth, month) %>%
      
      mutate(color_ =  if_else( {{pol}} > 0.5, "green3", 
                                if_else( {{pol}} > - 0.5, "blue", "orangered2")) ) %>%
      mutate(mean_per_day = mean({{pol}}))
   
   #df <- aggregate(polarity_tan~., df, FUN=mean)
   
   plot <- df %>%
      ggplot() +
      geom_col(aes(dayMonth, {{pol}},  color = color_)) +
      geom_line(aes(dayMonth, mean_per_day), size = 0.1, color = 'black') +
      #geom_smooth(method = "loess", se = FALSE, aes(color='black')) +
      
      facet_grid(facets = month ~ ., margins = FALSE)+
      scale_y_continuous(trans = scales::pseudo_log_trans()) +
      theme_minimal() + theme(plot.title = element_text(size = 10),
                              axis.text.x = element_blank(), 
                              axis.text.y.left = element_blank(),
                              legend.position = 'none') +
      xlab(NULL) +
      ggtitle("Sentiment Polarity Over Time")
   
   return(list(plot = plot, df = df ))
}

compute_top_usr_net <- function(dataf, file_name, num_usr){
   
   if(num_usr>100){errorCondition("Too much user for compute graph visualization")}
   
   author_link_unique <- dataf %>%
      select(author, link_id) %>%
      filter(author != '[deleted]' & author != 'AutoModerator') %>%
      group_by(link_id) %>%
      unique()
   
   
   # number of post which each author interact with 
   top_usr <- author_link_unique %>% 
      group_by(author) %>% 
      summarise( n_aut = n()) %>% 
      ungroup() %>% 
      arrange(-n_aut) %>%
      head(num_usr)
   
   # take usr with most comment
   top_usr_info <- dataf %>%
      select(author, id, link_id) %>%
      filter( author %in% top_usr$author)
   
   # join for track interaction between most "top" usr 
   net_top_usr <- top_usr_info %>%
      inner_join(top_usr_info, by='link_id') %>%
      filter(author.x != author.y) %>%
      select(author.x, author.y, link_id, id.x, id.y)
   
   # write it on file 
   write.csv(net_top_usr, file=file_name)
   
   nodes <- top_usr_info %>%
      distinct(author)
   
   return(list(nodes=nodes, edges=net_top_usr))
}

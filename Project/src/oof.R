data_clean <- doge_com_raw_data %>%
   mutate(date = as.Date(as_datetime(created_utc))) %>%
   mutate(id = row_number()) %>%
   filter(author != '[deleted]' & author != 'AutoModerator') %>% 
   distinct( author, date, body, parent_id, link_id, .keep_all= TRUE) %>%
   select(id, date, author, body, parent_id, link_id) 



# quanteda corpus function 
corpus <- corpus( as.character(data_clean$body), 
                  docnames = data_clean$id, 
                  docvars = data.frame(author = data_clean$author,data =  data_clean$date, post = data_clean$link_id))

summary(corpus)

# install gsl [sudo apt-get install gsl-bin libgsl0-dev]
library(topicmodels)
LDA_fit_20 <- convert(doge_tidy_post$dfm, to = "topicmodels") %>% 
   LDA(k = 20)

# get top five terms per topic
get_terms(LDA_fit_20, 5)

#################################################
setwd("~/0_Uni/DataScience/ADv-DS-Proj/DataScience-NetTxtAnalysis/Project/")

if(!(exists('doge_com_raw_data'))){
   doge_com_raw_data <- read.csv("./Data/dogecoin_com.csv")
}

data <- create_corpus_from_rawdata(doge_com_raw_data)
#remove(doge_com_raw_data)

corpus_doge <- data$corpus
doge_txt <- data$df

corpus_doge 
doge_txt

toks <- quanteda::tokens_select(
                  quanteda::tokens(corpus_doge,
                         remove_punct = TRUE, 
                         remove_symbols = TRUE, 
                         remove_numbers = TRUE, 
                         remove_url = TRUE),
               pattern = stopwords("en"), selection = "remove")
summary(toks)

head(toks)
dfm <- dfm(toks)

# Trim DFM with dfm_trim() function
trimdfm <- dfm_trim(dfm, min_count = 10, min_docfreq = 5)

# See what data looks like - optional
trimdfm[1:10, 1:15]

mydf = convert(trimdfm, to = "data.frame")

# Append variables on to that dataframe
mydf2 <- cbind(doge_txt$id, mydf)

# Rename the raw_data$Category variable to "Category"
names(mydf2)[1] <- "ID"


tidy_dfm <- tidy(dfm)



library(hunspell)

library("stringdist")
stringdist::stringdist("ciao", "cioa")
stringdist::stringsim("ciao", "cioa")

unique_word <- tidy_dfm %>%
   unnest_tokens(words, term)  %>% 
   distinct(words) 

test <- unique_word %>% 
   apply_regex(words) %>%
   mutate(check = hunspell_check(words)) 


# 5 min ... 
correct_false <- test %>%
   filter(check == FALSE) %>%
   filter(words != "cryptocurrencies", 
          words != "dogecoin", 
          words != "doge", 
          words != "btc", 
          words != "eth", 
          words != "binance", 
          words != "blockchain") %>%
   mutate( word_ste = hunspell_stem(words)) %>%
   mutate(suggest_lst = hunspell_suggest(words))

saveRDS(correct_false, "./correct_false_hunspell_check.rds")


## grammar checking 
## spelling correction 


summary(test)
rowwise() %>%
mutate(suggest = unlist(suggest_list)[1] ) %>%
mutate(dist = ifelse(!check, stringdist::stringdist(words, suggest), 0))


unlist(row)

tidy_dfm %>%
   filter("correct" == TRUE)


# char all equal 
# number ^(\d)+

########################################
### DFM ANALYSIS

doge_dfm_post <- doge_post$dfm
head( docvars(doge_dfm_post))

topfeatures(doge_dfm_post)
textstat_frequency(doge_dfm_post, n = 5)
sub_dfm <- dfm_subset(doge_dfm_post, n_com > 5)

textstat_readability()
textstat_entropy()

tstat_dist <- as.dist(textstat_dist(doge_dfm_post , margin = "documents", min_simil = 0.5))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)


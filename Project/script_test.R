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

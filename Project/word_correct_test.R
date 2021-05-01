source("./functions.R")

## SCRIPT's WORKGYM FOR CORRECT WORDS and buil a fantastic vocabulary

Sys.setenv(DICPATH = "~/0_Uni/dict")
hunspell:::dicpath()
my_dict <- dictionary("~/0_Uni/dict")



raw_data <- read.csv("./Data/dogecoin_com.csv")
head(raw_data, 1)

dir = "./Data/"
subR = "doge"

paste0(dir, subR, ".rds", sep = "")

df.create(raw_data, dir = dir, subR = subR)

data <- readRDS("./Data/doge.rds")


post_raw <- data$corpus_post
post_df <- data$df_post


post1 <- post_df$body[1]
write_file(post1, "./Data/first-post.txt")

# create a corpus 
post1_corpus <- corpus(post_df$body[1], 
                docnames = post_df$postID[1], 
                docvars = data.frame(post_df$n_com[1])
                )


# tokenize corpus 
post1_toks <- corpus.tokenize_dfmTidy(post1_corpus)

# clean tidy toks
post1_clean <- corpus.clean_tidy(post1_toks$tidy)

post1_clean_lemma <- corpus.clean_tidy(post1_toks$tidy, mode = "lemma")
# after lemma doing another aggregation 

post1_count <- corpus.countPlot_tidy(post1_clean, bool_plot_count = FALSE, 
                                     bool_plot_frequency = FALSE)

post1lemma_count <- corpus.countPlot_tidy(post1_clean_lemma, bool_plot_count = FALSE, 
                                          bool_plot_frequency = FALSE)


post1_clean$lemma = post1_clean$words
post1_clean$stem = post1_clean$words
# word.manipulation mi fa un mutate ... clono le colonne per modificarle
# all'interno della mia funzione 
df <- post1_clean %>%
   # lemma
   word.manipulation(lemma, mode = 1) %>%
   # stemmin
   word.manipulation(stem, mode = 2)

View(df)

# parole senza corrispondenza nel lemma 
post1_count %>%
   anti_join(post1lemma_count)

# parole nel lemma senza corrispondenza
post1lemma_count %>%
   anti_join(post1_count)



correct_post_lemma <- post1lemma_count %>%
   unnest_tokens(word, words)  %>% 
   mutate(check = hunspell_check(word)) %>%
   select(word, check, count, total_of_word)

summary(correct_post)
summary(correct_post_lemma)

correct_post_lemma %>% filter( check == FALSE) %>% arrange( desc(count) )

suggest <- correct_post_lemma %>% 
   filter( check == FALSE) %>%
   mutate(suggest = unnest(hunspell_suggest(word)))

View(suggest)


########### WHIT ALL POST #################

post_toks <- corpus.tokenize_dfmTidy(post_raw)

# clean and tidy
post_tidy <- corpus.clean_tidy(post_toks$tidy, mode = "lemma")


# verify regex for digit not worked
post_tidy %>% filter( words == regex("2"))

post_tidy <- post_tidy %>% 
   # this regex didn't works in apply regex pipeline
   mutate( words = gsub("^[[:digit:]]+", "", words))

# counts
post_counts <- corpus.countPlot_tidy(post_tidy, bool_plot_count = FALSE, 
                                                bool_plot_frequency = FALSE)

# compute correctness
post_correct <- post_counts %>%
   unnest_tokens(term, words)  %>% 
   mutate(check = hunspell_check(term)) 

summary(post_correct)

# correct post
correct <- post_correct %>%
   filter(check == TRUE) 

dim(correct)

# apply stemming 
correct_stem <- correct %>%
   word.manipulation(term, mode = 2) 

correct_unique <- aggregate(count ~ term, data=correct_stem, FUN=sum)

dim(correct_unique)

### uncorrect words
uncorrect <- post_correct %>%
   filter(check == FALSE) 

dim(uncorrect)

# apply stemming 
library(stringdist)
uncorrect_stem <- uncorrect %>%
   cbind(term_stem = uncorrect$term) %>%
   cbind(term_lem = uncorrect$term) %>%
   word.manipulation(term_stem, mode = 2) %>%
   word.manipulation(term_lem, mode = 1) 

uncorrect_stem_unique <- aggregate(count ~ term, data=uncorrect_stem, FUN=sum)

dim(correct_unique)

# rifaccio un check con lo stem
uncorrect_stem_check <- uncorrect_stem %>%
   mutate(check_stem = hunspell_check(term_stem)) 

summary(uncorrect_stem_check)

dim(uncorrect_stem_check)[1]

correct_spelling <- function(input) {
   output <- case_when(
      # any manual corrections
      input == 'aways' ~ 'away',
      input == 'covid' ~ 'covid',
      input == 'binance' ~ 'binance',
      # check and (if required) correct spelling
      !hunspell_check(input) ~
         hunspell_suggest(input) %>%
         # get first suggestion, or NA if suggestions list is empty
         map(1, .default = NA) %>%
         unlist() %>%
         tolower(),
      TRUE ~ input # if word is correct
   )
   # if input incorrectly spelled but no suggestions, return input word
   ifelse(is.na(output), input, output)
}

# apply suggest
suggest_stem_corFalse <- uncorrect_stem_check %>%
   mutate(suggest = correct_spelling(term)) %>%
   mutate(dist_term = stringdist::stringdist(term, suggest, "lv")) %>%
   mutate(dist_stem = stringdist::stringdist(term_stem, suggest, "lv")) %>%
   mutate(dist_source_stem = stringdist::stringdist(term, term_stem, "lv")) %>%
   mutate(dist_lem = stringdist::stringdist(term_lem, suggest, "lv")) %>%
   select(count, term, suggest, term_stem, term_lem, dist_term, dist_lem, dist_source_stem)
   
write_rds(suggest_stem_corFalse, "doge_suggest.rds")

## influnce word 
suggest_stem_corFalse %>%
   arrange( desc(count)) %>%
   filter(count > 500) %>%
   select(term, suggest, term_stem, term_lem) %>%
   View()

write_csv(suggest_stem_corFalse, "doge-dic-influence.csv")

suggest_stem_corFalse <- read.csv("./doge-dic-influence.csv")

as.factor(suggest_stem_corFalse$count)
# 302 levels 


## !! COLLAPSE SPACE IN SUGGEST 
suggest_stem_corFalse %>%
   filter(count < 2) %>%
   arrange( desc(count)) %>%
   select( term, count )

suggest <- suggest_stem_corFalse %>%
   filter(dist_term == 0 ) %>%
   select(count, term, suggest)

suggest_2 <- suggest_stem_corFalse %>%
   filter(dist_source_stem == dist_term ) %>%
   select(count, term, suggest)


# not perfect 
suggest_3 <- suggest_stem_corFalse %>%
   filter(dist_source_stem - dist_term < 2 )%>%
   select(count, term, suggest)

suggest_4 <- suggest_stem_corFalse %>%
   filter(count > 10 ) %>%
   select(count, term, suggest, term_stem,dist_term, dist_source_stem)

sum_my <- function(a, b, c){
   return(a+b+c)
}

print( paste(dim(suggest)[1], dim(suggest_2)[1], dim(suggest_3)[1], sum(
   dim(suggest)[1], dim(suggest_2)[1], dim(suggest_3)[1]
), sep = " --> " ))
   
# se la word stem la conosco prendo quella 

# se dist term-stem == dist suggest-term

# se la differenza tra le due è minore di uno prendo 


   

   


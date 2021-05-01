
# DEF DF
word1 = c("_aaanc_", "*as*bel*", ".._34ciao#", "1")
id = c(1, 2, 3, 4)

data = data_frame(word1, id)
data_bigrams <- data %>% 
   cbind( word2 = word1) %>%
   tibble() #!!

# OK?
data
data_bigrams

## TEST REGEX
data %>% word.apply_regexs(word1)

data_bigrams %>% word.apply_regexs(word2)

data_bigrams %>% 
   word.apply_regexs(word1) %>%
   word.apply_regexs(word2)

## TESTING CURLY CURLY

# fun R-base
g <- function(.df, term){
   # ???
   .df$term <- str_extract(.df$term, "[a-z]+")
}

s <- g(data_bigrams, word1)

f <- function(.df, term){
   .df <- .df %>% 
      mutate( {{term}} := str_extract({{term}},  "[a-z]+")) %>%
      arrange(desc({{term}} ))
   return(.df)
}

f_ <- function(.df, term){
   .df %>% 
      mutate( {{term}} := str_extract({{term}},  "[a-z]+")) %>%
      sort(desc({{term}}))
}

df2 <- f(data, word1)
df3 <- f(data, word1)

df2 == df3

f__ <- function(.df, term, term2){
   .df %>% 
      dplyr::mutate( {{term}} := str_extract({{term}},  "[a-z]+")) %>%
      tidygraph::mutate( {{term2}} := str_extract({{term2}},  "[a-z]+")) %>%
      tidygraph::arrange( desc({{term}}) ) 
}

df4 <- f__(data_bigrams, word1, word2)

data_bigrams == df4

data <- data_frame(word1, id)
data$word1 <- str_extract(data$word1, "[a-z]+")
data




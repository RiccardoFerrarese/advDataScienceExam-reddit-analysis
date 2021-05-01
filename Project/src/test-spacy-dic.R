library(spacyr)
? spacy_parse

post_spacy_parse <- spacy_parse(post_raw[1:100, ], 
            pos = T, 
            tag = T,
            lemma = T)
            #entity = T)
            #dependency = T)

View(post_spacy_parse)

##########################################
source("./functions.R")

df.typeof <- function(.df){
   print(paste0(
      typeof(.df), 
      dim(.df), sep = "<->"))
}

dic <- read.csv("./doge-dic-influence.csv")

# dic with influence uncorret words 
dic_infl <- dic %>%
   filter(count >= 500 ) %>%
   arrange( desc(count))

# select suggestion
my_dic <- dic_infl %>%
   mutate(suggest = str_replace(suggest, " ", "")) %>%
   # se dist tra source e stemming è pari a zero prendo quella 
   mutate(correction = ifelse(dist_source_stem == 0, as.character(term_stem), suggest)) %>%
   select(count, term, correction)

# little correction on selected suggest 
dic_large <- my_dic %>%
   mutate( term = as.character(term)) %>%
   mutate( correction = little_correction(term) )

#  ????????
#  holddddd         holddddd
#  buyyyy           buyyyy
little_correction <- function(input){
   output <- case_when(
      input == 'hodl' ~ 'hold',
      input == 'bois' ~ 'boys', 
      input == 'fuckin' ~ 'fucking', 
      input == 'ios' ~ 'ios', 
      input == 'gpu' ~ 'gpu', 
      grepl("^g+o*", input) ~ 'go',
      grepl("^usa", input) ~ 'usa',
      TRUE ~ input
   )
   ifelse(is.na(output), input, output)
}

# merge with dic of true word



dic_medium <- dic %>%
   filter( count >= 100 & count < 500 ) %>%
   arrange( desc(count)) %>%
   mutate(suggest = str_replace(suggest, " ", "")) %>%
   # se dist tra source e stemming è pari a zero prendo quella 
   mutate(correction = ifelse(dist_source_stem == 0, as.character(term_stem), suggest)) %>%
   mutate( term = as.character(term)) %>%
   mutate( correction = little_correction(term) ) %>%
   select(count, term, correction)



dic_small <- dic %>%
   filter( count < 100 ) %>%
   arrange( desc(count)) %>% 
   filter( term %in% dic_large$term )


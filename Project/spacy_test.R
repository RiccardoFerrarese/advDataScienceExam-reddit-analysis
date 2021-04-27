library(spacyr)
? spacy_parse

post_spacy_parse <- spacy_parse(post_raw[1:100, ], 
            pos = T, 
            tag = T,
            lemma = T)
            #entity = T)
            #dependency = T)

View(post_spacy_parse)

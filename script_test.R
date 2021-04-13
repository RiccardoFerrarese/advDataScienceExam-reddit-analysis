as.Date('2020-12-31')

head(doge_com_raw_data)

## for each user 
## comment which each user interact 

plot_com_for_date(doge_com_raw_data, doge_com_raw_data$created_utc, doge_com_raw_data$link_id)

doge_com_raw_data <- read.csv("../data/dogecoin_com.csv")
colnames(doge_com_raw_data)

remove(doge_com_raw_data)

# prendo la coppia utent + post univoca 
# se un utente ha commentato più volte non lo conto 
author_link_unique <- doge_com_raw_data %>%
   select(author, link_id) %>%
   filter(author != '[deleted]' & author != 'AutoModerator') %>%
   group_by(link_id) %>%
   unique()

# numero di post in cui ogni utente ha interagito 
top_usr <- author_link_unique %>% 
   group_by(author) %>% 
   summarise( n_aut = n()) %>% 
   ungroup() %>% 
   arrange(-n_aut) %>% 
   head(100)
   
top_usr_info <- author_link_unique %>%
   filter( author %in% top_usr$author)

net_top_usr <- top_usr_info %>%
   inner_join(top_usr_info, by='link_id') %>%
   filter(author.x != author.y) %>%
   select(author.x, author.y, link_id)


write.csv(net_top_usr, file="data/usr_doge_net",
          row.names=FALSE)

nlevels(net_top_usr$link_id)

data <- read.csv('data/usr_doge_net')
colnames(data)

library(tidygraph)

data %>% rename(
   from = author.x, 
   to = author.y, 
   post_id = link_id
)

graph_topcom_doge <- data %>% 
   as_tbl_graph()

graph_topcom_doge %>% 
   activate(nodes) %>%
   mutate(pagerank = centrality_pagerank()) %>%
   activate(edges) %>%
   mutate(betweenness = centrality_edge_betweenness()) %>%
   ggraph() +
   geom_edge_link(aes(alpha = betweenness, colour = link_id)) +
   geom_node_point(aes(size = pagerank, colour = pagerank)) + 
   # discrete colour legend
   scale_color_gradient(guide = 'legend')





# prendere i primi _tot_ che hanno interagito di più e costruisci la network 
# grandezza nodo -> numero commenti 
# centralità ????





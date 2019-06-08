library(tidyverse)
library(pdftools)
library(tidytext)
library(ggraph)
library(igraph)
library(png)
radio <- readPNG("download.png") 

# Download screenplay
episode_name <- c("11_23_45", "2Please-Remain-Calm", "3Open-Wide-O-Earth",
                  "4The-Happiness-Of-All-Mankind", "5Vichnaya-Pamyat")


for(episode in episode_name){
  download.file(
    paste0("https://johnaugust.com/wp-content/uploads/2019/06/Chernobyl_Episode-",
           episode, ".pdf"), 
    paste0(episode, ".pdf"), mode = "wb"
  )
  
}

pdf_names <- paste0(episode_name, ".pdf")
raw_text <- map(pdf_names, pdf_text)

chernobyl <- tibble(episode = pdf_names, text = raw_text) %>% 
  mutate(
    episode =
      case_when(
        .$episode == "11_23_45.pdf" ~ "1:23:45",
        .$episode == "2Please-Remain-Calm.pdf" ~ "Please Remain Calm",
        .$episode == "3Open-Wide-O-Earth.pdf" ~ "Open Wide, O Earth",
        .$episode == "4The-Happiness-Of-All-Mankind.pdf" ~ "The Happiness Of All Mankind",
        TRUE           ~ "Vichnaya Pamyat"
      )
  )

chernobyl_tidy <- chernobyl %>% 
  unnest %>% # pdfs_text is a list
  unnest_tokens(word, text, strip_numeric = TRUE) %>% 
  anti_join(stop_words) 

characters <- c("legasov", "shcherbina", "dyatlov", "bacho", "pavel",
                "shcherbina", "khomyuk", "toptunov", "akimov", "lyudmilla",
                "bryukhanov", "tarakanov", "fomin", "sitnikov", "gorbachev",
                "vasily", "pikalov", "dmitri", "yuvchenko", "gorbachenko")

chernobyl_tidy %>% 
  group_by(episode, word) %>% 
  count(sort = TRUE) %>% 
  filter(!word %in% characters) %>% 
  group_by(episode) %>% 
  top_n(10, n) %>%
  ungroup() %>% 
  mutate(
    episode = fct_reorder(episode, n)
  ) %>% 
  ggplot(aes(word, n))+
  geom_col(fill = "#0D0D0D")+
  facet_wrap(~episode, scales = "free_y")+
  coord_flip()+
  theme_minimal()+theme(legend.position = "none")+
  labs(caption = "Viz: @RiversArthur \nData: https://johnaugust.com ",
       x = NULL, y = NULL)+
  theme(panel.background = element_rect(fill = "#F2E205"), strip.text = element_text(size = 13),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.background = element_rect(fill = "#F2E205", color = "#F2E205"))

chernobyl_bigram <- chernobyl_tidy %>% 
  unnest_tokens(bigram, word, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  group_by(episode) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 4) %>% 
  graph_from_data_frame()


ggraph(chernobyl_bigram, layout = "auto")+ 
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label = name, vjust = 1, hjust = 1), size = 3)+
  theme_void()

a <- grid::arrow(type = "closed", length = unit(.10, "inches"))
my_net <- ggraph(chernobyl_bigram, layout = "fr") +  
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, end_cap = circle(.05, 'inches')) +
  geom_node_point(color = "#0D0D0D", size = 4) +  
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, color = "#736002", size = 5) +  
  theme_void()+
  labs(caption = "Viz: @RiversArthur \nData: https://johnaugust.com ")+
  theme(panel.background = element_rect(fill = "#F2E205"),plot.background = element_rect(fill = "#F2E205",
                                                                                         color = "#F2E205"))
my_net


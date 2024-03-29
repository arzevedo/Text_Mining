A text analysis of The Lonely Island’s lyrics
================
Arthur Azevedo
12/06/2019

# First things first

*“If you are under 30 and you’re totally cool, then you absolutely know
these guys”*  
The very first sentence of the first song in the third studio album by
*The Lonely Island* (TLI) did find me under 30 and now 6 years later I’m
stil totally cool. With the newest album finally out i had to that a
look, and **why not** thake a look in the old songs too.

So… let’s get the data.

## Extracting the data

I used the [geniusr](https://github.com/ewenme/geniusr) package to
extract all lyrics from all TLI. You can find the script
[here](https://github.com/arzevedo/Text_Mining/blob/master/The%20Lonely%20Island/the_lonely_island_lyrics_scrap.R)
with you want to do it yourself, but you will have to make a genius
account to use their API. Anyway, i scraped the hole thing into a .csv
file:

``` r
tli_lyrics <- read_csv("https://raw.githubusercontent.com/arzevedo/Text_Mining/master/The%20Lonely%20Island/the_lonely_island_lyrics.csv")
```

## *“There’s no way I’ll let my kids listen to it”*

The first thing that I’ll do is to look at the most frequent words,
let’s see per Album.

``` r
tli_lyrics_words <- tli_lyrics %>% 
  unnest_tokens(word, line) %>% 
  mutate(album_name = factor(album_name,
                             levels = c("Incredibad", "Turtleneck and Chain",
                                        "The Wack Album", "Popstar: Never Stop Never Stopping",
                                        "The Unauthorized Bash Brothers Experience", NA)))


tli_lyrics_words %>% 
  filter(!word %in% stop_words$word) %>% 
  group_by(album_name) %>% 
  filter(!is.na(album_name)) %>% 
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, n, album_name)) %>% 
  ggplot(aes(x = word, y = n))+
  geom_col()+
  labs(x = NULL, y = "Count", caption = "Viz: @RiversArthur",
       title = "No Homo")+
  scale_x_reordered()+
  coord_flip()+
  facet_wrap(~album_name, scales = "free_y")+
  theme_minimal()
```

![](The_Lonely_Island_files/figure-gfm/part1-1.png)<!-- -->

As I thought\! lots of *boss*, *jizz* and *pants* in the first one.
Well, a lot of f\*\*\* words in the popstar, and probabily mark and josé
are the so called bash brothers from the latest album.  
4

``` r
tli_lyrics_words_per_album <- tli_lyrics_words %>% 
  count(album_name, word, sort = TRUE) %>% 
  ungroup()

total_words <- tli_lyrics_words_per_album %>% 
  group_by(album_name) %>% 
  summarise(total = sum(n))

tli_lyrics_words_per_album <- left_join(tli_lyrics_words_per_album, total_words)
```

    ## Joining, by = "album_name"

``` r
tli_lyrics_words_per_album <- tli_lyrics_words_per_album %>%
  bind_tf_idf(word, album_name, n) %>% 
  filter(album_name != "NA")

tli_lyrics_words_per_album %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(album_name) %>%
  top_n(10) %>% 
  ungroup %>% 
  ggplot(aes(reorder_within(word, tf_idf, album_name), tf_idf)) +
  geom_col(show.legend = FALSE, aes(fill = album_name))+
  facet_wrap(~album_name, scales = "free_y") + 
  coord_flip()+
  scale_x_reordered()+ scale_y_continuous(labels = c("Less important words\n in each album",
                                                     "Important words\n in each album"),
                                          breaks = c(0.001,.023))+
  theme_minimal()+theme(legend.position = "none")+
  labs(caption = "Viz: @RiversArthur",
       x = NULL, y = NULL, title = "Highest tf-idf words in each of TLI's album")+
  theme(strip.text = element_text(size = 8),
        panel.grid.major = element_blank(), panel.grid = element_blank(),
        axis.text.y = element_text(size = 10))
```

    ## Selecting by tf_idf

![](The_Lonely_Island_files/figure-gfm/TF-IDF-1.png)<!-- -->

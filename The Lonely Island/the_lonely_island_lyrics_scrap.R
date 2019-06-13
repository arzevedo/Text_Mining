library(geniusr)

tli_id <- search_artist("The-lonely-island") %>% 
  pull(artist_id)
tli_meta <- get_artist_meta(
  artist_id = tli_id
)

tli_songs <- get_artist_songs(
  artist_id = tli_id
)
tli_songs_id <- tli_songs %>% pull(song_id)
tli_songs_link <- tli_songs %>% pull(song_lyrics_url)

tli_songs_meta <- map_df(
  tli_songs_id,
  get_song_meta
)

tli_songs_lyrics <- map_df(
  tli_songs_link,
  scrape_lyrics_url
)


tli_songs_lyrics %>% 
  group_by(song_name) %>% 
  mutate(line_number = row_number()) %>% 
  ungroup() %>% 
  inner_join(tli_songs_meta) %>% 
  write_csv("The Lonely Island/the_lonely_island_lyrics.csv")

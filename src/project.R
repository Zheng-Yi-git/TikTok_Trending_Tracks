# project for ASDA
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(corrgram)

general = read_csv("ASDA2022/TikTok/_TIKTOK_playlist_tracks_data.csv")
PH = read_csv("ASDA2022/TikTok/TIKTOK PHILIPPINES_playlist_tracks_data.csv")
DANCE_playlist_tracks = read_csv("ASDA2022/TikTok/TIKTOK DANCE_playlist_tracks_data.csv")
OPM_playlist_tracks = read_csv("ASDA2022/TikTok/TIKTOK OPM_playlist_tracks_data.csv")
# tmp = summarise(group_by(playlist_tracks, track_name), n())
top20 = playlist_tracks %>% 
    arrange(desc(popularity)) %>% 
    head(20)
top20 %>% 
    mutate(track_name = fct_reorder(track_name, desc(popularity))) %>% 
    ggplot(aes(track_name, popularity)) + 
    geom_bar(stat="identity", fill="steelblue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
cor.mat = cor(playlist_tracks[, 8:19])
corrgram(cor.mat, order = T, lower.panel = panel.shade, upper.panel = panel.pie)

# Whether the features of the top20 are significantly different with the others
top20_features = apply(top20[, 8:19], 2, mean)
overall_features = apply(playlist_tracks[, 8:19], 2, mean)
features = as.data.frame(rbind(top20_features, overall_features))
features$tag = c("top20", "overall")
ggplot(data = features, aes(tag, speechiness)) +
    geom_bar(stat="identity", fill=c("red", "steelblue"))
# seems no significant difference


# compare general tiktok with tiktok_PHILIPPINES

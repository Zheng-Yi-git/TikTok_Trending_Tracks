library(dplyr)
library(readr)
library(ggplot2)
library(colorspace)

# data frames
tracks1_df = read_csv("TIKTOK DANCE_playlist_tracks_data.csv")
tracks2_df = read_csv("TIKTOK OPM_playlist_tracks_data.csv")
tracks3_df = read_csv("TIKTOK PHILIPPINES_playlist_tracks_data.csv")
tracks4_df = read_csv("_TIKTOK_playlist_tracks_data.csv")

tracks1_df["duration"] = tracks1_df["duration"]/60000   # to represent "duration" in minutes
tracks2_df["duration"] = tracks2_df["duration"]/60000
tracks3_df["duration"] = tracks3_df["duration"]/60000
tracks4_df["duration"] = tracks4_df["duration"]/60000

dim (tracks4_df)

# bind "general" case with "PHILIPPINE" to plot!

tracks_bind = rbind(tracks3_df, tracks4_df)
tracks_bind$tag = 1:2495
tracks_bind$tag[1:770] = "PHILIPPINES"
tracks_bind$tag[771:2495] = "general"

fig = ggplot(data = tracks_bind, aes(x = tempo, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
fig


for(col in c("danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "tempo")){
  fig = ggplot(data = tracks_bind, aes(x = col, color = tag, fill = tag)) +
    geom_density(alpha = 0.5) +
    labs(title = "cat")
}

fig



#--------------------------------------------------------------------------------------

# comparison between "dance" and "OPM"
tracks_dance_OPM = rbind(tracks1_df, tracks2_df)
tracks_dance_OPM$tag = 1:2150
tracks_bind$tag[1:1891] = "dance"
tracks_bind$tag[1892:2150] = "OPM"

col = c("danceability")

fig = ggplot(data = tracks3_df)
fig + geom_bar()
fig + geom_histogram( fill="red",color="black",alpha=0.3,data = tracks3_df, aes(x = danceability), inherit.aes = F, show.legend = T)
#fig + geom_histogram( fill="blue",color="black",alpha=0.3, aes(x = tracks4_df$danceability))
#fig + geom_histogram(stat = "count", binwidth = 0.05, data = tracks4_df, aes(x = col))
#t = paste("TIKTOK PHILIPPINES vs _TIKTOK:", "danceability")
#fig + labs(title = t)
#fig + ylab("Frequency")
print(fig)








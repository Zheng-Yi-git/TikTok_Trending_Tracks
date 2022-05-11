library(dplyr)
library(readr)
library(ggplot2)
library(colorspace)
library(corrgram)
library(ellipse)
library(GGally)

# data frames
tracks1_df = read_csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/ASDA/Project/Code/07-TikTok Trending Tracks/TIKTOK DANCE_playlist_tracks_data.csv")
tracks2_df = read_csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/ASDA/Project/Code/07-TikTok Trending Tracks/TIKTOK OPM_playlist_tracks_data.csv")
tracks3_df = read_csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/ASDA/Project/Code/07-TikTok Trending Tracks/TIKTOK PHILIPPINES_playlist_tracks_data.csv")
tracks4_df = read_csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/ASDA/Project/Code/07-TikTok Trending Tracks/_TIKTOK_playlist_tracks_data.csv")

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

## tempo
fig = ggplot(data = tracks_bind, aes(x = tempo, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## danceability
fig = ggplot(data = tracks_bind, aes(x = danceability, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## energy
fig = ggplot(data = tracks_bind, aes(x = energy, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## key
fig = ggplot(data = tracks_bind, aes(x = key, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## loudness
fig = ggplot(data = tracks_bind, aes(x = loudness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## mode
fig = ggplot(data = tracks_bind, aes(x = mode, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## speechiness
fig = ggplot(data = tracks_bind, aes(x = speechiness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## acousticness
fig = ggplot(data = tracks_bind, aes(x = acousticness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## instrumentalness
fig = ggplot(data = tracks_bind, aes(x = instrumentalness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## liveness
fig = ggplot(data = tracks_bind, aes(x = liveness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)





#--------------------------------------------------------------------------------------

# comparison between "dance" and "OPM"
tracks_dance_OPM = rbind(tracks1_df, tracks2_df)
tracks_dance_OPM$tag = 1:2150
tracks_dance_OPM$tag[1:1891] = "dance"
tracks_dance_OPM$tag[1892:2150] = "OPM"

## tempo
fig = ggplot(data = tracks_dance_OPM, aes(x = tempo, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## danceability
fig = ggplot(data = tracks_dance_OPM, aes(x = danceability, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## energy
fig = ggplot(data = tracks_dance_OPM, aes(x = energy, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## key
fig = ggplot(data = tracks_dance_OPM, aes(x = key, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## loudness
fig = ggplot(data = tracks_dance_OPM, aes(x = loudness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## mode
fig = ggplot(data = tracks_dance_OPM, aes(x = mode, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## speechiness
fig = ggplot(data = tracks_dance_OPM, aes(x = speechiness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## acoustiness
fig = ggplot(data = tracks_dance_OPM, aes(x = acoustiness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## instrumentalness
fig = ggplot(data = tracks_dance_OPM, aes(x = instrumentalness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)

## liveness
fig = ggplot(data = tracks_dance_OPM, aes(x = liveness, color = tag, fill = tag)) +
  geom_density(alpha = 0.5) +
  labs(title = "asda")
print(fig)


# --------------------------------------------------------------------------------

# correlation plot of general

general_corr = cor(tracks4_df[,8:19])
corrgram(general_corr, order = T, lower.panel = panel.shade, upper.panel = panel.pie)

# pairplot of general
#### The following 2 methods are both damn ugly! Notice that it takes a loooooooong time to finish running, so please wait.

pairs(tracks4_df[8:19])
ggpairs(data = tracks4_df, columns = 8:19) + ggtitle("The General Pairplot")

# --------------------------------------------------------------------------------

# linear regression
## primitive regression model 

reg1 = lm(data = tracks4_df, popularity ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + tempo)

### summary and anova table
summary(reg1)
anova(reg1)

### diagnostics
plot(reg1)














library(dplyr)
library(readr)
library(ggplot2)
library(colorspace)

# data frames
rawdata = read_csv("ASDA2022/TikTok/data/_TIKTOK_playlist_tracks_data.csv")

## shuffle
set.seed(42)
rawdata = rawdata[sample(nrow(rawdata), nrow(rawdata)), ]

id = rep(1:10, length.out = nrow(rawdata))
id = sample(id, nrow(rawdata))

train.data = rawdata[id != 10, ]
test.data = rawdata[id == 10, ]
write.csv(train.data, file = "ASDA2022/TikTok/data/train.csv", row.names = T)
write.csv(test.data, file = "ASDA2022/TikTok/data/test.csv", row.names = T)

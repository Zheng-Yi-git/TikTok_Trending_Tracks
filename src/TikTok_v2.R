library(dplyr)
library(readr)
library(ggplot2)
library(colorspace)
library(corrgram)
library(ellipse)
library(GGally)
library(MASS)

# data frames
tracks1_df = read_csv("ASDA2022/TikTok/data/TIKTOK DANCE_playlist_tracks_data.csv")
tracks2_df = read_csv("ASDA2022/TikTok/data/TIKTOK OPM_playlist_tracks_data.csv")
tracks3_df = read_csv("ASDA2022/TikTok/data/TIKTOK PHILIPPINES_playlist_tracks_data.csv")
tracks4_df = read_csv("ASDA2022/TikTok/data/_TIKTOK_playlist_tracks_data.csv")

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

## acousticness
fig = ggplot(data = tracks_dance_OPM, aes(x = acousticness, color = tag, fill = tag)) +
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

### Partial Residuals Plot

#### danceability

reg.exdance = lm(data = tracks4_df, popularity ~ energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
reg.dance = lm(data = tracks4_df, danceability ~ energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.dance$residuals, y = reg.exdance$residuals)) + geom_smooth(method = "lm") + geom_point()

#### energy

reg.exenergy = lm(data = tracks4_df, popularity ~ danceability + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
reg.energy = lm(data = tracks4_df, energy ~ danceability + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.energy$residuals, y = reg.exenergy$residuals)) + geom_smooth(method = "lm") + geom_point()

#### key

reg.exkey = lm(data = tracks4_df, popularity ~ danceability + energy + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
reg.key = lm(data = tracks4_df, key ~ danceability + energy + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.key$residuals, y = reg.exkey$residuals)) + geom_smooth(method = "lm") + geom_point()

#### loudness

reg.exloud = lm(data = tracks4_df, popularity ~ danceability + energy +  key + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
reg.loud = lm(data = tracks4_df, loudness ~ danceability + energy +  key + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.loud$residuals, y = reg.exloud$residuals)) + geom_smooth(method = "lm") + geom_point()

#### mode

reg.exmode = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
reg.mode = lm(data = tracks4_df, mode ~ danceability + energy +  key + loudness + speechiness + acousticness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.mode$residuals, y = reg.exmode$residuals)) + geom_smooth(method = "lm") + geom_point()


#### speechiness

reg.exspeech = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + acousticness + instrumentalness + liveness + valence + tempo)
reg.speech = lm(data = tracks4_df, speechiness ~ danceability + energy +  key + loudness + mode + acousticness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.speech$residuals, y = reg.exspeech$residuals)) + geom_smooth(method = "lm") + geom_point()

#### log(speechiness)

tracks4_df$speechiness_log = log(tracks4_df$speechiness)
  
reg.exspeech_log = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + acousticness + instrumentalness + liveness + valence + tempo)
reg.speech_log = lm(data = tracks4_df, speechiness_log ~ danceability + energy +  key + loudness + mode + acousticness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.speech_log$residuals, y = reg.exspeech_log$residuals)) + geom_smooth(method = "lm") + geom_point()

#### acousticness

reg.exacoustic = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + speechiness + instrumentalness + liveness + valence + tempo)
reg.acoustic = lm(data = tracks4_df, acousticness ~ danceability + energy +  key + loudness + mode + speechiness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.acoustic$residuals, y = reg.exacoustic$residuals)) + geom_smooth(method = "lm") + geom_point()

#### log(acousticness) (totally useless, I might try another one later.)

tracks4_df$acousticness_log = log(tracks4_df$acousticness)

reg.exacoustic_log = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + speechiness + instrumentalness + liveness + valence + tempo)
reg.acoustic_log = lm(data = tracks4_df, acousticness_log ~ danceability + energy +  key + loudness + mode + speechiness + instrumentalness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.acoustic_log$residuals, y = reg.exacoustic_log$residuals)) + geom_smooth(method = "lm") + geom_point()

#### instrumentalness

reg.exinstru = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + liveness + valence + tempo)
reg.instru = lm(data = tracks4_df, instrumentalness ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.instru$residuals, y = reg.exinstru$residuals)) + geom_smooth(method = "lm") + geom_point()

#### sqrt(instrumentalness) (8 might be a good one to disperse)

tracks4_df$instrumentalness_sqrt = (tracks4_df$instrumentalness)^(1/8)

reg.exinstru_sqrt = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + liveness + valence + tempo)
reg.instru_sqrt = lm(data = tracks4_df, instrumentalness_sqrt ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + liveness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.instru_sqrt$residuals, y = reg.exinstru_sqrt$residuals)) + geom_smooth(method = "lm") + geom_point()

#### liveness

reg.exlive = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + instrumentalness + valence + tempo)
reg.live = lm(data = tracks4_df, liveness ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + instrumentalness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.live$residuals, y = reg.exlive$residuals)) + geom_smooth(method = "lm") + geom_point()

#### log(liveness)

tracks4_df$liveness_log = log(tracks4_df$liveness)

reg.exlive_log = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + instrumentalness + valence + tempo)
reg.live_log = lm(data = tracks4_df, liveness_log ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + instrumentalness + valence + tempo)
ggplot(data = reg.dance, aes(x = reg.live_log$residuals, y = reg.exlive_log$residuals)) + geom_smooth(method = "lm") + geom_point()

#### valence

reg.exvalence = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + instrumentalness +liveness + tempo)
reg.valence = lm(data = tracks4_df, valence ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + tempo)
ggplot(data = reg.dance, aes(x = reg.valence$residuals, y = reg.exvalence$residuals)) + geom_smooth(method = "lm") + geom_point()

#### tempo

reg.extempo = lm(data = tracks4_df, popularity ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + instrumentalness +liveness + valence)
reg.tempo = lm(data = tracks4_df, tempo ~ danceability + energy +  key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence)
ggplot(data = reg.dance, aes(x = reg.tempo$residuals, y = reg.extempo$residuals)) + geom_smooth(method = "lm") + geom_point()




#----------------------------------------------------------------------------------------------------------

## primitive regression model 

reg.general = lm(data = tracks4_df, popularity ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo)

### summary and anova table
summary(reg.general)
anova(reg.general)









### diagnostics
plot(reg.general)

#### BoxCox (This transformation seems uninterpretable, I shall try another one)

new_tracks4_df = tracks4_df
new_tracks4_df = new_tracks4_df[which(new_tracks4_df$popularity > 0), ]
loglike=boxcox(popularity ~ danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + tempo,lambda=seq(0, 2, length=10), data = new_tracks4_df)

loglike$x[which.max(loglike$y)]
k2=(prod((new_tracks4_df$popularity)^(1/length(new_tracks4_df$popularity))))
print(k2)
lambda=seq(1, 2, by=0.1)
transformed=NULL
for(i in 1:length(lambda))
{
  k1=1/(lambda[i]*k2^(lambda[i]-1))
  trans_y=if(lambda[i]==0)
  {k2*(log(new_tracks4_df$popularity))}else
  {k1*((new_tracks4_df$popularity)^lambda[i]-1)}
  a2=cbind(new_tracks4_df,lambda=rep(lambda[i],length(new_tracks4_df$popularity)),trans_y)
  transformed=rbind(transformed,a2)
}

reg1 = lm(trans_y~danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + tempo,data=new_tracks4_df)
summary(reg1)
anova(reg1)

## remedied regression model(make explanatory variables as normal as possible)

reg2 = lm(data = tracks4_df, popularity ~ danceability + energy + key + loudness + mode + speechiness_log + acousticness_log + instrumentalness_sqrt + liveness_log + valence + tempo)

### summary and anova table
summary(reg2)
anova(reg2)


## deleted remedied regression model(make explanatory variables as normal as possible)

reg3 = lm(data = tracks4_df, popularity ~ danceability + energy + loudness + instrumentalness_sqrt + valence + tempo)

### summary and anova table

summary(reg3)
anova(reg3)

### diagnostics
plot(reg3)
loglike3 = boxcox(popularity ~ danceability + energy + loudness + instrumentalness_sqrt + valence + tempo,lambda=seq(0, 2, length=10), data = new_tracks4_df)




















## SVM
data1 = tracks1_df[, c("danceability", "energy", "key", "loudness", "mode", "speechiness", 
                       "acousticness", "instrumentalness", "liveness", "valence" ,"tempo")]

data2 = tracks2_df[, c("danceability", "energy", "key", "loudness", "mode", "speechiness", 
                       "acousticness", "instrumentalness", "liveness", "valence" ,"tempo")]

data3 = tracks3_df[, c("danceability", "energy", "key", "loudness", "mode", "speechiness", 
                       "acousticness", "instrumentalness", "liveness", "valence" ,"tempo")]

data4 = tracks4_df[, c("danceability", "energy", "key", "loudness", "mode", "speechiness", 
                       "acousticness", "instrumentalness", "liveness", "valence" ,"tempo")]
## 使用径向基函数核的支持向量机
grid <- data.frame(sigma = seq(0.01,0.1,0.01),
                   C = seq(0.01,0.51,0.05))
library(caret)
ctr = trainControl(method = "repeatedcv", number = 5, repeats = 3)
pre = preProcess(data4, method = c("center", "scale"))
data1.1 = predict(pre, data1)
data2.1 = predict(pre, data2)
data3.1 = predict(pre, data3)
data4.1 = predict(pre, data4)
data1.1 = cbind(tracks1_df[, "popularity"], data1.1)
data2.1 = cbind(tracks2_df[, "popularity"], data2.1)
data3.1 = cbind(tracks3_df[, "popularity"], data3.1)
data4.1 = cbind(tracks4_df[, "popularity"], data4.1)
svm.fit1 = train(popularity ~ ., data1.1, method = "svmRadial", trControl = ctr, tuneGrid = grid)
svm.fit2 = train(popularity ~ ., data2.1, method = "svmRadial", trControl = ctr, tuneGrid = grid)
svm.fit3 = train(popularity ~ ., data3.1, method = "svmRadial", trControl = ctr, tuneGrid = grid)
svm.fit4 = train(popularity ~ ., data4.1, method = "svmRadial", trControl = ctr, tuneGrid = grid)




### boosting
library(gbm)
gbm1 = gbm(popularity ~ .,
           data = data2.1,
           n.trees = 1000,
           shrinkage = 0.05,
           cv.folds = 5,
           verbose = T,)
gbm.perf(gbm1, method = "cv")

library(MASS)
model.ridge = lm.ridge(popularity ~ ., data = data4.1, lambda = seq(0, 500, 1))
plot(model.ridge)


library(glmnet)
model.lasso = glmnet(as.matrix(data4.1[, -1]), as.matrix(data4.1[, 1]))
library(plotmo)
plot_glmnet(model.lasso)


library(xgboost)

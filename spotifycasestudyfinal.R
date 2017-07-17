df <- read.csv("df.csv")
df
range(df$Peak)

#plotting 
plot(df$Successful, df$Peak)
plot(df$Successful, df$danceability)
plot(df$Successful, df$energy)
plot(df$Successful, df$key)
plot(df$Successful, df$loudness)
plot(df$Successful, df$mode)
plot(df$Successful, df$speechiness)
plot(df$Successful, df$acousticness)
plot(df$Successful, df$instrumentalness)
plot(df$Successful, df$liveness)
plot(df$Successful, df$valence)
plot(df$Successful, df$tempo)
plot(df$Successful, df$duration_ms)
plot(df$Successful, df$time_signature)

#Removing noise
df1=df[!df$X == df[df$Successful == 0 & df$Peak < 10, ]$X, ]
df1

#Separating train dataset and testdata 
train_data=subset(df1,X<=350)
train_data
test_data=subset(df1,X>350)
test_data

#finding correlation between varibles
cor(train_data$Successful, train_data$Peak)
cor(train_data$Successful, train_data$danceability)
cor(train_data$Successful, train_data$energy)
cor(train_data$Successful, train_data$key)
cor(train_data$Successful, train_data$loudness)
cor(train_data$Successful, train_data$mode)
cor(train_data$Successful, train_data$speechiness)
cor(train_data$Successful, train_data$acousticness)
cor(train_data$Successful, train_data$instrumentalness)
cor(train_data$Successful, train_data$liveness)
cor(train_data$Successful, train_data$valence)
cor(train_data$Successful, train_data$tempo)
cor(train_data$Successful, train_data$duration_ms)
cor(train_data$Successful, train_data$time_signature)

#removing varibles
train_data$X=NULL
train_data$id=NULL
train_data$Title=NULL
train_data$Artist=NULL
train_data$Date.Entered=NULL
train_data$Peak=NULL
train_data$uri=NULL
train_data$track_href=NULL
train_data$analysis_url=NULL
train_data$type=NULL
train_data$loudness=NULL
train_data$energy=NULL
train_data$duration_ms=NULL
train_data$tempo=NULL
train_data
test_data$X=NULL
test_data$id=NULL
test_data$Title=NULL
test_data$Artist=NULL
test_data$Date.Entered=NULL
test_data$Peak=NULL
test_data$uri=NULL
test_data$track_href=NULL
test_data$analysis_url=NULL
test_data$type=NULL
test_data$loudness=NULL
test_data$energy=NULL
test_data$duration_ms=NULL
test_data$tempo=NULL
test_data

#Scaling the varibles with in the range of 0 to 1
train_data=data.frame(lapply(train_data, function(train_data) scale(train_data, center = FALSE, scale = max(train_data, na.rm = TRUE)/1)))
train_data
test_data=data.frame(lapply(test_data, function(test_data) scale(test_data, center = FALSE, scale = max(test_data, na.rm = TRUE)/1)))
test_data

#traning the model
model = glm(Successful ~.,family=binomial(link='logit'),data=train_data)
summary(model)

#Predicting the dependent varible
fitted.results = predict(model,newdata=test_data,type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)

#Calculating the accuracy 
aucc= mean(fitted.results != test_data$Successful)
print(paste('Accuracy',1-aucc))
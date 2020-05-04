library(readr)
library(party)
library(caret)
library(ggplot2)
library(pROC)

df <- read_csv(file.choose())
str(df)
head(df)
names(df)
df <- df[,-2]
sum(is.na(df))
df$alkphos[is.na(df$alkphos)] <- median(df$alkphos, na.rm = T)
mdf <- df
str(mdf$is_patient)
head(mdf)

#factor
mdf$is_patient <- factor(mdf$is_patient, levels = c(0,1))
str(mdf) #1st factor call 2 levels, 167 producing na values 
sum(is.na(mdf))
set.seed(1234)
mdf <- data.frame(mdf)
str(mdf) # 1st data.frame call, 2 levels
names(mdf)
#updsmpling
#mdf$is_patient <- factor(mdf$is_patient)
#str(mdf) # 2nd factor call 1 level, 167 na values
#sum(is.na(mdf))
mdf <- upSample(x=mdf, mdf$is_patient) 
mdf <- mdf[sample(nrow(mdf)),]
names(mdf) # "Class" column is added
str(mdf)
sum(is.na(mdf)) #832 na values
mdf <- subset(mdf[c(1:10)])
sum(is.na(mdf))#416 NA values
data_part <- createDataPartition(y= mdf$is_patient, p=0.7, list=FALSE)

#spliting the dataset
training <- mdf[data_part,]
testing <- mdf[-data_part,]
training[["is_patient"]] = factor(training[["is_patient"]])
sum(is.na(training))
trainCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(7777)

svmModel3 <- train(is_patient ~ ., data = training, method = "svmRadial",
                   trControl = trainCtrl, preProcess = c("center", "scale"),
                   tuneLength = 20)
svmModel3

predictor <- predict(svmModel3)

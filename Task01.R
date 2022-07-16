#Load the package
library(readr)
library(caTools)

#import the data
dataset <- read.csv(url("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"))

# view first few rows
head(dataset)

#Number of rows ans columns
dim(dataset)

#Visualize distribution of Scores
plot(dataset$Hours, dataset$Scores, main="Hours Vs. Score's percentage",
     xlab=" Hours Studied ", ylab=" Score's percentage ")


#Preparing the data
#split the data into testing set and training set

split <- sample.split(dataset, SplitRatio = 0.8)
train <- subset(dataset, split = 'TRUE')
test <- subset(dataset, split = 'FALSE')


#Build training model
model <- lm(Scores ~ Hours , data = dataset)
summary(model)

#plotting the regression line
abline(lm(Scores ~ Hours, data = dataset))

#Prediction
pred <- predict(model, test)
pred
res <- residuals(model)
res <- as.data.frame(res)
res


# compare the predicted vs actual values
results<-cbind(pred,test$Scores)

#test data
Hours <- 9.5
Hours <-as.data.frame(Hours)
p <- predict(model, Hours)
p

#Evaluating the model
rmse <- sqrt(mean(pred-dataset$Score)^2)
rmse

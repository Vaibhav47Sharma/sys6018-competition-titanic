# The titanic problem in Kaggle
# setwd('c:\\Users\\vaibhav\\Documents\\UVA\\Fall\\SYS 6081- Data Mining\\Homework\\Homework 0')
library('readr')
library('dplyr')
library('randomForest')
train_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")

#Dropping name, ticket and cabin in training and validation data set
train_data <- subset(train_data, select = c(1:3, 5:8, 10, 12))
test_data <- subset(test_data, select = c(1:2, 4:7, 9, 11))

sample <- sample(1 : nrow(train_data), size = (nrow(train_data)/4))
valid_data <- train_data[sample,]
train_data <- train_data[-sample,]

#Assigning the categorical variables in the training set
train_data$Embarked <- factor(train_data$Embarked)
train_data$Pclass <- factor(train_data$Pclass)

#Assigning the categorical variables in the validation set
valid_data$Embarked <- factor(valid_data$Embarked)
valid_data$Pclass <- factor(valid_data$Pclass)

#Assigning the categorical variables in the test set
test_data$Embarked <- factor(test_data$Embarked)
test_data$Pclass <- factor(test_data$Pclass)

# Testing linear models
lm_all <- glm(Survived~.-PassengerId, data = train_data, family = "binomial")
probs<-as.vector(predict(lm_all, newdata = valid_data, type="response"))
preds <- rep(0, nrow(valid_data))  # Initialize prediction vector
preds[probs > 0.5] <- 1 # p>0.5 -> 1
table(preds, valid_data$Survived)
"
preds   0   1
    0 111  38
    1  20  53
Accuracy- 73.87
"
summary(lm_all)

#Removing Fare
lm_fare <- glm(Survived~.-PassengerId-Fare, data = train_data, family = "binomial")
probs<-as.vector(predict(lm_fare, newdata = valid_data, type="response"))
preds <- rep(0, nrow(valid_data))  # Initialize prediction vector
preds[probs > 0.5] <- 1 # p>0.5 -> 1
table(preds, valid_data$Survived)
"
preds   0   1
    0 110  37
    1  21  54
Accuracy- 73.87
"
summary(lm_fare)

#Removing Parch
lm_fare_parch <- glm(Survived~.-PassengerId-Fare-Parch, data = train_data, family = "binomial")
probs<-as.vector(predict(lm_fare_parch, newdata = valid_data, type="response"))
preds <- rep(0, nrow(valid_data))  # Initialize prediction vector
preds[probs > 0.5] <- 1 # p>0.5 -> 1
table(preds, valid_data$Survived)
"
preds   0   1
    0 110  37
    1  21  54
Accuracy- 73.87
"
summary(lm_fare_parch)

#Removing Embarked
lm_emb_fare_parch <- glm(Survived~.-PassengerId-Embarked-Fare-Parch, data = train_data, family = "binomial")
probs<-as.vector(predict(lm_emb_fare_parch, newdata = valid_data, type="response"))
preds <- rep(0, nrow(valid_data))  # Initialize prediction vector
preds[probs > 0.5] <- 1 # p>0.5 -> 1
table(preds, valid_data$Survived)
"
preds   0   1
    0 111  36
    1  20  55
Accuracy- 74.77
"
summary(lm_emb_fare_parch)

y1 <- predict(lm_emb_fare_parch, newdata = test_data, type="response")
preds <- rep(0, nrow(test_data))
preds[y1 > 0.5] <- 1 # p > 0.5 -> 1
output <- data.frame('PassengerId' = test_data$PassengerId, 'Survived' = preds)
write.csv(output,'vs3br.csv', row.names = FALSE)

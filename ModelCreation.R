library(caret)

#X and user are removed

training <- read.csv("pml-training.csv")[,-c(1,2)]
testing <- read.csv("pml-testing.csv")[,-c(1,2)]

#The variables full of NA are removed

usl <- (apply(is.na(training),2,sum)/apply(training,2,length)) > 0.5

training <- training[, -which(usl)]
testing <- testing[, -which(usl)]

#The variables filled with empty strings are removed.

emp <- (apply(training == "",2,sum)/apply(training,2,length)) > 0.5

training <- training[, -which(emp)]
testing <- testing[, -which(emp)]

#raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window are not 
#considered important.

training <- training[, -c(1, 2, 3, 4)]
testing <- testing[, -c(1, 2, 3, 4)]

#Highly correlated variables are removed

cor1 <- cor(training[,-54])
cor1[upper.tri(cor1,diag=TRUE)] <- 0
numc <- apply(abs(cor1) > 0.8,2,sum) > 0

training <- training[, -which(numc)]
testing <- testing[, -which(numc)]

#A validation subset is created to evaluate the method

set.seed(546)
inTrain <- createDataPartition(y = training$classe, p = 0.6, list = F)
trainset <- training[inTrain,]
validtn <- training[-inTrain,]

#The model is trained and tested in validation

modeltree <- train(classe ~ ., trainset, method = "rf", ntree = 10)
pred <- predict(modeltree, validtn)
confusionMatrix(pred, as.factor(validtn$classe))
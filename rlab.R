library(caret)
set.seed("1234")
x <- read.csv('donner_party1.csv')
#x <- na.omit(x)
x[is.na(x$Age),]$Age <- mean(x$Age, na.rm = T)
x$Survived <- as.factor(x$Survived)
xsplit <- createDataPartition(x$Survived, p = 0.6, list = FALSE)
trainx <- x[xsplit,]
testx <- x[-xsplit,]


model <- train(Survived~Age+Sex, data = trainx, method="glm", family = "binomial")
pr <- predict(model, newdata = testx)

model
cmat <- confusionMatrix(pr,testx$Survived)
cmat


tc <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
modelcv <- train(Survived ~ Age + Sex, data = trainx, method = "glm", family = "binomial", trControl = tc)
modelcv
prcv <- predict(modelcv, newdata = testx)
cmatcv <- confusionMatrix(prcv,testx$Survived)
cmatcv
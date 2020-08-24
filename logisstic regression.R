library(caret)
library(broom)
library(ISLR)
data <- ISLR::Default
head(data)
model <- glm(formula = default ~ student + balance + income, family = "binomial", data = data)
tidy(model)

#CONFUSIONMATRIX
data <- data[sample(nrow(data)),]
response = data[,1]

err <- matrix(NA, nrow = 1, ncol = 10)
errcv = err

folds <- cut(seq(1,nrow(data)), breaks = 10, labels = FALSE)

for(i in 1:10){
  testIndexes <- which(folds==i, arr.ind = TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  model1 <- glm(default ~ ., family = "binomial", data = trainData)
  ans <- predict(model1, testData, type ="response")
  ans.pred = rep("NO", dim(testData)[1])
  ans.pred[ans_LR > .5] = "Yes"
  table(ans.pred, testData[,1])
  err_LR[1,i] = mean(ans.pred == testData[,1])}
  
err
errcv = rowMeans(err, na.rm = F, dims = 1)
confusionMatrix(table(result.pred, testData[,1]))

#From the output, the log add function will be

#log(P(y)/(1-P(y))) = -10.9-0.647student+0.00574(balance)

#For example, a student with a credit card balance of $1,500 and an income of $40,000 
#has an estimated probability of default of (หาความน่าจะเป็นที่จะผิดนัดชำระ)

#P(Y=1) = e^(-10.9-0.647(1)+0.00574(1500))/1+e^(-10.9-0.647(1)+0.00574(1500))
#       = 0.053 (< 0.5)
#so, in group Y=0 that mean not default

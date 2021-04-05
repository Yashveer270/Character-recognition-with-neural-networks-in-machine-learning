# machine learning with neural network
Character recognition with neural networks in machine learning.
//Create a csv file in excel of the matrix obtained from your charater.
// Then read the file in R studio and compile the below code.
// Download the neccessary packages in R studio and use the latest version
# Data

data <- read.csv("machinelearnig.csv", header = TRUE)
str(data)



# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]

# Neural Networks
library(neuralnet)
set.seed(123)
n <- neuralnet(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25,
               data = training,
               hidden = 5,
               err.fct = "ce",
               linear.output = FALSE,)
plot(n)

# Prediction
output <- compute(n, training[,-1])
head(output$net.result)
head(training[1,])



# Confusion Matrix & Misclassification Error - training data
output <- compute(n, training[,-1])
output
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
pred1
tab1 <- table(pred1, training$Y)
tab1
1-sum(diag(tab1))/sum(tab1)

# Confusion Matrix & Misclassification Error - testing data
output <- compute(n, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testing$Y)
tab2
1-sum(diag(tab2))/sum(tab2)

The Neural network model containing 5 hidden layers gives a misclassification error on the testing dataset and a misclassification Error  on testing dataset. We will now try to train more neural networks to check whether we can get lower test error in the test dataset.
// try for more hidden values by changing the hidden layers .

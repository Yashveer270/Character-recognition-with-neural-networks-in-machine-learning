# heydude07
Character recognition with neural networks in machine learning.
//Create a csv file in excel of the matrix obtained from your charater.
// Then read the file in R studio and compile the below code.
dataset=read.csv("C:/Users/DELL/Downloads/pj.csv")
str(dataset)
data.frame(dataset)

dataset

set.seed(222)
ind <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.7, 0.3))
training <- dataset[ind==1,]
testing <- dataset[ind==2,]
library(neuralnet)


n <- neuralnet(P~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+A21+A22+A23+A24+A25,
               data = training,
               hidden = 1,
               err.fct = "ce",
               linear.output=FALSE,)
plot(n)

output <- compute(n, training[,-1])
head(output$net.result)

head(training[1,])

output <- compute(n, training[,-1])
output

p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
pred1

tab1 <- table(pred1, training$P)
tab1

1-sum(diag(tab1))/sum(tab1)

output <- compute(n, testing[,-1])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(pred2, testing$P)
tab2

1-sum(diag(tab2))/sum(tab2)

The Neural network model containing 5 hidden layers gives a misclassification error on the testing dataset and a misclassification Error  on testing dataset. We will now try to train more neural networks to check whether we can get lower test error in the test dataset.
// try for more hidden values by changing the hidden layers .

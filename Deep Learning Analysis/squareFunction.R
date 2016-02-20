library("neurelnet")
set.seed(2016)
attribute <- as.data.frame(sample(seq(-2,2,length=50),50,replace=FALSE),ncol=1)
response <- attribute^2
data <- cbind(attribute, response)
colnames(data) <- c("attribute","response")
#head(data,15)
fit<-neuralnet(response~attribute,
               data=data,
               hidden=c(3,3),
               threshold = 0.01
)

testdata <- as.matrix(sample(seq(-2,2,length=10),10,replace=FALSE),ncol=1)
pred <- compute(fit,testdata)
result <- cbind(testdata,pred$net.result,testdata^2)
colnames(result) <- c("Attribute", "Prediction", "Actual")

round(result,4)

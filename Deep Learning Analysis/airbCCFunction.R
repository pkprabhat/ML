library("neuralnet")
set.seed(10000)

EAD = 200000
LGD = 0.5
R = 0.04

#i = 0:100
#PD = 0.000 to 0.200
M = 2

eq0 <- function(x) {x/500}
eq3 <- function(PD) {(LGD*pnorm(((1/(1-R))^0.5)*qnorm(PD)+((R/(1-R))^0.5)*qnorm(0.999))-LGD*PD)}
eq4 <- function(K) {K*12.5*EAD}

response = matrix(data=NA, nrow=100,ncol=1)
attribute <- as.data.frame(seq(0,0.2,length=100),ncol=1)
for (i in 1:100){
  response[i,1] = eq3(attribute[i,1])
  #RWA[i] = eq4(K[i])
}

data <- cbind(attribute, response)
colnames(data) <- c("attribute","response")

fit<-neuralnet(response~attribute,
               data = data,
               hidden = c(3,3),
               threshold = 0.01
)

resp = matrix(data=NA, nrow=40,ncol=1)
testdata <- as.matrix(seq(0.000,0.200,length=40),ncol=1)
for (i in 1:40){
  resp[i,1] = eq3(testdata[i,1])
  #RWA[i] = eq4(K[i])
}
pred <- compute(fit,testdata)
result <- cbind(testdata,pred$net.result,resp)
colnames(result) <- c("Attribute", "Predicted_K", "Actual_K")
round(result,4)
plot(testdata,pred$net.result, type="l", col="red", main="Exposure for Credit Card (Prediction)", cex.main = 2.0, cex.lab = 1.5, cex = 1.5, lwd = 3, 
     xlab = "Probability of Default (with fixed LGD = 50%)", ylab = "Capital Requirement (K)")
lines(testdata,resp, type="l", col="green", main="Exposure for Credit Card (Actual)", cex.main = 2.0, cex.lab = 1.5, cex = 1.5, lwd = 3, 
     xlab = "Probability of Default (with fixed LGD = 50%)", ylab = "Capital Requirement (K)")
legend('bottomright', c("ANN Prediction","Actual Calculculation"), lty=c(1,1), lwd=c(1,1),col=c("red","green"))
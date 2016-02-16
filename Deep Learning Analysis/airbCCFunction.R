library("neuralnet")
set.seed(10000)

testdata <- as.matrix(sample(seq(0.000,0.200,length=100),100,replace=FALSE),ncol=1)
pred <- compute(fit,testdata)

EAD = 200000
LGD = 0.5
R = 0.04

x = 0:100
#PD = 0.000 to 0.200
M = 2

eq0 <- function(x) {x/500}
eq3 <- function(PD) {(LGD*pnorm(((1/(1-R))^0.5)*qnorm(PD)+((R/(1-R))^0.5)*qnorm(0.999))-LGD*PD)}
eq4 <- function(K) {K*12.5*EAD}


PD <- data.frame(testdata)
colnames(PD)<- c("pd")

for (i in 1:100){
  K[i] = eq3(PD$pd[i])
  RWA[i] = eq4(K[i])
}
Ki = data.frame(k=K)

fit<-neuralnet(response~attribute,
               data=data,
               hidden=c(10,10,10),
               threshold = 0.01
)

testdata <- as.matrix(sample(seq(0.000,0.200,length=100),100,replace=FALSE),ncol=1)
pred <- compute(fit,testdata)
result <- cbind(testdata,pred$net.result,Ki)
colnames(result) <- c("Attribute", "Prediction", "Actual")
round(result,4)
plot(PD$pd,pred$net.result, type="l", col="red", main="Exposure for Credit Card (Prediction)", cex.main = 2.0, cex.lab = 1.5, cex = 1.5, lwd = 3, 
     xlab = "Probability of Default", ylab = "Capital Requirement (K)")
plot(PD$pd,Ki$k, type="l", col="red", main="Exposure for Credit Card (Actual)", cex.main = 2.0, cex.lab = 1.5, cex = 1.5, lwd = 3, 
     xlab = "Probability of Default", ylab = "Capital Requirement (K)")
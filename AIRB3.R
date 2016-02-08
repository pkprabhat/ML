EAD = 200000
LGD = 0.5
R = 0.15
B = {}
K = {}
RWA = {}

x = 0:10
#PD = 0.000 to 0.100
M = 2

eq0 <- function(x) {x/100}
eq3 <- function(PD) {(LGD*pnorm(((1/(1-R))^0.5)*qnorm(PD)+((R/(1-R))^0.5)*qnorm(0.999))-LGD*PD)}
eq4 <- function(K) {K*12.5*EAD}
  
  PD = data.frame(pd=eq0(x))
for (i in 1:11){
  K[i] = eq3(PD$pd[i])
  RWA[i] = eq4(K[i])
}
plot(PD$pd,RWA, type="l", col="red", main="Exposure for Retail Mortgages ($ 0.2 million)", cex.main = 2.0, cex.lab = 1.5, cex = 1.5, lwd = 3, xlab = "Probability of Default", ylab = "Risk Weighted Assets ($)")

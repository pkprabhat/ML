  EAD = 200000
  LGD = 0.5
  R = {}
  B = {}
  K = {}
  RWA = {}
  
  x = 0:10
  #PD = 0.001 to 0.020
  M = 2
  eq0 <- function(x) {x/100}
  eq1 <- function(PD) {0.12*((1-exp(-50*PD))/(1-exp(-50))) + 0.24*(1-((1-exp(-50*PD))/(1-exp(-50))))}
  eq2 <- function(PD) {(0.11852 - 0.05478*log(PD))^2}
  eq3 <- function(PD,b,R) {(LGD*pnorm(((1/(1-R))^0.5)*qnorm(PD)+((R/(1-R))^0.5)*qnorm(0.999))-LGD*PD)*(((1+(M-2.5)*b))/(1-1.5*b))}
  eq4 <- function(K) {K*12.5*EAD*1.06}
    
  PD = data.frame(pd=eq0(x))
  for (i in 1:11){
    R[i] = eq1(PD$pd[i])
    B[i] = eq2(PD$pd[i])
    K[i] = eq3(PD$pd[i],B[i],R[i])
    RWA[i] = eq4(K[i])
  }
  plot(PD$pd,RWA, type="l", col="red", main="Corporate Exposure ($ 0.2 million)", cex.main = 2.0, cex.lab = 1.5, cex = 1.5, lwd = 3, xlab = "Probability of Default", ylab = "Risk Weighted Assets ($)")

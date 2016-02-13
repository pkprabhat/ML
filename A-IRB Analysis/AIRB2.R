EAD = 200000
LGD = 0.1
S = 45
R = {}
B = {}
K = {}
RWA = {}

x = 0:20
#PD = 0.001 to 0.020
M = 2
eq0 <- b="" function="" x="">
  eq1 <- -="" 0.04="" 0.24="" b="" exp="" function="" max="">
  eq2 <- -="" 0.05478="" b="" function="" log="">
  eq3 <- -="" b="" function="" lgd="" pnorm="" qnorm="">
  eq4 <- b="" function="">
  
  PD = data.frame(pd=eq0(x))
for (i in 1:21){
  R[i] = eq1(PD$pd[i])
  B[i] = eq2(PD$pd[i])
  K[i] = eq3(PD$pd[i],B[i],R[i])
  RWA[i] = eq4(K[i])
}
plot(PD$pd,RWA, type="l", col="red", main="CE Adjusted for SME ($ 0.2 million)", cex.main = 2.0, cex.lab = 1.5, cex = 1.5, lwd = 3, xlab = "Probability of Default", ylab = "Risk Weighted Assets ($)")

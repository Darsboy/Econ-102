data = read.csv("GDP.csv")
data <- ts(data[,"VALUE"], frequency=4, start=c(1961,1))
data

#plot data
plot(data,col="blue", lwd = 1, lty=1,
     ylab="Expenditure (thousands of dollars)",
     main="Expenditure of data127")
# I saw an positive trend with seasonal fluctuations. The growth rate was high at first and plateaued near the end

logScale <- diff(log(data))
t_logScale <- time(logScale, offset=0.5)

fit_logScale <- lm(logScale~t_logScale)
coefT_logScale <- coef(fit_logScale)
T_logScale <- coefT_logScale[1] + coefT_logScale[2]*t_logScale

t_logScale2 <-t_logScale*t_logScale
fit_logScale2 <- lm(logScale~t_logScale +t_logScale2)
coefT_logScale2 <- coef(fit_logScale2)
T_logScale2 <- coefT_logScale2[1] + coefT_logScale2[2]*t_logScale + coefT_logScale2[3]*t_logScale2

coefT_logScale2

plot(logScale,col="blue", lwd = 2, lty=1,
     ylab="Precentage",
     main="Growth Rate of data127")
# The growth rate is constant fluctuating around (-0.05,0.05)
# with a mean of 0.003125722 overall
#lines(T_logScale,col=2, lty=2, lwd=4)
lines(T_logScale2,col=3, lty=3, lwd=4)
legend("topright", c("log(data127) scale", "Linear"), col=c("blue",2), lty=1:2,
       lwd=c(2,4), bty='n')







annualized_rate <- logScale  ## no percentage yet
annualized_rate <- (1+annualized_rate)^12-1 ## exact formula
annualized_rate <- annualized_rate*100 ## back to percentage
annualized_rate

plot(annualized_rate,col="blue", lwd = 1, lty=1,
     ylab="Precentage",
     main="Growth Rate of data127 Annualized")



t <- time(data, offset=0.5)
t2 <- t*t
fit <- lm(data~t)
coefT <- coef(fit)
T <- coefT[1] + coefT[2]*t

fit2 <- lm(data~t + t2)
coefT2 <- coef(fit2)

T2 <- coefT2[1] + coefT2[2]*t + coefT2[3]*t2
plot(data,col="black", lwd = 2, lty=1,
     ylab="Expenditure (thousands of dollars)",
     main="Expenditure of data127")
lines(T,col=2, lty=2, lwd=4)
lines(T2,col=3, lty=3, lwd=4)
legend("topleft", c("log(data127)", "Linear","Quadratic"), col=c("black",2,3), lty=1:3,
       lwd=c(2,4,4), bty='n')




#Detrended Linear
plot(data - T,col = 1, lwd = 1, lty=1,
     ylab="Thousands of dollars",
     main="Detrended of data127 (Linear)")


#Detrended Quadratic
plot(data - T2,col = 1, lwd = 1, lty=1,
     ylab="Thousands of dollars",
     main="Detrended of data127 (Quadratic)")
# 2 b)
ldata <- log(data)
lfit <- lm(ldata~t)
lcoefT <- coef(lfit)
lT <- lcoefT[1] + lcoefT[2]*t
lfit2 <- lm(ldata~t + t2)
lcoefT2 <- coef(lfit2)
lT2 <- lcoefT2[1] + lcoefT2[2]*t + lcoefT2[3]*t2


plot(ldata,col = 1, lwd = 1, lty=1,
     ylab="log(thousand of dollars)",
     main="Expenditure of data127 (log)")
lines(lT,col=2, lty=2, lwd=2)
lines(lT2,col=4, lty=3, lwd=2)
legend("topleft", c("log(data127)", "Linear","Quadratic"), col=c(1,2,4), lty=1:3,
       lwd=c(2,4,4), bty='n')



plot(ldata - lT,col = 1, lwd = 1, lty=1,
     ylab="log(thousand of dollars)",
     main="Detrended (Linear) of data127 (log)")

plot(ldata - lT2,col = 1, lwd = 1, lty=1,
     ylab="log(thousand of dollars)",
     main="Detrended (Quadratic) of data127 (log)")




# B3)
ldetrended <- ldata - lT2
plot(ldetrended,col = 1, lwd = 2, lty=1,
     ylab="log(data127)",
     main="log detrended of data127 (log-scale)")

# B4)
C <- filter(ldetrended, filter=rep(1/5,5))
lines(C, col = 2, lty=2, lwd=3)
abline(h = 0,lwd = 2,col = 3,lty=2)
legend("topright", c("Detrended data 127", "Cycle"), col=1:2, lty=1:2,
       lwd=2:3, bty='n') 

Dec <- decompose(ldetrended, filter=rep(1/5,5))

# B5)

CT <- lT2+C
plot(CT,col = 1, lwd = 2, lty=1,
     ylab="log(data127)",
     main="Low Freq Component of data127 (log-scale)")




# B6)
S <- Dec$figure[c(1:4)] ## 
barplot(S, main="Co2 Seasonal Component", xlab="season",
        names.arg=expression("S1","S2","S3","S4"))
S
plot(Dec$seasonal, main="Co2 Seasonal Component", ylab="Co2 (ppm)")
Dec$seasonal


# 2.3 macroeconomics Indexes
P1990 <- c(31,26,18)
P1991 <- c(33,29,17)
P1992 <- c(35,31,17)
P1993 <- c(37,34,16)
P <- c(P1990,P1991,P1992,P1993)
Q1990 <- c(140,100,129)
Q1991 <- c(152,103,166)
Q1992 <- c(165,107,213)
Q1993 <- c(180,111,273)

# unChained Paach Quanity
PQ9190 <- sum(P1991*Q1991)/sum(P1991*Q1990)
PQ9190
PQ9290 <- sum(P1992*Q1992)/sum(P1992*Q1990)
PQ9290
PQ9390 <- sum(P1993*Q1993)/sum(P1993*Q1990)
PQ9390

PQ9291 <- sum(P1992*Q1992)/sum(P1992*Q1991)
PQ9291
PQ9391 <- sum(P1993*Q1993)/sum(P1993*Q1991)
PQ9391


PQ9392 <- sum(P1993*Q1993)/sum(P1993*Q1992)
PQ9392
# Channed Paach QUantity
CPQ9190 <- PQ9190
CPQ9190*100
CPQ9290 <- CPQ9190*PQ9291
CPQ9290*100
CPQ9390 <- CPQ9290*PQ9392
CPQ9390*100

# unChained Laspeyres Quanity
LQ9190 <- sum(P1990*Q1991)/sum(P1990*Q1990)
LQ9190*100
LQ9290 <- sum(P1990*Q1992)/sum(P1990*Q1990)
LQ9290*100
LQ9390 <- sum(P1990*Q1993)/sum(P1990*Q1990)
LQ9390*100
LQ9291 <- sum(P1991*Q1992)/sum(P1991*Q1991)
LQ9391 <- sum(P1991*Q1993)/sum(P1991*Q1991)
LQ9392 <- sum(P1992*Q1993)/sum(P1992*Q1992)
# Channed Laspeyres QUantity
CLQ9190 <- LQ9190
CLQ9190*100
CLQ9290 <- CLQ9190*LQ9291
CLQ9290*100
CLQ9390 <- CLQ9290*LQ9392
CLQ9390*100


# unChained Paach Price
PP9190 <- sum(P1991*Q1991)/sum(P1990*Q1991)
PP9190*100
PP9290 <- sum(P1992*Q1992)/sum(Q1992*P1990)
PP9290*100
PP9390 <- sum(P1993*Q1993)/sum(Q1993*P1990)
PP9390*100
PP9291 <- sum(P1992*Q1992)/sum(Q1992*P1991)
PP9391 <- sum(P1993*Q1993)/sum(Q1993*P1991)
PP9392 <- sum(P1993*Q1993)/sum(Q1993*P1992)
# Channed Paach Price ind
CPP9190 <- PP9190
CPP9190*100
CPP9290 <- CPP9190*PP9291
CPP9290*100
CPP9390 <- CPP9290*PP9392
CPP9390*100

# unChained Laspeyres Price
LP9190 <- sum(Q1990*P1991)/sum(P1990*Q1990)
LP9190*100
LP9290 <- sum(Q1990*P1992)/sum(P1990*Q1990)
LP9290*100
LP9390 <- sum(Q1990*P1993)/sum(P1990*Q1990)
LP9390*100
LP9291 <- sum(Q1991*P1992)/sum(P1991*Q1991)
LP9391 <- sum(Q1991*P1993)/sum(P1991*Q1991)
LP9392 <- sum(Q1992*P1993)/sum(P1992*Q1992)
# Channed Laspeyres Price
CLP9190 <- LP9190
CLP9190*100
CLP9290 <- CLP9190*LP9291
CLP9290*100
CLP9390 <- CLP9290*LP9392
CLP9390*100

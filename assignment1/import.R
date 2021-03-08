data = read.csv("dat127.csv")
data
data <- ts(data[,"Expenditure"], frequency=4, start=c(1950,1))
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
annualized_rate <- (1+annualized_rate)^4-1 ## exact formula
annualized_rate <- annualized_rate*100 ## back to percentage
annualized_rate

plot(annualized_rate,col="blue", lwd = 1, lty=1,
     ylab="Precentage",
     main="Growth Rate of data127 Annualized")
abline(h=0,col=1,lwd = 2, lty=4)
legend("topright", c("log(data127) scale Annulized", "X-axis"), col=c("blue",1), lty=c(1,4),
       lwd=c(1,2), bty='n')

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
barplot(S,main="Co2 Seasonal Component", xlab="season",
        ylim = c(-0.04,0.04),
        names.arg=expression("S1","S2","S3","S4"))
S
plot(Dec$seasonal, main="Co2 Seasonal Component", ylab="Co2 (ppm)")
Dec$seasonal



# Part C
data128 = read.csv("dat128.csv")
data128 <- ts(data128[,"Expenditure"], frequency=4, start=c(1950,1))
data128 <- log(data128)
#plot data
plot(ldata,data128,col="blue", lwd = 1, lty=1,
     xlim = c(5.85,6.75),
     xlab="log(data127) in log(thousands of dollars)",
     ylab="log(data128) in log(thousands of dollars)",
     main="log of data127 vs log of data128")

lfit2_data128 <- lm(data128~t + t2)
lcoefT2_data128 <- coef(lfit2_data128)
lT2_data128 <- lcoefT2_data128[1] + lcoefT2_data128[2]*t + lcoefT2_data128[3]*t2

plot(data128,col="blue", lwd = 1, lty=1,
     ylab="Expenditure (thousands of dollars)",
     main="data128 ")
lines(lT2_data128,col=3, lty=3, lwd=4)


ldetrended_data128 <- data128 - lT2_data128
plot(ldetrended_data128,col="blue", lwd = 1, lty=1,
     ylab="Expenditure (thousands of dollars)",
     main="detrended log of data 128")

C128 <- filter(ldetrended_data128, filter=rep(1/5,5))
lines(C128,col=2, lty=2, lwd=4)
legend("topright", c("log(data128) detrended", "Cycle component"), col=c("blue",2), lty=1:2,
       lwd=c(2,4), bty='n')



plot(C,C128,col="blue", lwd = 1, lty=1,
     xlab="log(data127) in (log(thousands of dollars))",
     ylab="log(data128) in (log(thousands of dollars))",
     main="Cycle data 127 and data 128 scatterplot")



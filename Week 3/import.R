#2.1 code along
load("data/Climate_module2.rda")
load("data/ClimateEX_module2.rda")
time(co2M)[1:8]
time(co2M, offset=0.5)[1:8]
t <- time(co2M, offset=0.5)
fit <- lm(co2M~t)

coefT <- coef(fit)
coefT
t <- time(co2M, offset=0.5)
coefT <- coef(lm(co2M~t))
trend <- coefT[1] + coefT[2]*t

plot(co2M, lwd=1, main="Co2 Emissions With a Linear Trend",
     ylab="Co2 (ppm)")
lines(trend, col=2, lty=2, lwd=2)
legend("topleft", c("Co2", "Trend"), col=1:2, lty=1:2,
       lwd=2, bty='n')

t2 <- t^2
fit2 <- lm(co2M~t+t2)
coefT2 <- coef(fit2)
trend2 <- coefT2[1] + coefT2[2]*t + coefT2[3]*t2

plot(co2M, lwd=2, main="Co2 Emissions With a Quadratic Trend",
     ylab="Co2 (ppm)")
lines(trend2, col=2, lty=2, lwd=2)
legend("topleft", c("Co2", "Trend"), col=1:2, lty=1:2,
       lwd=2, bty='n')


lco2M <- log(co2M)
lcoefT <- coef(lm(lco2M~t)) ## linear coefficients
lcoefT2 <- coef(lm(lco2M~t+t2)) ## quadratic coefficients
ltrend <- lcoefT[1] + lcoefT[2]*t ## linear trend
ltrend2 <- lcoefT2[1] + lcoefT2[2]*t + lcoefT2[3]*t2 ## quadratic trend
plot(lco2M, lwd=2, main="Co2 Emissions With a Linear and Quadratic Trends (Log-Scale)",
     ylab="log(Co2)")
lines(ltrend, col=2, lty=2, lwd=4)
lines(ltrend2, col=4, lty=3, lwd=4)
legend("topleft", c("log(Co2)", "Linear","Quadratic"), col=c(1,2,4), lty=1:3,
       lwd=c(2,4,4), bty='n')


plot(co2M-trend, lwd=2,
     main="Detrended Co2 Emissions Series Using a Linear Trend",
     ylab="Co2 (ppm)")

plot(co2M-trend2, lwd=2,
     main="Detrended Co2 Emissions Series Using a Quadratic Trend",
     ylab="Co2 (ppm)") 


CSI <- co2M-trend2

CSI
x <- c(1,2,3,4,5,6)
filter(x, filter=c(1/3,1/3,1/3))


rep(1/5,5) # make a list of 1/5 with length 5 


C <- filter(CSI, filter=rep(1/13,13))
C


plot(CSI, lwd=2,
     main="Detrended Co2 Emissions and Its Cyclical Component",
     ylab="Co2 (ppm)")
lines(C, col=2, lwd=3, lty=2)
legend("topleft", c("Detrended Co2", "Cycle"), col=1:2, lty=1:2,
       lwd=2:3, bty='n') 


CT <- trend2+C
plot(CT, lwd = 2,
     main = "The Low Freq Component of CO2",
     ylab = "Co2(ppm)")

Dec <- decompose(CSI, filter = rep(1/13,13))
names(Dec)
C <- Dec$trend
C
plot(CSI,main="Detrended Co2 emmisions and it's cyclic component", ylab = "Co2(ppm)")
lines(C,col=2,lwd = 3, lty = 2)
legend("topleft", c("Detrended Co2","Cycle"), col = 1:2, lty = 1:2, lwd = 2:3,bty = 'n')

S <- Dec$figure[c(11,12,1:10)]
barplot(S, main="Co2 Seasonal Component", xlab="month",
        names.arg=month.abb)
plot(Dec$seasonal,main="Detrended Co2 emmisions", ylab = "Co2(ppm)")


t <- time(co2M, offset=0.5)
t2 <- t^2
coefT <- coef(lm(co2M~t+t2))
trend <- coefT[1] + coefT[2]*t + coefT[3]*t2
CSI <- co2M - trend

S <- decompose(CSI, filter=rep(1/13,13))$seasonal
Des_co2M <- co2M-S
plot(co2M, main="Co2 Emissions", ylab="Co2 (ppm)", lwd=2)
lines(Des_co2M, col=2, lty=2, lwd=2)
legend("topleft", c("Non-adjusted", "adjusted"), col=1:2, lty=1:2,
       lwd=2, bty='n')


tempM2 <- window(tempM, start(co2M), end(co2M))
t_temp <- time(tempM2, offset = 0.5)
t_temp2 <- t_temp^2
#Linear
coefT_temp <- coef(lm(tempM2~t_temp))
trend_temp <- coefT_temp[1] + coefT_temp[2]*t_temp
#Quadratic
coefT_temp2  <- coef(lm(tempM2~t_temp+t_temp2))
trend_temp2  <- coefT_temp2[1] + coefT_temp2[2]*t_temp+coefT_temp2[3]*t_temp2

plot(tempM2,main = "Temp Anom", ylab = "temperature (0.1 degres celc)")
lines(trend_temp, col=2, lty=2, lwd=3)
lines(trend_temp2, col=4, lty=3, lwd=3)
legend("topleft", c("Anomalies", "Linear", "Quadratic"),
       col=c(1,2,4), lty=1:3, lwd=3, bty='n')
CSI_temp  <- tempM2-trend_temp2
Dec_temp  <- decompose(CSI_temp, filter=rep(1/13,13))
Des_temp  <- tempM2 - Dec_temp$seasonal

C_co2 <- Dec$trend
C_temp  <- Dec_temp$trend
plot(C_co2, C_temp, pch=21, col=1, bg=1,
     main="Comovement Between the Cyclical Components of
 Temperature and Co2 Emissions",
     xlab="Co2 (ppm)", ylab="Temperature (0.1 degree Celsius)")
#Excersise

# excersise problems
t <- time(co2Q)
t2 <- t^2
coeft <- coef(lm(co2Q~ t)) 
trendLine <- coeft[1] + coeft[2]*t


coeft2 <- coef(lm(co2Q~t+t2))
trendLine2 <- coeft2[1] + coeft2[2]*t + coeft2[3]*t2
plot(co2Q)
lines(trendLine, col=2, lty=2, lwd=3)
lines(trendLine2, col=4, lty=3, lwd=3)

CSI <- co2Q-trendLine2
dec <- decompose(CSI, filter=rep(1/5,5))# to decompose and get the trend
dec


C <- dec$trend
plot(CSI,
     main="Detrended Quarterly Co2 Emissions and Its Cycle",
     ylab="Co2 (ppm)",
     lwd=2)
lines(C, col=2, lty=2, lwd=2)
legend("topleft", c("Detrended Co2","Cycle"), col=1:2,
       lty=1:2, lwd=2, bty='n')

plot(CSI,
     main="Detrended Quarterly Co2 Emissions and Its Cycle",
     ylab="Co2 (ppm)",
     lwd=2)
plot(trend+C,
     main="Low Frequency Component of the Quarterly Co2 Emissions Series",
     ylab="Co2 (ppm)",
     lwd=2)
#1 e)
barplot(dec$figure,names.arg=c("Qtr1","Qtr2", "Qtr3", "Qtr4"), main="Seasonal Component of Quarterly Co2 Emissions", ylab="Co2 (ppm)")
#1 f)
adj_co2Q <- co2Q-dec$seasonal
plot(co2Q,
     main="Quarterly Co2 Emissions",
     ylab="Co2 (ppm)",
     lwd=2)
lines(adj_co2Q, col=2, lty=2, lwd=2)
legend("topleft", c("Not adjusted","Adjusted"), col=1:2,
       lty=1:2, lwd=2, bty='n')

#centering over all averages
#dec$seasonal



#2
S<- tempY[,"JJA"]
W<- tempY[,"DJF"]
#2 a)
plot(S,W,pch = 21, bg = 2, xy.lables=FALSE, xy.lines=FALSE)
plot(S,W, pch=21, bg=2, xy.labels=FALSE, xy.lines=FALSE,
     main="Winter Anomalies Versus Summer Anomalies",
     ylab="Temperature (0.1 degree Celsius)",
     xlab="Temperature (0.1 degree Celsius)")

#2 b)
t  <- time(S, offset=0.5)
coefS  <- coef(lm(S~t))
trendS  <- coefS[1] + coefS[2]*t

coefW  <- coef(lm(W~t))
trendW  <- coefW[1] + coefW[2]*t

dS  <- S-trendS
dW  <- W-trendW

plot(dS,dW, pch=21, bg=2, xy.labels=FALSE,  xy.lines=FALSE,
     main="Detrended Winter Anomalies Versus Detrended Summer Anomalies",
     ylab="Temperature (0.1 degree Celsius)",
     xlab="Temperature (0.1 degree Celsius)")
#2 c)
plot(dS,
     main="Clyclical Components of the Summer and Winter Anomalies",
     ylab="Temperature (0.1 degree Celsius)", lwd=2)
lines(dW, col=2, lty=2, lwd=2)
legend("topleft", c("Summer","Winter"), col=1:2, lty=1:2,
       lwd=2, bty='n')
# 3 a)
t  <- time(precM, offset=0.5)
coefT <- coef(lm(precM~t))
trend <- coefT[1] + coefT[2]*t
CSI <- precM - trend

dec <- decompose(CSI, filter=rep(1/13,13)) 
C <- dec$trend 
lowF <- trend+C 

plot(lowF,
     main="Low Frequency Component of Monthly Precipitation",
     ylab="Precipitation (mm)", lwd=2)

# 3b)
barplot(dec$figure, names.arg=month.abb,
        main="Seasonal Component of Monthly Precipitation",
        ylab="Precipitation (mm)", lwd=2)
# 3 c)
adjprecM  <- precM-dec$seasonal
plot(adjprecM,
     main="Monthly Precipitation (seasonally adjusted)",
     ylab="Precipitation (mm)", lwd=2)
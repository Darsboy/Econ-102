data = read.csv("data.csv")
data
data <- ts(data[,"Data"], start=c(1960,1), frequency=4)
t <- time(data,offset = 0.5)
t2 <- t*t
coefT <- coef(lm(data~t+t2))
coefT



trend <- coefT[1] + coefT[2]*t + coefT[3]*t2
plot(data,main = "data")
lines(trend, col=2, lty=2, lwd=2)
CSI <- data - trend
plot(CSI,main = "detrended")
C <- filter(CSI, filter=rep(1/5,5))
Dec <- decompose(CSI, filter=rep(1/5,5))
# 1 
# a)
round(window(trend,c(2017,1),c(2017,1)),2)
# b)
round(window(CSI,c(1966,1),c(1966,1)),2)
names(Dec)

# 2
# a)
round(window(C,c(1992,3),c(1992, 3)),2)
lowFreqCompnenet <- trend+C
plot(lowFreqCompnenet,main = "lowFreqCompnenet")
plot(C,main = "C")
lines(Dec$trend, col=2, lty=2, lwd=2)


# b)
round(window(lowFreqCompnenet, c(2017,2),c(2017,2)),2)



#3
#a
barplot(Dec$figure,names.arg=c("Qtr1","Qtr2", "Qtr3", "Qtr4"))
round(Dec$figure,2)

#b
SeasonallyAdjusted  <- data-Dec$seasonal
round(window(SeasonallyAdjusted,c(2019,4),c(2019,4)),2)

marks <- c(70,89,65,76)
gpa <- mean(marks)
gpa
tempdata <- read.csv("NH.Ts+dSST.csv",skip=1,na.strings = "***",
                     header=TRUE)
head(tempdata, n=4)
co2data <- read.table("co2_mm_mlo.txt", skip=72, na.strings=-99.99, header=FALSE)
names(co2data) <- c("Year","Month","Date","Co2.miss", "Co2", "Trend", "ind")
head(co2data)
save(tempdata, co2data, file="climate.rda")


#Week 2 lesson 1
co2M <- ts(co2data, frequency=12, start=c(1958,3))
co2M

tempY <- ts(tempdata, start=1880)
tempY

window(tempY[,"Jan"], start=1990, end=1994)
cpi <- read.csv("Macro/USACPIALLMINMEI.csv")
tempM <- c(t(tempdata[,2:13]))
tempM

co2M
# b)
co2M_growth_rate <- diff(co2M)/lag(co2M,-1)*100


gRE <- (1+co2M_growth_rate/100)^12-1
gRE
gRA <- 12*co2M_growth_rate

#c)
mean(tempY, na.rm=TRUE)




#d)

diff(log(co2M))*100






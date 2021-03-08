temp_prec <- read.table("precipitation.txt", header=TRUE)
temp_prec
temp_climate <- read.csv("ontarioAll.csv", skip=18, header=TRUE,
                         na.strings=c("","M","E"))
temp_climate

cpi <- read.csv("USACPIALLMINMEI.csv")
names(cpi)[2] <- "CPI"

emp <- read.csv("LNU02300000.csv")
names(emp)[2] <- "Emp"

gdp <- read.csv("NA000334Q.csv")
names(gdp)[2] <- "GDP"


gdpW <- read.csv("API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_656861.csv", skip=4,
                 header=TRUE)

int <- read.csv("DP_LIVE_15012021222811533.csv")

co2data <- read.table("co2_mm_mlo.txt", skip=72, na.strings=-99.99, header=FALSE)

UnempCan <- read.csv("1410028701-eng (1).csv")
UnempCan <- ts(UnempCan[,-1], start = c(1976, 1), frequency = 12)
plot(UnempCan[,2])
lines(UnempCan[,1], col = 3, lty = 3)
abline(h = 12)
abline(v = 2012)

plot(UnempCan[,1:3], col = 1:3, lty = 1:3, plot.type = "single")
legend("topright", colnames(UnempCan)[1:3], lty = 1:3, col = 1:3, bty = 'n', cex = .5)
points(1990, 20, pch = 21, bg = 2)

barplot(t(UnempCan[1,]))
barplot(window(UnempCan, c(1980, 1), c(1980, 5)), beside=TRUE, las = 2, cex.names = .3)


hist(rnorm(500))


precY <- ts(temp_prec, start=1958)


precM <- c(t(precY[, c(5:13,2:4)]))
precM <- ts(precM, start=c(1921,1), frequency = 12)
precM

climateM= temp_climate[,c(4,6,8)]
names(climateM) = c("max","min","mean")
climateM <- ts(climateM, frequency=12, start=c(1866,1))
climateM

save(precY, precM, climateM, temp_prec, temp_climate,
     file="climateData_Ex.rda")



#compute precipitation series in months in millimeters to centimeters
precMCm=precM/10
precMCm
window(precMCm, c(1930,1),  c(1960,12))
sum(window(precMCm, c(1930,1),  c(1960,12)))

#create a new variable that represents the monthly growth rate of CO2 emmission in precentage on use
#exact formula and the other use precentage

# e
base <- window(precM, c(1930, 1), c(1930, 1))
precM_index <- precM / c(base) * 100
window(precM_index , c(2018, 1), c(2018, 12)) - 100
# f
precA <- aggregate(precM, FUN=sum)
base <- window(precA, 1930, 1930)
precA_index <- precA / c(base) * 100
window(precA_index , 2010, 2018) - 100

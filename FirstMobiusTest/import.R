data <- read.csv("data.csv")
data <- ts(data[,"Value"], start=c(1950,1), frequency=12)
data
View(data) 
round(mean(data),1)
growthRatePrecent <- diff(data)/lag(data , -1)*100
round(mean(growthRatePrecent),2)

growthRatePrecentAnual <- ((1+growthRatePrecent/100)^12-1)*100
round(mean(growthRatePrecentAnual),2)

base <- c(window(data, start = c(2002, 1), end = c(2002, 1)))
samplewindow <- window(data, start = c(1992, 10), end = c(1996, 1))
samplewindow <- samplewindow/base * 100
round(mean(samplewindow),2)


logGrowth <- diff(log(data)) 
#growthRatePrecent_e <- diff(samplewindow_e)/lag(samplewindow_e , -1)
#growthRatePrecent_e
logGrowth <- ((1+ logGrowth)^12 -1) * 100 
samplewindow_e <- window(logGrowth, start = c(1976, 11), end = c(1980, 10))
round(mean(samplewindow_e),2)

###############################
#Question 1


Q1_data <- read.csv("Question1Table.csv")
Q1_data <- ts(Q1_data[,"Value"],frequency=12)
Q1_data


growthRatePrecent_Q1 <- diff(Q1_data)/lag(Q1_data , -1)*100
growthRatePrecent_Q1

growthRatePrecentAnual_Q1 <- ((1+growthRatePrecent_Q1/100)^12-1)*100
growthRatePrecentAnual_Q1

base_Q1 <- c(window(Q1_data, start = c(2002, 1), end = c(2002, 1)))
samplewindow_Q1 <- window(Q1_data, start = c(1992, 10), end = c(1996, 1))
samplewindow_Q1 <- samplewindow_Q1/base_Q1 * 100
samplewindow_Q1

logGrowth_Q1 <- ((1+ diff(log(Q1_data)))^12 -1) * 100 
logGrowth_Q1



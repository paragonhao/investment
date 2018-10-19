library(readxl)
library(xts)


###common functions #####

cashPos <- function(value2yrStart, value10yrStart, capitalRequired){
  return(value2yrStart - value10yrStart + capitalRequired)
}



#####################

# Import data and extract data into to selected range from 1983-12-30 to 2018-06-30
# TODO: Figure out a better way to import data without needing to change
filepath <- "~/Documents/ucla/Quarter1/Investments/HW/feds200628.csv"
fed_data_raw <- read.csv(filepath, header = TRUE, sep=",")
fed_data_raw <- fed_data_raw[c("X","SVENY02", "SVENY10","BETA0","BETA1","BETA2","BETA3","TAU1","TAU2")]
row.names(fed_data_raw) <- fed_data_raw$X
fed_dat <- as.xts(fed_data_raw)
filter_date <- fed_dat["1983-12-30/2018-06-30"]
finalized_data <- cbind(filter_date$SVENY02,filter_date$SVENY10,filter_date$BETA0,filter_date$BETA1,filter_date$BETA2,filter_date$BETA3,filter_date$TAU1,filter_date$TAU2)
storage.mode(finalized_data) <- "double"

# Collapse into weekly data 
weekly <- endpoints(finalized_data, on="weeks",k=1)
weekly_data <- period.apply(finalized_data, INDEX = weekly, FUN = first)

#Calculate 2 year Bond price
xts_list = split(weekly_data, "weeks")
bond_price_2yr <- lapply(xts_list, function(x) {
  return(100 * exp(- x$SVENY02/100 * 2))
})
weekly_data$bond_price_2yr <- do.call(rbind, bond_price_2yr)  

#Calculate 10 year Bond price
xts_list = split(weekly_data, "weeks")
bond_price_10yr <- lapply(xts_list, function(x) {
  return(100 * exp(- x$SVENY10/100 * 10))
})
weekly_data$bond_price_10yr <- do.call(rbind, bond_price_10yr)  

#Calculate DV01_2yr
xts_list = split(weekly_data, "weeks")
DV01_2yr <- lapply(xts_list, function(x) {
  x$bond_price_2yr * (-x$SVENY02/100)
})
weekly_data$DV01_2yr <- do.call(rbind, DV01_2yr)  

#Calculate DV01_10yr
xts_list = split(weekly_data, "weeks")
DV01_10yr <- lapply(xts_list, function(x) {
  x$bond_price_10yr * (-x$SVENY10/100)
})
weekly_data$DV01_10yr <- do.call(rbind, DV01_10yr) 

xts_list = split(weekly_data, "weeks")
B2yrSellY <- lapply(xts_list, function(x) {
  t <- 1 + 51/52
  int_step_1  <- (1 - exp(-t/x$TAU1))/(t/x$TAU1)
  int_step_2  <- (1 - exp(-t/x$TAU2))/(t/x$TAU2)
  x$BETA0 + x$BETA1 * (int_step_1) + x$BETA2 * (int_step_1  - (exp(-t/x$TAU1))) + x$BETA3 * (int_step_2 - (exp(-t/x$TAU2)) )
})
weekly_data$B2yrSellY <- do.call(rbind, B2yrSellY) 

xts_list = split(weekly_data, "weeks")
B10yrSellY <- lapply(xts_list, function(x) {
  t <- 9 + 51/52
  int_step_1  <- (1 - exp(-t/x$TAU1))/(t/x$TAU1)
  int_step_2  <- (1 - exp(-t/x$TAU2))/(t/x$TAU2)
  x$BETA0 + x$BETA1 * (int_step_1) + x$BETA2 * (int_step_1  - (exp(-t/x$TAU1))) + x$BETA3 * (int_step_2 - (exp(-t/x$TAU2)) )
})
weekly_data$B10yrSellY <- do.call(rbind, B10yrSellY) 

# starting capital is 1 mil
starting_capital <- 1000000

#  Assume par value $100
# calculate the ratio short over long
weekly_data$TargetLOverShort <- weekly_data$DV01_2yr/weekly_data$DV01_10yr

# calculate the sell price for 2 yr and 10 yr,
weekly_data$B2yrSellPrice <- 100 * exp(- weekly_data$B2yrSellY/100 * (1 + 51/52))
weekly_data$B10yrSellPrice <- 100 * exp(- weekly_data$B10yrSellY/100 * (9 + 51/52))

leverage_10 <- 0.1

#################################################
# calculate the starting units for 2yr 10 yr 
unit2yrStartShort <- (starting_capital / leverage_10) / (weekly_data$bond_price_2yr[1] + weekly_data$TargetLOverShort[1] * weekly_data$bond_price_10yr[1] )
unit10yrStartLong <- unit2yrStartShort * weekly_data$TargetLOverShort[1]

# total number of the size
n <- dim(weekly_data)[1]

# the value of 2 yr and 10 yr at starting point
value2yrStart <- unit2yrStartShort * weekly_data$bond_price_2yr[1]
value10yrStart <- unit10yrStartLong * weekly_data$bond_price_10yr[1]

cashPosStart <- cashPos(value2yrStart,value10yrStart, starting_capital)

q2 <- c(as.numeric(unit2yrStartShort),rep(0,n-1))
q10 <- c(as.numeric(unit10yrStartLong$bond_price_2yr),rep(0,n-1))
value2yrShort <-c(as.numeric(value2yrStart$bond_price_2yr), rep(0, n-1))
value10yrLong <- c(as.numeric(value10yrStart$bond_price_2yr), rep(0, n-1))
t_7d <- 7/365
CashPos_7d<- c(as.numeric(cashPosStart),rep(0,n-1))
# total capital for each week, in millions
totalCap <- c(starting_capital, rep(0,n-1))

for( i in  1:(n-1)){
  totalCap[i+1] <- q10[i] * weekly_data$B10yrSellPrice[i+1] - q2[i]*weekly_data$B2yrSellPrice[i+1] + CashPos_7d[i]
  
  q2[i+1] <- (totalCap[i+1] / leverage_10) / (weekly_data$bond_price_2yr[i+1] + weekly_data$TargetLOverShort[i+1] * weekly_data$bond_price_10yr[i+1])
  q10[i+1] <- q2[i+1] * weekly_data$TargetLOverShort[i+1]
  
  value2yrShort[i+1] <- q2[i+1] * weekly_data$bond_price_2yr[i+1] 
  value10yrLong[i+1] <- weekly_data$bond_price_10yr[i+1] * q10[i+1]
  
  # update the size of the cash position
  CashPos_7d[i+1] <- cashPos(value2yrShort[i+1], value10yrLong[i+1], totalCap[i+1])
}


result <- as.xts(totalCap, order.by=index(weekly_data))

head(weekly_data)
head(q2)
head(q10)
head(value2yrShort)
head(value10yrLong)
head(totalCap)
head(CashPos_7d)


library(readxl)
library(xts)

###global variables #####
Duration_2yr <- 2
Duration_10yr <-10
t2yrMinus7 <- 1 + 51/52
t10yrMinus7 <- 9 + 51/52

###common functions #####
cashPos <- function(value2yrStart, value10yrStart, capitalRequired){
  return(value2yrStart - value10yrStart + capitalRequired)
}

NSS_model <- function(key, t){
  t1 <- t/weekly_data$TAU1
  t2 <- t/weekly_data$TAU2
  int_step_1  <- (1 - exp(-t1))/(t1)
  int_step_2  <- (1 - exp(-t2))/(t2)
  y <- weekly_data$BETA0[key] + weekly_data$BETA1[key] * (int_step_1) + weekly_data$BETA2[key] * (int_step_1  - (exp(-t1))) + weekly_data$BETA3[key] * (int_step_2 - (exp(-t2)))
  return(as.double(y))
}

compute_short_long_quantity <- function (total_capital, leverage, weekly_data, key){
  lhs <- matrix(c(weekly_data$DV01_2yr[key], -weekly_data$DV01_10yr[key],weekly_data$bond_price_2yr[key],weekly_data$bond_price_10yr[key]), nrow =2, ncol = 2,byrow = TRUE)
  rhs <- matrix(c(0, total_capital/leverage))
  ans <- solve(lhs) %*% rhs
}

#########################################################################################################
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
##########################################################################################################

##########################################Add 2yr, 10 yr bond price, and DV01s##################################################
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
  x$bond_price_2yr * Duration_2yr / 10000
})
weekly_data$DV01_2yr <- do.call(rbind, DV01_2yr)  

#Calculate DV01_10yr
xts_list = split(weekly_data, "weeks")
DV01_10yr <- lapply(xts_list, function(x) {
  x$bond_price_10yr * Duration_10yr / 10000
})
weekly_data$DV01_10yr <- do.call(rbind, DV01_10yr) 
##################################################################################################################################

############################################ Back Testing ##########################################
# starting capital is 1 mil
starting_capital <- 1000000

#  Assume par value $100
# calculate the ratio short over long
weekly_data$TargetLOverShort <- weekly_data$DV01_2yr/weekly_data$DV01_10yr
leverage <- 0.1

# calculate the starting units for 2yr 10 yr 
positions <- compute_short_long_quantity(starting_capital, leverage_10, weekly_data, key=1)

unit2yrStartShort <- positions[1]
unit10yrStartLong <- positions[2]

# total number of the size
n <- dim(weekly_data)[1]

# the value of 2 yr and 10 yr at starting point
value2yrStart <- unit2yrStartShort * weekly_data$bond_price_2yr[1]
value10yrStart <- unit10yrStartLong * weekly_data$bond_price_10yr[1]

# yield rate to sell bond
b2yrSell <- c(as.double(weekly_data$bond_price_2yr[1]),rep(0,n-1))
b10yrSell <- c(as.double(weekly_data$bond_price_10yr[1]),rep(0,n-1))

# price to sell bond
lt2yrPrc <- c(as.double(weekly_data$bond_price_2yr[1]),rep(0,n-1))
lt10yrPrc <- c(as.double(weekly_data$bond_price_10yr[1]),rep(0,n-1))

cashPosStart <- cashPos(value2yrStart,value10yrStart, starting_capital)

q2yr <- c(as.numeric(unit2yrStartShort),rep(0,n-1))
q10yr <- c(as.numeric(unit10yrStartLong),rep(0,n-1))
value2yrShort <-c(as.double(value2yrStart), rep(0, n-1))
value10yrLong <- c(as.double(value10yrStart), rep(0, n-1))

CashPos_7d<- c(as.double(cashPosStart),rep(0,n-1))
# total capital for each week, in millions
totalCap <- c(starting_capital, rep(0,n-1))

# consider interest from holding capitals
interest_pnl <- rep(0, n)
cummulative_i <- rep(0, n)

for( i in  1:(n-1)){
  # calculate the yield and price for 2 year minus 1 week
  lt2yrYield <- NSS_model(key = i+1, t = t2yrMinus7)
  lt2yrPrc[i+1] <- 100 / exp(lt2yrYield / 100 * t2yrMinus7)
  
  # calculate the yield and price for 10 year minus 1 week
  lt10yrYield <- NSS_model(key = i+1, t = t10yrMinus7)
  lt10yrPrc[i+1] <- 100 / exp(lt10yrYield / 100 * t10yrMinus7)
  
  # calculate one week yield，use NSS model to calculate the yield 
  one_week_yield <- NSS_model(key = i, t = 1/52)
  
  # TODO: interest calculation, we should change it to 
  interest_pnl[i+1] <- CashPos_7d[i] * (exp( one_week_yield * (1/52) / 100) - 1 )
  cummulative_i[i+1] <- cummulative_i[i] + interest_pnl[i+1]
  
  # close the trade from last week
  totalCap[i+1] <- q10yr[i] * lt10yrPrc[i+1] - q2yr[i]*lt2yrPrc[i+1] + CashPos_7d[i] + interest_pnl[i+1]
  
  cur_pos <- compute_short_long_quantity(totalCap[i+1], leverage, weekly_data,key = i+1)
  q2yr[i+1] <- cur_pos[1]
  q10yr[i+1] <- cur_pos[2]
    
  value2yrShort[i+1] <- q2yr[i+1] * weekly_data$bond_price_2yr[i+1] 
  value10yrLong[i+1] <- q10yr[i+1] * weekly_data$bond_price_10yr[i+1] 
  
  # update the size of the cash position
  CashPos_7d[i+1] <- cashPos(value2yrShort[i+1], value10yrLong[i+1], totalCap[i+1])
}

cummulative_r <- totalCap - starting_capital
result <- as.xts(cummulative_r, order.by=index(weekly_data))

rownames(result) <- c("Total Capital")
plot(result)

head(weekly_data)
head(q2yr)
head(q10yr)
head(value2yrShort)
head(value10yrLong)
head(totalCap)
head(CashPos_7d)


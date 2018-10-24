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
positions <- compute_short_long_quantity(starting_capital, leverage, weekly_data, key=1)

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
CashPosition<- c(as.double(cashPosStart),rep(0,n-1))
# total capital for each week, in millions
totalCap <- c(starting_capital, rep(0,n-1))
one_week_yield <- c(rep(0, n))
# consider interest from holding capitals
interest_pnl <- rep(0, n)
cummulative_i <- rep(0, n)
spread_return <- rep(0, n)
deltaY10r <- rep(0, n)
deltaY2r <- rep(0, n)
time_return <- rep(0, n)

for( i in  1:(n-1)){
  # calculate the yield and price for 2 year minus 1 week
  lt2yrYield <- NSS_model(key = i+1, t = t2yrMinus7)
  lt2yrPrc[i+1] <- 100 / exp(lt2yrYield / 100 * t2yrMinus7)
  
  # calculate the yield and price for 10 year minus 1 week
  lt10yrYield <- NSS_model(key = i+1, t = t10yrMinus7)
  lt10yrPrc[i+1] <- 100 / exp(lt10yrYield / 100 * t10yrMinus7)
  
  # calculate one week yield，use NSS model to calculate the yield 
  one_week_yield[i] <- NSS_model(key = i, t = 7/365)
  one_week_yield_compound <-  exp( one_week_yield[i] * (7/365) / 100) - 1 
  
  #interest calculation, we should change it to 
  interest_pnl[i+1] <- CashPosition[i] * one_week_yield_compound
  cummulative_i[i+1] <- cummulative_i[i] + interest_pnl[i+1]
  
  #time return 
  y_10yr <- exp((weekly_data$SVENY10[i] / 100) * Duration_10yr)
  y_2yr <- exp((weekly_data$SVENY02[i] / 100) * Duration_2yr)
  
  y_10yr_modifed <- exp((NSS_model(key = i, t = t10yrMinus7) / 100) * t10yrMinus7)
  y_2yr_modified <- exp((NSS_model(key = i, t = t2yrMinus7) / 100) * t2yrMinus7)
  
  time_passage_10yr <- ((100/y_10yr_modifed) - weekly_data$bond_price_10yr[i]) * q10yr[i]
  time_passage_2yr <- ((100/y_2yr_modified) - weekly_data$bond_price_2yr[i]) * -q2yr[i]
  time_return[i+1] <- time_passage_10yr + time_passage_2yr
  
  # close the trade from last week
  totalCap[i+1] <- q10yr[i] * lt10yrPrc[i+1] - q2yr[i]*lt2yrPrc[i+1] + CashPosition[i] + interest_pnl[i+1]
  
  cur_pos <- compute_short_long_quantity(totalCap[i+1], leverage, weekly_data,key = i+1)
  q2yr[i+1] <- cur_pos[1]
  q10yr[i+1] <- cur_pos[2]
    
  value2yrShort[i+1] <- q2yr[i+1] * weekly_data$bond_price_2yr[i+1] 
  value10yrLong[i+1] <- q10yr[i+1] * weekly_data$bond_price_10yr[i+1] 
  
  # update the size of the cash position
  CashPosition[i+1] <- cashPos(value2yrShort[i+1], value10yrLong[i+1], totalCap[i+1])

  # Change in delta r_1
  changeDeltaY10yr  <-  (as.double(weekly_data$SVENY10[i+1]) - as.double(weekly_data$SVENY10[i]))
  changeDeltaY2yr  <-  (as.double(weekly_data$SVENY02[i+1])- as.double(weekly_data$SVENY02[i]))
  spread_return[i+1]  <- -q10yr[i] * weekly_data$DV01_10yr[i] *100* changeDeltaY10yr + q2yr[i] * weekly_data$DV01_2yr[i] * changeDeltaY2yr*100
}

# Q1 plot cumulative return 
cummulative_return <- totalCap - starting_capital
result <- as.xts(cummulative_return, order.by=index(weekly_data))
plot(result, main = "Cumulative Return")

# Q2 
bp10 <- 0.01
convexity_risk <- q10yr[i]* 0.5 * weekly_data$bond_price_10yr * Duration_10yr^2 *  (bp10/100)^2 * starting_capital
plot.xts(convexity_risk, main = "Convexity Risk",major.ticks="years" )

# Q3 
# spread return
spread_return_cum <- cumsum(spread_return)
# Convexity return
deltaY10r <- c(0, diff(as.double(weekly_data$SVENY10), differences=1))
deltaY2r <- c(0, diff(as.double(weekly_data$SVENY02), differences=1))
convex_return_10yr <- 0.5 * weekly_data$bond_price_10yr * 100 *  (deltaY10r/100)^2
convex_return_2yr <- 0.5 * weekly_data$bond_price_2yr * 4 *  (deltaY2r/100)^2
convex_return <- q10yr * convex_return_10yr - q2yr * convex_return_2yr
convex_return_cum <- as.numeric(cumsum(convex_return))

# Time return passage of time 
time_return_cum <- cumsum(time_return + interest_pnl)

#residual return
residual <- cummulative_return - spread_return_cum - convex_return_cum - time_return_cum

#plot the graph
summary <- cbind(cummulative_return, spread_return_cum, convex_return_cum,time_return_cum, residual)
colnames(summary) <- c("Cum Return", "Spread Return", "Convexity Return", "Time Return", "Residual Return")
summary_table <- as.xts(summary, order.by=index(weekly_data))
plot.xts(summary_table, main = "Cumulative Return Breakdown", col=c("red", "blue", "black", "green","yellow"), legend.loc = "bottomleft", yaxis.right=TRUE)

############################################ Back Testing For Two Percent##########################################
# duplicate code as the strategy above, too lazy to optimize the code efficiency because its saturday  0_o 
# Be mindful if the data is overidden by as it shares some variables with the first strategy
# Q4
# starting capital is 1 mil
starting_capital <- 1000000

#  Assume par value $100
# calculate the ratio short over long
weekly_data$TargetLOverShort <- weekly_data$DV01_2yr/weekly_data$DV01_10yr
leverage <- 0.02

# calculate the starting units for 2yr 10 yr 
positions <- compute_short_long_quantity(starting_capital, leverage, weekly_data, key=1)

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
CashPosition<- c(as.double(cashPosStart),rep(0,n-1))
# total capital for each week, in millions
totalCap <- c(starting_capital, rep(0,n-1))
one_week_yield <- c(rep(0, n))
# consider interest from holding capitals
interest_pnl <- rep(0, n)
cummulative_i <- rep(0, n)
spread_return <- rep(0, n)
time_return <- rep(0,n)

for( i in  1:(n-1)){
  # calculate the yield and price for 2 year minus 1 week
  lt2yrYield <- NSS_model(key = i+1, t = t2yrMinus7)
  lt2yrPrc[i+1] <- 100 / exp(lt2yrYield / 100 * t2yrMinus7)
  
  # calculate the yield and price for 10 year minus 1 week
  lt10yrYield <- NSS_model(key = i+1, t = t10yrMinus7)
  lt10yrPrc[i+1] <- 100 / exp(lt10yrYield / 100 * t10yrMinus7)
  
  # calculate one week yield，use NSS model to calculate the yield 
  one_week_yield[i] <- NSS_model(key = i, t = 7/365)
  
  #interest calculation, we should change it to 
  interest_pnl[i+1] <- CashPosition[i] * (exp( one_week_yield[i] * (7/365) / 100) - 1 )
  cummulative_i[i+1] <- cummulative_i[i] + interest_pnl[i+1]
  
  # close the trade from last week
  totalCap[i+1] <- q10yr[i] * lt10yrPrc[i+1] - q2yr[i]*lt2yrPrc[i+1] + CashPosition[i] + interest_pnl[i+1]
  
  cur_pos <- compute_short_long_quantity(totalCap[i+1], leverage, weekly_data,key = i+1)
  q2yr[i+1] <- cur_pos[1]
  q10yr[i+1] <- cur_pos[2]
  
  value2yrShort[i+1] <- q2yr[i+1] * weekly_data$bond_price_2yr[i+1] 
  value10yrLong[i+1] <- q10yr[i+1] * weekly_data$bond_price_10yr[i+1] 
  
  # update the size of the cash position
  CashPosition[i+1] <- cashPos(value2yrShort[i+1], value10yrLong[i+1], totalCap[i+1])
  
  # Change in delta r_1
  changeDeltaY10yr  <-  (as.double(weekly_data$SVENY10[i+1]) - as.double(weekly_data$SVENY10[i]))
  changeDeltaY2yr  <-  (as.double(weekly_data$SVENY02[i+1])- as.double(weekly_data$SVENY02[i]))
  spread_return[i+1]  <- -q10yr[i] * weekly_data$DV01_10yr[i] * 100 * changeDeltaY10yr + q2yr[i] * weekly_data$DV01_2yr[i] * changeDeltaY2yr*100
}

# Q1 plot cumulative return 
cummulative_return_2pct <- totalCap - starting_capital
cumReturnCompare <- cbind(cummulative_return_2pct, cummulative_return)
colnames(cumReturnCompare) <- c("2% Margin", "10% Margin")
result_2pct <- as.xts(cumReturnCompare,order.by=index(weekly_data))
plot.xts(result_2pct, main = "Cumulative Return 10% vs 2% Margin",  col=c("red", "blue"), legend.loc = "bottomright", yaxis.right=TRUE)






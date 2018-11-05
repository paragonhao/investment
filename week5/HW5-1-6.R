library(xts)

########################### convert the data into xts object ###########################
sp500_raw <- read.csv("p5-sp500.csv", header = TRUE, sep=",")
sp500 <- transform(sp500_raw, caldt = as.Date(as.character(caldt), "%Y%m%d"))
row.names(sp500) <- sp500$caldt
sp500xts <- as.xts(sp500)
sp500xts <- sp500xts[,-1]
storage.mode(sp500xts) <- "double"
tdays <- 252

########################### Find out arithmetic average returns ###########################
# sp500xts is the daily return data
#arithmetic daily returns
annualized_daily_return_avg <- sum(sp500xts)/length(sp500xts) * tdays

#arithmetic monthly return
#find the position of the months
monthly <- endpoints(sp500xts, on="months",k=1)
#collapse into monthly data
monthly_data <- period.apply(sp500xts, INDEX = monthly, FUN = sum)
# sum and find the average
annualized_monthly_return_avg <- sum(monthly_data)/length(monthly_data) * 12

#arithmetic yearly return
yearly <- endpoints(sp500xts, on="years",k=1)
#collapse into yearly data
yearly_data <- period.apply(sp500xts, INDEX = yearly, FUN = sum)
# sum and find the average
annualized_yearly_return_avg <- sum(yearly_data)/length(yearly_data)

# 5 years return
sp500_5yrRet <- sp500xts['1972-01-03/2016-12-30']
#collapse into yearly data，using 60 months
yearly_5 <- endpoints(sp500_5yrRet, on="months",k=60)
# sum and find the average
yearly_5_data <- period.apply(sp500_5yrRet, INDEX = yearly_5, FUN = sum)
annualized_yearly_5_return_avg <- sum(yearly_5_data)/length(yearly_5_data) * (1/5)

########################### Find out geometric average returns ###########################
# Geometric mean of daily return
geomean <- function (return_data) {
  numeric_data <- as.numeric(return_data) + 1
  return(prod(numeric_data)^(1/length(numeric_data)) -1)
}
daily_geo_ret <- geomean(sp500xts)
annualized_daily_return_geo_mean <- (daily_geo_ret + 1)^tdays - 1

# Geometric mean of monthly return
total_ret <- (daily_geo_ret + 1)^ length(sp500xts)
monthly_return_geo_mean <- total_ret^(1/length(monthly_data)) - 1
annualized_monthly_return_geo_mean <- (monthly_return_geo_mean + 1) ^ 12 - 1

# Geometric mean of yearly return
annualized_yearly_return_geo_mean <- total_ret^(1/length(yearly_data)) - 1

# Geometric mean of 5 year return
daily_geo_ret_2016 <- geomean(sp500_5yrRet)
total_ret <- (daily_geo_ret_2016 + 1)^ length(sp500_5yrRet)
return_geo_mean_5yr <- total_ret ^ (1/length(yearly_5_data)) - 1
annualized_5yearly_return_geo_mean <-(return_geo_mean_5yr +1)^(1/5) -1

print("################### Arithmetic Mean Solutions ###################")
avgmeansoln <- matrix(c(annualized_daily_return_avg,annualized_monthly_return_avg,
                        annualized_yearly_return_avg,annualized_yearly_5_return_avg),nrow = 1, ncol = 4)
colnames(avgmeansoln) <- c("Daily", "Monthly", "Yearly", "5 Years")
avgmeansoln

print("################### Geometric Mean Solutions ###################")
geomeansoln <- matrix(c(annualized_daily_return_geo_mean,annualized_monthly_return_geo_mean,
         annualized_yearly_return_geo_mean,annualized_5yearly_return_geo_mean), nrow = 1, ncol = 4)
colnames(geomeansoln) <- c("Daily", "Monthly", "Yearly", "5 Years")
geomeansoln

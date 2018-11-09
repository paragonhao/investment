library(quantmod)

getSymbols(Symbols = c("INTC","MSFT"), auto.assign = TRUE, src="yahoo" ,from = "1989-12-29", to = "2018-9-29")

INTC_total <- INTC$INTC.Adjusted
MSFT_total <- MSFT$MSFT.Adjusted

################################## Question 1 ################################# 
weekly_ret_intel <- weeklyReturn(INTC_total, type="arithmetic",leading=TRUE)
weekly_ret_ms <- weeklyReturn(MSFT_total, type="arithmetic",leading=TRUE)

intel_weekly_mean <- mean(weekly_ret_intel)
intel_weekly_sd <- sd(weekly_ret_intel)

ms_weekly_mean <- mean(weekly_ret_ms)
ms_weekly_sd <-sd(weekly_ret_ms)

# Annualize
# Intel
intel_ret <- intel_weekly_mean * 52
intel_sd <- intel_weekly_sd * sqrt(52)

# Microsoft
msft_ret <- ms_weekly_mean * 52
msft_sd <- ms_weekly_sd * sqrt(52)
################################# Question 2 ################################# 

A <- 4
rf <- 0.01

getWeight <- function(A, variance, risk_free, expected_ret){
  return((expected_ret - risk_free)/ (A * variance))
}

intel_w <- getWeight(A, intel_sd^2, rf, intel_ret)
msft_w <- getWeight(A, msft_sd^2, rf, msft_ret)

################################# Question 3 ################################# 

getSharpeRatio <- function(stddev, expected_ret){
  return((expected_ret - rf)/ stddev)
}

sharpeRatio_intel <- getSharpeRatio(intel_sd, intel_ret)
sharpeRatio_msft <- getSharpeRatio(msft_sd, msft_ret)

ifelse(sharpeRatio_intel>sharpeRatio_msft, "Higher Sharp Ratio Intel", "Higher Sharp Ratio with microsoft")

################################# Question 4 ################################# 
covariance <- cov(dailyReturn(MSFT_total), dailyReturn(INTC_total))
port_ret[] <- rep(0,10000)
port_var[] <- rep(0,10000)

for (i in 1:10000) {
  weight_msft <- 0.0001 * i 
  weight_intel <- 1 - weight_msft
  port_ret[i] <- weight_msft * msft_ret + weight_intel * intel_ret
  port_var[i] <- weight_msft^2 * msft_sd^2 + weight_intel^2 * intel_sd^2 + 2 * weight_msft * weight_intel * covariance 
  weight_msft <- weight_msft + 0.0001
} 
min_var_pos <- which.min(port_var)
mean_var <- port_ret[min_var_pos]

plot(x= port_var, y = port_ret,pch=16, ylab = "Expected Return", xlab = "Volatility", type = "l", main="efficient frontier")
text (x=msft_sd^2, y=msft_ret, "MSFT")
text (x=intel_sd^2, y=intel_ret, "Intel", adj = c(1,1))
text(x=port_var[min_var_pos], y=port_ret[min_var_pos], "Mean-Variance Portfolio",adj = c(-.1,-1))
points(x=port_var[min_var_pos], y=port_ret[min_var_pos],pch=19,bg="red")
res <- matrix(c(port_var[min_var_pos],port_ret[min_var_pos]), nrow = 1)
colnames(res) <- c("Port Variance", "Port Return")

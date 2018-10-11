#Problem Set 3 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
mysoln <- list(student = c("Xiahao Wang", "Juan Manuel Ferreyra Maspero", "Xinyue Zhu", "Yichu Li","Mu Lin"))
if("FinancialMath" %in% rownames(installed.packages()) == FALSE) {install.packages("FinancialMath")}
# 1
library(FinancialMath)
coupon_rate <- 0.1 
n <- 3 
r1 <- 0.05
r2 <- 0.055
r3 <- 0.065
cf <- c(rep(coupon_rate * 100,2),coupon_rate * 100+100)
bond_price <- sum(cf / (c(r1,r2,r3) + 1) ^seq(1,3,by=1))
ytm <- IRR(bond_price,cf,c(1,2,3))

# 3 forward rates: year 1 to year 2 and year 1 to year 3 and year 2 to year 3
one_f_one <- (1+r2)^2/(1+r1)  # year 1 to year 2
two_f_one <- (1+r3)^3/(1+r2)^2 # year 2 to year 3
one_f_two <-  (1+r3)^3/(1+r1)  # year 1 to year 3
forward_rates <- c(one_f_one,two_f_one,one_f_two) -1
# save down your final answers for part a, b, and c
return.3yr <- sum(cf  * c((one_f_two)^2 , two_f_one,1))

a = c(bond_price, ytm)
b = forward_rates #increase/decrease vector depending on how many forward rates you can calculate
# use 1 to 3 and 2 to 3 forward rates
c = return.3yr

# add answers to list for "Q2"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2
# put a and c in pdf
# part a: use the forward rate from year 1 to year 3
b = (1+ 0.07)^3/(1+0.05) - 1
mysoln[["Q2"]] = list(b=b)
# part c:(1 + 2f1)^2 * 1mil = future value of the zero coupon
# 3
# part a: bond price = sum of future cash flow discounted by yield

# save down your final answers for part b,c,d,and e (a and f in PDF writeup)
#a = "Put in PDF Write Up"
ytm <- 0.035
compute_bond_prc <- function(n,coupon_rate,ytm){
  par_val <-100
  single_cf <- 100 * coupon_rate * 0.5
  cf <- c(rep(single_cf, n * 2 -1), single_cf + par_val)
  discount_rate <- rep(1+ytm/2, n*2) ^seq(1,n*2,by=1)
  bond_prc <- sum(cf/discount_rate)
  return(bond_prc)
}
b = c(compute_bond_prc(5,0.01,ytm),compute_bond_prc(10,0.01,ytm),compute_bond_prc(5,0.04,ytm),compute_bond_prc(10,0.04,ytm)) #(Bond A, Bond B, Bond C, Bond D)
bond_price_list <- b

ytm <- 0.038
c.prices = c(compute_bond_prc(5,0.01,ytm),compute_bond_prc(10,0.01,ytm),compute_bond_prc(5,0.04,ytm),compute_bond_prc(10,0.04,ytm)) #(Bond A, Bond B, Bond C, Bond D)
c.changes = (c.prices - b)/b * 100  #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form

ytm <-0.032
d.prices = c(compute_bond_prc(5,0.01,ytm),compute_bond_prc(10,0.01,ytm),compute_bond_prc(5,0.04,ytm),compute_bond_prc(10,0.04,ytm)) #(Bond A, Bond B, Bond C, Bond D)
d.changes = (d.prices -b)/b * 100 #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form

# part e
ytm <- 0.035
compute_duration <- function(n, coupon_rate, ytm){
  par_val <-100
  single_cf <- coupon_rate * par_val * 0.5 # because semiannual
  time <- c(seq(from=1,to= n*2,by=1))
  cf <- c(rep(single_cf,n*2-1),par_val + single_cf)
  print(cf)
  rate <- rep(1+ytm/2, n*2) ^seq(1,n*2,by=1)
  pv <- cf / rate
  bond_price = sum(pv)
  print(bond_price)
  weight <- pv / bond_price
  maculy_duration <- sum(weight * time) / 2 # because semiannual
  modified_duration <- maculy_duration /(1+ytm)
  return(modified_duration)
}

e = c(compute_duration(5,0.01,ytm),compute_duration(10,0.01,ytm),compute_duration(5,0.04,ytm),compute_duration(10,0.04,ytm)) #(Bond A duration, Bond B duration, Bond C duration, Bond D duration)
eu = -bond_price_list * e * (0.003) # price changes for 3.0->3.5 yld chg
ed = -bond_price_list * e * (-0.003) # price changes for 3.0->2.5 yld chg
#f = "Put in PDF Write up" 

# add answers to list for "Q2"
mysoln[["Q3"]] = list(#a=a, put in PDF writeup only
  b=b, 
  c.pric = c.prices, 
  c.chg = c.changes, #changes are percent changes in decimal
  d.pric = d.prices, 
  d.chg = d.changes, #changes are percent changes in decimal
  e = e,
  eu = eu,
  ed = ed)
#f = f put in PDF writeup only
# part f: 
# -higher coupon, higher sensitivity to change in price
# - higher the maturity, lower the price 


# 4
bondA.weight <- 2000000/100

lhs <- matrix(c(4,6,104,106), nrow =2, ncol = 2,byrow = TRUE)
rhs <- matrix(c(0,1000000), nrow = 2, ncol =1, byrow = TRUE)
ans <- solve(lhs) %*% rhs
bondB.weight <- ans[1,1]
bondC.weight <- ans[2,1]
# answers
# Note: Remember to add conclusions in write-up
a = c(bondA.weight, bondB.weight, bondC.weight)
# part b
flat_rate <- 0.06
cf <- c(1000000,2000000)
rate <- c((1+flat_rate)^30, (1+flat_rate)^31)
pv <- sum(cf/rate)
duration <- sum((cf/rate * c(30,31))/pv)
modified_duration <- duration/(1+flat_rate)
DV01L <- 0.01^2 * pv * modified_duration

compute_hedging_portfolio <- function(n,r){
  pv <- 100*(1+flat_rate)^-n
  duration <- n
  modified_duration <- n/(1+r)
  DV01 <- pv * modified_duration/10000
  x <- DV01L/DV01
  bond_pos <- x/(1+r)^n
  return(bond_pos)
}

# 10 year bond 
bond_10yr <- compute_hedging_portfolio(10,0.06)/((1+0.06)^10)
bond_15yr <- compute_hedging_portfolio(15,0.06)/((1+0.06)^15)
# num of bond to buy
b = c(bond_10yr,bond_15yr)
#c = "Put in PDF writeup"
#d = "Put in PDF writeup"

# add answers to list for "Q4"
mysoln[["Q4"]] =  list(a=a, b=b) #c and d in writeup


# return my solution
mysoln

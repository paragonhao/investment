# Problem Set 2 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
library(FinancialMath)

mysoln = list(student = c("Molin Liang", "Meghana Rao", "Chengbo Du", "Shardul Kulkarni"))

# 1
maturity <- c(0.05, 0.055, 0.06, 0.063)

zero_coupon <- 100/ (1+0.06)^3
zero_coupon_ytm <- IRR(zero_coupon,c(0,0,100),c(1,2,3))

cf <-  100 * 0.06 
bond_b <- cf * (1/ (1 + maturity[1])) + (cf+100) * (1/(1+maturity[2])^2)
bond_b_ytm <- IRR(bond_b,c(6,106),c(1,2))

cf = 100 * 0.08 
discount_rate <- (1/ (1 +maturity[1:3]))^seq(1,3, by=1)
bond_c <- sum(cf * discount_rate) + (cf +100) * ((1/(1+maturity[4]))^4)
bond_c_ytm <- IRR(bond_c, c(8,8,8,108),c(1,2,3,4))

# save down your final answers for part a, b, and c
a = c(round(zero_coupon,2),zero_coupon_ytm) #ytm in decimal form
b = c(round(bond_b,2),bond_b_ytm) #ytm in decimal form
c = c(round(bond_c,2),bond_c_ytm) #ytm in decimal form

# add answers to list for "Q1"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2
cf_x <- 0.04 * 100 * 0.5
p_x <- 100.98
r.6month <- ((cf_x + 100)/p_x -1) * 2

p_y <- 103.59
r.1yr <- ((103/(103.59 - (3/(1+ 0.5 * r.6month))))^0.5 - 1) * 2

# save down your final answers
a <- c(r.6month, r.1yr) #in decimal form

# add answers to list for "Q2"
mysoln[["Q2"]] = list(a=a)

# 3
r.1yr <- 100/95.238 -1

r.2yr <- (105 / (98.438 - (5/(1+r.1yr))))^0.5 - 1
spot_rate <- (c(r.1yr,r.2yr) + 1)^c(1,2)
cf <- c(7, 107)
bond_price = sum(cf /spot_rate)
arbitrage = 103.370 - bond_price

lhs <- matrix(c(5,100,105,0), nrow =2, ncol = 2,byrow = TRUE)
rhs <- matrix(c(7,107))
ans <- solve(lhs) %*% rhs
statmt <- paste0("Buy ", ans[1,1]," units of bond b and ", ans[2,1], " units of bond a")
# save down your final answers
mysoln[["Q3"]] = list(a=statmt)
# Put the answer in your PDF writeup

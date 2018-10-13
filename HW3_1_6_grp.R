#Problem Set 3 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
mysoln <- list(student = c("Mu Lin", "Xiaohao Wang", "Xinyue Zhu", "Juan Ferreyra Maspero", "Yichu Li"))
#install.packages("FinCal")
library(FinCal)
# 1x

# your intermediary code to get your answers here
R_1 <- 0.05
R_2 <- 0.055
R_3 <- 0.065

price <- 10/(1+R_1) +10/(1+R_2)^2+110/(1+R_3)^3
YTM <- irr(cf=c(-price,10,10,110))

F1_1 <- (1+R_2)^2/(1+R_1)-1
F1_2 <- ((1+R_3)^3/(1+R_1))^0.5-1
F2_1 <- (1+R_3)^3/(1+R_2)^2-1


# save down your final answers for part a, b, and c
a = c(price, YTM)
b = c(F1_1,F1_2,F2_1) #increase/decrease vector depending on how many forward rates you can calculate
   
c = return.3yr = YTM # use 1F2 and 2F1, effective IRR=YTM

# add answers to list for "Q2"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2
# put a and c in pdf

b = ((1+0.07)^3/(1+0.06))^0.5-1
mysoln[["Q2"]] = list(b=b)

# 3

# your intermediary code to get your answers here
yield <- 0.035

# To create a function that does the pricing work
semi_bondprc <- function(p, r, ttm, yield) {
  cf <- c(rep(p * r/2, ttm*2 - 1), p * (1 + r/2))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + yield/2)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}

# Bond prices when yield=3.5%
price_A <- semi_bondprc(100,0.01,5,0.035)
price_B <- semi_bondprc(100,0.01,10,0.035)
price_C <- semi_bondprc(100,0.04,5,0.035)
price_D <- semi_bondprc(100,0.04,10,0.035)

# Bond prices when yield=3.8%
newprice_A <- semi_bondprc(100,0.01,5,0.038)
newprice_B <- semi_bondprc(100,0.01,10,0.038)
newprice_C <- semi_bondprc(100,0.04,5,0.038)
newprice_D <- semi_bondprc(100,0.04,10,0.038)

chg_A <- (newprice_A/price_A-1)*100
chg_B <- (newprice_B/price_B-1)*100
chg_C <- (newprice_C/price_C-1)*100
chg_D <- (newprice_D/price_D-1)*100

# Bond prices when yield=3.2%
C_newprice_A <- semi_bondprc(100,0.01,5,0.032)
C_newprice_B <- semi_bondprc(100,0.01,10,0.032)
C_newprice_C <- semi_bondprc(100,0.04,5,0.032)
C_newprice_D <- semi_bondprc(100,0.04,10,0.032)

C_chg_A <- (C_newprice_A/price_A-1)*100
C_chg_B <- (C_newprice_B/price_B-1)*100
C_chg_C <- (C_newprice_C/price_C-1)*100
C_chg_D <- (C_newprice_D/price_D-1)*100

# To compute durations for each when yield=3.5%

Dbond <- function(p, r, ttm, yield) {
  cf <- c(rep(p * r/2, ttm*2 - 1), p * (1 + r/2))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + yield/2)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  price <-sum(cf$pv)
  cf$weight <- cf$pv/price
  duration <- sum(cf$weight * cf$t/2)
}

D_A <- Dbond(100,0.01,5,0.035)
D_B <- Dbond(100,0.01,10,0.035)
D_C <- Dbond(100,0.04,5,0.035)
D_D <- Dbond(100,0.04,10,0.035)

# For modified duration and DV01
MD_A <- D_A/(1+yield/2)
MD_B <- D_B/(1+yield/2)
MD_C <- D_C/(1+yield/2)
MD_D <- D_D/(1+yield/2)

DV01_A <- MD_A * price_A
DV01_B <- MD_B * price_B
DV01_C <- MD_C * price_C
DV01_D <- MD_D * price_D

# Apply delta_P=-P*Modified Duration*dealta_r
delta_price_A_u=-price_A*MD_A*0.003
delta_price_B_u=-price_B*MD_B*0.003
delta_price_C_u=-price_C*MD_C*0.003
delta_price_D_u=-price_D*MD_D*0.003

delta_price_A_d=-price_A*MD_A*-0.003
delta_price_B_d=-price_B*MD_B*-0.003
delta_price_C_d=-price_C*MD_C*-0.003
delta_price_D_d=-price_D*MD_D*-0.003


# save down your final answers for part b,c,d,and e (a and f in PDF writeup)
#a = "Put in PDF Write Up"
b = c(price_A,price_B,price_C,price_D) #(Bond A, Bond B, Bond C, Bond D)
c.prices = c(newprice_A,newprice_B,newprice_C,newprice_D) #(Bond A, Bond B, Bond C, Bond D)
c.changes = c(chg_A,chg_B,chg_C,chg_D) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form
d.prices = c(C_newprice_A,C_newprice_B,C_newprice_C,C_newprice_D) #(Bond A, Bond B, Bond C, Bond D)
d.changes = c(C_chg_A,C_chg_B,C_chg_C,C_chg_D) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form
e = c(D_A,D_B,D_C,D_D) #(Bond A duration, Bond B duration, Bond C duration, Bond D duration)
eu = c(delta_price_A_u,delta_price_B_u,delta_price_C_u,delta_price_D_u) # price changes for 3.0->3.5 yld chg
ed = c(delta_price_A_d,delta_price_B_d,delta_price_C_d,delta_price_D_d) # price changes for 3.0->2.5 yld chg
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

# 4

# your intermediary code to get your answers here
# Basics about the liability
r <- 0.048
liability <- 1000000/(1+r)^30 + 2000000/(1+r)^31
duration_liab <- (1000000/(1+r)^30/liability)*30 + (2000000/(1+r)^31/liability)*31
MD_liab <- duration_liab/(1+r)
DV01_liab <- MD_liab*liability

price_bond_A <- 100/(1+r)^31
bondprc <- function(p, r, ttm, yield) {
  cf <- c(rep(p * r, ttm - 1), p * (1 + r))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + yield)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}

price_bond_B <- bondprc(100,0.04,30,0.06)
price_bond_C <- bondprc(100,0.06,30,0.06)

unit_A <- (2000000/(1+r)^31)/price_bond_A # unit of long position needed to repay $2M at Year 31
unit_C <- (liability-unit_A*price_bond_A)/(price_bond_C-1.5*price_bond_B)
unit_B <- -1.5*unit_C # this is true in order to net out the coupon pmts

total_contribution <- unit_A*price_bond_A+unit_B*price_bond_B+unit_C*price_bond_C
weight_A <- 2000000/(1+r)^31/total_contribution*100
weight_B <- price_bond_B*unit_B/total_contribution*100
weight_C <- price_bond_C*unit_C/total_contribution*100

# Duration-based hedging using 10- and 15-yr ZCB's
bond_10_duration <- 10 # since ZCB
bond_15_duration <- 15 # since ZCB
price_bond_10 <- 100/(1+r)^10
price_bond_15 <- 100/(1+r)^15
DV01_10 <- bond_10_duration/(1+r)*price_bond_10
DV01_15 <- bond_15_duration/(1+r)*price_bond_15

# same market value and DV01*x + DVO1*y = DV01(liab), we can solve x and y
y <- (DV01_liab*price_bond_10-DV01_10*liability)/(DV01_15*price_bond_10-DV01_10*price_bond_15)
x <- (DV01_liab-DV01_15*y)/DV01_10

weight_10 <- x*price_bond_10/liability*100 # multiply by 100 to show %
weight_15 <- y*price_bond_15/liability*100 # multiply by 100 to show %

# answers
# Note: Remember to add conclusions in write-up
a = c(weight_A, weight_B, weight_C)
b = c(weight_10, weight_15)
#c = "Put in PDF writeup"
#d = "Put in PDF writeup"

# add answers to list for "Q4"
mysoln[["Q4"]] =  list(a=a, b=b) #c and d in writeup


# return my solution
mysoln

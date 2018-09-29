# Problem Set 1 Solution Template
# mysoln is a list to store your answers

# the first element of the list is a vector of names for the students in your group
# make sure these match the names shown on the pdf document provided by the MFE office
# using group 1 as an example:
mysoln <- list(student = c("Xiahao Wang", "Juan Manuel Ferreyra Maspero", "Xinyue Zhu", "Yichu Li","Mu Lin"))

# 1

p <- 10000
r <- 0.06
n <- 3
compounded_rate <- 0
# r is the rate, n is the period
total <- function (p, r, n) {
  compounded_rate <- (1 + r) ^ n
  return(round(compounded_rate * p, 2))
}

# answers
a <- total(p, r, n)
b <- total(p, r = r/4, n = n * 4)
c <- total(p, r = r/12, n = n * 12)

# add to list (start at the second element - the first element is a vectopr of names)
mysoln[["Q1"]] <- c(a, b, c)

# 2
# Charlott receives 10 payments in 30 years
p <- 0.1 * 500000
r <- 0.05
cf_500k <- rep(p, 10)
discount_rate <- (1+r) ^ seq(3, by =3, len = 10)

# answers
a <- sum (cf_500k/discount_rate)
b <- 700000 / sum(1/discount_rate)
mysoln[["Q2"]] <- c(a, b)

# 3
saving <- 200000 * 0.3
n <- 35
r <- 0.04
cf <- rep(saving, n)
discount_rate <- (1+0.04) ^ seq(35, by =-1, len = 35)
npv <- sum(cf / discount_rate)
fv <- npv * (1+r) ^ n

dr_after_retirement <- (1+0.04) ^ seq(19, by =-1, len = 20)
monthly_payment <- (fv * (1 + r)^20)/sum(dr_after_retirement)

# answers
a <- fv
b <- monthly_payment
mysoln[["Q3"]] <- c(a, b)

# 4
r <- 0.07
ear <- (1 + (r/12)) ^ 12 - 1
mortgage <- 400000
n<- 12 *30
monthly_irate <- r /12
monthly_payment <- (mortgage * (1 + monthly_irate)^360) / sum((1 + monthly_irate) ^ seq(359, by=-1, 0))
discount_rate <- (1 + monthly_irate) ^ seq(1, by=1, 340) 
amt_owned <- sum(monthly_payment / discount_rate)

# answers
a <- ear
b <- monthly_payment
c <- amt_owned
mysoln[["Q4"]] <- c(a, b, c)


# 5
# answers: change the numbers or extend the vector as needed
a <- c(0,1000,1000,1000,1000) # Receive positive cash flow without initial investment
b <- c(-100,100,300,50,-100,-100,-100,-100,-100)

mysoln[["Q5"]] <- list(a = a, b = b)
#return my solution

mysoln



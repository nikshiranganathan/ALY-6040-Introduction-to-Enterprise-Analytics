#---------------------- Week_4_Module_4 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Module 4 Project - A Prescriptive Model for Strategic Decision-making: An Inventory Model")
print("Course Name -  ALY6050: Introduction to Enterprise Analytics")

############################## PART 2 ##############################
# Installing the libraries
library(triangle)

# Q1 1000 simulations
n<-1000
ad <- rtriangle(n,13000,17000,15000) 
ad
orderquantity <- vector()

for (i in 1:1000) {
  totalcost <- function(orderq) {14.4 * orderq + 220 * ad[i] / orderq + 80 * ad[i]}
  op <- optimize(totalcost, interval = c(1, 1000))
  orderquantity[i] <- op$minimum
}
totalcost
invcost <- 14.4 * orderquantity + 220 * ad / orderquantity + 80 * ad
invcost

# 2. Minimum total cost - Probability distribution
hist(invcost, main = 'Histogram of Minimum Total Cost', col = '#F8766D', xlab = 'Min Total Cost',ylab='Frequency')

# H0: Minimum Total Cost follows the normal distribution
# H1: Minimum Total Cost does not follow the normal distribution
observed1 <- invcost
prob.exp <- pnorm(observed1, mean(observed1), sd(observed1))
chisq.test(observed1, p = prob.exp, rescale.p = TRUE)

# Shapiro test
shapiro.test(invcost)

# 3. Order Quantity - Probability distribution
hist(orderquantity, main = 'Histogram of Order Quantity', col = '#00C19A', xlab = 'Order Quantity')

# H0: Expected Order Quantity follows the normal distribution
# H1: Alternative hypothesis : Expected Order Quantity does not follow the normal distribution

observed2 <- orderquantity
prob.exp <- pnorm(observed2, mean(observed2), sd(observed2))
chisq.test(observed2, p = prob.exp, rescale.p = TRUE)

# Shapiro test
shapiro.test(orderquantity)

# 4. Annual Number of Orders - Probability distribution
nooforder <- ad / orderquantity
nooforder
hist(nooforder, main = 'Histogram of Annual Number of Orders', col = '#00A9FF', xlab = 'Annual Number of Orders')

# H0: Expected Annual number of orders follows the normal distribution
# H1: Expected Annual number of orders does not follow the normal distribution
observed3 <- nooforder
prob.exp <- pnorm(observed3, mean(observed3), sd(observed3))
chisq.test(observed3, p = prob.exp, rescale.p = TRUE)

# Shapiro test
shapiro.test(nooforder)

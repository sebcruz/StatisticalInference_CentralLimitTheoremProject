# Load Tooth Growth Dataset
ToothGrowth



## Quiz 3.
# Problem 1.
x_bar <- 1100
s <- 30
n <- 9
alpha <- 0.05
ts <- qt(1 - alpha / 2, n - 1) # 2.306004
round(x_bar + c(-1, 1) * ts * s / sqrt(n)) # 1077 1123

# Problem 2.
x_bar <- -2
n <- 9
alpha <- 0.05
ts <- qt(1 - alpha / 2, n - 1) # 2.306004
s <- -x_bar*sqrt(n) / ts
s # 2.601903

# Problem 3.
# Independent tests: not related participants
# Paired tests: related participants, same group uses 2 different tests
# A paired interval.

# Problem 4.
n_x <- 10
n_y <- 10
x_bar <- 5 # old_system
y_bar <- 3 # new_system
var_x <- 0.6
var_y <- 0.68
alpha <- 0.05
sp_2 <- ((n_x - 1)*var_x + (n_y - 1)*var_y) / (n_x + n_y - 2)
sp <- sqrt(sp_2)
ts <- qt(1 - (alpha/2), n_x + n_y - 2)
round((y_bar - x_bar) + c(-1, 1) * ts * sp * (sqrt(1/n_x + 1/n_y)), 2)
# -2.75 -1.25

# Problem 5.
# 90% confidence interval gives a lower t-value then 95% confidence interval.
# => The interval will be narrower.

# Problem 6.
n_x <- 100
n_y <- 100
x_bar <- 6
y_bar <- 4
s_x <- 2
s_y <- 0.5
alpha <- 0.05
sp_2 <- ((n_x - 1)*s_x^2 + (n_y - 1)*s_y^2) / (n_x + n_y - 2)
sp <- sqrt(sp_2)
ts <- qt(1 - (alpha/2), n_x + n_y - 2)
round((x_bar - y_bar) + c(-1, 1) * ts * sp * (sqrt(1/n_x + 1/n_y)), 2)
# 1.59 2.41 => The new system appears to be effective.

# Problem 7.
n1 <- n2 <- 9
x1 <- -3  ##treated
x2 <- 1  ##placebo
s1 <- 1.5  ##treated
s2 <- 1.8  ##placebo
s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))
(x1 - x2) + c(-1, 1) * qt(0.95, n1 + n2 - 2) * s * sqrt(1/n1 + 1/n2)
#-5.363579 -2.636421
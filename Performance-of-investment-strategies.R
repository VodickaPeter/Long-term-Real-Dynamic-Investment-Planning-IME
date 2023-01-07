---
title: "Long-term real dynamic investment planning"
subtitle: "Comparison of myopic and nonmyopic investment strategies on S&P 500"
journal: "Insurance: Mathematics and Economics"
authors: "Peter Vodička"
co-authors: "Russell Gerrard, Munir Hiabu, Jens Perch Nielsen"
institution: "Bayes Business School (formerly Cass), City Univeristy of London"
date: "2019-September"
---
  
# Average Exposure

Original_Merton_Theta*(1/((1-gamma)*sigma_fix))
mean(theta_last30*(1/((1-gamma)*sigma)))
mean(((1/((1-gamma)*sigma)))*(theta_last30+rho*tau_theta*b_theta))

# Sample Standard Deviation of Realized Returns

sd(Original_Merton_Theta*(1/((1-gamma)*sigma_fix))*returns_last30)
sd((theta_last30*(1/((1-gamma)*sigma)))*returns_last30)
sd((((1/((1-gamma)*sigma)))*(theta_last30+rho*tau_theta*b_theta))*returns_last30)

# Mean Return

(Y_Original_Merton[31]/10000)^(1/30)-1
(Y_Naive_Dynamic_Merton[31]/10000)^(1/30)-1
(Y_Extended_Dynamic_Merton[31]/10000)^(1/30)-1

# Mean Return Divided by Exposure

((Y_Original_Merton[31]/10000)^(1/30)-1)/(Original_Merton_Theta*(1/((1-gamma)*sigma_fix)))
((Y_Naive_Dynamic_Merton[31]/10000)^(1/30)-1)/(mean(theta_last30*(1/((1-gamma)*sigma))))
((Y_Extended_Dynamic_Merton[31]/10000)^(1/30)-1)/(mean(((1/((1-gamma)*sigma)))*(theta_last30+rho*tau_theta*b_theta)))

# Mean Return Divided by Standard Deviation

((Y_Original_Merton[31]/10000)^(1/30)-1)/sd(Original_Merton_Theta*(1/((1-gamma)*sigma_fix))*returns_last30)
((Y_Naive_Dynamic_Merton[31]/10000)^(1/30)-1)/sd((theta_last30*(1/((1-gamma)*sigma)))*returns_last30)
((Y_Extended_Dynamic_Merton[31]/10000)^(1/30)-1)/sd((((1/((1-gamma)*sigma)))*(theta_last30+rho*tau_theta*b_theta))*returns_last30)

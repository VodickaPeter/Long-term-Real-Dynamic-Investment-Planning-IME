
---
title: "Long-term real dynamic investment planning"
subtitle: "Earnings-by-price-double-benchmark"
journal: "Insurance: Mathematics and Economics"
authors: "Russell Gerrard, Munir Hiabu, Jens Perch Nielsen, Peter Vodička"
Institution: "Bayes Business School (formerly Cass), London"
date: "2019-September"
---
  
# Loading Robert J Shiller's Data (http://www.econ.yale.edu/~shiller/)
  
library(xtable)
data <- read.csv2("Shiller_data.csv")
head(data)
n <-  dim(data)[1]
n1 <- n-1           
n2 <- n-2
n3 <- n-3  
n4 <- n-4
n5 <- n-5
n6 <- n-6
n7 <- n-7
n8 <- n-8
n9 <- n-9
n10 <- n-10
n11 <- n-11
n12 <- n-12
na <- 75

# focus on S&P 500 Index after World War 2 

# $Year
# $P        : S&P Composite Stock Price Index
# $D        : Dividends accruing to index
# $E        : Earnings accruing to index
# $R        : One year interest rate
# $RLONG    : Long Government Bond Yield (10yrpost53)
# $CPI      : Consumer Price Index
# $RealR    : Real One-Year Interest Rate
# $C        : Real per capita consumption
# $Year2    :
# $RealP    : Real Stock Price
# $P.       : Present Value of Real Dividends Const r
# $P.r      : Present Value of Real Dividends Market r
# $P.C      : Present Value of Real Dividends Cons disc.
# $RealD    : RealD S&P Dividend
# $Return   : on S&P Composite
# $ln.1.ret.: ln(1+ret)
# $RealE    : RealE  Earnings
# $P.E      : Price Earnings Ratio One-Year Earnings
# $E10      : Ten-Year Average of Real Earnings
# $P.E10    : Price Earnings Ratio Ten-Year Earnings

variable <- matrix (0.0,n2,8)
# inflation excess stock return 1873-2018 
variable[,1] <- log((data[3:n,2]+data[2:n1,3])/data[2:n1,2])-log(data[2:n1,7]/data[1:n2,7])  
#short term interest excess stock return 1873-2018
variable[,2] <- log((data[3:n,2]+data[2:n1,3])/data[2:n1,2])-log(data[2:n1,5]/100+1)  
# no benchmark dividend by price 1872-2018  
variable[,3] <- data[1:n2,3]/data[2:n1,2]                       
# inflation adj dividend by price 1872-2018  
variable[,4] <- data[1:n2,3]/data[2:n1,2] /(data[2:n1,7]/data[1:n2,7])  
# short term interest adj dividend by price 1872-2018  
variable[,5] <- data[1:n2,3]/data[2:n1,2] /(data[2:n1,5]/100+1)  
# no benchmark earnings by price 1872-2018 
variable[,6] <- (data[1:n2,4]/data[2:n1,2]+1)         
# inflation adj earnings by price 1872-2018
variable[,7] <- (data[1:n2,4]/data[2:n1,2]+1)/(data[2:n1,7]/data[1:n2,7])            
# short term interest adj earnings by price 1872-2018  
variable[,8] <- (data[1:n2,4]/data[2:n1,2]+1)/(data[2:n1,5]/100+1)   

# linear regressions

# earnings with double inflation
earnings.i <- variable[,7]-1
intercept.e.di <- (lm(variable[,1]~earnings.i)[1])[[1]][1]
slope.e.di <- (lm(variable[,1]~earnings.i)[1])[[1]][2]

epsilon.e.di <- intercept.e.di + slope.e.di * earnings.i

# dividends with double inflation
dividends.i <- variable[,4]
intercept.d.di <- (lm(variable[,1]~dividends.i)[1])[[1]][1]
slope.d.di <- (lm(variable[,1]~dividends.i)[1])[[1]][2]

epsilon.d.di <- intercept.d.di + slope.d.di * dividends.i

# earnings with single inflation
earnings <- variable[,6]-1
intercept.e.i <- (lm(variable[,1]~earnings)[1])[[1]][1]
slope.e.i <- (lm(variable[,1]~earnings)[1])[[1]][2]

epsilon.e.i <- intercept.e.i + slope.e.i * earnings

# dividends with single inflation
dividends <- variable[,3]
intercept.d.i <- (lm(variable[,1]~dividends)[1])[[1]][1]
slope.d.i <- (lm(variable[,1]~dividends)[1])[[1]][2]

epsilon.d.i <- intercept.d.i + slope.d.i * dividends

# earnings with double short interest 
earnings.s <- variable[,8]-1
intercept.e.ds <- (lm(variable[,2]~earnings.s)[1])[[1]][1]
slope.e.ds <- (lm(variable[,2]~earnings.s)[1])[[1]][2]

epsilon.e.ds <- intercept.e.ds + slope.e.ds * earnings.s

# dividends with double short interest 
dividends.s <- variable[,5]
intercept.d.ds <- (lm(variable[,2]~dividends.s)[1])[[1]][1]
slope.d.ds <- (lm(variable[,2]~dividends.s)[1])[[1]][2]

epsilon.d.ds <- intercept.d.ds + slope.d.ds * dividends.s

# earnings with single short interest 
intercept.e.s <- (lm(variable[,2]~earnings)[1])[[1]][1]
slope.e.s <- (lm(variable[,2]~earnings)[1])[[1]][2]

epsilon.e.s <- intercept.e.s + slope.e.s * earnings

# dividends with single short interest 
intercept.d.s <- (lm(variable[,2]~dividends)[1])[[1]][1]
slope.d.s <- (lm(variable[,2]~dividends)[1])[[1]][2]

epsilon.d.s <- intercept.d.s + slope.d.s * dividends

# Choose predictor!

# predictor <- epsilon.d.di
# predictor <- epsilon.e.s

predictor <- epsilon.e.di
gamma<- -2
 

myfunction <- function(data,variable,predictor,gamma,realreturn=TRUE){
    if ( prod ( predictor == epsilon.e.di | predictor == epsilon.d.di | predictor == epsilon.e.i | predictor == epsilon.d.i)){
            ModelData <- data.frame(data$YEAR[2:n1], variable[,1], predictor)};
    if (prod ( predictor == epsilon.e.ds | predictor == epsilon.d.ds | predictor== epsilon.e.s | predictor == epsilon.d.s)){
             ModelData <- data.frame(data$YEAR[2:n1], variable[,2], predictor)};
    colnames(ModelData) <- c('Year', 'Returns', 'Epsilon');
    
    mean(ModelData$Returns) 
    sd(ModelData$Returns) 
    mean(ModelData$Epsilon) 
    sd(ModelData$Epsilon)
  
# Figure::Earnings explaining the returns in US

par(mfrow=c(1,1));
par(mar=c(4, 4, 2, 1));
plot(ModelData$Year, ModelData$Returns, type = "l", lty = 1, pch = 19, col='black', xlab = '', ylab='', yaxt="n", 
     ylim = c(min(ModelData$Returns), max(ModelData$Returns)))
lines(ModelData$Year, ModelData$Epsilon, type = "b", lty = 2, pch = 18, col='red')
grid(NA, NA, lwd = 1) 
axis(2, at=pretty(ModelData$Returns), 
     lab=paste(pretty(ModelData$Returns)*100,"%"))

text(ModelData$Year[seq(from=1, to=length(ModelData$Year), by=5)], 
     ModelData$Returns[seq(from=1, to=length(ModelData$Returns), by=5)],
     labels = paste(round(ModelData$Returns[seq(from=1, to=length(ModelData$Returns), by=5)]*100, 0), "%"),
     pos=3, cex=1.4, font=2, col='black')

text(ModelData$Year[seq(from=1, to=length(ModelData$Year), by=5)], 
     ModelData$Epsilon[seq(from=1, to=length(ModelData$Epsilon), by=5)],
     labels = paste(round(ModelData$Epsilon[seq(from=1, to=length(ModelData$Epsilon), by=5)]*100, 0), "%"), 
     pos=3, cex=1.4, font=2, col='red')

title(ylab = " Value", line=2, cex.lab=2)
title(xlab = 'Year', line=2.5, cex.lab=2)
title(main = c(paste(" Historical returns in excess of inflation and transformed earnings for S&P 500 (1872 - 2018)")), cex.main=1.5, font.main=1, line=1, cex.lab=1)
legend("bottomleft",
       legend=c(paste("returns"),
                paste("transformed earnings")),
       col=c("black", "red"), lty = 1:2, cex=1.5, bty = "n", seg.len=0.5, xpd = TRUE, horiz = FALSE);

# Parameter Estimation

head(ModelData)
tail(ModelData)
n <- length(ModelData$Year); 
# 147 years, length of the vector, we observe one value for earnings and returns during course of year 
T <- 30; 
# length is 117 years for estimation
length(earnings)
returns <- ModelData$Returns[as.numeric(row.names(ModelData[ModelData$Year==(1872),])):(length(ModelData$Year)-T)];
epsilon <- ModelData$Epsilon[as.numeric(row.names(ModelData[ModelData$Year==(1872),])):(length(ModelData$Year)-T)];

# length is 30 years for estimation, length(earnings_last30)
returns_last30 <- ModelData$Returns[as.numeric(row.names(ModelData[ModelData$Year==(2018-T+1),])):length(ModelData$Year)];
epsilon_last30 <- ModelData$Epsilon[as.numeric(row.names(ModelData[ModelData$Year==(2018-T+1),])):length(ModelData$Year)];

min(epsilon_last30)
max(epsilon_last30)
mean(epsilon_last30)
mean(ModelData$Epsilon)
min(ModelData$Epsilon)
max(ModelData$Epsilon)
mean(ModelData$Returns)
min(ModelData$Returns)
max(ModelData$Returns)
mean(returns_last30)
sd(ModelData$Returns)
sd(ModelData$Epsilon)

# Lagging

# n

epsilon_n <- epsilon[2:length(epsilon)]
returns_n <- returns[2:length(returns)]

# n-1 
epsilon_n_lagged <- epsilon[1:length(epsilon)-1]
returns_n_lagged <- returns[1:length(returns)-1]

# Sigma from returns

sigma <- sd(returns - epsilon); 

# Linear regression of \varepsilon(n) - exp(-\kappa)*\varepsilon(n-1)
# model_earnings <- lm(earnings_n ~ earnings_n_lagged)
# summary(model_earnings)
# kappa <- - log(0.23374731) # 1.453515
# mu_theta <- (0.04632876/(1+exp(-kappa_2))+1/2*sigma_r^2)/sigma_r # 0.2959801

regression_epsilon <- lm(epsilon_n ~ epsilon_n_lagged);
summary(regression_epsilon);
regression_epsilon$coefficients;

beta_0 <- as.numeric(regression_epsilon$coefficients[1]);
beta_1 <- as.numeric(regression_epsilon$coefficients[2]);

# kappa <- - log(0.2293798) # 1.472376;
# mu_theta <- (0.0557601/(1+exp(-kappa))+1/2*sigma^2)/sigma # 0.3479915

my_function<-function(kappa){
  t<-0.5* exp(-kappa)*(1-exp(-kappa))^2*(exp(kappa)-exp(-2*kappa*n))
  b<-exp(-kappa)-1+kappa-0.5*exp(-2*kappa*n)*(1-exp(kappa))^2
  (t/b)-beta_1 
}

kappa<- uniroot(my_function,lower=0.001,upper=5, tol = 1e-20)$root
beta_1<-exp(-kappa)
beta_0<-mean(epsilon_n-beta_1*epsilon_n_lagged)

mu_theta <- (beta_0/(1-exp(-kappa))+1/2*sigma^2)/sigma # 0.3479915

# \tau_\theta 

variance <- var(epsilon_n-exp(-kappa)*epsilon_n_lagged) # 0.004923322
quantity <- (1+exp(-2*kappa)-1/kappa*(1-exp(-2*kappa))) # 0.4091757
tau_theta <- sqrt((kappa^2*variance)/(quantity*sigma^2)) #  0.9299305

# \tau_\theta

# rho_old <- (kappa*cov((returns_n-epsilon_n_lagged),(epsilon_n-exp(-kappa)*epsilon_n_lagged)))/(sigma^2*tau_theta*(1/kappa*(1-exp(-kappa))-1)) #  
rho <- (kappa^2*cov((returns-epsilon),(epsilon)))/(sigma^2*tau_theta*(kappa-(1-exp(-kappa)))) #  2) why was cov old version with other formula

# Figure 2: Functions b1, b2

# Variables necessary
kappa
sigma
r <- 0

tau_theta # 0.8882635
mu_theta # 0.3316346
#gamma <- -1
rho

# Mean-revering model

xi <- kappa/tau_theta; #  1.636355
eta <- gamma/(1-gamma); # -0.5
Rp <- sqrt(xi^2-(1+2*rho*xi)*eta); # sqrt(3.403923) = 1.844972
# Case II: xi^2 - eta*(1+2*rho*xi) => 0
psi1_case2 <- ((xi-eta*rho)+Rp)/(2*tau_theta*(1+eta*rho^2)); #  2.017832
psi2_case2 <- ((xi-eta*rho)-Rp)/(2*tau_theta*(1+eta*rho^2)); # -0.07927077

# b1 and b2 as functions of t
T <- 30;
b1_case2 <- function(t) {(kappa*mu_theta*eta)/(2*tau_theta^2*Rp*(1+eta*rho^2))*(exp(Rp*tau_theta*(T-t))-2+exp(-Rp*tau_theta*(T-t)))/(psi1_case2*exp(Rp*tau_theta*(T-t))-psi2_case2*exp(-Rp*tau_theta*(T-t)))};
b2_case2 <- function(t) {(eta)/(4*tau_theta^2*(1+eta*rho^2))*(exp(Rp*tau_theta*(T-t))-exp(-Rp*tau_theta*(T-t)))/(psi1_case2*exp(Rp*tau_theta*(T-t))-psi2_case2*exp(-Rp*tau_theta*(T-t)))};

# y stands for one yearly computed value
N <- 12; 
b1_case2_y <- b2_case2_y  <- vector(mode="numeric", length = T*N);
for (t in (1:(T*N))) 
{
  b1_case2_y[t] <- b1_case2(t/N);
  b2_case2_y[t] <- b2_case2(t/N);
}

year.x <- seq(from=(2018-30)+1/N, to=2018, by=1/N);

pdf('Figure2.pdf', paper = "a4r",width=10.8, height=15)
par(mar=c(4.3, 5.5, 4.5, 0));
#quartz()
plot(year.x, b1_case2_y, type = "l", lty = 1, pch = 19, col='red', xlab = '', ylab='', 
     lwd=3, cex.axis=2.5,
     ylim = c(min(b1_case2_y, b2_case2_y), 
              max(b1_case2_y, b2_case2_y))); 
lines(year.x, b2_case2_y, type='l',  lty = 2, pch = 18, col='blue', lwd=3);
# text(year.x[seq(from=round(length(year.x)/1.2), to=length(year.x), by=10)], 
#      b1_case2_y[seq(from=round(length(year.x)/1.2), to=length(b1_case2_y), by=10)],
#      labels = paste(round(b1_case2_y[seq(from=round(length(year.x)/1.2), to=length(b1_case2_y), by=10)], 3)), 
#      pos=3, cex=1.4, font=2, col='red');    
# 
# text(year.x[seq(from=round(length(year.x)/1.2), to=length(year.x), by=3)], 
#      b2_case2_y[seq(from=12+1, to=length(b2_case2_y), by=3)],
#      labels = paste(round(b2_case2_y[seq(from=12+1, to=length(b2_case2_y), by=3)], 3)), 
#      pos=3, cex=1.4, font=2, col='blue'); 

title(ylab = 'Value', line=3.5, cex.lab=3);
title(xlab = 'Year', line=3, cex.lab=3);
title(main = expression(paste(bold("Behaviour of functions"))), 
      line=2, cex.lab=0.1, cex.main=3, font.main=2, col.main= 'black');
legend('topleft',
       legend=c(expression(b[1], b[2])),
       col=c('red','blue'),
       lty=1:2,
       lwd=3,
       cex=2.5, 
       seg.len=1, 
       bty="n",
       y.intersp=0.8);
dev.off()

# Figure 3: Comparison of Naive and Extended Strategy

# b1 and b2 as functions of t

T <- 30;
b1_case2 <- function(t) {(kappa*mu_theta*eta)/(2*tau_theta^2*Rp*(1+eta*rho^2))*(exp(Rp*tau_theta*(T-t))-2+exp(-Rp*tau_theta*(T-t)))/(psi1_case2*exp(Rp*tau_theta*(T-t))-psi2_case2*exp(-Rp*tau_theta*(T-t)))};
b2_case2 <- function(t) {(eta)/(4*tau_theta^2*(1+eta*rho^2))*(exp(Rp*tau_theta*(T-t))-exp(-Rp*tau_theta*(T-t)))/(psi1_case2*exp(Rp*tau_theta*(T-t))-psi2_case2*exp(-Rp*tau_theta*(T-t)))};

# y stands for one yearly computed value

N <- 1; 
b1_case2_y <- b2_case2_y <- vector(mode="numeric", length = T*N);

for (t in (1:(T*N))) 
{
  b1_case2_y[t] <- b1_case2(t/N);
  b2_case2_y[t] <- b2_case2(t/N);
}

# Last 30 years of Earnings and Returns

# earnings_last30 
returns_last30 
epsilon_last30

theta_last <- (epsilon_last30 + 1/2*sigma^2)/sigma

N <- 1; 
naive_strategy <- extended_strategy <- vector(mode="numeric", length = T*N);

for (t in (1:(T*N))) 
{
  naive_strategy[t] <- 1/((1-gamma)*sigma)*(theta_last[t]);
  extended_strategy[t] <- 1/((1-gamma)*sigma)*(theta_last[t]+rho*tau_theta*(b1_case2(t/N)+2*b2_case2(t/N)*theta_last[t]));
}

# # Mean-Reverting Model
# par(mar=c(4.1, 5, 4.5, 2.2));
# year.x <- seq(from=(2009-T)+1/N, to=2009, by=1/N);
# plot(year.x, naive_strategy, type='l', col='red', xlab = '', ylab='', 
#      ylim = c(min(naive_strategy, extended_strategy), 
#               max(naive_strategy, extended_strategy))); 
# lines(year.x, extended_strategy, type='l', col='blue');
# 
# text(year.x[seq(from=1, to=length(year.x), by=N*3)], 
#      naive_strategy[seq(from=1, to=length(naive_strategy), by=N*3)],
#      labels = paste(round(naive_strategy[seq(from=1, to=length(naive_strategy), by=N*3)], 3)), 
#      pos=3, cex=1.4, font=2, col='red');    
# 
# text(year.x[seq(from=1, to=length(year.x), by=N*3)], 
#      extended_strategy[seq(from=1, to=length(extended_strategy), by=N*3)],
#      labels = paste(round(extended_strategy[seq(from=1, to=length(extended_strategy), by=N*3)], 3)), 
#      pos=1, cex=1.4, font=2, col='blue'); 
# 
# title(ylab = 'Value', line=3.5, cex.lab=2);
# title(xlab = 'Year', line=3, cex.lab=2);

# Computing more than one value a year, keeping theta constant during the curse of a year

theta_last_stepwise <- stepfun(c(seq(from=2, to=30, by=1)), c(theta_last));
fun_theta <- function(t) {theta_last_stepwise(t)}
N <- 12; # monthly 
sigma_fix <- sd(returns); # 0.1719975
mu_fix <- mean(returns) # 0.06255975
# mu_fix <- mean(earnings)
# sigma_fix <- sigma
sigma_fix <- sd(returns); # 0.1719975
mu_fix <- mean(returns) # 0.06255975
# mu_fix <- mean(earnings)

Original_Merton_Theta <- mu_fix/sigma_fix
Original_Merton_Strategy <- Original_Merton_Theta*(1/((1-gamma)*sigma_fix))
classic <- naive_strategy_cont <- extended_strategy_cont <- vector(mode="numeric", length = T*N);
for (t in (1:(T*N))) 
{
  classic[t] <- Original_Merton_Theta*(1/((1-gamma)*sigma_fix))
  naive_strategy_cont[t] <- 1/((1-gamma)*sigma)*(fun_theta(t/N))
  extended_strategy_cont[t] <- 1/((1-gamma)*sigma)*(fun_theta(t/N)+rho*tau_theta*(b1_case2(t/N)+2*b2_case2(t/N)*fun_theta(t/N)));
}

mean(naive_strategy_cont)
mean(extended_strategy_cont)

pdf('Figure3.pdf', paper = "a4r",width=10.8, height=15)
par(mar=c(4.3, 6, 4.5, 2.5));

year.x <- seq(from=(2018-T)+1/N, to=2018, by=1/N);
plot(year.x, naive_strategy_cont, type='l', lty = 2, pch = 18, col='blue', xlab = '', ylab='', lwd=1.5, cex.axis=2.5,
     ylim = c(min(naive_strategy_cont, extended_strategy_cont), 
              max(naive_strategy_cont, extended_strategy_cont)+0.05)); 
lines(year.x, extended_strategy_cont, type = "l", lty = 4, pch = 19, col="red", lwd=1.5);
lines(year.x, classic, type = "l", lty = 1, pch = 19, col="black", lwd=1.5);
text(year.x[seq(from=1, to=length(year.x), by=N*2)], 
     naive_strategy_cont[seq(from=1, to=length(naive_strategy_cont), by=N*2)],
     labels = paste(round(naive_strategy_cont[seq(from=1, to=length(naive_strategy_cont), by=N*2)], 3)), 
     pos=3, cex=1.4, font=2, col='blue');    
text(year.x[seq(from=1, to=length(year.x), by=N*2)], 
     extended_strategy_cont[seq(from=1, to=length(extended_strategy_cont), by=N*2)],
     labels = paste(round(extended_strategy_cont[seq(from=1, to=length(extended_strategy_cont), by=N*2)], 3)), 
     pos=1, cex=1.4, font=2, col='red'); 
text(year.x[1], classic[1],
     labels = paste(round(classic[1],3)), 
     pos=1, cex=1.4, font=2, col='black');

title(ylab = 'Optimal risky asset allocation', line=3.5, cex.lab=3);
title(xlab = 'Year', line=3, cex.lab=3);
# title(main =  expression(paste(bold("Dividends adjusted for inflation: Proportion of optimal wealth in risky stock"))),
#      line=2, cex.lab=0.1, cex.main=3, font.main=2, col.main= 'black');
title(main =  expression(paste(bold("Proportion of optimal wealth in risky stock"))),
      line=2, cex.lab=0.1, cex.main=3, font.main=2, col.main= 'black');
legend(1987, 1.44,
       legend=c(expression("Classical Merton","Dynamic Merton","Optimal Nonmyopic")),
       col=c('black','blue','red'),
       lty=c(1,2,4), 
       lwd = 3,
       cex=2, 
       seg.len=1, 
       horiz=FALSE,
       text.font=3,
       bty = "n",
       y.intersp=0.8);
dev.off()

# Figure 4: Historical performance

if(realreturn==TRUE) returns_last30<- variable[,1][as.numeric(row.names(ModelData[ModelData$Year==(2018-T+1),])):length(ModelData$Year)];

Y_Original_Merton <- Y_Extended_Dynamic_Merton <- Y_Naive_Dynamic_Merton <- vector(mode='numeric', length = T+1)
Y_Original_Merton[1] <- Y_Extended_Dynamic_Merton[1] <- Y_Naive_Dynamic_Merton[1] <- 10000;

b1_case2_y  <- b2_case2_y <- vector(mode="numeric", length = T);
for (n in (1:T)) 
{
  b1_case2_y[n] <- b1_case2(n);
  b2_case2_y[n] <- b2_case2(n);
}
b_theta <- vector(mode="numeric", length = T);
theta_last30 <- (epsilon_last30 + 1/2*sigma^2)/sigma
for (n in (1:T)) 
{
  b_theta[n] <- b1_case2_y[n]+2*theta_last30[n]*b2_case2_y[n]
}

# theta_before <- (epsilon + 1/2*sigma^2)/sigma

# sigma_fix <- sigma

sigma_fix <- sd(returns); # 0.1719975
mu_fix <- mean(returns) # 0.06255975
# mu_fix <- mean(earnings)

Original_Merton_Theta <- mu_fix/sigma_fix
Original_Merton_Strategy <- Original_Merton_Theta*(1/((1-gamma)*sigma_fix))

for (n in (1:30))
{
  Y_Original_Merton[n+1] <- Y_Original_Merton[n]*(1+Original_Merton_Strategy*returns_last30[n])
  Y_Naive_Dynamic_Merton[n+1] <- Y_Naive_Dynamic_Merton[n]*(1+theta_last30[n]*(1/((1-gamma)*sigma))*returns_last30[n])
  Y_Extended_Dynamic_Merton[n+1] <- Y_Extended_Dynamic_Merton[n]*(1+((1/((1-gamma)*sigma)))*(theta_last30[n]+rho*tau_theta*b_theta[n])*returns_last30[n])
}

# total variation
sum(abs(diff(Y_Original_Merton)))
sum(abs(diff(Y_Naive_Dynamic_Merton)))
sum(abs(diff(Y_Extended_Dynamic_Merton)))
# quadratic variation
sum(diff(Y_Original_Merton)^2)
sum(diff(Y_Naive_Dynamic_Merton)^2)
sum(diff(Y_Extended_Dynamic_Merton)^2)

pdf('Figure4.pdf', paper = "a4r",width=10.8, height=15)
par(mar=c(4.3, 6, 4.5, 0));

year.x <- seq(from=2018-T, to=2018, by=1);
plot(year.x, Y_Original_Merton, type='l', xlab = '', lty=1, ylab='', lwd=1.5, cex.axis=2.5, 
     ylim = c(min(Y_Original_Merton, Y_Naive_Dynamic_Merton, Y_Extended_Dynamic_Merton), 
              max(Y_Original_Merton, Y_Naive_Dynamic_Merton, Y_Extended_Dynamic_Merton)));
lines(year.x, Y_Naive_Dynamic_Merton, type='l', lty=2, col='blue', lwd=1.5);
lines(year.x, Y_Extended_Dynamic_Merton, type='l', lty=4, col='red', lwd=1.5);

# text(year.x[seq(from=1, to=length(year.x), by=3)], 
#      Y_Original_Merton[seq(from=1, to=length(Y_Original_Merton), by=3)],
#      labels = paste(round(Y_Original_Merton[seq(from=1, to=length(Y_Original_Merton), by=3)], 0)), 
#      pos=3, cex=1.4, font=2, col='black');  
# 
# text(year.x[seq(from=1, to=length(year.x), by=3)], 
#      Y_Naive_Dynamic_Merton[seq(from=1, to=length(Y_Naive_Dynamic_Merton), by=3)],
#      labels = paste(round(Y_Naive_Dynamic_Merton[seq(from=1, to=length(Y_Naive_Dynamic_Merton), by=3)], 0)), 
#      pos=3, cex=1.4, font=2, col='blue');  
# 
 text(year.x[seq(from=1, to=length(year.x), by=3)], 
      Y_Extended_Dynamic_Merton[seq(from=1, to=length(Y_Extended_Dynamic_Merton), by=3)],
      labels = paste(round(Y_Extended_Dynamic_Merton[seq(from=1, to=length(Y_Extended_Dynamic_Merton), by=3)], 0)), 
      pos=3, cex=1.4, font=2, col='red');  

title(ylab = 'Fund size in real terms in USD', line=3.5, cex.lab=3);
title(xlab = 'Year', line=3.5, cex.lab=3)
title(main = expression(paste(bold("Historical performance"))),
      line=2, cex.lab=0.1, cex.main=3, font.main=2,
      col.main= 'black')
legend("topleft",
       legend=c(expression("Classical Merton",
                           "Dynamic Merton",
                           "Optimal Nonmyopic")),
       col=c('black', 'navy', 'red'),
       lty=c(1,2,4), 
       lwd = 3,
       cex=2, 
       seg.len=1, 
       horiz=FALSE,
       text.font=3,
       bty = "n",
       y.intersp=0.8);

dev.off()

# Figure 5: Sensitivity Analysis

# Variables necessary
kappa
sigma
r <- 0
tau_theta # 0.8882635
mu_theta # 0.3316346
#gamma <- -1
rho
rho_high <- 0.9;
rho_neg_high <- -0.9;

tau_theta_50 <- 2;

# Mean-revering model
xi <- kappa/tau_theta; # 2.430895
xi_extrem <- kappa/tau_theta_50; #  0.5560412

eta <- gamma/(1-gamma); # -0.5

Rp <- sqrt(xi^2-(1+2*rho*xi)*eta); # sqrt(6.88838) = 1.844972
Rp_high_rho <- sqrt(xi^2-(1+2*rho_high*xi)*eta); # 1.998959
Rp_neg_high_rho <- sqrt(xi^2-(1+2*rho_neg_high*xi)*eta); # 1.53606
Rp_extrem <- sqrt(xi_extrem^2-(1+2*rho_neg_high*xi_extrem)*eta); # sqrt(0.6647975) = 0.8153512

# Case II: xi^2 - eta*(1+2*rho*xi) => 0

psi1_case2 <- ((xi-eta*rho)+Rp)/(2*tau_theta*(1+eta*rho^2)); # 4.007884
psi1_case2_high_rho <- ((xi-eta*rho_high)+Rp_high_rho)/(2*tau_theta*(1+eta*rho_high^2)); # 4.706366
psi1_case2_neg_high_rho <- ((xi-eta*rho_neg_high)+Rp_neg_high_rho)/(2*tau_theta*(1+eta*rho_neg_high^2)); # 3.854084
psi1_case2_extrem <- ((xi_extrem-eta*rho_neg_high)+Rp_extrem)/(2*tau_theta_50*(1+eta*rho_neg_high^2)); #  0.2062092

psi2_case2 <- ((xi-eta*rho)-Rp)/(2*tau_theta*(1+eta*rho^2)); # -0.07365435
psi2_case2_high_rho <- ((xi-eta*rho_high)-Rp_high_rho)/(2*tau_theta*(1+eta*rho_high^2)); # -0.06999913
psi2_case2_neg_high_rho <- ((xi-eta*rho_neg_high)-Rp_neg_high_rho)/(2*tau_theta*(1+eta*rho_neg_high^2)); # -0.08547855
psi2_case2_extrem <- ((xi_extrem-eta*rho_neg_high)-Rp_extrem)/(2*tau_theta_50*(1+eta*rho_neg_high^2)); # -0.08424263

# b1 and b2 as functions of t

N <- 1;
T <- 30;
b1_case2_y <- b1_case2_y_high_rho <- b1_case2_y_neg_high_rho <- b1_case2_y_extrem <- b2_case2_y <- b2_case2_y_high_rho <- b2_case2_y_neg_high_rho <- b2_case2_y_extrem <- vector(mode="numeric", length = T*N);

b1_case2 <- function(t) {(kappa*mu_theta*eta)/(2*tau_theta^2*Rp*(1+eta*rho^2))*(exp(Rp*tau_theta*(T-t))-2+exp(-Rp*tau_theta*(T-t)))/(psi1_case2*exp(Rp*tau_theta*(T-t))-psi2_case2*exp(-Rp*tau_theta*(T-t)))};
b1_case2_high_rho <- function(t) {(kappa*mu_theta*eta)/(2*tau_theta^2*Rp_high_rho*(1+eta*rho_high^2))*(exp(Rp_high_rho*tau_theta*(T-t))-2+exp(-Rp_high_rho*tau_theta*(T-t)))/(psi1_case2_high_rho*exp(Rp_high_rho*tau_theta*(T-t))-psi2_case2_high_rho*exp(-Rp_high_rho*tau_theta*(T-t)))};
b1_case2_neg_high_rho <- function(t) {(kappa*mu_theta*eta)/(2*tau_theta^2*Rp_neg_high_rho*(1+eta*rho_neg_high^2))*(exp(Rp_neg_high_rho*tau_theta*(T-t))-2+exp(-Rp_neg_high_rho*tau_theta*(T-t)))/(psi1_case2_neg_high_rho*exp(Rp_neg_high_rho*tau_theta*(T-t))-psi2_case2_neg_high_rho*exp(-Rp_neg_high_rho*tau_theta*(T-t)))};
b1_case2_extrem <- function(t) {(kappa*mu_theta*eta)/(2*tau_theta_50^3*Rp_extrem*(1+eta*rho_neg_high^2))*(exp(Rp_extrem*tau_theta_50*(T-t))-2+exp(-Rp_extrem*tau_theta_50*(T-t)))/(psi1_case2_extrem*exp(Rp_extrem*tau_theta_50*(T-t))-psi2_case2_extrem*exp(-Rp_extrem*tau_theta_50*(T-t)))};

b2_case2 <- function(t) {(eta)/(4*tau_theta^2*(1+eta*rho^2))*(exp(Rp*tau_theta*(T-t))-exp(-Rp*tau_theta*(T-t)))/(psi1_case2*exp(Rp*tau_theta*(T-t))-psi2_case2*exp(-Rp*tau_theta*(T-t)))};
b2_case2_high_rho <- function(t) {(eta)/(4*tau_theta^2*(1+eta*rho_high^2))*(exp(Rp_high_rho*tau_theta*(T-t))-exp(-Rp_high_rho*tau_theta*(T-t)))/(psi1_case2_high_rho*exp(Rp_high_rho*tau_theta*(T-t))-psi2_case2_high_rho*exp(-Rp_high_rho*tau_theta*(T-t)))};
b2_case2_neg_high_rho <- function(t) {(eta)/(4*tau_theta^2*(1+eta*rho_neg_high^2))*(exp(Rp_neg_high_rho*tau_theta*(T-t))-exp(-Rp_neg_high_rho*tau_theta*(T-t)))/(psi1_case2_neg_high_rho*exp(Rp_neg_high_rho*tau_theta*(T-t))-psi2_case2_neg_high_rho*exp(-Rp_neg_high_rho*tau_theta*(T-t)))};
b2_case2_extrem <- function(t) {(eta)/(4*tau_theta_50^3*(1+eta*rho_neg_high^2))*(exp(Rp_extrem*tau_theta_50*(T-t))-exp(-Rp_extrem*tau_theta_50*(T-t)))/(psi1_case2_extrem*exp(Rp_extrem*tau_theta_50*(T-t))-psi2_case2_extrem*exp(-Rp_extrem*tau_theta_50*(T-t)))};

for (t in (1:(T))) 
{
  b1_case2_y[t] <- b1_case2(t/N);
  b1_case2_y_high_rho[t] <- b1_case2_high_rho(t/N);
  b1_case2_y_neg_high_rho[t] <- b1_case2_neg_high_rho(t/N);
  b1_case2_y_extrem[t] <- b1_case2_extrem(t/N);
  
  b2_case2_y[t] <- b2_case2(t/N);
  b2_case2_y_high_rho[t] <- b2_case2_high_rho(t/N);
  b2_case2_y_neg_high_rho[t] <- b2_case2_neg_high_rho(t/N);
  b2_case2_y_extrem[t] <- b2_case2_extrem(t/N);
}

theta_last
theta_last30

theta_last_stepwise <- stepfun(c(seq(from=2, to=30, by=1)), c(theta_last));
fun <- function(t) {theta_last_stepwise(t)}
N <- 12; # monthly 
fraction_01_cont <-fraction_02_cont <- fraction_03_cont <- fraction_04_cont <- fraction_e_01_cont <-fraction_e_02_cont <- fraction_e_03_cont <- fraction_e_04_cont <- vector(mode="numeric", length = T*N);

for (t in (1:(T*N))) 
{
  fraction_01_cont[t] <- (fun(t/N)+rho*tau_theta*(b1_case2(t/N)+2*b2_case2(t/N)*fun(t/N)))/fun(t/N);
  #fraction_01_cont[t] <- fun(t/N)/fun(t/N);
  fraction_02_cont[t] <- (fun(t/N)+rho_high*tau_theta*(b1_case2_high_rho(t/N)+2*b2_case2_high_rho(t/N)*fun(t/N)))/fun(t/N);
  fraction_03_cont[t] <- (fun(t/N)+rho_neg_high*tau_theta*(b1_case2_neg_high_rho(t/N)+2*b2_case2_neg_high_rho(t/N)*fun(t/N)))/fun(t/N);
  fraction_04_cont[t] <- (fun(t/N)+rho_neg_high*tau_theta_50*(b1_case2_neg_high_rho(t/N)+2*b2_case2_neg_high_rho(t/N)*fun(t/N)))/fun(t/N);
}

pdf('Figure5.pdf',width=16.5, height=11.8)
par(mar=c(7.3, 7, 4.5, 5));

year.x <- seq(from=(2018-30)+1/N, to=2018, by=1/N);
plot(year.x, fraction_01_cont, type='l', lty=1, col='red', xlab = '', ylab='', lwd = 3,
     ylim = c(min(fraction_01_cont, fraction_02_cont, fraction_03_cont, fraction_04_cont), 
              max(fraction_01_cont, fraction_02_cont, fraction_03_cont, fraction_04_cont)),
     cex.axis=2.5); 
lines(year.x, fraction_02_cont, type='l', lty=2, lwd =3, col='blue');
lines(year.x, fraction_03_cont, type='l', lty=3, lwd =3, col='black');
lines(year.x, fraction_04_cont, type='l', lty=4, lwd =3, col='tan4');
title(ylab = 'Ratio between proportions of wealth invested', line=3.5, cex.lab=3);
title(xlab = 'Year', line=3.5, cex.lab=3);
title(main = expression(paste(bold("Sensitivity analysis: Dynamic Merton vs. Optimal Nonmyopic"))),
      line=2, cex.lab=0.1, cex.main=3, font.main=2, 
      col.main= 'black');
legend('topright',
       legend=c(expression(rho==-0.03, rho==0.9, rho==-0.9, rho==-0.9~","~tau[theta]==2)),
       col=c('red', 'blue', 'black', 'tan4'), 
       lty=1:4, 
       lwd = 3,
       cex=2.5, 
       seg.len=1.5, 
       horiz=FALSE,
       text.font = 3,
       bty = "n",
       y.intersp=0.8)

dev.off()

return(list(Y_Original_Merton=Y_Original_Merton,Y_Naive_Dynamic_Merton=Y_Naive_Dynamic_Merton,Y_Extended_Dynamic_Merton=Y_Extended_Dynamic_Merton, theta_last30=theta_last30, sigma=sigma, rho=rho,tau_theta=tau_theta,b_theta=b_theta,kappa=kappa, sigma_fix=sigma_fix,Original_Merton_Theta=Original_Merton_Theta,returns_last30=returns_last30))
}


models<-c('epsilon.e.di','epsilon.d.di','epsilon.d.i','epsilon.e.s','epsilon.d.s')

myfunction(data,variable,epsilon.d.s,-2,realreturn=TRUE)

mytable<-data.frame(strategy=numeric(),avg.exposure=numeric(), sample.sd=numeric(), mean.reurn=numeric(), ratio=numeric(), sharpe.ratio=numeric())
for(i in 1:length(models)){
  
  res<-myfunction(data,variable,get(models[i]),gamma, realreturn=TRUE)
  mytable[((4*i)-3),] <- c(models[i],'','','','','')
  
  mytable[((4*i)-3)+1,] <- c('Classical Merton',
                             res$Original_Merton_Theta*(1/((1-gamma)*res$sigma_fix)),
                          sd(res$Original_Merton_Theta*(1/((1-gamma)*res$sigma_fix))*res$returns_last30),
                             (res$Y_Original_Merton[31]/10000)^(1/30)-1,
                           ((res$Y_Original_Merton[31]/10000)^(1/30)-1)/(res$Original_Merton_Theta*(1/((1-gamma)*res$sigma_fix))),
                           ((res$Y_Original_Merton[31]/10000)^(1/30)-1)/sd(res$Original_Merton_Theta*(1/((1-gamma)*res$sigma_fix))*res$returns_last30)
                           )
  mytable[((4*i)-3)+2,] <-c('Dynamic Merton',
                            mean(res$theta_last30*(1/((1-gamma)*res$sigma))),
                             sd((res$theta_last30*(1/((1-gamma)*res$sigma)))*res$returns_last30),
                             (res$Y_Naive_Dynamic_Merton[31]/10000)^(1/30)-1,
                            ((res$Y_Naive_Dynamic_Merton[31]/10000)^(1/30)-1)/(mean(res$theta_last30*(1/((1-gamma)*res$sigma)))),
                            ((res$Y_Naive_Dynamic_Merton[31]/10000)^(1/30)-1)/sd((res$theta_last30*(1/((1-gamma)*res$sigma)))*res$returns_last30)
                            
                            )
  mytable[((4*i)-3)+3,] <-c('Optimal Nonmyopic', 
                            mean(((1/((1-gamma)*res$sigma)))*(res$theta_last30+res$rho*res$tau_theta*res$b_theta)),
                             sd((((1/((1-gamma)*res$sigma)))*(res$theta_last30+res$rho*res$tau_theta*res$b_theta))*res$returns_last30),
                            (res$Y_Extended_Dynamic_Merton[31]/10000)^(1/30)-1,
                            ((res$Y_Extended_Dynamic_Merton[31]/10000)^(1/30)-1)/
                              (mean(((1/((1-gamma)*res$sigma)))*(res$theta_last30+res$rho*res$tau_theta*res$b_theta))),
                            ((res$Y_Extended_Dynamic_Merton[31]/10000)^(1/30)-1)/
                             sd((((1/((1-gamma)*res$sigma)))*(res$theta_last30+res$rho*res$tau_theta*res$b_theta))*res$returns_last30)
                            )
}

mytable[,2]<-as.numeric(mytable[,2])
mytable[,3]<-as.numeric(mytable[,3])
mytable[,4]<-as.numeric(mytable[,4])
mytable[,5]<-as.numeric(mytable[,5])
mytable[,6]<-as.numeric(mytable[,6])

mytable[,2:6] <- mytable[,2:6]*100
print(xtable(mytable, digits=c(1,2,2,2,2,2,2)), include.rownames=FALSE)

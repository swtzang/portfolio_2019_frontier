# Reference: 
# https://systematicinvestor.wordpress.com/category/factor-model/
# 3-asset example
# 
rm(list=ls())
setwd("D:/亞洲大學上課資料/Portfolio management 2017 Fall")
#
#=======================================
# Step 1: Import data from excel
#=======================================
# If data has percentage format, then you'd better convert it into numeric format in excel
# file = 3firmExample_data.csv
# First you have to convert factor data into numeric
#===========================================================================================================     
# http://stackoverflow.com/questions/28977777/r-converting-data-frame-of-percentages-from-factor-to-numeric
#==========================================================================================================
# Delete % from data and convert into numeric
#==========================================================================================================
# After conversion, we save the file as 3firmExample_data3.csv
setwd("~/portfolio_2017_Fall")
firm_data1 = read.csv('3firmExample_data3.csv')
str(firm_data1)
firm_data1$date

library(xts)
library(PerformanceAnalytics)
date1 = as.Date(firm_data1[,1], "%Y/%m/%d")
#convert firm_data1 into time series data: xts
firm_data1.xts = as.xts(firm_data1[,-1], order.by = date1)
firm.data1<-coredata(firm_data1.xts)
summary(firm.data1)
skewness(firm.data1)
rbind(apply(firm.data1, 2, summary),
      apply(firm.data1, 2, skewness),
      apply(firm.data1, 2, kurtosis))

#====================================================================================
# IF you know the ticker of stocks, then you can 
# download directly from yahoo finance
#=====================================================================================
#library(quantmod)
#tickers<-c("JWN", "SBUX", "MSFT")
#getSymbols(tickers, from = '2010-12-31', to = '2016-12-31', auto.assign = TRUE)


#=================================
# Minimum variance portfolio
#=================================
library(fBasics)
Sigma = cov(firm_data1[,2:4])
std = sqrt(diag(Sigma))
ones = rep(1,3)     
one.vec = matrix(ones, ncol=1)
a = inv(Sigma)%*%one.vec
b = t(one.vec)%*%a
mvp.w =a / as.numeric(b)
mvp.w
mvp.ret<-sum((mvp.w)*colMeans(firm_data1[,2:4]))
mvp.ret
#==================================
# Assume return is specified as 0.06.  
# Try to find its optimal weight and standard deviation (tangency portfolio), 
# expected return and Sharpe ratio.
#=================================
mu<-0.06/12
return <- firm_data1[,2:4]
Ax <- rbind(2*cov(return), colMeans(return), rep(1, ncol(return)))
Ax <- cbind(Ax, rbind(t(tail(Ax, 2)), matrix(0, 2, 2)))
b0 <- c(rep(0, ncol(return)), mu, 1)
out<-solve(Ax, b0)
wgt<-out[1:3]
wgt
sum(wgt)
ret.out<-sum(wgt*colMeans(return))
ret.out.annual<-ret.out*12
ret.out.annual
std.out<-sqrt(t(wgt)%*%cov(return)%*%wgt)
std.out.annual<-std.out*sqrt(12)
std.out.annual
#====================================================================================================
# Or write your own function to find min var for a  specified return mu;
# Reference: Introduction to R for Quantitative Finance: Chapter 2, p31
# Solve a diverse range of problems with R, one of the most powerful tools for quantitative finance
#====================================================================================================
return = firm_data1[,2:4]
#specified portfolio return: mu
mu=0.06/12

minvariance <- function(return, mu) {
  #return <- log(tail(assets, -1) / head(assets, -1))
  Ax <- rbind(2*cov(return), colMeans(return), rep(1, ncol(return)))
  Ax <- cbind(Ax, rbind(t(tail(Ax, 2)), matrix(0, 2, 2)))
  b0 <- c(rep(0, ncol(return)), mu, 1)
  zx<-solve(Ax, b0)
  weight<-zx[1:ncol(return)]
  ret.out<-sum(weight*colMeans(return))
  std.out<-sqrt(t(wgt)%*%cov(return)%*%wgt)
  list(weight=weight, rtn=ret.out, sd=std.out)
}

minvariance(return, mu)
#======================================================
# Create frontier function to plot efficient frontier
#======================================================

frontier <- function(return){
  #return <- log(tail(assets, -1) / head(assets, -1))
  n = ncol(return)
  Q = cov(return)
  Ax <- rbind(2*cov(return), colMeans(return), rep(1, n))
  Ax <- cbind(Ax, rbind(t(tail(Ax, 2)), matrix(0, 2, 2)))
  r <- colMeans(return)
  rbase <- seq(min(r), max(r), length = 100)
  s <- sapply(rbase, function(x) {
    b0 <- c(rep(0, ncol(return)), x, 1)
    y <- head(solve(Ax, b0), n)
    sqrt(y%*%Q%*%y)
  })
  plot(s, rbase, xlab = 'Std', ylab = 'Return')
}

frontier(return)

#===============================================
# Use package fPortfolio to plot frontier
#===============================================
library(timeSeries)
library(PerformanceAnalytics)
#install.packages("rugarch", dependencies=TRUE)
#install.packages("PerformanceAnalytics", dependencies=TRUE)
#install.packages("fAssets", dependencies=TRUE)
#install.packages("fPortfolio",dependencies=TRUE)

library(fPortfolio)
return = firm_data1[,2:4]
# convert data to timeseries
ret.ts<- timeSeries(return, date1)
chart.CumReturns(ret.ts, legend.loc = 'topleft', main = '')


plot(portfolioFrontier(ret.ts))

1#To mimic what we have implemented in the preceding code, let us render the
#frontier plot of short sale constraints
Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
setTargetReturn(Spec) = mean(colMeans(ret.ts))## or set your own target return
Spec
#DIFFERENT COVARIANCE ESTIMATORS
#1. MCd ESTIMATOR 
setEstimator(Spec)="covMcdEstimator"
#2. OGK estimator
setEstimator(Spec)= "covOGKEstimator"
#3.  Shrinkage estimator
setEstimator(Spec)= "shrinkEstimator"
# Display structure of constraints;
Spec
Constraints="Short"
efficientPortfolio(ret.ts, Spec, Constraints)
tangencyPortfolio(ret.ts, Spec, Constraints)
minvariancePortfolio(ret.ts, Spec, Constraints)
#
portfolioConstraints(ret.ts, Spec, constraints = "LongOnly")
#setSolver(Spec) = "solveRquadprog"
Frontier <- portfolioFrontier(as.timeSeries(ret.ts), Spec, constraints = "Short") #vs "LongOnly" "Short"
frontierPlot(Frontier, col = c("orange", "red"), pch = 19)
#sharpeRatioLines(Frontier, col = "orange", lwd = 2) #### shows the Sharpe Ratio line

monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
grid()
#
weightsPlot(Frontier)

#EQUAL WEIGTH PORTFOLIO
Data<-ret.ts
nAssets = getNAssets(portfolioData(Data))
Weights <- rep(1/nAssets, times = nAssets)
covRisk(Data, Weights)
varRisk(Data, Weights, alpha = 0.05)###VaR of equal weights portfolio
cvarRisk(Data, Weights, alpha = 0.05)### CVaR for equal weight portfolio

#===========================================================
# Tangency portfolio with risk free rate = 0.01
#===========================================================
# But the answer seems confusing because it remains the same 
# when we change risk free rate to other values!
# ==========================================================
setRiskFreeRate(Spec)<-0.01/12
tgPortfolio <- tangencyPortfolio(ret.ts, Spec, constraints = "Short")
tgPortfolio

#=============================================================
# Tangency portfolio by closed form
#==============================================================
rf = 0.01/12
mr = colMeans(firm_data1[,2:4])
mr.mtx = matrix(mr, ncol=1)
mr_rf = mr - rf
mr_rf = matrix(mr_rf, ncol=1)
mr_rf
a1 = inv(Sigma)%*%mr_rf
b1 = t(one.vec)%*%a1
tp = a1 / as.numeric(b1)
tp
#portfolio expected return
ret.tp  = sum(mr.mtx*tp)
ret.tp
#portfolio standard deviation
std.tp = sqrt((t(tp)%*%Sigma)%*%tp)
std.tp
#sharpe ratio
sharpe.tp = (ret.tp - rf)/std.tp
sharpe.tp

#=============================================
# Global min var portfolio
#=============================================
globminSpec <- portfolioSpec()
# Answer will not change whether Constraints is "Short" or "LongOnly"
globminPortfolio <- minvariancePortfolio(as.timeSeries(ret.ts), spec = globminSpec, constraints = "LongOnly")
print(globminPortfolio)

col = rampPalette(ncol(ret.ts), "purple2green")
weights <- 100 * as.vector(getWeights(globminPortfolio))
names <- as.vector(names(ret.ts))
barplot(height = weights, names.arg = names,
          horiz = TRUE, las = 1, col = col)
title(main = "Weights of Global Min Variance Portfolio",
        xlab = "Weights %")

#=================================
# quadratic programming
#=================================
library(quadprog)

mu = apply(firm_data1[,2:4], 2, mean)
Amat = cbind(rep(1,3),mu)  # set the constraints matrix

muP = seq(.01,.08,length=300)  # set of 300 possible given returns 
# for the expect portfolio return
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=3) # storage for portfolio weights

i=1
for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i])  # constraint vector
  result = solve.QP(Dmat=2*Sigma,dvec=rep(0,3),Amat=Amat,bvec=bvec,meq=2)
    sdP[i] = sqrt(result$value)
    weights[i,] = result$solution
}

#==================================================
# Here is the graph output
#===================================================
#postscript("3firmExample.ps",width=6,height=5)  
pdf("3firmExample.pdf",width=6,height=5)  
plot(sdP,muP,type="l",xlim=c(0,0.25),ylim=c(0,0.08),lty=1)  #  plot 
# the efficient frontier (and inefficient frontier)
mufree = 0.01/12 # input value of risk-free interest rate
points(0,mufree,cex=3, pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=2,lty=3)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=1,pch=19, col="red") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+", col="blue") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.08),lwd=1)  #  plot the efficient frontier
points(c(std[1],std[2], std[3]), c(mu[1], mu[2], mu[3]), cex=1, pch="o", col="red") 
text(std[1],mu[1],"Nordstrom",cex=1, pos=4)
text(std[2],mu[2],"Starbucks",cex=1, pos=4)
text(std[3],mu[3],"Microsoft",cex=1, pos=4)
graphics.off()

#===================================
# no short sales constraint
#===================================
Amat1 = cbind(rep(1,3),mu, diag(1,nrow=3))  # set the constraints matrix
t(Amat1)
# muP = seq(.01,.08,length=300)  # set of 300 possible target values 
# When short sales are prohibited, the target expected return on the portfolio must lie between the smallest 
# and largest expected returns on the stocks. 
muP1 = seq(min(mu)+.0001,max(mu)-.0001,length=300)

# for the expect portfolio return
sdP1 = muP1 # set up storage for standard deviations of portfolio returns
weights1 = matrix(0,nrow=300,ncol=3) # storage for portfolio weights

i=1
for (i in 1:length(muP1))  # find the optimal portfolios for each target expected return
{
  bvec1 = c(1,muP1[i], rep(0,3))  # constraint vector
  result = solve.QP(Dmat=2*Sigma,dvec=rep(0,3),Amat=Amat1,bvec=bvec1,meq=2)
  sdP1[i] = sqrt(result$value)
  weights1[i,] = result$solution
}

postscript("3firmExample_noshort.ps",width=6,height=5)  
plot(sdP1,muP1,type="l",xlim=c(0,0.25),ylim=c(0,0.08),lty=1)  #  plot 
#the efficient frontier (and inefficient frontier)
par(new=TRUE)
plot(sdP,muP,type="l",xlim=c(0,0.25),ylim=c(0,0.08),lty=1, col="green")  #  plot 
mufree = 0.005 # input value of risk-free interest rate
points(0,mufree,cex=3,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=2,lty=3)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=1,pch=19, col="red") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+", col="blue") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.08),lwd=1)  #  plot the efficient frontier
points(c(std[1],std[2], std[3]), c(mu[1], mu[2], mu[3]), cex=1, pch="o", col="red") 
text(std[1],mu[1],"Nordstrom",cex=1, pos=4)
text(std[2],mu[2],"Starbucks",cex=1, pos=4)
text(std[3],mu[3],"Microsoft",cex=1, pos=4)
graphics.off()

#======================================================
# Use systematic investors toolbox (SIT)
# Load Systematic Investor Toolbox (SIT)
# http://www.r-bloggers.com/backtesting-minimum-variance-portfolios/
#=========================================================
#setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)
#*****************************************************************
# Create Constraints
#*****************************************************************
n <- dim(firm_data1.xts)[2]
constraints = new.constraints(n, lb = -Inf, ub = +Inf)
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)  
ia <- create.historical.ia(firm_data1.xts, 12)
#s0 <- apply(coredata(firm_data1.xts),2,sd)     
#ia$cov <- cor(coredata(firm_data1.xts), use='complete.obs',method='pearson') * (s0 %*% t(s0))
weight <- min.risk.portfolio(ia, constraints)
#===================================================
# Try to plot efficient frontier using SIT
# create sample historical input assumptions
ia <- create.historical.ia(firm_data1.xts, 12) # 12 is annual factor for monthly return
# create long-only, fully invested efficient frontier
# 0 <= x.i <= 1
# If short sale allowed: constraints = new.constraints(n, lb = -Inf, ub = +Inf)
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(diag(n), type='>=', b=0, constraints)
constraints = add.constraints(diag(n), type='<=', b=1, constraints)
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)

# create efficient frontier
ef = portopt(ia, constraints, 50, 'Efficient Frontier') 


#====================================================
# David Ruppert's example in his textbook
#===================================================
install.packages("Ecdat",dependencies=TRUE,repos="http://ftp.yzu.edu.tw/CRAN/")
install.packages("Ecfun", dependencies = TRUE, repos="http://ftp.yzu.edu.tw/CRAN/")
library(Ecfun)
library(Ecdat)
library(quadprog)

data(CRSPday)
#daily observations from 1969-1-03 to 1998-12-31
#number of observations : 2528
#ge the return for General Electric, Permno 12060
#ibm the return for IBM, Permno 12490
#mobil the return for Mobil Corporation, Permno 15966
#crsp the return for the CRSP value-weighted index, including dividends

class(CRSPday)
CRSP.df = as.data.frame(CRSPday)

R = 100*CRSP.df[,4:6]
mean_vect = apply(R,2,mean)
cov_mat = cov(R)
sd_vect = sqrt(diag(cov_mat))

Amat = cbind(rep(1,3),mean_vect) 

muP = seq(.05,.14,length=300)  # set of 300 possible target values 
# for the expect portfolio return
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=3) # storage for portfolio weights

for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i])  # constraint vector
  result = 
    solve.QP(Dmat=2*cov_mat,dvec=rep(0,3),Amat=Amat,bvec=bvec,meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}

postscript("CRSP_3firm.ps",width=6,height=5)  #  Figure 11.3
plot(sdP,muP,type="l",xlim=c(0,2.5),ylim=c(0,.15),lty=3)  #  plot 
# the efficient frontier (and inefficient frontier)
mufree = 1.3/253 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
options(digits=3)
weights[ind,] # Find tangency portfolio# show line of optimal portfolios
lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=4,lty=2)
# show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*", col='blue') # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
      ylim=c(0,.3),lwd=2)  #  plot the efficient frontier
points(c(sd_vect[1],sd_vect[2], sd_vect[3]), c(mean_vect[1], mean_vect[2], mean_vect[3]), cex=1, pch="o", col="red") # show tangency portfolio
text(sd_vect[1],mean_vect[1],"GE",cex=1, pos=4)
text(sd_vect[2],mean_vect[2],"IBM",cex=1, pos=4)
text(sd_vect[3],mean_vect[3],"Mobil",cex=1, pos=4)
graphics.off()










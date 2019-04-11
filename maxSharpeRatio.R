

#======================================================
# Use systematic investors toolbox (SIT)
# Load Systematic Investor Toolbox (SIT)
# http://www.r-bloggers.com/backtesting-minimum-variance-portfolios/
# https://systematicinvestor.wordpress.com/2011/12/13/backtesting-minimum-variance-portfolios/
# https://systematicinvestor.wordpress.com/2013/03/22/maximum-sharpe-portfolio/
#=========================================================
#setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)
library(quantmod)
#
firm_data1 = read.csv('3firmExample_data3.csv')
str(firm_data1)
firm_data1$date

library(xts)
library(PerformanceAnalytics)
date1 = as.Date(firm_data1[,1], "%Y/%m/%d")
#convert firm_data1 into time series data: xts
firm_data1.xts = as.xts(firm_data1[,-1], order.by = date1)
#*****************************************************************
# Create Constraints
#*****************************************************************
n <- dim(firm_data1.xts)[2]
constraints = new.constraints(n, lb = -Inf, ub = +Inf)
# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)  
ia <- create.historical.ia(firm_data1.xts, 12)
names(ia)
#s0 <- apply(coredata(firm_data1.xts),2,sd)     
#ia$cov <- cor(coredata(firm_data1.xts), use='complete.obs',method='pearson') * (s0 %*% t(s0))
weight <- min.risk.portfolio(ia, constraints)
weight
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
#
weight <- min.risk.portfolio(ia, constraints)
# create efficient frontier
ifelse(!require(corpcor), install.packages("corpcor"), library(corpcor))
ifelse(!require(lpSolve), install.packages("lpSolve"), library(lpSolve))
ef = portopt(ia, constraints, 50, 'Efficient Frontier') 
ef
#------------------------------------------------------
#*****************************************************************
# Create Plot
#******************************************************************     
# plot efficient frontier
plot.ef(ia, list(ef), transition.map=F)  

# find maximum sharpe portfolio
max(portfolio.return(ef$weight,ia) /  portfolio.risk(ef$weight,ia))

# plot minimum variance portfolio
weight = min.var.portfolio(ia,constraints)  
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='red')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)

# plot maximum Sharpe or tangency portfolio
weight = max.sharpe.portfolio()(ia,constraints) 
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)

plota.legend('Minimum Variance,Maximum Sharpe','red,orange', x='topright') 

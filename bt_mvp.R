#======================================================
# Use systematic investors toolbox (SIT)
# Load Systematic Investor Toolbox (SIT)
# http://www.r-bloggers.com/backtesting-minimum-variance-portfolios/
# https://systematicinvestor.wordpress.com/2011/12/13/backtesting-minimum-variance-portfolios/
# https://systematicinvestor.wordpress.com/2013/03/22/maximum-sharpe-portfolio/
#=========================================================
#setInternet2(TRUE)
# Load Systematic Investor Toolbox (SIT)
rm(list=ls())
#setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod,quadprog,lpSolve')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')


data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

data.weekly <- new.env()
for(i in tickers) data.weekly[[i]] = to.weekly(data[[i]], indexAt='endof')

bt.prep(data, align='remove.na', dates='1990::2018')
bt.prep(data.weekly, align='remove.na', dates='1990::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   
n = ncol(prices)
n
# find week ends
week.ends = endpoints(prices, 'weeks')
week.ends = week.ends[week.ends > 0]     


# Equal Weight 1/N Benchmark
data$weight[] = NA
data$weight[week.ends,] = ntop(prices[week.ends,], n)       

capital = 100000
data$weight[] = (capital / prices) * data$weight
equal.weight = bt.run(data, type='share')


#*****************************************************************
# Create Constraints
#*****************************************************************
constraints = new.constraints(n, lb = -Inf, ub = +Inf)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        


ret = prices / mlag(prices) - 1
weight = coredata(prices)
weight[] = NA

for( i in week.ends[week.ends >= (63 + 1)] ) {
  # one quarter is 63 days
  hist = ret[ (i- 63 +1):i, ]
  
  # create historical input assumptions
  ia = create.historical.ia(hist, 252)
  s0 = apply(coredata(hist),2,sd)     
  ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
  
  weight[i,] = min.risk.portfolio(ia, constraints)
}

# Minimum Variance
data$weight[] = weight      
capital = 100000
data$weight[] = (capital / prices) * data$weight
min.var.daily = bt.run(data, type='share', capital=capital)

#
#*****************************************************************
# Code Strategies: Weekly
#******************************************************************     
retw = data.weekly$prices / mlag(data.weekly$prices) - 1
weightw = coredata(prices)
weightw[] = NA

i=1793
for( i in week.ends[week.ends >= (63 + 1)] ) {   
  # map
  j = which(index(ret[i,]) == index(retw))
  
  # one quarter = 13 weeks
  hist = retw[ (j- 13 +1):j, ]
  
  # create historical input assumptions
  ia = create.historical.ia(hist, 52)
  s0 = apply(coredata(hist),2,sd)     
  ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
  
  weightw[i,] = min.risk.portfolio(ia, constraints)
}   

data$weight[] = weightw     
capital = 100000
data$weight[] = (capital / prices) * data$weight
min.var.weekly = bt.run(data, type='share', capital=capital)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt.custom.report.part1(min.var.weekly, min.var.daily, equal.weight)

# plot Daily and Weekly transition maps
layout(1:2)
plotbt.transition.map(min.var.daily$weight)
legend('topright', legend = 'min.var.daily', bty = 'n')
plotbt.transition.map(min.var.weekly$weight)
legend('topright', legend = 'min.var.weekly', bty = 'n')



# https://github.com/fdupuis659/Quant-Finance-with-R/tree/master/Tutorial%20Video%20Scripts
# https://www.rpubs.com/hgjerning/517730
# https://soprasteriaanalytics.se/2018/03/10/portfolio-optimization-and-the-portfolioanalytics-package-in-r/
# portfolio optimizaton using PerformanceAnalytics package
#============================================================
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

tickers <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL", "SQ", "NVDA")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from='2016-01-03', periodicity = 'daily', auto.assign=FALSE)[,6])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type="box", min=.10, max=.40)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev")

optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)

chart.Weights(optPort)

ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                               risk_aversion = NULL)

chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.6)
#--------------------------------------------------------------------------------------

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1.01)
portf <- add.constraint(portf, type="transaction_cost", ptc = 0.001)
portf <- add.constraint(portf, type="box", min=.10, max=.40)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev", target=0.005)
#
# A special case of box constraints is long only where min=0 and max=1
# The default action is long only if min and max are not specified
# portf <- add.constraint(portfolio = portf, type="box")
# portf <- add.constraint(portfolio = portf, type="long_only")
#

rp <- random_portfolios(portf, 10000, "sample")
apply(rp, 1, sum)
#
opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                            portf,
                                            optimize_method = "random",
                                            rp = rp,
                                            rebalance_on = "months",
                                            training_period = 1,
                                            rolling_window = 10)

equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
colnames(benchmark) <- "Benchmark Portfolio"

sp500prices <- getSymbols.yahoo("SPY", from='2016-01-03', periodicity = 'daily', auto.assign=FALSE)[,4]
sp500Rets <- na.omit(ROC(sp500prices))
sp500Rets <- as.xts(sp500Rets)

chart.Weights(opt_rebal, main="Rebalanced Weights Over Time")

rebal_weights <-extractWeights(opt_rebal)
rebal_returns <- Return.portfolio(portfolioReturns, weights=rebal_weights)

rets_df <- cbind(rebal_returns, benchmark, sp500Rets)

charts.PerformanceSummary(rets_df, main="P/L Over Time")


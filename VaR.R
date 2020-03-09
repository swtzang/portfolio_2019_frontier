# Reference: NF Katzke
# Financial Econometrics Practical
# Practical 5: Portfolio Risk and Performance Analysis

library(rmsfuns)
load_pkg(c("tidyverse","tbl2xts","devtools","lubridate","PerformanceAnalytics","ggplot2"))

library(zoo)
library(PerformanceAnalytics)
library(tidyquant)

dailydata <- read_csv("https://raw.githubusercontent.com/Nicktz/ExDat/master/extdata/DailyTRIs.csv", 
                      col_types = cols(.default = "d", Date = "D"))
dailydata
write_rds(dailydata, path = "../portfolio_2019_frontier/dailydata.rds")
#
load_pkg("TTR")
#
dailydata <- dailydata %>% arrange(Date) %>%
  mutate_at(.vars = vars(-Date),.funs = funs(ROC(.,type = c("continuous","discrete")[2]))) %>%
  # Equivalent to: # mutate_at(.vars = vars(-Date), .funs = funs(./lag(.)-1) ) %>%
  # continuous equivalent to: # mutate_at(.vars = vars(-Date), .funs = funs((log(.)-log(lag(.)))))
  mutate_at(.vars = vars(-Date), funs(na.locf(., na.rm =F, maxgap =5)))
  # Pad NA's back max 5 days:
#
# Let's not waste our time - remove spaces in column names!
colnames(dailydata) <- gsub(" SJ","",colnames(dailydata))
#
library(timetk)

tablestats <- dailydata %>% tk_xts(silent = TRUE) %>%
              table.Stats(.,ci = 0.95, digits =3)

print(tablestats[,1:5])

#
load_pkg("tbl2xts")
#install:
load_pkg("DEoptimR")
load_pkg("robustbase")
rtnc <- data(managers)
Return.clean(managers[,1:6],
             method = c("none","boudt","geltner")[2],alpha =0.01)


# install.packages("rportfolios")
library(rportfolios)
# Let's select a random set of the available Tickers:
Tickers <- c("AGL","AMS","AGL","APN","ASR",
             "BGA","BIL","SBK","SAB","BTI","NPN","OML","WHL","NED","SBK")
dailydata.subset <-dailydata[, which(names(dailydata) %in% c("Date", Tickers))] %>%
            gather(Stocks,returns, -Date) 

dailydata.subset                   
                   
# Let's assume the portfolio rebalances each January and July.
# First, let's save the exact rebalance dates and 
# save the random weight and date information to be
# Below is a very nice way to save months and years: let's rebalance at month 1 and 7...                                                                                         
RebMonths <- c(1,7) # Make a parameter that can easily be changed later.


RandomWeights <- dailydata.subset %>%
                 mutate(Months = as.double(format(Date,format ="%m")),
                        YearMonths = as.double(format(Date,format ="%Y%m"))) %>%
                 filter(Months %in% RebMonths) %>% 
                 group_by(YearMonths, Months, Stocks) %>%
                 filter(Date ==last(Date)) %>%
                 ungroup()
glimpse(RandomWeights)

# Now let's create a column with the random weights assigned to each stock conforming to the following
# Let's also create a random weighting vector for our selected stocks, with the following parameters:
# They have to sum to 1, have minimum weights of zero (no short) and max: 0.09:
# Now to append the weight vector, let's use the random.bounded function from rportfolios.

RandomWeights_adj <-bind_cols(RandomWeights %>% arrange(Date),
                              RandomWeights %>% group_by(Date) %>%
                              do(Randweights = random.bounded(n = nrow(.),
                                                              x.t =1, # Full investment...
                                                              x.l = rep(0, nrow(.)), # Lower Bound
                                                              x.u = rep(0.09, nrow(.)),
                                                              max.iter =1000)) %>%
                              ungroup() %>% unnest() )

#
RandomWeights_adj <- RandomWeights_adj %>%
    group_by(Date) %>%
    mutate(EqualWeights =1/n()) %>% 
    ungroup() %>% 
    select(-Months, -YearMonths, -Date1)

load_pkg("PerformanceAnalytics")
# Now we use the Return.portfolio function from PerformanceAnalytics
# Note, as with most PA functions, the inputs are xts and wide...
# Also, let's assume you are investing R1000 at the start:
Fund_Size_at_Start <-1000
Rand_weights <- RandomWeights_adj %>% 
                select(Date, Stocks, Randweights) %>%
                spread(Stocks, Randweights) %>%
                tbl_xts

EW_weights <- RandomWeights_adj %>% 
              select(Date, Stocks, EqualWeights) %>%
              spread(Stocks, EqualWeights) %>%
              tbl_xts
#
df_Returns <- dailydata.subset %>% spread(Stocks, returns)
df_Returns[is.na(df_Returns)] <- 0
xts_df_Returns <- df_Returns %>% tbl_xts()

#
Rand_RetPort <- Return.portfolio(xts_df_Returns,
                               weights = Rand_weights,
                               verbose =TRUE, 
                               contribution =TRUE,
                               value = Fund_Size_at_Start)
names(Rand_RetPort)
# [1] "returns" "contribution" "BOP.Weight" "EOP.Weight" "BOP.Value" "EOP.Value"

EW_RetPort <- Return.portfolio(xts_df_Returns,
                             weights = EW_weights,
                             verbose = TRUE,
                             contribution = TRUE,
                             value = Fund_Size_at_Start)
# 
Rand_Contribution <- Rand_RetPort$"contribution" %>% 
                     xts_tbl() %>%
                     mutate(date = lag(date), date = coalesce(date, index))
                            
                            
                            ,
                            date = coalesce(date, 
#
Rand_BPWeight <Rand_RetPort$"BOP.Weight"
                                                        %>%xts_tbl() %>%mutate(date = lag(date),date = coalesce(date,
                                                                                                                Rand_BPValue <Rand_RetPort$"BOP.Value"
                                                                                                                %>%xts_tbl() %>%mutate(date = lag(date),date = coalesce(date, index
W_RetPort$"BOP.Weight" %>%xts_tbl() %>%mutate(date = lag(date),date = coalesce(date, index
                                                                                                                                  
x <- sample(c(1:5, NA, NA, NA))
coalesce(x, 0L)                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                       EW_RetPort$"BOP.Value" %>%xts_tbl() %>%mutate(date = lag(date),date = coalesce(date, index                                                                                                                                                                                                                                          
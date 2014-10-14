library(Quandl)  # use to get Fama/French factors
library(xts)

# use Quandl Kenneth French Fama/French factors
# http://www.quandl.com/KFRENCH/FACTORS_D
f <- Quandl("KFRENCH/FACTORS_D",type = "xts") / 100

f.cumul <- cumprod(1+f)[endpoints(f, "years")]

nyear = 10

plot.zoo(
  # annualized return
  ( f.cumul / stats::lag(f.cumul,nyear) ) ^ ( 1/nyear ) - 1
)

# let's calculate the Rmkt - Rf on rolling basis
RmktlessRf = (( f.cumul / stats::lag(f.cumul,nyear) ) ^ ( 1/nyear ) - 1)[,1]

# let's assume a 1% expense ratio
#   and a Beta of 1
# the rolling 5 year R^2 necessary would be
reqRsq <- 1 /
  (
    1 + 
      ( 
        0.01 / 
        ( 1 * ifelse(RmktlessRf<0, NA, RmktlessRf ) )
        #( 1 * apply(RmktlessRf, MARGIN=1, function(x){max( 0 , x )} ) )
      )
  )

#reqRsq <- as.xts(reqRsq, order.by = as.Date(names(reqRsq)) )
plot(
  reqRsq
  ,main = paste0(
    " R^2 Rolling "
    , nyear
    , " year to Justify \n 1% Exp Ratio and Beta of 1"
  )
)


# now let's do for the total
# make same assumptions as paper
# 1.1% expense ratio and 1 Beta
f.cumul <- cumprod(1+f)[endpoints(f, "months"),1]
f.cumul.ann <- tail(f.cumul,1) ^ (1/(NROW(f.cumul)/12)) - 1

1 /
  (
    1 + 
      ( 
        0.011 / 
          ( 1 * f.cumul.ann[,1] )
        #( 1 * apply(RmktlessRf, MARGIN=1, function(x){max( 0 , x )} ) )
      )
  )

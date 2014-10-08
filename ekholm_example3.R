# perform Ekholm (2012,2014) analysis on some popular mutual funds

# Ekholm, A.G., 2012
# Portfolio returns and manager activity:
#    How to decompose tracking error into security selection and market timing
# Journal of Empirical Finance, Volume 19, pp 349-358

# Ekholm, Anders G., July 21, 2014
# Components of Portfolio Variance:
#    R2, SelectionShare and TimingShare
# Available at SSRN: http://ssrn.com/abstract=2463649


# gone too long without an interactive
# let's use the newest from RStudio - dygraphs htmlwidget
#devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))
library(dygraphs)


library(Quandl)  # use to get Fama/French factors
library(pipeR)   # pipes are the future or R
library(rlist)   # rlist - like underscore/lodash for R lists
library(dplyr)   # super fast and really powerful
library(tidyr)   # next gen wide/long formatter package
library(latticeExtra) # old but still awesome
library(directlabels) # fantastic and works with ggplot & lattice
library(quantmod) # also will load xts

# use Quandl Kenneth French Fama/French factors
# http://www.quandl.com/KFRENCH/FACTORS_D
f <- Quandl("KFRENCH/FACTORS_D",type = "xts") / 100

# grab our function from post
# http://timelyportfolio.blogspot.com/2014/10/selectionshare-timingshare-masterfully.html
source( paste0(
  "https://gist.githubusercontent.com/timelyportfolio/e5728c8c7fb45dbdb6e0",
  "/raw/e124379f19225fcdee18f30cb848da6fc6cae764/ekholm.R"
))

# let's have a look at SMB and HML decomposition
c( "SMB", "HML" ) %>>%
  (~ lapply(
    .
    ,function(ticker) {
      f[,c( ticker, colnames(f)[1] )] %>>%
        na.omit %>>%
        rollapply (
          FUN= function(x){
            x %>>%
              jensen_ekholm %>>% 
              ( data.frame( summary(.[["linmod"]])$"r.squared" , .$ekholm ) )  %>>%
              xts(order.by=tail(index(x),1)) -> return_df
            colnames(return_df)[1] <- "R_sq"
            return(return_df)
          }
          , width = 500
          , by = 250
          , by.column=F
          , fill = NULL
        ) %>>%
        na.fill(0)
    }) -> ekFunds
  ) %>>%
  ( names(ekFunds) = . )

ekFunds %>>%
  (~ list.map(
    .
    ,f(fund,i) -> {
      ( 
        dygraph(
          fund[,c(1,4,5)]
          , main = paste0("Ekholm Decomposition of ", names(ekFunds)[i], " Factor")
        ) %>>%
        dyOptions( stackedGraph = TRUE ) %>>%
        print
      )
    }
  ) ) %>>%
  ( list.map(
    .
    , f(fund,i) -> {
        structure(
          data.frame(
            date = index(fund)
            , fund = names(ekFunds)[i]
            , fund
          )
        ) %>>%
        gather(measure,value,-date,-fund)
    }
  ) ) %>>%
  (
    do.call( rbind , . )
  ) -> ekT

ekT %>>%
  # just plot at R^2, SelectionShare, and TimingShare
  filter( measure %in% c("R_sq","SelectionShare","TimingShare") ) %>>%
  (
    xyplot(
      value ~ date | measure
      , groups = fund
      , data = .
      , type = "l"
      # using direct.label so not necessary
      , auto.key = list( space = "right" )
      # title our plot
      , main = paste(
        "Comparison of Ekholm Decomposition for Fama/French Factors"
        ,paste0("2 Year Rolling since ",format(.$date[1],format="%b %d, %Y"))
        ,sep="\n"
      )
      # layout one on top of the other
      , layout = c(1,length(unique(.$measure)))
    )
  ) %>>%
  # I like labels on plot rather than legend
  directlabels::direct.label( method = "last.qp" ) %>>%
  # pretty it up with the latticeExtra Economist theme
  asTheEconomist



# just for fun, let's do a simple moving avg strategy on the mkt.rf factor
f %>>%
  (r ~ 
    ifelse(
      cumprod( 1 + r[,1]) > stats::lag( runMean(cumprod(1+r[,1]),n=200), n = 250 )
      , r[,1]
      , 0
    ) %>>%
    structure(dimnames = list(NULL,"MovAvg")) %>>%
    (
      merge(
        .
        , r[,1]
      )
    )
  ) %>>%
  na.omit %>>%
  ( ~ plot.zoo ) %>>%
  ( ~ jE <- jensen_ekholm ) %>>%
  rollapply (
    FUN= function(x){
      x %>>%
        jensen_ekholm %>>% 
        ( data.frame( summary(.[["linmod"]])$"r.squared" , .$ekholm ) )  %>>%
        xts(order.by=tail(index(x),1)) -> return_df
      colnames(return_df)[1] <- "R_sq"
      return(return_df)
    }
    , width = 2500
    , by = 250
    #, by = 100
    , by.column=F
    , fill = NULL
  ) %>>%
  na.fill(0) %>>%
  ( ~
      dygraph(
        .[,c(1,4,5)]
        , main = paste0("Ekholm Decomposition of Moving Average Strategy")
      ) %>>%
      dyOptions( stackedGraph = TRUE ) %>>%
      print
  ) %>>%
  (
    data.frame(
      date = index(.)
      , .
    )
  ) %>>%
  gather( measure, value, -date ) %>>%
  # just plot at R^2, SelectionShare, and TimingShare
  filter( measure %in% c("R_sq","SelectionShare","TimingShare") ) %>>%
  (
    xyplot(
      value ~ date | measure
      , groups = measure
      , data = .
      , type = "l"
      # title our plot
      , main = paste(
        "Comparison of Ekholm Decomposition for Simple Moving Average (250) Strategy"
        ,paste0("10 Year Rolling since ",format(.$date[1],format="%b %d, %Y"))
        ,sep="\n"
      )
      # layout one on top of the other
      , layout = c(1,length(unique(.$measure)))
    )
  ) %>>%
  # pretty it up with the latticeExtra Economist theme
  asTheEconomist



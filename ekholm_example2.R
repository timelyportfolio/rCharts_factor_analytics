# perform Ekholm (2012,2014) analysis on some popular mutual funds

# Ekholm, A.G., 2012
# Portfolio returns and manager activity:
#    How to decompose tracking error into security selection and market timing
# Journal of Empirical Finance, Volume 19, pp 349-358

# Ekholm, Anders G., July 21, 2014
# Components of Portfolio Variance:
#    R2, SelectionShare and TimingShare
# Available at SSRN: http://ssrn.com/abstract=2463649


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
source(paste0(
  "https://gist.githubusercontent.com/timelyportfolio/e5728c8c7fb45dbdb6e0",
  "/raw/e124379f19225fcdee18f30cb848da6fc6cae764/ekholm.R"
))

c( "FCNTX", "AGTHX", "SEQUX", "FPACX" ) %>>%
  (~ lapply(
    .
    ,function(ticker) {
      ticker %>>%
        getSymbols( from = "1896-01-01", auto.assign = F ) %>>%
        (fund ~  
          structure(
            fund[,6] / stats::lag( fund[,6], 1 ) - 1
            ,dimnames = list(NULL,gsub(x = colnames(fund)[6], pattern  = "[\\.]Adjusted", replacement = ""))
          )
        ) %>>%
        merge( f ) %>>% #  Quandl("KFRENCH/FACTORS_D",type = "xts") / 100
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
          #, by = 100
          , by.column=F
          , fill = NULL
        ) %>>%
        na.fill(0)
    }) -> ekFunds
  ) %>>%
  ( names(ekFunds) = . )

ekFunds %>>%
  list.map(
    f(fund,i) -> {
      structure(
        data.frame(
          date = index(fund)
          , fund = names(ekFunds)[i]
          , fund
        )
      ) %>>%
      gather(measure,value,-date,-fund)
    }
  ) %>>%
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
        "Comparison of Ekholm Decomposition for Various Mutual Funds"
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


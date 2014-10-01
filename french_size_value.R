#redo analysis with some interactivity
#original post is here
#http://timelyportfolio.blogspot.com/2011/11/after-reading-fine-article-style.html

require(rCharts)


#use Ken French momentum style indexes for style analysis
#http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_12_2.zip

require(PerformanceAnalytics)
require(factorAnalytics)
require(quantmod)

my.url ="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_2x3_daily.zip"
my.tempfile<-paste(tempdir(),"\\frenchmomentum.zip",sep="")
my.usefile<-paste(tempdir(),"\\6_Portfolios_2x3_daily.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
              quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_size_value <- read.table(file=my.usefile,
                              header = TRUE, sep = "",
                              as.is = TRUE,
                              skip = 19, nrows=23257)
colnames(french_size_value) <- c(paste("Small",
                                     colnames(french_size_value)[1:3],sep="."),
                               paste("Large",colnames(french_size_value)[1:3],sep="."))

#get dates ready for xts index
datestoformat <- rownames(french_size_value)
datestoformat <- paste(substr(datestoformat,1,4),
                       substr(datestoformat,5,6),substr(datestoformat,7,8),sep="-")

#get xts for analysis
french_size_value_xts <- as.xts(french_size_value[,1:6],
                              order.by=as.Date(datestoformat))

french_size_value_xts <- french_size_value_xts/100

#get price series from monthly returns
french_price<-as.xts(
  apply(1+coredata(french_size_value_xts[,1:6]),MARGIN=2,cumprod),
  index(french_size_value_xts))
#check data for reasonability
plot.zoo(log(french_price),screens=1)
Return.annualized(french_size_value_xts)






# to get data from Axys export
fund <- read.csv("f:/axys3/keekent/4609tecs.csv",stringsAsFactors = F)
fund.roc <- as.xts(
  fund [,-c(1,ncol(fund))]
  ,order.by = as.Date(
    paste(
      substr(fund[,1],1,2)
      ,"01"      
      ,substr(fund[,1],7,8)
      ,sep = "-"
    )
    ,format = "%m-%d-%y"
  )
)


perfComp <- na.omit(merge(fund.roc[,3],french_size_value_xts)) #, french_momentum_xts))


fit.time <- fitTsfm(
  asset.names=colnames(perfComp[,1]),
  factor.names=colnames(perfComp[,-1]),
  data=perfComp,
  fit.method="OLS"
)

betasRolling <- rollapply(
  perfComp
  , width = 12
  , by.column=FALSE
  , by=1
  , FUN = function(x){
    fit.time <- fitTsfm(
      asset.names=colnames(x[,1]),
      factor.names=colnames(x[,-1]),
      data=x,
      fit.method="DLS" # "OLS" , "Robust"
    )
    return(xts(fit.time$beta,order.by=index(tail(x,1))))
  }
)
colnames(betasRolling) <- colnames(perfComp)[-1]

require(reshape2)
betasRolling.melt <- melt(data.frame(index(betasRolling),betasRolling),id.vars=1)
colnames(betasRolling.melt) <- c("date", "factor", "beta")

nBeta <- nPlot(
  beta ~ date,
  group = "factor",
  data = na.omit(betasRolling.melt),
  type = "multiBarChart", #lineChart #stackedAreaChart", #bar, area don't work with negative
  height = 400,
  width = 700
)
#nBeta$chart(stacked = TRUE, useInteractiveGuideline=TRUE)
nBeta$xAxis(tickFormat = 
              "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#"
)
nBeta$yAxis(tickFormat =
              "#!function(d) {return d3.format('0.2f')(d);}!#"
)

nBeta
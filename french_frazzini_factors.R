#use monthly factor data from Betting Against Beta paper
#This file contains monthly return of the Betting against Beta factors used in Frazzini A and L.H. Pedersen (2013), “Betting Against Beta"
#Copyright ©2013 Andrea Frazzini and Lasse Heje Pedersen

#combine factor data from Fama/French
#See Fama and French, 1993
#"Common Risk Factors in the Returns on Stocks and Bonds"
#Journal of Financial Economics



require(gdata)
require(PerformanceAnalytics)
require(factorAnalytics)
require(quantmod)
require(rCharts)

#read spreadsheet
babFactors <- read.xls(
  "http://www.econ.yale.edu/~af227/data/BAB%20factors%20-%20Frazzini%20and%20Pedersen.xlsx"
  ,pattern = "caldt"
  ,blank.lines.skip = T
  ,stringsAsFactors = F
)

#convert spreadsheet data to R xts
#remove % with gsub, make numeric, and divide by 100
babFactors.xts <- as.xts(
  do.call(cbind,
    lapply(
      babFactors[,-1]
      ,function(x){
        df<-data.frame(as.numeric(
          gsub(
            x=x
            ,pattern="%"
            ,replacement=""
          )
        )/100)
        colnames(df) <- colnames(x)
        return(df)
      }
    )
  )  #date is first column; will use in order.by
  ,order.by = as.Date(paste0(babFactors[,1],"-01"),format="%Y%m-%d")
)


#now read Fama/French Factor data
my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors.zip"
my.tempfile<-paste(tempdir(),"\\F-F_Research_Data_Factors.zip",sep="")
my.usefile<-paste(tempdir(),"\\F-F_Research_Data_Factors.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
              quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_factors <- read.table(file=my.usefile,
                              header = TRUE, sep = "",
                              as.is = TRUE,
                              skip = 3, nrows=1052)

#get dates ready for xts index
datestoformat <- rownames(french_factors)
datestoformat <- paste(substr(datestoformat,1,4),
                       substr(datestoformat,5,7),"01",sep="-")

#get xts for analysis
french_factors.xts <- as.xts(french_factors,
                              order.by=as.Date(datestoformat))

french_factors.xts <- french_factors.xts/100


factorsUS <- na.omit(merge(french_factors.xts,babFactors.xts[,1]))
colnames(factorsUS)[5] <- "BAB"

#not necessary but grab DJIA from FRED for sanity check
djia <- to.monthly(getSymbols("DJIA",src="FRED",auto.assign=F))[,4]
colnames(djia) <- "DowJonesIndu"
index(djia) <- as.Date(index(djia))


returnsFactors <- na.omit(
  merge(
    ROC(djia,n=1,type="discrete")
    ,factorsUS
  )
)



betasRolling <- rollapply(
  returnsFactors[,-1]
  , width = 36  #3 year or 36 month rolling
  , by.column=FALSE
  , by=1
  , FUN = function(x){
    fit.time <- fitTimeSeriesFactorModel(
      assets.names=colnames(x[,1]),
      factors.names=colnames(x[,-c(1,4)]),
      data=x,
      fit.method="OLS"
    )
    return(fit.time$beta)
  }
)
colnames(betasRolling) <- colnames(returnsFactors)[-c(1,2,5)]

require(reshape2)
betasRolling.melt <- melt(data.frame(index(betasRolling),betasRolling),id.vars=1)
colnames(betasRolling.melt) <- c("date", "factor", "beta")

nBeta <- nPlot(
  beta ~ date,
  group = "factor",
  data = na.omit(betasRolling.melt),
  type = "multiBarChart", #lineChart #stackedAreaChart, #bar, area don't work with negative
  height = 400,
  width = 700
)
#nBeta$chart(stacked = TRUE, useInteractiveGuideline=TRUE)
nBeta$xAxis(tickFormat = 
              "#!function(d) {return d3.time.format('%Y')(new Date(d * 24 * 60 * 60 * 1000));}!#"
)
nBeta$yAxis(tickFormat =
              "#!function(d) {return d3.format('0.2f')(d);}!#"
)
#nBeta

nBeta$params$type = "lineChart"
nBeta


#chart.Correlation(returnsFactors)

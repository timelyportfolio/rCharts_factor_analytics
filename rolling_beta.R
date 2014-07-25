#redo analysis with some interactivity
#original post is here
#http://timelyportfolio.blogspot.com/2011/11/after-reading-fine-article-style.html

require(rCharts)


#use Ken French momentum style indexes for style analysis
#http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_12_2.zip

require(PerformanceAnalytics)
require(factorAnalytics)
require(quantmod)

my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_12_2.zip"
my.tempfile<-paste(tempdir(),"\\frenchmomentum.zip",sep="")
my.usefile<-paste(tempdir(),"\\6_Portfolios_ME_Prior_12_2.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
	quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_momentum <- read.table(file=my.usefile,
	header = TRUE, sep = "",
	as.is = TRUE,
	skip = 12, nrows=1046)
colnames(french_momentum) <- c(paste("Small",
	colnames(french_momentum)[1:3],sep="."),
	paste("Large",colnames(french_momentum)[1:3],sep="."))

#get dates ready for xts index
datestoformat <- rownames(french_momentum)
datestoformat <- paste(substr(datestoformat,1,4),
	substr(datestoformat,5,7),"01",sep="-")

#get xts for analysis
french_momentum_xts <- as.xts(french_momentum[,1:6],
	order.by=as.Date(datestoformat))

french_momentum_xts <- french_momentum_xts/100

#get price series from monthly returns
french_price<-as.xts(
	apply(1+coredata(french_momentum_xts[,1:6]),MARGIN=2,cumprod),
	index(french_momentum_xts))
#check data for reasonability
plot.zoo(french_price,log="y")

#for this example let's use Bill Miller's fund LMVTX
ticker <- 'NBGIX' #"POGRX" #LBSCX" #Columbia Income "CDDRX"
getSymbols(ticker,from="1896-01-01", to=Sys.Date(), adjust=TRUE)#, auto.assign=F)
fund <- get(ticker)
fund <- to.monthly(fund)
index(fund) <- as.Date(format(as.Date(index(fund)),"%Y-%m-01"))
fund.roc <- ROC(fund[,4],type="discrete",n=1)

perfComp <- na.omit(merge(fund.roc,french_momentum_xts))


fit.time <- fitTsfm(
  asset.names=colnames(perfComp[,1]),
  factor.names=colnames(perfComp[,-1]),
  data=perfComp,
  fit.method="DLS"
)

betasRolling <- rollapply(
  perfComp
  , width = 36
  , by.column=FALSE
  , by=1
  , FUN = function(x){
    fit.time <- fitTsfm(
      asset.names=colnames(x[,1]),
      factor.names=colnames(x[,-1]),
      data=x,
      fit.method="OLS"
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
nBeta$templates$page = "rChart2.html"
nBeta$srccode = 
  '#redo analysis with some interactivity
#original post is here
#http://timelyportfolio.blogspot.com/2011/11/after-reading-fine-article-style.html




#use Ken French momentum style indexes for style analysis
#http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_12_2.zip

require(PerformanceAnalytics)
require(FactorAnalytics)
require(quantmod)

my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_12_2.zip"
my.tempfile<-paste(tempdir(),"\\frenchmomentum.zip",sep="")
my.usefile<-paste(tempdir(),"\\6_Portfolios_ME_Prior_12_2.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_momentum <- read.table(file=my.usefile,
header = TRUE, sep = "",
as.is = TRUE,
skip = 12, nrows=1038)
colnames(french_momentum) <- c(paste("Small",
colnames(french_momentum)[1:3],sep="."),
paste("Large",colnames(french_momentum)[1:3],sep="."))

#get dates ready for xts index
datestoformat <- rownames(french_momentum)
datestoformat <- paste(substr(datestoformat,1,4),
substr(datestoformat,5,7),"01",sep="-")

#get xts for analysis
french_momentum_xts <- as.xts(french_momentum[,1:6],
order.by=as.Date(datestoformat))

french_momentum_xts <- french_momentum_xts/100

#get price series from monthly returns
french_price<-as.xts(
apply(1+coredata(french_momentum_xts[,1:6]),MARGIN=2,cumprod),
index(french_momentum_xts))
#check data for reasonability
plot.zoo(french_price,log="y")

#for this example lets use Bill Millers fund
getSymbols("fund",from="1896-01-01", to=Sys.Date(), adjust=TRUE)
fund <- to.monthly(fund)
index(fund) <- as.Date(format(as.Date(index(fund)),"%Y-%m-01"))
fund.roc <- ROC(fund[,4],type="discrete",n=1)

perfComp <- na.omit(merge(fund.roc,french_momentum_xts))


fit.time <- fitTsfm(
assets.names=colnames(perfComp[,1]),
factors.names=colnames(perfComp[,-1]),
data=perfComp,
fit.method="DLS"
)

betasRolling <- rollapply(perfComp, width = 36, by.column=FALSE, by=1, FUN = function(x){
fit.time <- fitTsfm(
assets.names=colnames(x[,1]),
factors.names=colnames(x[,-1]),
data=x,
fit.method="OLS"
)
return(fit.time$beta)
})
colnames(betasRolling) <- colnames(perfComp)[-1]

require(reshape2)
betasRolling.melt <- melt(data.frame(index(betasRolling),betasRolling),id.vars=1)
colnames(betasRolling.melt) <- c("date", "factor", "beta")

nBeta <- nPlot(
beta ~ date,
group = "factor",
data = na.omit(betasRolling.melt),
type = "lineChart"
)
#nBeta$chart(stacked = TRUE)
nBeta$xAxis(tickFormat = 
"#!function(d) {return d3.time.format(\'%Y-%m-%d\')(new Date(d * 24 * 60 * 60 * 1000));}!#"
)
nBeta'
nBeta

#nBeta$publish(id=6467559)


#######################old code no longer working#########################################
chart.RollingStyle(perfComp[,1],perfComp[,2:NCOL(perfComp)],
	width=36,
	colorset=c("darkseagreen1","darkseagreen3","darkseagreen4","slateblue1","slateblue3","slateblue4"),
	main="LMVTX Rolling 36mo French Momentum Weights")
#could use the packaged chart.Style but does not allow the
#flexibility I would like
#chart.Style(perfComp[,1],perfComp[,2:NCOL(perfComp)],
#	colorset=c("darkseagreen1","darkseagreen3","darkseagreen4","slateblue1","slateblue3","slateblue4"),
#	main="LMVTX French Momentum Weights")

#get weights for the cumulative period
style.weight <- as.matrix(style.fit(perfComp[,1],
	perfComp[,2:NCOL(perfComp)])$weights)
barplot(style.weight,beside=TRUE,ylim=c(0,max(style.weight)+0.2),
	names.arg=rownames(style.weight),cex.names=0.7,
	col=c("darkseagreen1","darkseagreen3","darkseagreen4",
		"slateblue1","slateblue3","slateblue4"),
	main=paste("LMVTX French Momentum Weights
	Since ",format(index(LMVTX)[1],"%b %Y"),sep=""))

#look at total R to determine goodness of fit
style.R <- style.fit(perfComp[,1],
	perfComp[,2:NCOL(perfComp)])$R.squared


styleR <- function(x) {
	as.numeric(style.fit(R.fund=x[,1,drop=FALSE],R.style=x[,2:NCOL(x),drop=FALSE],method="constrained",selection="none",leverage=FALSE)$R.squared)
}
#convert to matrix since I get
#error "The data cannot be converted into a time series."
#when I use xts as data
style.RollingR <- as.xts(rollapply(data=as.matrix(perfComp),
	width=12,FUN=styleR,by.column=FALSE,by=1),
	order.by=index(perfComp)[12:NROW(perfComp)])
chart.TimeSeries(style.RollingR,ylab="Rolling 12-mo R",
	main=paste("LMVTX Rolling R versus French Momentum
	Since ",format(index(LMVTX)[1],"%b %Y"),sep=""))
abline(h=style.R,col="indianred")
text(x=1,y=style.R,labels="r for entire series",adj=0,col="indianred")

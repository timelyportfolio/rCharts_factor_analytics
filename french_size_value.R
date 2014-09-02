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
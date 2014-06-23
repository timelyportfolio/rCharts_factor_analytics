require(quantmod)

#use monthly French data to replicate research on momentum in R
# data source : Kenneth French Data Library
#               http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html 
# paper       : Fact, Fiction and Momentum Investing
#               Asness, Clifford S. and Frazzini, Andrea and Israel, Ronen and Moskowitz, Tobias J.
#               May 9, 2014
#               http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2435323

#gather french factor data
my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors.zip"
my.tempfile<-paste(tempdir(),"\\frenchfactors.zip",sep="")
my.usefile<-paste(tempdir(),"\\F-F_Research_Data_Factors.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
              quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_factors <- read.table(file=my.usefile,
                             header = TRUE, sep = "",
                             as.is = TRUE,
                             skip = 3, nrows=1054)
#get xts for analysis
french_factors_xts <- as.xts(
  french_factors,
  order.by=as.Date(
    paste0(rownames(french_factors),"01"),
    format="%Y%m%d"
  )
)

#now get the momentum factor
my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor.zip"
my.usefile<-paste(tempdir(),"\\F-F_Momentum_Factor.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
              quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_momentum <- read.table(file=my.usefile,
                              header = TRUE, sep = "",
                              as.is = TRUE,
                              skip = 12, nrows=1048)
#get xts for analysis
french_momentum_xts <- as.xts(
  french_momentum,
  order.by=as.Date(
    paste(rownames(french_momentum),"01"),
    format="%Y%m%d"
  )
)

#merge UMD (momentum) with other french factors
french_factors_xts <- na.omit( merge( french_factors_xts, french_momentum_xts ) )
french_factors_xts <- french_factors_xts/100


#test our numbers
periods <- c("1927::2013","1963::2013","1991::2013")
t(sapply(
  periods,
  function(x){
    return(data.frame(Return.annualized(french_factors_xts[x,-4],geometric=F)))
  }
))
t(do.call(cbind,lapply(
  periods,
  function(x){
    df <- data.frame(
      SharpeRatio.annualized(
        french_factors_xts[x,-4],
        Rf = 0,
        geometric=F
      )[1,]
    )
    colnames(df) <- x
    return(df)
  }
)))

#ok Table 1 matches so now let's move on to other calculations
#Table 2
do.call(rbind,lapply(
  periods,
  function(x){
    df <- data.frame(lapply(
      rollapply(french_factors_xts[x,-4], width = 12, by = 1, FUN = Return.cumulative, geometric=F),
      function(y){sum(na.omit(y)>=0)/nrow(na.omit(y))}
    ))
    rownames(df) <- x
    return(df)
  }
))

do.call(rbind,lapply(
  periods,
  function(x){
    df <- data.frame(lapply(
      rollapply(french_factors_xts[x,-4], width = 60, by = 1, FUN = Return.cumulative, geometric=F),
      function(y){sum(na.omit(y)>=0)/nrow(na.omit(y))}
    ))
    rownames(df) <- x
    return(df)
  }
))


#Table 3
SharpeRatio.annualized(
  apply(
    french_factors_xts[x,c(3,5)],
    MARGIN = 1,
    function(x){ x[1] * 0.6 + x[2] * 0.4}
  ),
  Rf = 0,
  geometric=F
)

lapply(
  c(12,60),
  function(width){
    y = rollapply(
      apply(
        french_factors_xts[,c(3,5)],
        MARGIN = 1,
        function(x){ x[1] * 0.6 + x[2] * 0.4}
      ),
      width = width,
      by = 1,
      FUN = Return.cumulative,
      geometric=F
    )
    return(sum(na.omit(y)>=0)/length(na.omit(y)))
  }
)


#Table 4
#need some additional data here
#http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_12_2.zip
my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_12_2.zip"
my.tempfile<-paste(tempdir(),"\\frenchfactors.zip",sep="")
my.usefile<-paste(tempdir(),"\\6_Portfolios_ME_Prior_12_2.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
              quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_hml_smb <- read.table(file=my.usefile,
                             header = TRUE, sep = "",
                             as.is = TRUE,
                             skip = 12, nrows=1048)
colnames(french_hml_smb) <- c(
  paste0(
    "Small.",
    colnames(french_hml_smb)[1:3]
  ),
  paste0(
    "Big.",
    colnames(french_hml_smb)[1:3]
  )
)
#get xts for analysis
french_hml_smb_xts <- as.xts(
  french_hml_smb/100,
  order.by=as.Date(
    paste0(rownames(french_hml_smb),"01"),
    format="%Y%m%d"
  )
)

#so short side will be average of Small.Low and Big.Low
#and long side will be the average of Small.High and Big.High
french_short_long_xts <- as.xts(do.call(rbind,apply(
  french_hml_smb_xts,
  MARGIN=1,
  function(period){
    data.frame(
      shortside = mean(period[c(1,4)]),
      longside = mean(period[c(3,6)])
    )
  }
)),order.by = index(french_hml_smb_xts))

#add umd from the factors
#as check this should be same as sum of -short and long
french_short_long_xts <- merge(
  french_short_long_xts,
  french_factors_xts[,c(5,1,4)] #Market and Mom (UMD)
)


#UMD market-adjusted returns (alpha)
do.call(rbind,lapply(
  periods,
  function(period){
    df <- data.frame(
      shortside = -((CAPM.alpha(
        Ra = french_short_long_xts[period,]$shortside,
        Rb = french_short_long_xts[period,]$Mkt.RF + french_short_long_xts[period,]$RF,
        Rf = french_short_long_xts[period,]$RF
      )+1)^12-1),
      longside = (CAPM.alpha(
        Ra = french_short_long_xts[period,]$longside,
        Rb = french_short_long_xts[period,]$Mkt.RF + french_short_long_xts[period,]$RF,
        Rf = french_short_long_xts[period,]$RF
      )+1)^12-1
    )
    df$UMD <- df$shortside + df$longside
    rownames(df) <- period
    return(df)
  }
))

#UMD returns minus market
do.call(rbind,lapply(
  periods,
  function(period){
    df <- data.frame(
      shortside = -Return.annualized(
        french_short_long_xts[period,]$shortside - 
          french_short_long_xts[period,]$Mkt.RF - 
          french_short_long_xts[period,]$RF,
        geometric=F
      ),
      longside = Return.annualized(
        french_short_long_xts[period,]$longside - 
          french_short_long_xts[period,]$Mkt.RF - 
          french_short_long_xts[period,]$RF,
        geometric=F
      )
    )
    df$UMD <- df$shortside + df$longside
    rownames(df) <- period
    return(df)
  }
))




#table 5
table5 <- do.call(rbind,lapply(
  periods,
  function(period){
    df <- data.frame(Return.annualized(
      french_hml_smb_xts[period,],
      geometric = F
    ))
    rownames(df) <- period
    return(df)
  }
))
#umd small
umdsmall <- data.frame(table5$Small.High - table5$Small.Low)
rownames(umdsmall) <- periods
colnames(umdsmall) <- "UMD Small"
umdsmall

#umd big
umdbig <- data.frame(table5$Big.High - table5$Big.Low)
rownames(umdbig) <- periods
colnames(umdbig) <- "UMD Big"
umdbig

#by this point I hope you can do the Value piece of table 5 on your own
#if you really can't figure it out, let me know
umdbig










####################### with daily ####################################
#get Mkt.RF, SMB, HML, and RF
#UMD is in a different file
my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily.zip"
my.tempfile<-paste(tempdir(),"\\frenchfactors.zip",sep="")
my.usefile<-paste(tempdir(),"\\F-F_Research_Data_Factors_daily.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
              quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_factors <- read.table(file=my.usefile,
                             header = TRUE, sep = "",
                             as.is = TRUE,
                             skip = 4, nrows=23215)
#get xts for analysis
french_factors_xts <- as.xts(
  french_factors,
  order.by=as.Date(
    rownames(french_factors),
    format="%Y%m%d"
  )
)

#now get the momentum factor
my.url="http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_daily.zip"
my.usefile<-paste(tempdir(),"\\F-F_Momentum_Factor_daily.txt",sep="")
download.file(my.url, my.tempfile, method="auto", 
              quiet = FALSE, mode = "wb",cacheOK = TRUE)
unzip(my.tempfile,exdir=tempdir(),junkpath=TRUE)
#read space delimited text file extracted from zip
french_momentum <- read.table(file=my.usefile,
                              header = TRUE, sep = "",
                              as.is = TRUE,
                              skip = 13, nrows=23114)
#get xts for analysis
french_momentum_xts <- as.xts(
  french_momentum,
  order.by=as.Date(
    rownames(french_momentum),
    format="%Y%m%d"
  )
)

#merge UMD (momentum) with other french factors
french_factors_xts <- na.omit( merge( french_factors_xts, french_momentum_xts ) )
french_factors_xts <- french_factors_xts/100

#get price series from monthly returns
french_price<-as.xts(
  apply(1+coredata(french_factors_xts),MARGIN=2,cumprod),
  index(french_factors_xts))

#now we should have all the french factor data that we need
#we can start to do our exploration

require(PerformanceAnalytics)

Return.annualized(french_factors_xts["1991::2013"],geometric=F)




require(reshape2)
require(ggplot2)
require(dplyr)
require(lattice)

Omega(french_factors_xts)

#the traditional way to get rolling
#but this takes a really long time except if we do as.matrix
system.time(
  df_matrix <- rollapply(as.matrix(french_factors_xts), width=200, by = 1, FUN=Omega)
)

system.time(
  df_lapply <- do.call(rbind,lapply(
    french_factors_xts,function(f){
      answer <- data.frame(rollapply(as.numeric(f),Omega,width=200,by=1))
      colnames(answer) <- "omega"
      answer$date = index(french_factors_xts)[seq(1,nrow(french_factors_xts)-199,by=1)]
      answer$mkt_factor = colnames(f)
      return(answer)
    }
  ))
)

xyplot(omega~date, groups = mkt_factor, data = df_lapply,type="l",ylim=c(-1,4))

require(tidyr)
system.time(
df_dplyr <- #melt(
#  data.frame(
#    date=as.Date(index(french_factors_xts)),
#    french_factors_xts
#  ),
#  id.vars = "date",
#  variable.name = "mkt_factor",
#  value.name = "roc"
#)
data.frame("date"=index(french_factors_xts),french_factors_xts) %>%
  gather(ff_factor,roc,-date) %.%
  group_by( ff_factor )  %.%
  do(
    data.frame(
      date = .$date[seq(1,nrow(.)-199,by=1)],
      omega = rollapply( as.numeric(.$roc) , Omega, width=200, by=1)
    )
  )
)

xyplot(omega~date, groups = ff_factor, data = df_dplyr,type="l",ylim=c(-1,4))


#play with tidyr
data.frame("date"=index(french_factors_xts),french_factors_xts) %>%
    gather(ff_factor,roc,-date)

data.frame("date"=index(french_factors_xts),french_factors_xts) %>%
  gather(ff_factor,roc,-date) %>%
  ggplot(data = .,aes(x=date,y=roc,colour=ff_factor)) + geom_line()

data.frame("date"=index(french_factors_xts),french_factors_xts) %>%
  gather(ff_factor,roc,-date) %>%
  group_by( ff_factor ) %>%
  mutate(cumul = cumsum(roc)) %>%
  ggplot(data = .,aes(x=date,y=cumul,colour=ff_factor)) + geom_line()

require(rCharts)
data.frame("date"=format(index(french_factors_xts)),french_factors_xts) %>%
  gather(ff_factor,roc,-date) %>%
  group_by( ff_factor ) %>%
  mutate(cumul = cumsum(roc)) %>%
  dPlot(
    cumul~date
    ,groups="ff_factor"    
    ,data = .
    ,type="line"
    ,xAxis = list( type = "addTimeAxis", inputFormat = '%Y-%m-%m', outputFormat = "%b %Y"  )
  )
  
#very hacky way of accomplishing
#need to iterate to something better
modifyChartList <- function( x, element, val ) {
  rTemp <- x$copy()
  rTemp[[element]] <- modifyList(rTemp[[element]], val)
  return(rTemp)
}

data.frame("date"=format(index(french_factors_xts)),french_factors_xts) %>%
  gather( ff_factor, roc, -date ) %>%
  group_by( ff_factor ) %>%
  mutate(cumul = cumsum(roc)) %>%
  dPlot(
    cumul~date
    ,groups="ff_factor"    
    ,data = .
    ,type="line"
    ,xAxis = list( type = "addTimeAxis", inputFormat = '%Y-%m-%m', outputFormat = "%b %Y"  )
  ) %>%
  modifyChartList(
    element = "templates",
    val = list(afterScript = '
      <script>
        myChart.axes[0].shapes.selectAll(".tick")[0].forEach(function(d,i){
          if (!(+d3.time.format("%Y")(new Date(+d3.select(d).datum())) % 10 == 0)) {
            d.remove()
          } else {
            d3.select(d).select("text")
              .attr("transform",null)
              .attr("y","0")
              .attr("dy","2em")
              .style("text-anchor","middle")
              .text(d3.time.format("%Y")(new Date(+d3.select(d).datum())))
          }
        });
      </script>
     '
    )
  )
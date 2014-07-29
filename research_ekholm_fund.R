# perform Ekholm (2012,2014) analysis on mutual fund return data

# Ekholm, A.G., 2012
# Portfolio returns and manager activity:
#    How to decompose tracking error into security selection and market timing
# Journal of Empirical Finance, Volume 19, pp 349–358

# Ekholm, Anders G., July 21, 2014
# Components of Portfolio Variance:
#    R2, SelectionShare and TimingShare
# Available at SSRN: http://ssrn.com/abstract=2463649

require(quantmod)
require(PerformanceAnalytics)
require(pipeR)
require(magrittr)


#daily factors from Kenneth French Data Library
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
                             skip = 4, nrows=23257)
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
                              skip = 13, nrows=23156)
#get xts for analysis
french_momentum_xts <- as.xts(
  french_momentum,
  order.by=as.Date(
    rownames(french_momentum),
    format="%Y%m%d"
  )
)

#merge UMD (momentum) with other french factors
french_factors_xts %:>%
  merge( . , french_momentum_xts ) %>>%
  na.omit %:>%
  .[] / 100 -> french_factors_xts



#get a fund to analyze
ticker <- 'NBGIX'
ticker %>>% 
  getSymbols( from="1896-01-01", adjust=TRUE, auto.assign=F ) %:>%
  .[,4] %>>%
  ROC( type = "discrete", n = 1 ) %>>%
  merge ( french_factors_xts ) %>>%
  na.omit -> perfComp

colnames(perfComp)[1] <- gsub( ".Close", "", colnames(perfComp)[1] )

carhartLM <- lm( data = perfComp, NBGIX ~ Mkt.RF + SMB + HML + UMD )

carhartLM %:>%
  .$residuals %:>%
  . ^ 2 %>%
  data.frame(
    perfComp
    , "fitted_sq" = .
    , lapply(perfComp[,-c(1,5)],function(x){
        data.frame( as.numeric(x) ^ 2 ) %>%
          set_colnames( paste0(names(x),"_sq") ) %>%
          return
      }) %:>% do.call( cbind, . )
  ) %T>% assign(x = "return_data_carhart", value = ., pos = .GlobalEnv) %:>%
  lm( fitted_sq ~ Mkt.RF_sq + SMB_sq + HML_sq + UMD_sq, data = . ) %>>%
  coefficients %:>% . ^ (1/2) %>>%
  t %:>%
  data.frame(
    #.[,1],
    #sum(.[,-1])
    .
  ) %>% 
  set_colnames(c("ActiveAlpha", paste0("ActiveBeta_",colnames(.)[-1])))

jensen_ekholm <- function( data, ticker = NULL ){
  
  if(is.null(ticker)) ticker <- colnames(data)[1]
  
  as.formula ( paste0(ticker, " ~  Mkt.RF" ) ) %:>%
   lm( data = data, . ) -> jensenLM
  
  jensenLM %>%
    .$residuals %:>%
    . ^ 2 %>%
    data.frame(
      data
      , "fitted_sq" = .
      , lapply(data[,2],function(x){
        data.frame( as.numeric(x) ^ 2 ) %>%
          set_colnames( paste0(names(x),"_sq") ) %>%
          return
      }) %:>% do.call( cbind, . )
    ) -> return_data_jensen
  
  return_data_jensen %:>%
    lm( fitted_sq ~ Mkt.RF_sq, data = . ) %>>%
    coefficients %:>% . ^ (1/2) %>>%
    t %:>%
    data.frame(
      #.[,1],
      #sum(.[,-1])
      .
    ) %>% 
    set_colnames(c("ActiveAlpha", paste0("ActiveBeta_",colnames(.)[-1])))  %:>%
    data.frame(
      .
      , "SelectionShare" = .$ActiveAlpha ^ 2 / (var(return_data_jensen[,ticker]) * (nrow(return_data_jensen) - 1) / nrow(return_data_jensen))
      , "TimingShare" = .$ActiveBeta_Mkt.RF_sq ^ 2* mean( return_data_jensen$Mkt.RF_sq ) / (var(return_data_jensen[,ticker]) * (nrow(return_data_jensen) - 1) / nrow(return_data_jensen))
      
    ) %:>%
    list( "ekholm" = ., "linmod" = jensenLM ) %>>%
    return
}

jensen_ekholm( perfComp ) -> jE

jE %:>% summary(.$linmod)$"r.squared" + jE$ekholm[1,3] + jE$ekholm[1,4]

#make silly little function to combine mutual fund returns with french factors
getPerformance <- function ( ticker, from = "1896-01-01" ) {
  ticker %>>% 
    getSymbols( from = from, adjust=TRUE, auto.assign=F ) %:>%
    .[,4] %>>%
    ROC( type = "discrete", n = 1 ) %>>%
    merge ( french_factors_xts ) %>>%
    na.omit -> perfComp
}

getPerformance( "SEQUX" ) %>>%
  rollapply (
    FUN= function(x){
      x %:>%
        jensen_ekholm(.) %:>% 
        cbind( t(coef(.$linmod)) , .$ekholm ) %>>%
        xts(order.by=tail(index(x),1)) %>>%
        return
    }
    , width = 250
    #, by = 100
    , by.column=F
    , fill = NULL
  ) %T>% plot.zoo(type ="b", main = "Ekholm (2014) Return Decomposition")

 

library(Quandl)  # use to get Fama/French factors
library(quantmod)
library(pipeR)
library(rlist)
library(dplyr)
library(tidyr)
library(rCharts)

# use Quandl Kenneth French Fama/French factors
# http://www.quandl.com/KFRENCH/FACTORS_D
f <- Quandl("KFRENCH/FACTORS_D",type = "xts") / 100

# make a function to quickly calculate ekholm
# fund and benchmark will be prices
# rf will be returns
calcEkholm <- function( fund, benchmark, rf ){
  perf = merge(fund,benchmark) %>>%
    na.omit %>>%
    (merge(
      . / stats::lag(.,1) - 1,
      rf
    )) %>>%
    na.omit
  
  # subtract risk-free from mkt or benchmark fund
  perf[,2] = perf[,2] - perf[,3]
  colnames(perf)[2] = "Mkt.RF"
  
  perf %>>% jensen_ekholm
}

lapply(
  c("GVEQX","VV","GEQAX","VIVAX","POGRX","VUG","VVPLX","GVMCX","VO","ARTQX","VOE","RPMGX","VOT","POAGX")
  ,function(ticker){
    return ( getSymbols(ticker,from = "1900-01-01",auto.assign=F)[,6] )
  }
) %>>% (do.call( merge, .))  -> perf

# remove Adjusted from colnames
colnames(perf) = gsub(x=colnames(perf),pattern="\\.Adjusted",replacement="")

ek <- list(
calcEkholm( perf[,"GVEQX"], perf[,"VV"], f[,"RF"] )
#cddrx against vtv (large value)
,calcEkholm( perf[,"GEQAX"], perf[,"VIVAX"], f[,"RF"] )
#pogrx against vug (large growth)
,calcEkholm( perf[,"POGRX"], perf[,"VUG"], f[,"RF"] )
#vvplx against vug (large growth)
,calcEkholm( perf[,"VVPLX"], perf[,"VUG"], f[,"RF"] )

#And maybe on the midcap do
#Gvmcx against vo (mid)
,calcEkholm( perf[,"GVMCX"], perf[,"VO"], f[,"RF"] )
#Artqx against voe (mid value)
,calcEkholm( perf[,"ARTQX"], perf[,"VOE"], f[,"RF"] )
#Rpmgx against vot (mid growth)
,calcEkholm( perf[,"RPMGX"], perf[,"VOT"], f[,"RF"] )
#Poagx against vot (mid growth)
,calcEkholm( perf[,"POAGX"], perf[,"VOT"], f[,"RF"] )
)

names(ek) <- list.mapv(
  ek,
  return( colnames(.$linmod$model)[1] )
)

ek %>>%
  list.map( data.frame( r2 = summary(.[["linmod"]])$"r.squared" , .$ekholm ) ) %>>%
  ( do.call(rbind,.) ) %>>%
  (data.frame(
    fund = rownames(.)
    , .
  )) %>>%
  (~ ek.df) %>>%
  gather(measure,value,-fund) %>>%
  filter( measure %in% c("r2","SelectionShare","TimingShare") ) %>>%
  (
    dPlot(
      y = "value"
      ,x = c("measure","fund")
      ,groups = "fund"
      ,data = .
      ,type = "bar"
      ,facet = list(x = "measure")
      ,yAxis = list( outputFormat = ".2%" )
    )
  ) %>>%
  (~
    .$setTemplate(afterScript = 
'
<script>
d3.selectAll(".dimple-bar")
  .on("mouseover",function(){
    var that = d3.select(this);
    that.style("opacity",1)
    d3.selectAll(".dimple-bar")
      .style("opacity",0.4)
      .filter(function(b){
        return b.key.substring(0,5) == that.datum().key.substring(0,5)
      })[0].map(function(b){
        d3.select(b).style("opacity",1)
      })
  })
  .on("mouseout",function(){
    d3.selectAll(".dimple-bar").style("opacity",0.8)
  })
  .each(function(bar){
    d3.select(this.parentNode).append("text")
      .style("pointer-events","none")
      .text(bar.key.substring(0,5))
      .attr("x", d3.select(this).attr("x"))
      .attr("y", 280)
      .attr("transform","rotate(-90," + (parseInt(d3.select(this).attr("x")) + 14) + ",280)")
  })
</script>
'               
    ) 
  ) %>>%
  ( .$show("static") )
  
htmlTable(round(ek.df[,-1],3),rowlabel="")


merge(perf[,c("GEQAX","VIVAX")], f[,"RF"]) %>>% na.omit %>>%
  rollapply (
    FUN= function(x){
      x %>>%
        ( calcEkholm(x[,1],x[,2],x[,3]) )%>>% 
        ( data.frame( summary(.[["linmod"]])$"r.squared" , .$ekholm ) )  %>>%
        xts(order.by=tail(index(x),1)) -> return_df
      colnames(return_df)[1] <- "R_sq"
      return(return_df)
    }
    , width = 500
    , by.column=F
    , fill = NULL
  ) %>>%
  na.fill(0) %>>% (list(GEQAX = .)) -> ekFunds

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
  filter( measure %in% c("R_sq","SelectionShare","TimingShare"), as.Date(date) > as.Date("2012-12-31") ) %>>% 
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
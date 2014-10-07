library(htmltools)
library(rvest)
library(rlist)
library(pipeR)
library(xts)


'https://www.kimonolabs.com/api/31mc81iu?apikey=bc64ec01cf2034a687d584c3f29358e1&kimseries=1' %>>%
  readLines %>>%
  jsonlite::fromJSON() %>>%
  (~ ssrnStat) %>>%
  (
    list.map(
      .$results$ekholm_2014
      ,data.frame(.)
    )
  ) %>>%
  (~ viewStat) #%>>%

viewStat %>>%
  (
    lapply(
      .
      ,function(l){
        as.xts(
          as.numeric(gsub(x=l[,2],pattern=",",replacement=""))
          ,order.by = gsub(x=l[,1], pattern="T", replacement = " ") %>>% strftime %>>% as.POSIXlt
        )
      }
    )
  ) %>>%
  ( do.call(merge,.) ) %>>%
  (~ plot.zoo( ., main = "SSRN Statistics Since Blog Post" ) )
  
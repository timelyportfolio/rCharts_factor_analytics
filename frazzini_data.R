#use monthly factor data from Betting Against Beta paper
#This file contains monthly return of the Betting against Beta factors used in
#Frazzini A and L.H. Pedersen (2013), "œBetting Against Beta"
#Copyright©2013 Andrea Frazzini and Lasse Heje Pedersen

#ssrn citation to the paper
#Asness, Clifford S. and Frazzini, Andrea and Pedersen, Lasse Heje
#Quality Minus Junk (October 9, 2013)
#Available at SSRN: http://ssrn.com/abstract=231243



require(gdata)
require(quantmod)
require(latticeExtra)

#read spreadsheet
qmjFactors <- read.xls(
  "http://www.econ.yale.edu/~af227/data/QMJ%20-%20Factors%20-%20monthly.xlsx"
  ,pattern = "DATE"
  ,blank.lines.skip = T
  ,stringsAsFactors = F
)
#convert spreadsheet data to R xts
#remove % with gsub, make numeric, and divide by 100
qmjFactors.xts <- as.xts(
  do.call(cbind,
          lapply(
            qmjFactors[,-1]
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
  ,order.by = as.Date(qmjFactors[,1]) #as.Date(paste0(qmjFactors[,1],"-01"),format="%Y%m%d")
)

#xyplot(qmjFactors.xts)

asTheEconomist(
  xyplot(
    cumprod(1+na.omit(qmjFactors.xts))
    , scales = list( y = list( relation = "same" ) )
    , main = "Quality Minus Junk (2013)\nAsness,Frazzini, Pedersen\nhttp://ssrn.com/abstract=231243"
  )
)


asTheEconomist(xyplot(qmjCumul))
#need a transform for the horizon to work properly
#but leave it here as a placeholder
xyplot(qmjFactors.xts,panel=panel.horizonplot)

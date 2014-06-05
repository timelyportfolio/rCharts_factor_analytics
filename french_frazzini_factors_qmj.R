#full factor set for 1956-2012 from Andrea Frazzini Data
#http://www.econ.yale.edu/~af227/data_library.htm

#direct link to Excel File
#http://www.econ.yale.edu/~af227/data/QMJ%20factors%20-%20Asness,%20Frazzini%20and%20Pedersen.xlsx

#ssrn citation to the paper
#Asness, Clifford S. and Frazzini, Andrea and Pedersen, Lasse Heje
#Quality Minus Junk (October 9, 2013)
#Available at SSRN: http://ssrn.com/abstract=2312432 


require(gdata)
#require(PerformanceAnalytics)
#require(factorAnalytics)
require(quantmod)
require(reshape2)
require(rCharts)

#read spreadsheet
qmjFactors <- read.xls(
  "http://www.econ.yale.edu/~af227/data/QMJ%20factors%20-%20Asness,%20Frazzini%20and%20Pedersen.xlsx"
  ,pattern = "Caldt"
  ,blank.lines.skip = T
  ,stringsAsFactors = F
)

#convert spreadsheet data to R xts
#remove % with gsub, make numeric, and divide by 100
qmjFactors.xts <- as.xts(
  do.call(cbind,
          lapply(
            qmjFactors[,-(c(1,7))]
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
  ,order.by = as.Date(paste0(qmjFactors[,1],"-01"),format="%Y%m%d")
)

colnames(qmjFactors.xts) <- gsub(
  x = colnames(qmjFactors.xts),
  ,pattern = "\\.1"
  ,replacement = "\\.Glbl"
)

cumul <- melt(
  data.frame(
    date = format(index( na.omit( qmjFactors.xts )))
    , cumprod( 1 + na.omit( qmjFactors.xts ) )
  )
  , id.vars = 1
  , value.name = "cumul"
  , variable.name = "factor"
)

d1 <- dPlot(
  cumul ~ date
  , groups = "factor"
  , data = cumul
  , type = "line"
  , height = 400
  , width = 700
  , bounds = list(x = 80, y = 70, width = 600 , height = 250)  
)
d1$xAxis(
  type = "addTimeAxis"
  , inputFormat = "%Y-%m-%d"
  , outputFormat = "%b %Y"
)
d1$setTemplate(
  afterScript = 
'
<script>
//get fewer ticks on x axis
//this is a dimple issue that might or might not get fixed
myChart.svg.select(".axis").selectAll(".tick")[0].forEach(function(d,i){
  if (!(+d3.time.format("%y")(new Date(+d3.select(d).datum())) % 5 == 0)) {
    d.remove()
  } else {
    var dtext = d3.select(d).selectAll("text");
    dtext
      .text(d3.time.format("%Y")(new Date(dtext.text())))
      .attr("transform","none")
      .attr("y",12)
      .style("text-anchor","middle");
  }
});

//remove x axis label
myChart.axes[0].titleShape.remove()

myChart.svg.append("text")
  .attr("id","charttitle")
  .attr("x", 0)
  .attr("y", 18)
  .text("US and Global Factors")
  .style("text-anchor","beginning")
  .style("font-size","16px")
  .style("font-family","sans-serif")
myChart.svg.append("text")
  .attr("id","charttitle")
  .attr("x", 0)
  .attr("y", 34)
  .text("source: Asness, Frazzini, Pedersen | Quality Minus Junk (2013)")
  .style("text-anchor","beginning")
  .style("font-size","14px")
  .style("font-family","sans-serif")

</script>
'
)
d1



#### now use the correlation plot
#### from rCharts issue #381
corrmatrix<-cor(na.omit(qmjFactors.xts)) #store corr matrix
# The following steps are generic and can all be placed in a function with some tweaks to customize output 
corrdata=as.data.frame(corrmatrix)
corrdata$Factor1=names(corrdata)
corrdatamelt=melt(corrdata,id="Factor1")
names(corrdatamelt)=c("Factor1","Factor2","CorrelationCoefficient")
corrmatplot = dPlot(
  Factor2 ~ Factor1
  ,z = "CorrelationCoefficient"
  ,data = corrdatamelt
  ,type = 'bubble'
  ,height = 350
  ,width = 500
  ,bounds = list( x = 150, y = 50, width = 330, height = 200)
)
corrmatplot$yAxis ( type= "addCategoryAxis" )
corrmatplot$zAxis (
  type= "addMeasureAxis"
  , outputFormat = "0.5f"
  , overrideMin = -2
  , overrideMax = 2
)
corrmatplot$colorAxis(
  type = "addColorAxis"
  ,colorSeries = 'CorrelationCoefficient'
  ,palette = c('red','white','blue')
  ,outputFormat = "0.2f"
)
#corrmatplot


#now do the bar
corrmatplot$set(type = "bar")
#corrmatplot


#with the bar let's play with tooltips
#first tooltip will just be the number in the center of the bar
corrmatplot$templates$script = 
  "http://timelyportfolio.github.io/rCharts_dimple/chart_tooltip_flexible.html"

#this template is designed to let us define onHover and onLeave
#with thought afterScript but does not work since separate <script> block
#so use chartDiv instead
corrmatplot$setTemplate(
  chartDiv = 
    '
  <div id = "{{chartId}}"></div>
  <script>
  function onHover(e){
  //ugly but it works; if tooltip exists then select otherwise append
  //whole d3 enter, update, exit makes things difficult here
  var custTool = (
    d3.select(e.selectedShape[0][0].parentNode).select("#chartTooltip")[0][0] ? 
    d3.select(e.selectedShape[0][0].parentNode).select("#chartTooltip") :
    d3.select(e.selectedShape[0][0].parentNode).append("text").attr("id","chartTooltip")
  )
  
  custTool
  //reads outputFormat and assumes it exists
  .text(d3.format(opts.colorAxis.outputFormat)(e.selectedShape.data()[0].cValue))
  //turn dispay on with css since none from leave
  .style("display",null)
  .style("pointer-events","none")
  .style("font-size",10)
  //use x and y from selected rectangle to position with transform for center
  .attr("x",e.selectedShape.attr("x"))
  .attr("y",e.selectedShape.attr("y"))
  //move to center
  .attr("transform",
  "translate(" + e.selectedShape.attr("width") / 2 + "," + e.selectedShape.attr("height")/2 + ")"
  )
  .attr("dy",4)
  .style("text-anchor","middle")
  }
  function onLeave(e){
    myChart.svg.select("#chartTooltip")
    .style("display","none");
  }
  </script>
  '
,
afterScript = '
<script>
  myChart.svg.append("text")
    .attr("id","charttitle")
    .attr("x", 0)
    .attr("y", 18)
    .text("US and Global Factors Correlations")
    .style("text-anchor","beginning")
    .style("font-size","16px")
    .style("font-family","sans-serif")
  myChart.svg.append("text")
    .attr("id","charttitle")
    .attr("x", 0)
    .attr("y", 34)
    .text("source: Asness, Frazzini, Pedersen | Quality Minus Junk (2013)")
    .style("text-anchor","beginning")
    .style("font-size","14px")
    .style("font-family","sans-serif")
</script>'
)
corrmatplot


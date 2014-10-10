library(htmltools)
library(pipeR)
library(jsonlite)
library(Quandl)
library(xts)

# use Quandl Kenneth French Fama/French factors
# http://www.quandl.com/KFRENCH/FACTORS_D
#f <- Quandl("KFRENCH/FACTORS_D",type = "xts", start_date="2010-12-31") / 100
      
tagList(
  tags$h1( "Sparsest Test in Javascript of Ekholm")
  , tags$div( style = "width:100%"
    ,tags$div(
      style = "display:inline-block; width: 25%;"
      ,"Note: Date range currently limited to one year, but there is a fairly easy workaround
      for the next version."
      ,tags$br()
      ,tags$br()
      ,"Mutual Fund Symbol", tags$input( id = "mfsymbol" )
      ,tags$br()
      ,"Start Date", tags$input( type = "date", id = "stdate" )
      ,tags$br()
      ,"End Date", tags$input( type = "date", id= "enddate" )
      ,tags$br()
      ,tags$input(
        type="submit", id = "calc", value = "Calculate"
      )
      ,tags$br()
    )
    , tags$div(style = "display:inline-block;height:100%;width:50%;"
      , tags$textarea(id = "results", style="width:100%; height:200px")
    )
  )
  ,tags$script(sprintf(
'
  var french = %s;
' 
  , toJSON(data.frame("Date"=index(f),f)) %>>% HTML
  ))
  ,tags$script(
'
    function calculateEkholm( data ) { // data in form of x,y or fund-rf, mkt-rf
       /* get an error with regression.js
       var myReg = regression(
         "linear",
         data
       )
       */
         
         // so use the great simple-statistics library
       var myReg = ss.linear_regression().data(data);
       
       //get residuals
       var resid = data.map(function(p){return myReg.line()(p[0]) - p[1]});
       
       //regress residuals^2 on (mkt-rf)^2
       var myReg2 = ss.linear_regression().data(
         data.map(function(d,i){
           return [ Math.pow(d[0],2), Math.pow(resid[i],2) ]
         })
       )
       //coefficients ^ 1/2 will give us ActiveAlpha and ActiveBeta
       var activeAlpha = Math.pow( myReg2.b(), 0.5 );
       var activeBeta = Math.pow( myReg2.m(), 0.5 );
       
       //now do the next step to get ActiveShare and SelectionShare
       var selectionShare = Math.pow(activeAlpha, 2 ) / ( ss.variance(data.map(function(d){return d[1]})) * (data.length - 1) / data.length )
       var timingShare = Math.pow(activeBeta, 2 ) * ss.mean( data.map(function(d){return Math.pow(d[0],2)}) ) / ( ss.variance(data.map(function(d){return d[1]})) * (data.length - 1) / data.length )
       
       //pass correlation result also
       var correlation = ss.sample_correlation(data.map(function(d){return d[0]}),data.map(function(d){return d[1]}));
       
       return { 
         regression: myReg,
         correlation: correlation,
         activeAlpha: activeAlpha,
         activeBeta: activeBeta,
         selectionShare: selectionShare,
         timingShare: timingShare
       }
    }
    

  // thanks https://gist.github.com/fincluster/6145995
   function getStock(opts, type, complete) {
        var defs = {
            desc: false,
            baseURL: "http://query.yahooapis.com/v1/public/yql?q=",
            query: {
                quotes: \'select * from yahoo.finance.quotes where symbol = \"{stock}\" | sort(field=\"{sortBy}\", descending=\"{desc}\")\',
                historicaldata: \'select * from yahoo.finance.historicaldata where symbol = \"{stock}\" and startDate = \"{startDate}\" and endDate = \"{endDate}\"\'
            },
            suffixURL: {
                quotes: "&env=store://datatables.org/alltableswithkeys&format=json&callback=?",
                historicaldata: "&env=store://datatables.org/alltableswithkeys&format=json"
            }
        };
 
        opts = opts || {};
 
        if (!opts.stock) {
            complete("No stock defined");
            return;
        }
 
        var query = defs.query[type]
          .replace("{stock}", opts.stock)
          .replace("{sortBy}", defs.sortBy)
          .replace("{desc}", defs.desc)
          .replace("{startDate}", opts.startDate)
          .replace("{endDate}", opts.endDate)
 
        var url = defs.baseURL + query + (defs.suffixURL[type] || "");
        
        return url;
    }
    
  
    d3.select("#calc").on("click",function(){
      calculateFund(
        d3.select("#mfsymbol")[0][0].value,
        d3.select("#stdate")[0][0].value,
        d3.select("#enddate")[0][0].value
      )
    })
    
    function calculateFund( symbol, startdate, enddate ) {
      
      d3.json(getStock({stock:symbol.toUpperCase(),startDate:startdate,endDate:enddate},"historicaldata"), function(e1,fund){
      
        
          if( e1 || !fund.query.results ) {
            updateResults ( {e1:e1, e2:e2, queryresults: "query problems"} );
          } else {
            var fund_factor = [];
          
            //manipulate data to join fund with factors
            //would be nice to have a xts merge in javascript
            
                
            // query.results.quote will have the data stripped of meta
            // also we will sort date ascending
            fund = fund.query.results.quote
                .sort(function(a,b){ return d3.ascending( new Date(a.Date), new Date(b.Date) ) } );
            
            
            
            
            // now lets go period by period with fund.map
            fund.map( function(per, i){
              if( i > 0 ) {
                var frenchThisPer = french.filter(function(d){return d.Date == per.Date})[0];
                fund_factor.push([
                  //Date: per.Date,
                  //FundPrice: 
                  per.Adj_Close / fund[ i - 1 ].Adj_Close - 1 - frenchThisPer["RF"]/100,
                  //Rm_Rf:
                  +frenchThisPer["Mkt.RF"]/100//,
                  //Rf: +frenchThisPer["RF"]/100
                ])
              }
            })
            
            updateResults( calculateEkholm( fund_factor ) );
          }
          
      })
    }
    
    function updateResults( ekholmCalc ){
      var ekhArr = [];
      Object.keys(ekholmCalc).map(function(k){
        ekhArr.push( [ k,": ", ekholmCalc[k] ].join("") )
      })
      d3.select("#results").text(ekhArr.join("\\n"))
    }
 ' %>>% HTML )
) %>>%
  attachDependencies(
    list(
      htmlDependency(
        name="d3"
        ,version="3.4"
        ,src=c("href"="http://d3js.org/")
        ,script="d3.v3.min.js"
      )
      ,htmlDependency(
        name="simple_statistics"
        ,version="0.1"
        ,src=c("href"=
           "http://timelyportfolio.github.io/rCharts_factor_analytics/js"
        )
        ,script = "simple_statistics.js"
      )
    )
  ) %>>% html_print
---
title: Replicate Paper with R and rCharts
author: Timely Portfolio
github: {user: timelyportfolio, repo: rCharts_factor_analytics, branch: "gh-pages"}
framework: bootplus
layout: post
mode: selfcontained
highlighter: prettify
hitheme: twitter-bootstrap
lead : >
  Fact, Fiction and Momentum Investing
assets:
  jshead:
    - http://d3js.org/d3.v3.min.js
    - http://dimplejs.org/dist/dimple.v2.0.0.min.js
  css:
    - "http://fonts.googleapis.com/css?family=Raleway:300"
    - "http://fonts.googleapis.com/css?family=Oxygen"    
---

# New Working Paper on Momentum

<style>
body{
  font-family: 'Oxygen', sans-serif;
  font-size: 15px;
  line-height: 22px;
}

h1,h2,h3,h4 {
  font-family: 'Raleway', sans-serif;
}

.tooltip{
  opacity:1 !important
}
</style>



It seems that [Gary Antonacci from Optimal Momentum](http://optimalmomentum.blogspot.com/2014/06/fact-fiction-and-momentum-investing.html) and I were simultaneously enjoying this fine working paper on momentum.

<blockquote>
<strong>Fact, Fiction and Momentum Investing</strong><br>
Asness, Clifford S. and Frazzini, Andrea and Israel, Ronen and Moskowitz, Tobias J.<br>
available at <a href="http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2435323">SSRN</a><br>
May 9, 2014<br>
</blockquote>

I have had it in an open Chrome tab since the day it was posted to SSRN. Even after reading a couple of times, I left it open intending to attempt a replication in R since the data was available, the math was understandable, and the topic was interesing.  Beyond a simple replication, I also wanted to add some [rCharts](http://rcharts.io) and [slidify](http://slidify.io).  Below is a fairly complete replication of myths 1 - 3.  I have chosen `echo = F` to hide most of the code.  If you would like to see the code and replicate for yourself, please see the [Github repository](https://github.com/timelyportfolio/rCharts_factor_analytics/).


---
### Data Source

Once again this amazing resource [Kenneth French Data Library](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html) will be our data source. We will use the monthly data files.  My code to retrieve these is ugly.  Feel free to functionalize it if you have the desire.




---
### Table 1

For Table 1 the authors try to dispel the notion

<blockquote>
Myth #1: Momentum returns are too “small and sporadic”.
</blockquote>

Instead of a table I thought some [rCharts](http://rcharts.io) + [dimplejs](http://dimplejs.org) for an interactive table might help visualize the annualized returns and Sharpe ratios of each of the four factors.

<iframe src=' assets/fig/unnamed-chunk-3.html ' scrolling='no' frameBorder='0' seamless class='rChart dimple ' id=iframe- chart1ddc8a11780 ></iframe> <style>iframe.rChart{ width: 100%; height: 400px;}</style>

Combining Sharpe and return on the same chart is not ideal, but I thought it would demonstrate some of the power of dimplejs.  A facetted approach or separate (small-multiples) charts here would work much better since the scales are so different.

---
### Table 2



For Table 2 which also seeks to refute the "small and sporadic" Myth #1, we can employ the [Gmisc](http://gforge.se/gmisc/) package to easily produce a fairly good looking HTML table.

<table class='gmisc_table' style='border-collapse: collapse;' >
	<thead>
	<tr><td colspan='5' style='text-align: left;'>
	Table 2: Probability of 1 Year Positive Returns</td></tr>
	<tr>
		<th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 4px double grey;'>Sample</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>Mkt.RF</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>SMB</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>HML</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>Mom</th>
	</tr>
	</thead><tbody>
	<tr>
		<td style='text-align: center;'>1927::2013</td>
		<td style='text-align: right;'>70.96</td>
		<td style='text-align: right;'>58.08</td>
		<td style='text-align: right;'>63.41</td>
		<td style='text-align: right;'>80.54</td>
	</tr>
	<tr>
		<td style='text-align: center;'>1963::2013</td>
		<td style='text-align: right;'>71.88</td>
		<td style='text-align: right;'>60.07</td>
		<td style='text-align: right;'>64.23</td>
		<td style='text-align: right;'>80.03</td>
	</tr>
	<tr>
		<td style='border-bottom: 1px solid grey; text-align: center;'>1991::2013</td>
		<td style='border-bottom: 1px solid grey; text-align: right;'>78.11</td>
		<td style='border-bottom: 1px solid grey; text-align: right;'>61.89</td>
		<td style='border-bottom: 1px solid grey; text-align: right;'>61.13</td>
		<td style='border-bottom: 1px solid grey; text-align: right;'>75.85</td>
	</tr>
	</tbody>
</table>
<br>
I will not produce a chart for every table, but I feel like a couple more will not hurt anything.  As in the previous chart, this will use rCharts and dimplejs.

<iframe src=' assets/fig/unnamed-chunk-6.html ' scrolling='no' frameBorder='0' seamless class='rChart dimple ' id=iframe- chart1ddc1ad34985 ></iframe> <style>iframe.rChart{ width: 100%; height: 400px;}</style>

Now for the right half of Table 2 which shows the probability of 5 year positive returns, let's do the same thing with a table and then a chart.

<table class='gmisc_table' style='border-collapse: collapse;' >
	<thead>
	<tr><td colspan='5' style='text-align: left;'>
	Table 2: Probability of 5 Year Positive Returns</td></tr>
	<tr>
		<th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 4px double grey;'>Sample</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>Mkt.RF</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>SMB</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>HML</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>Mom</th>
	</tr>
	</thead><tbody>
	<tr>
		<td style='text-align: center;'>1927::2013</td>
		<td style='text-align: right;'>81.73</td>
		<td style='text-align: right;'>64.97</td>
		<td style='text-align: right;'>88.53</td>
		<td style='text-align: right;'>87.61</td>
	</tr>
	<tr>
		<td style='text-align: center;'>1963::2013</td>
		<td style='text-align: right;'>77.03</td>
		<td style='text-align: right;'>65.1</td>
		<td style='text-align: right;'>87.52</td>
		<td style='text-align: right;'>88.79</td>
	</tr>
	<tr>
		<td style='border-bottom: 1px solid grey; text-align: center;'>1991::2013</td>
		<td style='border-bottom: 1px solid grey; text-align: right;'>73.27</td>
		<td style='border-bottom: 1px solid grey; text-align: right;'>74.65</td>
		<td style='border-bottom: 1px solid grey; text-align: right;'>74.19</td>
		<td style='border-bottom: 1px solid grey; text-align: right;'>71.43</td>
	</tr>
	</tbody>
</table>

<br>
I have been using `iframe` mode with rCharts, but for example purposes, I will show this next chart with `inline`. `inline` can be cleaner but with multiple charts and especially multiple libraries can be problematic with conflicts.


<div id = 'chart1ddc695a2b3' class = 'rChart dimple'></div>
<script type="text/javascript">
  var opts = {
 "dom": "chart1ddc695a2b3",
"width":    800,
"height":    350,
"xAxis": {
 "type": "addCategoryAxis",
"showPercent": false,
"orderRule": "Sample" 
},
"yAxis": {
 "type": "addMeasureAxis",
"showPercent": false,
"outputFormat": ".1%" 
},
"zAxis": [],
"colorAxis": [],
"defaultColors": d3.scale.ordinal().range(['#196396','#d56a0b','#9d9e1c','#139ead']).domain(['SMB','Mom','HML','Mkt.RF']),
"layers": [],
"legend": [],
"x": [ "Sample", "factor" ],
"y": "value",
"groups": [ "Sample", "factor" ],
"type": "bar",
"id": "chart1ddc695a2b3" 
},
    data = [{"Sample":"1927::2013","factor":"Mkt.RF","value":0.709583736689255},{"Sample":"1963::2013","factor":"Mkt.RF","value":0.718801996672213},{"Sample":"1991::2013","factor":"Mkt.RF","value":0.781132075471698},{"Sample":"1927::2013","factor":"SMB","value":0.580832526621491},{"Sample":"1963::2013","factor":"SMB","value":0.600665557404326},{"Sample":"1991::2013","factor":"SMB","value":0.618867924528302},{"Sample":"1927::2013","factor":"HML","value":0.634075508228461},{"Sample":"1963::2013","factor":"HML","value":0.642262895174709},{"Sample":"1991::2013","factor":"HML","value":0.611320754716981},{"Sample":"1927::2013","factor":"Mom","value":0.805421103581801},{"Sample":"1963::2013","factor":"Mom","value":0.800332778702163},{"Sample":"1991::2013","factor":"Mom","value":0.758490566037736}];
  var svg = dimple.newSvg("#" + opts.id, opts.width, opts.height);

  //data = dimple.filterData(data, "Owner", ["Aperture", "Black Mesa"])
  var myChart = new dimple.chart(svg, data);
  if (opts.bounds) {
    myChart.setBounds(opts.bounds.x, opts.bounds.y, opts.bounds.width, opts.bounds.height);//myChart.setBounds(80, 30, 480, 330);
  }
  //dimple allows use of custom CSS with noFormats
  if(opts.noFormats) { myChart.noFormats = opts.noFormats; };
  //for markimekko and addAxis also have third parameter measure
  //so need to evaluate if measure provided
  
  //function to build axes
  function buildAxis(position,layer){
    var axis;
    var axisopts = opts[position+"Axis"];
    
    if(axisopts.measure) {
      axis = myChart[axisopts.type](position,layer[position],axisopts.measure);
    } else {
      axis = myChart[axisopts.type](position, layer[position]);
    };
    if(!(axisopts.type === "addPctAxis")) axis.showPercent = axisopts.showPercent;
    if (axisopts.orderRule) axis.addOrderRule(axisopts.orderRule);
    if (axisopts.grouporderRule) axis.addGroupOrderRule(axisopts.grouporderRule);  
    if (axisopts.overrideMin) axis.overrideMin = axisopts.overrideMin;
    if (axisopts.overrideMax) axis.overrideMax = axisopts.overrideMax;
    if (axisopts.overrideMax) axis.overrideMax = axisopts.overrideMax;
    if (axisopts.inputFormat) axis.dateParseFormat = axisopts.inputFormat;
    if (axisopts.outputFormat) axis.tickFormat = axisopts.outputFormat;    
    return axis;
  };
  
  var c = null;
  if(d3.keys(opts.colorAxis).length > 0) {
    c = myChart[opts.colorAxis.type](opts.colorAxis.colorSeries,opts.colorAxis.palette) ;
    if(opts.colorAxis.outputFormat){
      c.tickFormat = opts.colorAxis.outputFormat;
    }
  }
  
  //allow manipulation of default colors to use with dimple
  if(opts.defaultColors.length) {
    defaultColorsArray = [];
    if (typeof(opts.defaultColors) == "function") {
      //assume this is a d3 scale
      //if there is a domain for the color scale given
      //then we will need to assign colors with dimples assignColor
      if( opts.defaultColors.domain().length > 0 ){
        defaultColorsArray = opts.defaultColors.range();
        opts.defaultColors.domain().forEach( function( d, i ) {
          myChart.assignColor( d, opts.defaultColors.range()[i] )
        })
      } else {
        for (var n=0;n<opts.defaultColors.range().length;n++) {
          defaultColorsArray.push(opts.defaultColors(n));
        };
      }
    } else {
      defaultColorsArray = opts.defaultColors;
    }

    myChart.defaultColors = defaultColorsArray.map(function(d) {
      return new dimple.color(d);
    });
  }  
  
  //do series
  //set up a function since same for each
  //as of now we have x,y,groups,data,type in opts for primary layer
  //and other layers reside in opts.layers
  function buildSeries(layer, hidden){
    //inherit from primary layer if not intentionally changed or xAxis, yAxis, zAxis null
    if (!layer.xAxis) layer.xAxis = opts.xAxis;    
    if (!layer.yAxis) layer.yAxis = opts.yAxis;
    if (!layer.zAxis) layer.zAxis = opts.zAxis;
    
    var x = buildAxis("x", layer);
    x.hidden = hidden;
    
    var y = buildAxis("y", layer);
    y.hidden = hidden;
    
    //z for bubbles
    var z = null;
    if (!(typeof(layer.zAxis) === 'undefined') && layer.zAxis.type){
      z = buildAxis("z", layer);
    };
    
    //here think I need to evaluate group and if missing do null
    //as the group argument
    //if provided need to use groups from layer
    var s = new dimple.series(myChart, null, x, y, z, c, dimple.plot[layer.type], dimple.aggregateMethod.avg, dimple.plot[layer.type].stacked);
    
    //as of v1.1.4 dimple can use different dataset for each series
    if(layer.data){
      //convert to an array of objects
      var tempdata;
      //avoid lodash for now
      datakeys = d3.keys(layer.data)
      tempdata = layer.data[datakeys[1]].map(function(d,i){
        var tempobj = {}
        datakeys.forEach(function(key){
          tempobj[key] = layer.data[key][i]
        })
        return tempobj
      })
      s.data = tempdata;
    }
    
    if(layer.hasOwnProperty("groups")) {
      s.categoryFields = (typeof layer.groups === "object") ? layer.groups : [layer.groups];
      //series offers an aggregate method that we will also need to check if available
      //options available are avg, count, max, min, sum
    }
    if (!(typeof(layer.aggregate) === 'undefined')) {
      s.aggregate = eval(layer.aggregate);
    }
    if (!(typeof(layer.lineWeight) === 'undefined')) {
      s.lineWeight = eval(layer.lineWeight);
    }
    if (!(typeof(layer.barGap) === 'undefined')) {
      s.barGap = eval(layer.barGap);
    }    
  
   /* if (!(typeof(layer.eventHandler) === 'undefined')) {
      layer.eventHandler = (layer.eventHandler.length === "undefined") ? layer.eventHandler : [layer.eventHandler];
      layer.eventHandler.forEach(function(evt){
        s.addEventHandler(evt.event, eval(evt.handler))
      })
    }*/
      
    myChart.series.push(s);
    
    /*placeholder fix domain of primary scale for new series data
    //not working right now but something like this
    //for now just use overrideMin and overrideMax from rCharts
    for( var i = 0; i<2; i++) {
      if (!myChart.axes[i].overrideMin) {
        myChart.series[0]._axisBounds(i==0?"x":"y").min = myChart.series[0]._axisBounds(i==0?"x":"y").min < s._axisBounds(i==0?"x":"y").min ? myChart.series[0]._axisBounds(i==0?"x":"y").min : s._axisBounds(i==0?"x":"y").min;
      }
      if (!myChart.axes[i].overrideMax) {  
        myChart.series[0]._axisBounds(i==0?"x":"y")._max = myChart.series[0]._axisBounds(i==0?"x":"y").max > s._axisBounds(i==0?"x":"y").max ? myChart.series[0]._axisBounds(i==0?"x":"y").max : s._axisBounds(i==0?"x":"y").max;
      }
      myChart.axes[i]._update();
    }
    */
    
    return s;
  };
  
  buildSeries(opts, false);
  if (opts.layers.length > 0) {
    opts.layers.forEach(function(layer){
      buildSeries(layer, true);
    })
  }
  //unsure if this is best but if legend is provided (not empty) then evaluate
  if(d3.keys(opts.legend).length > 0) {
    var l =myChart.addLegend();
    d3.keys(opts.legend).forEach(function(d){
      l[d] = opts.legend[d];
    });
  }
  //quick way to get this going but need to make this cleaner
  if(opts.storyboard) {
    myChart.setStoryboard(opts.storyboard);
  };
  myChart.draw();

</script>


---
### Table 3

Table 3 offers even more proof for the arguments against "small and sporadic".  It blends pieces of Table 2 with the Sharpe Ratio and probability of positive returns on a portfolio of 60% HML (Value) and 40% UMD (Momentum).  This simple portfolio produces very solid results and inspires me to further my exploration of the rebalancing concept introduced in my post [Unsolved Mysteries of Rebalancing](http://timelyportfolio.blogspot.com/2014/02/unsolved-mysteries-of-rebalancing.html).

<table class='gmisc_table' style='border-collapse: collapse;' >
	<thead>
	<tr><td colspan='6' style='text-align: left;'>
	Table 3: 1927 - 2013</td></tr>
	<tr>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey;'>&nbsp;</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>Mkt.RF</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>SMB</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>HML</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>Mom</th>
		<th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center;'>HML60_UMB40</th>
	</tr>
	</thead><tbody>
	<tr>
		<td style='text-align: left;'>Sharpe Ratios</td>
		<td style='text-align: center;'>0.41</td>
		<td style='text-align: center;'>0.26</td>
		<td style='text-align: center;'>0.39</td>
		<td style='text-align: center;'>0.50</td>
		<td style='text-align: center;'>0.80</td>
	</tr>
	<tr>
		<td style='text-align: left;'>% Positive, 1-year Rolling</td>
		<td style='text-align: center;'>0.71</td>
		<td style='text-align: center;'>0.58</td>
		<td style='text-align: center;'>0.63</td>
		<td style='text-align: center;'>0.81</td>
		<td style='text-align: center;'>0.81</td>
	</tr>
	<tr>
		<td style='border-bottom: 1px solid grey; text-align: left;'>% Positive, 5-year Rolling</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>0.82</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>0.65</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>0.89</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>0.88</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>0.92</td>
	</tr>
	</tbody>
</table>

---
### More French Data

We will need to pull in some more French data to work through the next set of tables.  This data [6 Portfolios Formed Monthly on Size (3) x Momentum (2)](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_6_port_form_sz_pr_12_2.html) will allow us to decompose the momentum factor into short and long buckets and also small and large size buckets.



---
### Table 4

Now that we have the additional French data, we can replicate the authors` attempt to disprove

<blockquote>
Myth #2: Momentum cannot be captured by long-only investors as “momentum can only be exploited on the short side”.
</blockquote>

My numbers differ slightly on the market-adjusted returns (left half), but are close enough to think that the approach is the same.


<table class='gmisc_table' style='border-collapse: collapse;' >
	<thead>
	<tr><td colspan='10' style='text-align: left;'>
	Table 4</td></tr>
	<tr>
		<th style='border-top: 4px double grey;'></th>
		<th colspan='4' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 4px double grey;'>UMD market-adjusted returns</th><th style='border-top: 4px double grey;; border-bottom: hidden;'>&nbsp;</th>
		<th colspan='4' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 4px double grey;'>UMD returns minus market</th>
	</tr>
	<tr>
		<th style='font-weight: 900; border-bottom: 1px solid grey; '>Sample</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>Short Side</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>Long Side</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>UMD</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>% Long</th>
		<th style='border-bottom: 1px solid grey;' colspan='1'>&nbsp;</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>Short Side</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>Long Side</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>UMD</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>% Long</th>
	</tr>
	</thead><tbody>
	<tr>
		<td style='text-align: center;'>1927::2013</td>
		<td style='text-align: center;'>4.98</td>
		<td style='text-align: center;'>5.62</td>
		<td style='text-align: center;'>10.59</td>
		<td style='text-align: center;'>53.0</td>
		<td style='' colspan='1'>&nbsp;</td>
		<td style='text-align: center;'>2.19</td>
		<td style='text-align: center;'>6.09</td>
		<td style='text-align: center;'>8.28</td>
		<td style='text-align: center;'>73.6</td>
	</tr>
	<tr>
		<td style='text-align: center;'>1963::2013</td>
		<td style='text-align: center;'>3.75</td>
		<td style='text-align: center;'>5.43</td>
		<td style='text-align: center;'> 9.18</td>
		<td style='text-align: center;'>59.1</td>
		<td style='' colspan='1'>&nbsp;</td>
		<td style='text-align: center;'>2.45</td>
		<td style='text-align: center;'>5.92</td>
		<td style='text-align: center;'>8.38</td>
		<td style='text-align: center;'>70.7</td>
	</tr>
	<tr>
		<td style='border-bottom: 1px solid grey; text-align: center;'>1991::2013</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>3.74</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>4.96</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'> 8.70</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>57.0</td>
		<td style='border-bottom: 1px solid grey;' colspan='1'>&nbsp;</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>1.05</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>5.24</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>6.29</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>83.2</td>
	</tr>
	</tbody>
</table>

---
### Table 5

The last myth that we will tackle here, but only the 3rd of 10 myths in the paper, is

<blockquote>
Myth #3: Momentum is much stronger among small cap stocks than large caps.
</blockquote>

We will use the same French data (HML (3) x SMB (2)) from Table 4 to replicate Table 5.  I will leave the right half (Value) of this table as homework.


<table class='gmisc_table' style='border-collapse: collapse;' >
	<thead>
	<tr><td colspan='4' style='text-align: left;'>
	Table 5</td></tr>
	<tr>
		<th style='border-top: 4px double grey;'></th>
		<th colspan='3' style='font-weight: 900; border-bottom: 1px solid grey; border-top: 4px double grey;'>Momentum</th>
	</tr>
	<tr>
		<th style='font-weight: 900; border-bottom: 1px solid grey; '>Sample</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>UMD Small</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>UMD Big</th>
		<th style='border-bottom: 1px solid grey; text-align: center;'>Mom</th>
	</tr>
	</thead><tbody>
	<tr>
		<td style='text-align: left;'>1927::2013</td>
		<td style='text-align: center;'> 9.75</td>
		<td style='text-align: center;'>6.81</td>
		<td style='text-align: center;'>8.28</td>
	</tr>
	<tr>
		<td style='text-align: left;'>1963::2013</td>
		<td style='text-align: center;'>11.26</td>
		<td style='text-align: center;'>5.50</td>
		<td style='text-align: center;'>8.38</td>
	</tr>
	<tr>
		<td style='border-bottom: 1px solid grey; text-align: left;'>1991::2013</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'> 8.10</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>4.49</td>
		<td style='border-bottom: 1px solid grey; text-align: center;'>6.30</td>
	</tr>
	</tbody>
</table>

---
### Thanks
As I hope you can tell, this post was more a function of the efforts of others than of my own.

Thanks specifically:
- [Kenneth French](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/) for his very generous [data library](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)
- [the AQR research factory](http://www.aqr.com/) for this and all their other research
- [Ramnath Vaidyanathan](http://ramnathv.github.io/) for [rCharts](http://rcharts.io/site) and [slidify](http://slidify.org).
- [John Kiernander](https://twitter.com/jkiernander) for [dimplejs](http://dimplejs.org)
- Nameless Fixed Income Shop for the original chart.
- [Mike Bostock](http://bost.ocks.org/mike/) for everything.
- [Marcello Palmitessa](http://aozora.github.io/bootplus/) for the Bootplus framework.
- Google fonts [Raleway](http://www.google.com/fonts/specimen/Raleway) and [Oxygen](http://www.google.com/fonts/specimen/Oxygen)

// Turning pages and elements on or off

function toggleContent1() {
    // Get the DOM reference
    var contentId = document.getElementById("getcat1Data");
    // Toggle 
    contentId.style.display == "block" ? contentId.style.display = "none" : 
      contentId.style.display = "block"; 
}

// functions to provide same utilities as R does
  
function cut(input, dividers) {
	var i;
	dividers = dividers.sort(function(a,b){return a - b;});
	if (input < dividers[0] | input > dividers[dividers.length - 1]) {
		return (NaN);
	}
	for ( i = 1; i < dividers.length; i++) {
		if (input <= dividers[i]) {
			return (i - 1);
			break;
		}
	}
	return (i - 1);  // index to the category split by dividers
}
   
 
function rle(x) {
	var i,
	    current = 0,
	    label = [],
	    run = [];
	label[0] = x[0];
	run[0] = 1;
	for ( i = 1; i < x.length; i++) {
		if (x[i] === x[i - 1]) {
			run[current] += 1;
		} else {
			current += 1;
			label[current] = x[i];
			run[current] = 1;
		}
	}
	return [{
		"runs" : run
	}, {
		"labels" : label
	}];
}


function rgeom(prob) {
	return Math.ceil(Math.log(Math.random()) / Math.log(1 - prob));
}

function rbern(reps, prob){
	var i, 
		results = [];
	for(i=0; i< reps; i++){
		results.push(Math.random() > prob? 0: 1)
	}
	return results;
}

function rbinom(n, prob, times) {
	//return a random count of number of successes in n trials with prob = prob of success for each
	// make a matrix of Bernoullis with n rows, times columns, ans sum over rows
    var i,
    	results = [];
    for( i =0; i < times; i++){
    	results.push(d3.sum( rbern(n, prob)));
    }	
	return results;
}

function repeat(x, n){
	var out = [];
	for(i=0; i<n; i++){
		out.push(x);
	}
	return out;
}

function sequence(start, stop, inc){
	var i, out = [];
	for(i =start; i <= stop; i += inc){
		out.push(i);
	}
	return out;
}



function resample1Mean(values, nreps){
	//take resamples of values with replacement (nreps times), return the mean of each
	var cumProb = [],
	    nVals = values.length,
	    prob = repeat(1/nVals, nVals),
	    i, j, k,
	    out = [],
	    resamples = [],
	    totalProb = 1;
	cumProb = jStat.cumsum(prob);
	cumProb.unshift(0);
	for ( i = 0; i < nreps; i++) {
		resamples = [];
		for(j=0; j < nVals; j++){
			k = cut(Math.random(), cumProb);
			resamples.push( values[k ] );
		}
		out[i] = d3.mean(resamples);
	}
	//console.log(out);
	// return the vector of means
	return out ;
}

function sample1(nItems) {
	// draw  one value assuming each is equally likely
	return Math.floor(Math.random() * nItems);
}


function sampleWOrep(values, nreps){
  var i, k, len = values.length, 
	  ids = [],
	  out = [];
 var  seq1 = sequence(0, len - 1, 1);	  
	  nreps = Math.min(nreps, len); // can't draw more than the number of values
	for ( i = 0; i < nreps; i++) {
		k = sample1(seq1.length);
		//console.log(k, seq1[k]);
		ids.push(seq1[k]);
		out.push( values[seq1[k]] );
		seq1.splice(k , 1);    // remove kth element and repeat as needed
		//console.log(seq1);
	}
	//console.log(out);
	// return the values in sampled order and their ids or positions in the original list
	return [out, ids];  
}

function sampleN(values, nreps){
	// sample nreps with replacement from values assuming equal weights
  var i, k, 
  	len = values.length,
	out = [];
	
	for ( i = 0; i < nreps; i++) {
		k = Math.floor(Math.random() * len);
		out.push( values[k] );
	}
	//console.log(out);
	return out;  
}


function sampleWrep(values, nreps, prob) {
	// draw  values  (with replacement) at random using probs as weights
	var cumProb = [],
	    nCat = prob.length,
	    totalProb = jStat.sum(prob),
	    i, k,
	    ids = [],
	    out = [];
	stdize = function(x) {
		return x / totalProb;
	};
	prob = jStat.map(prob, stdize);
	cumProb = jStat.cumsum(prob);
	cumProb.unshift(0);
	//console.log(cumProb);
	for ( i = 0; i < nreps; i++) {
		k = cut(Math.random(), cumProb);
		out.push( values[k ] );
		ids.push(k);
	}
	//console.log(out);
	// return the values in sampled order and their ids or positions in the original list
	return [out, ids];
}

 function inArray(array, value){
 	var i = array.length;
 	while(i--){
 		if (array[i] === value){ return true;}
 	}
 	return false;
 }
 
  function indexOfXY(array, X, Y){
  	var  xIndx=[], yIndx=[];
  	xIndx = array.findIndex(i => i.x === X);
  	yIndx = array.findIndex(i => i.y === Y);
  	if(xIndx.length == 1){	return xIndx;
  	} else if (yIndx.length == 1){	return yIndx;
  	}  // X,Y pairs are unique
  	return yIndx.filter(function(d) {return inArray(xIndx, d);} );
  }
 
  function sturgesFormula(arr){
  	// number of bins for a histogram
    var n = arr.length;
    var k = Math.ceil((Math.log(n)/Math.log(2))+1);
    var h = (d3.max(arr) - d3.min(arr)) / k;  // length of each bin
    return {"binCount":k, "binLength":h};
  };

 function formatPvalue(extremeCount, reps){
 	if(extremeCount === 0){
 		return("Less than 1/"+ reps);
 	} else{
 		return ((extremeCount/reps).toPrecision(4));
 	}
 }    
//
//  Need a generic plotting function for dotcharts.
//

function histogram(sample, svgObject, interactFunction){
	// stacks dots up creating integer y values (1, 2, 3,...) for each unique x value
	// builds a d3 svg plot in the svg object which will respond to a mouse-click by calling
	//  interactFunction on that point
	// input: sample is of length 2 containing (1) x values and (2) color indices
	// returns: Dots (svg objects) and the original sample
	var circleColors = ["steelblue","red"],
		color = [],
		j = 0,
		margin = 40,
		myArray =[],
	    nN = sample.length,
	    leftX,
	    ypos = 0,
	    radii,
	    xbinWidth,
	    xmin,
	    xmax ,
	    wdth = 440 - margin * 2,
	    hght = 320 - margin * 2;
	
        if(svgObject.getAttribute("width")>50){
	     wdth= svgObject.getAttribute("width") - margin * 2;
	     hght = svgObject.getAttribute("height") - margin * 2;
	}
	if (nN === 2){
		color = sample[1];
		sample = sample[0];
		nN = sample.length;
	}    
	sample.sort(function(a,b){return a - b}) ;   
          // numeric sort to build bins for y values
          // start on left with smallest x.	
	
	    xmin = sample[0];
	     xmin *= (sample[0] <= 0)? 1.01: 0.99;
	    xmax = sample[nN-1] ;
	     xmax *= (sample[nN-1] >= 0)? 1.01: 0.99;
	    //console.log([xmin,sample[0], sample[nN-1], xmax])
	
	
	var radii = (nN < 101)? 10:
	    (nN < 501)? 7:
	    (nN < 1001)? 5:
	    (nN < 5001)? 4: 3; // perhaps this should relate to width/height of svg]
    var gees = d3.select(svgObject).selectAll("g");
	if(typeof(gees) === "object"){
		gees.remove();
	}
	xbinWidth = (xmax - xmin) /(wdth / radii); //sturgesFormula(sample).binLength;  
	 // console.log(xbinWidth); 
	//  first dot goes at y=1, then add one to each from there
	j = 0;
	ypos = 1;	            // y value is a count starting at 1
	leftX = sample[0];	
	sampMax = 1;
    while( j <  nN ){    
	    if( Math.abs(sample[j] - leftX) > xbinWidth ){
			 leftX = sample[j];	    // start a fresh bin with left edge at sample[j] xvalue
			 if(ypos > sampMax){ sampMax = ypos; } // only check max y height at right edge of each bin
	         ypos = 1;
	         //console.log(plotX, ypos);
	    };
	    myArray[j] = {"x": sample[j], "y": ypos++, "color" : circleColors[color[j++]]};
	}
	// console.log(myArray);



   var DCyScale = d3.scaleLinear()
    	.range([hght, 0])
    	.domain([1, sampMax + .5]);

	var DCxScale = d3.scaleLinear()
    	.range([margin, wdth - margin/2])
    	.domain([xmin, xmax]);

	// change scales to hold all x, all y
   var DCxAxis = d3.axisBottom(DCxScale)
      .ticks(5);

   var DCyAxis = d3.axisLeft(DCyScale)
      .ticks(5);
      
  var graph = d3.select(svgObject)
    .attr("width", wdth + margin*2)
    .attr("height", hght + margin*2)
  .append("g")
   .attr("transform", "translate("+ (2 * margin) + "," + margin + ")");
        
    graph.append("g")
      .attr("class", "y axis")
      .call(DCyAxis);
      
      
	graph.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + (radii + hght) +")")
      .call(DCxAxis);
          
  //  graph.append("g")
  //  	.attr("class", "text")
  //  	.attr("x", 20)
  //  	.attr("y", hght + 5)
  //  	.text(xlegend);  
      
   	Dots =  graph.selectAll("g.circle")
            .data(myArray); 
	Dots.enter().append("circle")
            .attr("cx", function(d){ return DCxScale(d.x);} ) 
            .attr("r", radii ) 
            .attr("cy", function(d){ return DCyScale(d.y);} ) 
            .style("fill",function(d){return d.color;})
            .style("fill-opacity", 0.6)
            .on("click", interactFunction );
            
  return [Dots, sample];
}

function discreteChart(sample, svgObject, interactFunction){
	// stacks dots up creating integer y values (1, 2, 3,...) for each unique x value
	// builds a d3 svg plot in the svg object which will respond to a mouse-click by calling
	//  interactFunction on that point
	// returns: Dots (svg objects) and the original sample
	var circleColors = ["steelblue","red"],
		color = [],
		margin = 40,
		myArray =[],
	    nN = sample.length,
	    plotX,
	    ypos =0,
	    xmin,
	    xmax ,
	    wdth = 440 - margin * 2,
	    hght = 320 - margin * 2;
        if(svgObject.getAttribute("width")>50){
	     wdth= svgObject.getAttribute("width") - margin * 2;
	    hght = svgObject.getAttribute("height") - margin * 2;
	}
	if (nN ===2){
		color = sample[1];
		sample = sample[0];
		nN = sample.length;
	}

	sample.sort(function(a,b){return a - b}) ;   
          // numeric sort to build bins for y values
          // start on left with smallest x.	
	
	    xmin = sample[0];
	     xmin *= (sample[0] <= 0)? 1.01: 0.99;
	    xmax = sample[nN-1] ;
	     xmax *= (sample[nN-1] >= 0)? 1.01: 0.99;
	   // console.log([xmin,sample[0], sample[nN-1], xmax])
	
	var radii = (nN < 101)? 10:
	    (nN < 501)? 7:
	    (nN < 1001)? 5:
	    (nN < 5001)? 4: 3; // perhaps this should relate to width/height of svg]
    var gees = d3.select(svgObject).selectAll("g");
	if(typeof(gees) === "object"){
		gees.remove();
	}    
	//  first dot goes at y=0, then add one to each from there
	var j = 0;
	while( j <  nN ){    
	    plotX = sample[j];	    // start a fresh bin with left edge at sample[j]
	    ypos = 0;	            // bin y starts at 0
	    myArray[j] = {"x": sample[j], "y": ypos++, "color" : circleColors[color[j++]]};
        while( (sample[j] === sample[j-1]) & (j <= nN)){
		  //stay in same bin -- increment yposition
		  myArray[j] = {"x": sample[j], "y": ypos++, "color" : circleColors[color[j++]]};
	    };
	     // console.log(x(plotX));
	}
	//myArray[nN-1].color = circleColors[1];
	
	sampMax = d3.max(myArray, function(d) { return d.y;});

   var DCyScale = d3.scaleLinear()
    	.range([hght, 0])
    	.domain([0, sampMax + .5]);

	var DCxScale = d3.scaleLinear()
    	.range([margin, wdth - margin/2])
    	.domain([xmin, xmax]);

	// change scales to hold all x, all y
   var DCxAxis = d3.axisBottom(DCxScale)
      .ticks(5);
      
//   d3.selectAll(".xAxis>.tick>text")
  //	.each(function(d, i){
  //  	d3.select(this).style("font-size","10px");
  //	});   

   var DCyAxis = d3.axisLeft(DCyScale);
      
  var graph = d3.select(svgObject)
    .attr("width", wdth + margin*2)
    .attr("height", hght + margin*2)
  .append("g")
   .attr("transform", "translate("+ (2 * margin) + "," + margin + ")");
    
    
    graph.append("g")
      .attr("class", "y axis")
      .call(DCyAxis);
      
      
	graph.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + (radii + hght) +")")
      .call(DCxAxis);

  //  graph.append("g")
  //  	.attr("class", "text")
  //  	.attr("x", 20)
  //  	.attr("y", hght + 5)
  //  	.text(xlegend);  
      
   	Dots =  graph.selectAll("g.circle")
            .data(myArray); 
	Dots.enter().append("circle")
            .attr("cx", function(d){ return DCxScale(d.x);} ) 
            .attr("r", radii ) 
            .attr("cy", function(d){ return DCyScale(d.y);} ) 
            .style("fill", function(d){return d.color;})
            .style("fill-opacity", 0.6)
            .on("click", interactFunction ); 
    return [Dots, sample];
}


function ciColor(resample, cnfLvl) {
	// changes colors for CI illustration
	var color = [],
	    lowerBd,
	    upperBd,
	    quantile,
	    twoTail,
	    sLen = resample.length;
	resample = resample.sort(function(a,b){return a - b});    
	if (sLen > 0) {
		twoTail = Math.round((1 - cnfLvl) * sLen);
		quantile = Math.floor(twoTail / 2);
		if (twoTail % 2) {// check for odd number
			cnfLvl = (sLen - twoTail -1) / sLen;
			quantile += 1;
			// reduce to lower confidence
		}

		for ( i = quantile; i < sLen - quantile; i++) {
			color[i] = 0;
			// color for middle circles
		}
		for ( i = 0; i < quantile; i++) {
			color[i] = 1;
			// color lower tail
			color[sLen - i - 1] = 1;
			// color upper tail
			lowerBd = resample[i];
			// move lowerBd up
			upperBd = resample[sLen - i - 1];
			// move upperBd down
		}
	} else {
		console.log("No Data for CI");
	}
	return ([color, lowerBd, upperBd, cnfLvl]);
}

function propBarChart() {
	// updatable chart to show proportion bars
	//TODO:  add text labels (y axis?) for multiple bars
	// thanks to Rob Moore https://www.toptal.com/d3-js/towards-reusable-d3-js-charts 
	// All options that should be accessible to caller
	var data = [];
	var width = 400;
	var height = 100;
    var fillColor = 'steelblue';
	//var updateData;
	
	
    var barPadding = 1, 
    	margin = 10;
	var xScale = d3.scaleLinear()
		.range([0, width - 2 * margin])
		.domain([0,1]);
    
	function chart(selection){
        selection.each(function () {
			var barSpacing = height / (data.length + 1);
            var barHeight = barSpacing - barPadding;
            
 			var myDiv = d3.select(this);
 			var svg = myDiv.append('svg')
 					.attr('class', 'bar-chart')
                    .attr('height', height + margin)
                    .attr('width', width)
                    .style('fill', fillColor);
                    
            var bars = svg.append('g')
    				.attr('transform', 'translate(' + margin + ', 0)')
    				.selectAll('rect.display-bar')
                    .data(data)
                	.enter()
                    .append('rect')
                    .attr('class', 'display-bar')
                    .attr('y', function (d, i) { return  i * barSpacing;  })
                    .attr('height', barHeight)
					//.attr("transform", "translate(" +margin +",0)")
                    .attr('x', margin)
                    .attr('width', function (d) { return xScale(d);});
        	
        	var xAxis = d3.axisBottom(xScale).ticks(5);
        	
        	svg.append("g")
        		.attr("class","xaxis")
        		.attr("transform", "translate(" + (2*margin) + "," + (barSpacing * data.length + 2) +")")
        		.call(xAxis);
    	
            // update functions
            updateWidth = function() {
            	//xScale.range([0, width-margin]);
                //widthScale = width;
                bars.transition().duration(1000).attr('width', function(d) { return xScale(d); });
                svg.transition().duration(1000).attr('width', width);
            };

            updateHeight = function() {
                barSpacing = height / (data.length +1);
                barHeight = barSpacing - barPadding;
                bars.transition().duration(1000).attr('y', function(d, i) { return i * barSpacing; })
                    .attr('height', barHeight);   
                svg.transition().duration(1000).attr('height', height);
				//svg.selectAll('xaxis')
					//.transition().duration(1000).attr('y', barSpacing * data.length + 2);
            };

            updateFillColor = function() {
                svg.transition().duration(1000).style('fill', fillColor);
            };
                    	
  			updateData = function() {
                barSpacing = height / (data.length +1);
                barHeight = barSpacing - barPadding;
                
                var update = svg.selectAll('rect.display-bar')
                    .data(data);

                update
                    .transition()
                    .duration(500)
                    .attr('y', function(d, i) { return i * barSpacing; })
                    .attr('height', barHeight)
                    .attr('x', margin)
                    .attr('width', function(d) { return xScale(d); });

                update.enter()
                    .append('rect')
                    .attr('class', 'display-bar')
                    .attr('y', function(d, i) { return i * barSpacing; })
                    .attr('height', barHeight)
                    .attr('x', margin)
                    .attr('width', 0)
                    .style('opacity', 0)
                    .transition()
                    .duration(500)
                    .delay(function(d, i) { return (data.length - i) * 40; })
                    .attr('width', function(d) { return xScale(d); })
                    .style('opacity', 1);

                update.exit()
                    .transition()
                    .duration(350)
                    .delay(function(d, i) { return (data.length - i) * 20; })
                    .style('opacity', 0)
                    .attr('height', 0)
                    .attr('x', margin)
                    .attr('width', 0)
                    .remove();
            }

        });
    }
    chart.width = function(value) {
        if (!arguments.length) return width;
        width = value;
        if (typeof updateWidth === 'function') updateWidth();
        return chart;
    };

    chart.height = function(value) {
        if (!arguments.length) return height;
        height = value;
        if (typeof updateHeight === 'function') updateHeight();
        return chart;
    };

    chart.fillColor = function(value) {
        if (!arguments.length) return fillColor;
        fillColor = value;
        if (typeof updateFillColor === 'function') updateFillColor();
        return chart;
    };

	chart.data = function(value) {
    	if (!arguments.length) return data;
    	data = value;
    	if (typeof updateData === 'function') updateData();
    	return chart;
	};
  
	return chart;
}

//  var dataA = [.45,1], dataB = [.06,1],
//      updatableChart = propBarChart().data(dataA);
//        d3.select('#cat1SummarySVGgoesHere').call(updatableChart);
// updatableChart.data(dataB);

/* global d3 */

function scatterPlot(data, svgObject, interactFunction, intercept, slope) {
 // not updateable
	var circleColors = ["steelblue","red"],
		color = [],
		margin = 40,
		myArray =[],
	    nN = data.length,
	    plotX,
	    regLine,
	    xmin = d3.min(data, function(d) { return d.x; }),
	    xmax = d3.max(data, function(d) { return d.x; }),
	    yhat1,
	    yhat2,
	    wdth = 440 - margin * 2,
	    hght = 320 - margin * 2;
        if(svgObject.getAttribute("width")>50){
	     wdth= svgObject.getAttribute("width") - margin * 2;
	    hght = svgObject.getAttribute("height") - margin * 2;
	}
	if (nN ===2){
		color = data[1];
		sample = data[0];
		nN = data.length;
	}

	//sample.sort(function(a,b){return a - b}) ;   
    var xScale = d3.scaleLinear()
				.range([margin, width - 3 * margin])
				.domain([xmin, xmax]),
		
		yScale = d3.scaleLinear()
				.range([margin, height - 2*margin])
				.domain([d3.max(data, function(d) { return d.y; }), d3.min(data, function(d) { return d.y; })]);
		  

	var radii = (nN < 101)? 10:
	    (nN < 501)? 7:
	    (nN < 1001)? 5:
	    (nN < 5001)? 4: 3; // perhaps this should relate to width/height of svg]
    var gees = d3.select(svgObject).selectAll("g");
	if(typeof(gees) === "object"){
		gees.remove();
	}    
   var SPxAxis = d3.axisBottom(xScale)
      .ticks(5);   

   var SPyAxis = d3.axisLeft(yScale);
      
  var graph = d3.select(svgObject)
    .attr("width", wdth + margin*2)
    .attr("height", hght + margin*2)
  .append("g")
   .attr("transform", "translate("+ (2 * margin) + "," + margin + ")");
    
    
    graph.append("g")
      .attr("class", "y axis")
      .call(SPyAxis);
      
      
	graph.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + (radii + hght) +")")
      .call(SPxAxis);
      
    if (intercept !== 'undefined'){
    	//console.log(intercept, slope);
		yhat1 = intercept + slope * xmin;
		yhat2 = intercept + slope * xmax;
	 	regLine = graph.append("svg:line")
            .attr("x1", xScale(xmin))
            .attr("y1", yScale(yhat1) )
            .attr("x2", xScale(xmax))
            .attr("y2", yScale(yhat2))
            .style("stroke", "black")
      		.attr("class", "regression line");        
    }  

  //  graph.append("g")
  //  	.attr("class", "text")
  //  	.attr("x", 20)
  //  	.attr("y", hght + 5)
  //  	.text(xlegend);  
      
   	Dots =  graph.selectAll("g.circle")
            .data(data); 
	Dots.enter().append("circle")
            .attr("cx", function(d){ return xScale(d.x);} ) 
            .attr("r", radii ) 
            .attr("cy", function(d){ return yScale(d.y);} ) 
            .style("fill", function(d){return 'blue';})
            .style("fill-opacity", 0.6)
            .on("click", interactFunction ); 
    //return [Dots, sample];

}


function xyChart() {
	// updatable chart to show scatterplot
	// thanks to Rob Moore https://www.toptal.com/d3-js/towards-reusable-d3-js-charts 
	// All options that should be accessible to caller
	//TODO:
		// yaxis has no labels, xaxis is at top instead of bottom,
		// need update function to transition axes
		// data update not working for plot, but consolelog prints it fine 
	var data =[],
	 	width = 200,
	 	height = 200,
	 	margin =40,
	 	radius = 5,
     	fillColor = [];
	//var updateData;
    
	function chart(selection){
        selection.each(function () {
			var myDiv = d3.select(this);
 			var svg = myDiv.append('svg')
 					.attr('class', 'xyChart')
                    .attr('height', height + margin)
                    .attr('width', width + margin);
 			var xScale = d3.scaleLinear()
				.range([margin, width - 2 * margin])
				.domain([d3.min(data, function(d) { return d.x; }), d3.max(data, function(d) { return d.x; })]),
		
				yScale = d3.scaleLinear()
				.range([margin, height - 2*margin])
				.domain([d3.max(data, function(d) { return d.y; }), d3.min(data, function(d) { return d.y; })]);
		
  		    var dots = svg.selectAll(".dot")
        		.data(data)
       	 		.enter().append("circle")
        		.attr("class", "dot")
       			.attr("r", radius)
        		.attr("cx", function(d) {
            		return xScale(d.x);
        			})
        		.attr("cy", function(d) {
            		return yScale(d.y);
        		});
                   
            //var dots = svg.append('g')
    			//	.attr('transform', 'translate(' + margin + ', 0)')
    			//	.selectAll('circle')
                 //   .data(data)
                //	.enter()
                //    .append('circle')
                    //.attr('class', 'display-dot')
               //     .style('fill', 'lightblue')//function (d,i) { return fillColor[i];})
               //     .attr('cy', function (d) {  return  d.y ;  })
               //     .attr('cx', function (d) {  return  d.x ;  })
               //     .attr('r', radius);
        	
        	var xAxis = d3.axisBottom(xScale).ticks(5);
        	var yAxis = d3.axisLeft(yScale).ticks(5);
        	
        	svg.append("g")
        		.attr("class","xaxis")
        		.call(xAxis);
        	svg.append("g")
        		.attr("class","yaxis")
        		.call(yAxis);
    	
            // update functions
            updateWidth = function() {
                svg.transition().duration(1000).attr('width', width);
            };

            updateHeight = function() {   
                svg.transition().duration(1000).attr('height', height);
            };

            updateFillColor = function() {
                svg.transition().duration(1000).style('fill',  function (d,i) { 
                	return fillColor[i];});
            };
                    	
  			updateData = function() {
                var update = svg.selectAll('dot')
                    .data(data);
				xScale = d3.scaleLinear()
					.range([0, width - 2 * margin])
					.domain([d3.min(data, function(d) { return d.x; }), d3.max(data, function(d) { return d.x; })]),
		
				yScale = d3.scaleLinear()
					.range([margin, height - 2*margin])
					.domain([d3.max(data, function(d) { return d.y; }), d3.min(data, function(d) { return d.y; })]);
				 xAxis = d3.axisBottom(xScale).ticks(5);
        		 yAxis = d3.axisLeft(yScale).ticks(5);
        	
        		svg.append("g")
        			.attr("class","xaxis")
        			.call(xAxis);
        		svg.append("g")
        			.attr("class","yaxis")
        			.call(yAxis);
    	
                update
                    .transition()
                    .duration(500)
                    .style('fill', function (d,i) { return fillColor[i];})
                    .attr('y', function (d) { return  yScale(d.y);  })
                    .attr('x', function (d) { return  xScale(d.x);  })
                    .attr('r', radius);
                    

                update.enter()
                    .append('circle')
                    .attr('class', 'dot')
                    .style('fill', function (d,i) { return fillColor[i];})
                    .attr('y', 0)
                    .attr('x', function (d) { return  xScale(d.x);  })
                    .attr('r', radius)
                    .style('opacity', 0)
                    .transition()
                    .duration(500)
                    .delay(function(d, i) { return (data.length - i) * 40; })
                    .style('fill', function (d,i) { return fillColor[i];})
                    .attr('y', function (d) { return  yScale(d.y);  })
                    .attr('x', function (d) { return  xScale(d.x);  })
                    .attr('r', radius);

                update.exit()
                    .transition()
                    .duration(350)
                    .delay(function(d, i) { return (data.length - i) * 20; })
                    .style('opacity', 0)
                    .remove();
                //console.log(data[0], d3.min(data, function(d) { return d.x; }));
            }

        });
    }
    chart.width = function(value) {
        if (!arguments.length) return width;
        width = value;
        if (typeof updateWidth === 'function') updateWidth();
        return chart;
    };

    chart.height = function(value) {
        if (!arguments.length) return height;
        height = value;
        if (typeof updateHeight === 'function') updateHeight();
        return chart;
    };

    chart.fillColor = function(value) {
        if (!arguments.length) return fillColor;
        if(Array.isArray(value) ){
        	fillColor = value;
        } else {
        	for(i=0; i<data.length; i++){
        		fillColor[i] = value;
        	}
        }
        if (typeof updateFillColor === 'function') updateFillColor();
        return chart;
    };

	chart.data = function(value) {
    	if (!arguments.length) return data;
    	data = value;
    	if (typeof updateData === 'function') updateData();
    	return chart;
	};
  
	return chart;
}
  var dataA = [{x:1, y:2}, {x:2, y:4}, {x:3, y:5}], 
      dataB = [{x:2, y:2}, {x:1, y:4}, {x:0, y:6}],
      updatableChart = xyChart().data(dataA);
//        d3.select('#quant2SummarySVGgoesHere').call(updatableChart);
// updatableChart.data(dataB);

 var lnFactorial = [
   0.000000000000000,
   0.000000000000000,
   0.693147180559945,
   1.791759469228055,
   3.178053830347946,
   4.787491742782046,
   6.579251212010101,
   8.525161361065415,
   10.604602902745251,
   12.801827480081469,
   15.104412573075516,
   17.502307845873887,
   19.987214495661885,
   22.552163853123421,
   25.191221182738683,
   27.899271383840894,
   30.671860106080675,
   33.505073450136891,
   36.395445208033053,
   39.339884187199495,
   42.335616460753485,
   45.380138898476908,
   48.471181351835227,
   51.606675567764377,
   54.784729398112319,
   58.003605222980518,
   61.261701761002001,
   64.557538627006323,
   67.889743137181526,
   71.257038967168000,
   74.658236348830158,
   78.092223553315307,
   81.557959456115029,
   85.054467017581516,
   88.580827542197682,
   92.136175603687079,
   95.719694542143202,
   99.330612454787428,
   102.968198614513810,
   106.631760260643450,
   110.320639714757390,
   114.034211781461690,
   117.771881399745060,
   121.533081515438640,
   125.317271149356880,
   129.123933639127240,
   132.952575035616290,
   136.802722637326350,
   140.673923648234250,
   144.565743946344900,
   148.477766951773020,
   152.409592584497350,
   156.360836303078800,
   160.331128216630930,
   164.320112263195170,
   168.327445448427650,
   172.352797139162820,
   176.395848406997370,
   180.456291417543780,
   184.533828861449510,
   188.628173423671600,
   192.739047287844900,
   196.866181672889980,
   201.009316399281570,
   205.168199482641200,
   209.342586752536820,
   213.532241494563270,
   217.736934113954250,
   221.956441819130360,
   226.190548323727570,
   230.439043565776930,
   234.701723442818260,
   238.978389561834350,
   243.268849002982730,
   247.572914096186910,
   251.890402209723190,
   256.221135550009480,
   260.564940971863220,
   264.921649798552780,
   269.291097651019810,
   273.673124285693690,
   278.067573440366120,
   282.474292687630400,
   286.893133295426990,
   291.323950094270290,
   295.766601350760600,
   300.220948647014100,
   304.686856765668720,
   309.164193580146900,
   313.652829949878990,
   318.152639620209300,
   322.663499126726210,
   327.185287703775200,
   331.717887196928470,
   336.261181979198450,
   340.815058870798960,
   345.379407062266860,
   349.954118040770250,
   354.539085519440790,
   359.134205369575340,
   363.739375555563470,
   368.354496072404690,
   372.979468885689020,
   377.614197873918670,
   382.258588773060010,
   386.912549123217560,
   391.575988217329610,
   396.248817051791490,
   400.930948278915760,
   405.622296161144900,
   410.322776526937280,
   415.032306728249580,
   419.750805599544780,
   424.478193418257090,
   429.214391866651570,
   433.959323995014870,
   438.712914186121170,
   443.475088120918940,
   448.245772745384610,
   453.024896238496130,
   457.812387981278110,
   462.608178526874890,
   467.412199571608080,
   472.224383926980520,
   477.044665492585580,
   481.872979229887900,
   486.709261136839360,
   491.553448223298010,
   496.405478487217580,
   501.265290891579240,
   506.132825342034830,
   511.008022665236070,
   515.890824587822520,
   520.781173716044240,
   525.679013515995050,
   530.584288294433580,
   535.496943180169520,
   540.416924105997740,
   545.344177791154950,
   550.278651724285620,
   555.220294146894960,
   560.169054037273100,
   565.124881094874350,
   570.087725725134190,
   575.057539024710200,
   580.034272767130800,
   585.017879388839220,
   590.008311975617860,
   595.005524249382010,
   600.009470555327430,
   605.020105849423770,
   610.037385686238740,
   615.061266207084940,
   620.091704128477430,
   625.128656730891070,
   630.172081847810200,
   635.221937855059760,
   640.278183660408100,
   645.340778693435030,
   650.409682895655240,
   655.484856710889060,
   660.566261075873510,
   665.653857411105950,
   670.747607611912710,
   675.847474039736880,
   680.953419513637530,
   686.065407301994010,
   691.183401114410800,
   696.307365093814040,
   701.437263808737160,
   706.573062245787470,
   711.714725802289990,
   716.862220279103440,
   722.015511873601330,
   727.174567172815840,
   732.339353146739310,
   737.509837141777440,
   742.685986874351220,
   747.867770424643370,
   753.055156230484160,
   758.248113081374300,
   763.446610112640200,
   768.650616799717000,
   773.860102952558460,
   779.075038710167410,
   784.295394535245690,
   789.521141208958970,
   794.752249825813460,
   799.988691788643450,
   805.230438803703120,
   810.477462875863580,
   815.729736303910160,
   820.987231675937890,
   826.249921864842800,
   831.517780023906310,
   836.790779582469900,
   842.068894241700490,
   847.352097970438420,
   852.640365001133090,
   857.933669825857460,
   863.231987192405430,
   868.535292100464630,
   873.843559797865740,
   879.156765776907600,
   884.474885770751830,
   889.797895749890240,
   895.125771918679900,
   900.458490711945270,
   905.796028791646340,
   911.138363043611210,
   916.485470574328820,
   921.837328707804890,
   927.193914982476710,
   932.555207148186240,
   937.921183163208070,
   943.291821191335660,
   948.667099599019820,
   954.046996952560450,
   959.431492015349480,
   964.820563745165940,
   970.214191291518320,
   975.612353993036210,
   981.015031374908400,
   986.422203146368590,
   991.833849198223450,
   997.249949600427840,
   1002.670484599700300,
   1008.095434617181700,
   1013.524780246136200,
   1018.958502249690200,
   1024.396581558613400,
   1029.838999269135500,
   1035.285736640801600,
   1040.736775094367400,
   1046.192096209724900,
   1051.651681723869200,
   1057.115513528895000,
   1062.583573670030100,
   1068.055844343701400,
   1073.532307895632800,
   1079.012946818975000,
   1084.497743752465600,
   1089.986681478622400,
   1095.479742921962700,
   1100.976911147256000,
   1106.478169357800900,
   1111.983500893733000,
   1117.492889230361000,
   1123.006317976526100,
   1128.523770872990800,
   1134.045231790853000,
   1139.570684729984800,
   1145.100113817496100,
   1150.633503306223700,
   1156.170837573242400
]

factorialln = function( x ) {
    // assumes x is an integer. returns ln(factorial(x))
	if ( x < 0 ) {
		return NaN;
	}
	if  ( x <= 256 ) {
		return lnFactorial[ x ];
	} else {
		x++;
		return (x - 0.5) * Math.log(x) - x + 0.5 * Math.log( 2 * Math.PI ) + 1 / ( 12 * x );
	}
}

function hypergeomPMF(n,m,k){
	//computes hypergeometric probabilities of all possible values  (0 or k-m) <= x <= (k or n)
	// from hypergeometric distribution with n of type of interest, m others, sample size k
	var i,  x, 
		flk = factorialln(k),
		fln = factorialln(n),
		flm = factorialln(m),
		flnPlusm = factorialln( n + m),
		flnPlusMlessK = factorialln( m + n - k),
		out = [], sample = [],
		lower = Math.max(0, k-m),
		upper = Math.min(k, n),
		range = upper - lower + 1,
		fixedpart = factorialln(n) + factorialln(m) + factorialln(k) + factorialln(n+m-k) -factorialln(n+m) ;
		
	for (i=0; i < range; i++){
		x = lower + i;
		out[i] = Math.exp(fixedpart -factorialln( x ) - factorialln( k - x ) - factorialln( n - x ) - factorialln( m + x - k ));
		sample[i] = x;
	}	 
	return ([sample, out]);
}


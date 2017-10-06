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
	if (input < dividers[0] | input > dividers[dividers.length - 1]) {
		return (null);
	}
	for ( i = 1; i < dividers.length; i++) {
		if (input < dividers[i]) {
			break;
		}
	}
	return (i - 1);
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
	return [out, ids];
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
	
	return [out, ids];  
}

 function inArray(array, value){
 	var i = array.length;
 	while(i--){
 		if (array[i] === value){ return true;}
 	}
 	return false;
 }
 
  var sturgesFormula = function(arr){
  	// number of bins for a histogram
    var n = size(arr);
    var k = Math.ceil((Math.log(n)/Math.log(2))+1);
    var h = (d3.max(arr) - d3.min(arr)) / k;  // length of each bin
    return {"binCount":k, "binLength":h};
  };
    
//
//  Need a generic plotting function for dotcharts.
//

var dotChart = function(sample, svgObject, xlegend){
	var margin = 40,
		myArray =[],
	    nN = sample.length,
	    plotX,
	    ypos =0,
	    wdth = 440 - margin * 2,
	    hght = 320 - margin * 2;
	   // xlegend = spinStopRule === "Fixed"? "spins to get one of the first type":
	     //  			spinStopRule === "OneOfOneType"? "Spins to get a " + spinGroups[spinMatch]:
	       //			"spins to get one of each type";
	    
	sample.sort(function(a,b){return a - b}) ;   
          // numeric sort to build bins for y values
          // start on left with smallest x.

	
	var radii = (nN < 101)? 6:
	    (nN < 501)? 5:
	    (nN < 1001)? 4:
	    (nN < 5001)? 3: 2; // perhaps this should relate to width/height of svg]
    var gees = d3.select(svgObject).selectAll("g");
	if(typeof(gees) === "object"){
		gees.remove();
	}    
	//  first dot goes at y=0, then add one to each from there
	var j = 0;
	while( j <  nN ){    
	    plotX = sample[j];	    // start a fresh bin with left edge at sample[j]
	    ypos = 0;	            // bin y starts at 0
	    myArray[j] = {"x": sample[j++], "y": ypos++};
        while( (sample[j] - plotX < radii/6) & (j < nN)){
		  //stay in same bin -- increment yposition
		  myArray[j] = {"x": sample[j++], "y": ypos++};
	    };
	     // console.log(x(plotX));
	}
	sampMax = d3.max(myArray, function(d) { return d.y;});

   var DCyScale = d3.scale.linear()
    	.range([hght, 0])
    	.domain([0, sampMax + .5]);

	var DCxScale = d3.scale.linear()
    	.range([margin, wdth])
    	.domain([-0.1, sample[nN - 1] +0.5]);

	// change scales to hold all x, all y
   var DCxAxis = d3.svg.axis()
      .scale(DCxScale)
      .orient("bottom");

   var DCyAxis = d3.svg.axis()
      .scale(DCyScale)
      .orient("left");
      
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

    graph.append("g")
    	.attr("class", "text")
    	.attr("x", 20)
    	.attr("y", hght + 5)
    	.text(xlegend);  
      
   	Dots =  graph.selectAll("g.circle")
            .data(myArray); 
	Dots.enter().append("circle")
            .attr("cx", function(d){ return DCxScale(d.x);} ) 
            .attr("r", radii ) 
            .attr("cy", function(d){ return DCyScale(d.y);} ) 
            .style("fill","steelblue")
            .style("fill-opacity", 0.6);
    //return Dots; // and myArray?
}


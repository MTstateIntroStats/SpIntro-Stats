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


function sample(values, nreps, prob) {
	// draw  values  (with replacement) at random using probs as weights
	var cumProb = [],
	    nCat = prob.length,
	    totalProb = jStat.sum(prob),
	    i,
	    out = [];
	stdize = function(x) {
		return x / totalProb;
	};
	prob = jStat.map(prob, stdize);
	cumProb = jStat.cumsum(prob);
	cumProb.unshift(0);
	//console.log(cumProb);
	for ( i = 0; i < nreps; i++) {
		out.push(values[ cut(Math.random(), cumProb)]);
	}
	return out;
}


  var sturgesFormula = function(arr){
  	// number of bins for a histogram
    var n = size(arr);
    var k = Math.ceil((Math.log(n)/Math.log(2))+1);
    var h = (d3.max(arr) - d3.min(arr)) / k;  // length of each bin
    return {"binCount":k, "binLength":h};
  };
    
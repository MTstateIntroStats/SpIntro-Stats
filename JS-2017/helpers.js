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
	return Math.ceiling(ln(Math.rand()) / ln(1 - prob));
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
	cumProb = jStat.cumsum(prob).unshift(0);
	for ( i = 0; i < nreps; i++) {
		out[i] = values[ cut(Math.random(), cumProb)];
	}
	return out;
}


  var sturgesFormula = function(arr){
  	// number of bins for a histogram
    var n = size(arr);
    var k = Math.ceil((Math.log(n)/Math.log(2))+1);
    var h = (max(arr) - min(arr)) / k;
    return {"k":k, "h":h};
  };
    
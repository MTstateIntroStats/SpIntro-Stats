// subroutine to estimate a proportion or test for a special value
// Inputs:
//     2 category labels (default = Success/Failure) and a count for each
//
//  TODO:
	// summary plot isn't showing now unless i run summarizeP1()
	// need to better understand the update procedure
	
var c1SummDiv = d3.select("#cat1Inference"),
    c1Tstdata,
    c1CIdata,
    cat1Label1,
    cat1Label2,
    cat1ftr,
    cat1hdr,
    cat1CnfLvl = .80,
    cat1CLvl,
    cat1lowerBd,
    cat1upperBd,
    cat1N1,
    cat1N2,
    cat1Pval,
    cat1TestDirection,
    cat1TestInpt2,
    c1Data = [],
    c1bars,
    chartC1,
    cat1Bars,
    cat1Color = [],
    confLevels = [{	key : "80%",	value : "0.80"}, 
    				{key : "90%",	value : "0.90"}, 
    				{key : "95%",	value : "0.95"}, 
    				{key : "99%",	value : "0.99"}
    			],
    cat1Inference,
    cat1InfOutput,
    noChoice = "undefined",
    targetQuantile,
    upperBd,
    upperCI,
    cat1Phat,
    resampleC1 = [],
    sampleC1 = [],
    margin = 30,
	padding = 40,
	barHeight = 20,
	w = 300,
	h = 50,
    total;

  var svgCat1 = d3.select("#cat1InfSVG"),
	cat1SmryUpDiv = d3.select("#cat1SummarySVGgoesHere");

  var cat1BarSvg = cat1SmryUpDiv.append("svg") 
		.attr("width", w + margin * 2)
		.attr("height", h + margin),
	  c1x = d3.scaleLinear()
	   	.domain([0, 1])
	   	.range([0, w - margin * 2]);       

function summarizeP1() {
	// builds summary table and plot for 1 categorical variable
	var colors = []; //240

	cat1Label1 = document.getElementById("cat1Label1").value;
	cat1Label2 = document.getElementById("cat1Label2").value;
	cat1N1 = +document.getElementById("cat1N1").value;
	cat1N2 = +document.getElementById("cat1N2").value;
	cat1Phat = cat1N1 / (cat1N1 + cat1N2);
	cat1Summ = document.getElementById("cat1SummaryText");

	resampleC1 = [];
	sampleC1 = [];

	cat1Summ.innerHTML = "p&#770; =  " + cat1Phat.toPrecision(5) + " <br> se(p&#770) = " + (Math.sqrt(cat1Phat * (1 - cat1Phat)) / (cat1N1 + cat1N2)).toPrecision(5);
	cat1Summ.style = "display: block";

	c1Data = [{	"label" : cat1Label1,		"xx" : cat1Phat	}, 
			  //{	"label" : cat1Label2,		"xx" : 1 - cat1Phat	}
			  ];
	
     cat1Bars = cat1BarSvg.selectAll("rect")
        .data(c1Data);
        
     cat1Bars.enter()
     	.append("rect")
		.attr("width", function(d) {return c1x(Math.max(0.005, d.xx));})// - 14;} )
		.attr("height", barHeight - 1)
		.attr("fill", "blue")
		//.attr("transform", function(d, i) {	return "translate(" + padding + "," + i * barHeight + ")";})
		;
	cat1Bars.append("text")
		.attr("x", function(d) {return padding + 16 + c1x(d.xx);})
		.attr("y", function(d, i) {return (i + .5) * barHeight;	})
		.attr("dy", ".35em")
		.text(function(d) {	return d.label;	})
		.attr("fill", "black");

      //cat1Bars.exit().remove(); 
      
		//chartC1.selectAll("rect").transition().attr("width", function(d){ return c1x(0 * d.xx)});
		//chartC1.selectAll("text").transition().attr("fill", "white");
		//chartC1.selectAll("g").data(c1Data).remove();
		//chartC1.selectAll("g").data(c1Data).remove();
		// why do I have to do this twice?
	

				
	//barC1 = chartC1.selectAll("g")
	//	.data(c1Data);
	//barC1.enter()
	//	.append("g")
	//	.each(function(d) {	this.current = d;	});
	//resets the figure to the current data
	//.attr("fill", "blue")
	//.attr("transform", function(d, i) { return "translate(14," + i * barHeight + ")"; })

	//barC1.append("rect")
	//	.attr("width", function(d) {return c1x(Math.max(0.005, d.xx));})// - 14;} )
	//	.attr("height", barHeight - 1)
	//	.attr("fill", "blue")
	//	.attr("transform", function(d, i) {	return "translate(" + padding + "," + i * barHeight + ")";});


	var c1xAxis = d3.axisBottom(c1x)
		.ticks(5);

	cat1Bars.append("g")  //barC1.append("g")
		.attr("class", "x axis")
		//.attr("transform", "translate(" + padding + "  ," + 1.5 * barHeight + ")")
		.call(c1xAxis);

	//check for any old output plots. If present, erase them due to change in data
	if (cat1InfOutput) {
		c1CIdata = [];
		cat1BarSvg.selectAll("g").remove();
		document.getElementById("cat1Output").style.display = "none";
		document.getElementById("cat1MoreSims").style.display = "none";
		document.getElementById("cat1ConfLvlInpt").style.display = "none";
		document.getElementById("cat1TestInpt1").style.display = "none";
		document.getElementById("cat1TestInpt2").style.display = "none";
		document.getElementById("cat1WhichDot").style.display = "none";
	}
}

function cat1CLChange(arg) {
	// set colors for dots to illustrate confidence interval
	var cnfLvl = .6,
		sC1Len,
	    tempColors,
	    twoTail;
	if (arg.value) {
		cnfLvl = +arg.value;
	} 
	if (c1CIdata) {
		sC1Len = c1CIdata[0].length;
		tempColors = ciColor(c1CIdata[0], cnfLvl);
		if(tempColors[1]){
			cat1lowerBd = tempColors[1].toPrecision(4);
			cat1upperBd = tempColors[2].toPrecision(4);
			cnfLvl = tempColors[3];
			c1CIdata = [c1CIdata[0], tempColors[0]];
		} else{
			c1CIdata = [c1CIdata[0], tempColors];
		}
		cat1InfOutput = discreteChart(c1CIdata, cat1InfSVG, cat1CIinteract);
		sC1Len = c1CIdata[0].length;
	} else {
		//console.log("No resampled data for CI");
	}
	cat1ftr = document.getElementById("cat1Results");
	cat1ftr.innerHTML = //"<div style = 'height = 10'> </div>" +
	"<div style = 'width:360px'> Proportion " + cat1Label1 + " in  " + sC1Len + " Re-samples" + "<br> <br>" + Math.round(cnfLvl * 100) + "% Confidence Interval: (" + cat1lowerBd + ", " + cat1upperBd + " )</div>";
	cat1ftr.style.display = 'block';
	document.getElementById("cat1MoreSims").style.display = 'block';

}

var cat1CIrangeslide = rangeslide("#cat1ConfLvlInpt", {
	data : confLevels,
	showLabels : true,
	startPosition : 0,
	showTicks : false,
	dataSource : "value",
	labelsContent : "key",
	valueIndicatorContent : "key",
	thumbWidth : 24,
	thumbHeight : 24,
	handlers : {
		"valueChanged" : [cat1CLChange]
	}
});



function estimateP1() {
	//function to estimate the true proportion based on a sample of 'success/failure' data
	summarizeP1();
	cat1Inference = 'estimate';
	// Gather Inputs:
	cat1Label1 = document.getElementById("cat1Label1").value;
	cat1Label2 = document.getElementById("cat1Label2").value;
	cat1N1 = +document.getElementById("cat1N1").value;
	cat1N2 = +document.getElementById("cat1N2").value;
	var CI=[],
		cnfLvl = cat1CnfLvl,
		sC1Len,
	    total = cat1N1 + cat1N2;
	cat1Phat = cat1N1 / total;

	cat1hdr = document.getElementById("cat1ConfLvlInpt");
	document.getElementById("cat1TestInpt1").style.display = "none";
	document.getElementById("cat1WhichDot").style.display = "none";
	cat1hdr.style.display = "block";

	resampleC1 = rbinom(total, cat1Phat, 100).sort(function(a, b) {	return a - b;});
	sC1Len = resampleC1.length;
	for ( i = 0; i < sC1Len; i++) {
		resampleC1[i] *= 1 / total;
	}

	CI = ciColor(resampleC1, cat1CnfLvl);
	cat1Color = CI[0];

	c1CIdata = [resampleC1, cat1Color];
	//cat1InfOutput = discreteChart(c1CIdata, cat1InfSVG, cat1CIinteract);

	cat1lowerBd = CI[1].toPrecision(4);
	cat1upperBd = CI[2].toPrecision(4);
    cnfLvl = CI[3];
	cat1ftr = document.getElementById("cat1Results");
	cat1ftr.innerHTML = "<div style='width=50px'></div>" + "<div style = 'width:360px'> Proportion " + cat1Label1 + " in  " + sC1Len + " Re-samples" + "<br> <br>" + Math.round(cnfLvl * 100) + "% Confidence Interval: (" + cat1lowerBd + ", " + cat1upperBd + " )</div>";

	cat1ftr.style.display = "block";
	document.getElementById("cat1Output").style.display = "block";
	document.getElementById("cat1MoreSims").style.display = 'block';
	//console.log(cat1lowerBd, cat1upperBd);

	return (c1CIdata);
}

function testP1(tailChoice) {
	//function to test 'Is the true proportion  = some value?' for 'success/failure' data
	// Gather Inputs:
	cat1Label1 = document.getElementById("cat1Label1").value;
	cat1Label2 = document.getElementById("cat1Label2").value;
	cat1N1 = +document.getElementById("cat1N1").value;
	cat1N2 = +document.getElementById("cat1N2").value;
	summarizeP1();
	
	cat1hdr = document.getElementById("cat1TestInpt1");
	cat1hdr.style.display = "block";
	cat1ftr = document.getElementById("cat1Results");

	cat1Pnull = +document.getElementById("cat1trueP").value;
	// turn off Confidence interval, turn on test inputs
	document.getElementById("cat1ConfLvlInpt").style.display = "none";
	document.getElementById("cat1WhichDot").style.display = "none";

	var sC1Len,
	    total = cat1N1 + cat1N2;
	
	cat1TestInpt2 = document.getElementById("cat1TestInpt2");
	cat1TestInpt2.style.display = 'block';
	cat1TestInpt2.innerhtml = "  <div class = 'w3-cell' style = 'width:50%'> Stronger evidence is a proportion </div>"+
		" <div class = 'w3-cell' > " +
  	    " <select class = 'w3-select w3-card w3-border w3-mobile w3-pale-yellow' id='cat1Extreme'" +
  	    "    onchange = 'cat1TestUpdate()' >" +
  		" 	<option value='lower'>Less Than or =</option>" +
  		" 	<option value='both' selected >As or More Extreme Than</option>" +
  		" 	<option value='upper'>Greater Than or =</option>" +
	   	" </select> </div>  <div class ='w3-cell'> &nbsp;&nbsp;" + 
	   	  cat1Phat.toPrecision(4) + " (from above)	   	</div>";
		   	   
	if (tailChoice === 'undefined') {
		cat1ftr = document.getElementById("cat1Results");
		cat1ftr.innerHTML = "<div  style = 'width:320px'> Proportion " + 
		         cat1Label1 + " in samples from H<sub>0</sub>";
		sampleC1 = rbinom(total, cat1Pnull, 100).sort(function(a, b) {return a - b;	});
		sC1Len = sampleC1.length;
		for ( i = 0; i < sC1Len; i++) {
			sampleC1[i] *= 1 / total
		}
	} else {

	}
	cat1ftr.style.display = "block";
	document.getElementById("cat1Output").style.display = "block";
	document.getElementById("cat1MoreSims").style.display = 'block';
	// TODO: clicking a point changes a table to show that proportion
	return (sampleC1);
}

function cat1TestUpdate() {
	// run whenever tail direction changes
	var check,
	    extCount = 0,
	    lowP,
	    hiP,
	    sC1Len;
	cat1Inference = 'test';
	// get direction of evidence:
	cat1TestDirection = document.getElementById("cat1Extreme").value;

	if (!(sampleC1)) {
		sampleC1 = testP1();
	}
	sC1Len = sampleC1.length;
	if (cat1TestDirection === "lower") {
		for ( i = 0; i < sC1Len; i++) {
			check = 0 + (sampleC1[i] <= cat1Phat);
			extCount += check;
			cat1Color[i] = check;
		}
	} else if (cat1TestDirection === "upper") {
		for ( i = 0; i < sC1Len; i++) {
			check = 0 + (sampleC1[i] >= cat1Phat);
			extCount += check;
			cat1Color[i] = check;
		}

	} else {
		lowP = cat1Phat * (cat1Phat <= cat1Pnull) + (2 * cat1Pnull - cat1Phat) * (cat1Phat > cat1Pnull) + 1 / 1000000;
		hiP = cat1Phat * (cat1Phat >= cat1Pnull) + (2 * cat1Pnull - cat1Phat) * (cat1Phat < cat1Pnull) - 1 / 1000000;
		for ( i = 0; i < sC1Len; i++) {
			check = 0 + ((sampleC1[i] <= lowP) | (sampleC1[i] >= hiP));
			extCount += check;
			cat1Color[i] = check;
		}
	}
	//console.log(d3.sum(cat1Color));
	cat1Pval = extCount / sC1Len;
	c1Tstdata = [sampleC1, cat1Color];
	cat1InfOutput = discreteChart(c1Tstdata, cat1InfSVG, cat1TestInteract);

	cat1ftr = document.getElementById("cat1Results");
	cat1ftr.innerHTML = "<div  style = 'width:320px'> Proportion " + cat1Label1 + " in " + sC1Len + " Samples from H<sub>0</sub> <br>" + "p-value (strength of evidence): " + formatPvalue(extCount, sC1Len) + "</div>";
	//
	//cat1ftr.style.display = 'block';
	document.getElementById("cat1MoreSims").style.display = 'block';

}

function cat1CIinteract(d, i) {
	// open box to show success and failure counts in the selected sample;
	var cat1Modal = document.getElementById("cat1WhichDot"),
		 cat1ModalContent = document.getElementById("cat1SelectedSample");
	cat1Modal.style.display = "block";
	cat1ModalContent.innerHTML = "Proportion " + c1Data[0].label + ":  p&#770; =  "   + c1CIdata[0][i].toPrecision(4) +
	  "<br> Click to Close" ;
	// open modal box to show success and failure counts in the selected resample;

} 


function cat1TestInteract(d, i) {
	// open modal box to show success and failure counts in the selected sample;
	var cat1Modal = document.getElementById("cat1WhichDot"),
		 cat1ModalContent = document.getElementById("cat1SelectedSample");
	cat1Modal.style.display = "block";
	cat1ModalContent.innerHTML = "Proportion " + c1Data[0].label + ":  p&#770; =  "   + c1Tstdata[0][i].toPrecision(4) +
	  "<br> Click to Close" ;
	// open modal box to show success and failure counts in the selected resample;

} 

function cat1MoreSimFn() {
	// function to add more points to an estimate or test of one proportion
	var sC1Len,
	    more = +document.getElementById("cat1More").value,
	    newValues = [];
	document.getElementById("cat1WhichDot").style.display = "none";    
	if (more > 0) {
		total = cat1N1 + cat1N2;
		cat1Phat = cat1N1 / total;

		if (cat1Inference === 'test') {
			newValues = rbinom(total, cat1Pnull, more);
			for ( i = 0; i < more; i++) {
				sampleC1.push(newValues[i] / total);
			}
			sampleC1 = sampleC1.sort(function(a, b) {return a - b;	});
			cat1TestUpdate();
			//cat1InfOutput = discreteChart(sampleC1, cat1InfSVG, cat1TestInteract );
			return (sampleC1);
		} else {
			newValues = rbinom(total, cat1Phat, more);
			for ( i = 0; i < more; i++) {
				resampleC1.push(newValues[i] / total);
			}
			resampleC1 = resampleC1.sort(function(a, b) {return a - b;	});
			cat1CLChange(cat1CnfLvl);

			return (resampleC1);
		}
	}
}

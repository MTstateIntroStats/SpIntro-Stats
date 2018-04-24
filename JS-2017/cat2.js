// subroutine to estimate a proportion or test for a special value
// Inputs:
//     2 sets of category labels (default = Success/Failure) and a count for each of 4 outcomes
// TODO:
// need to clear out 'estimate' header if data change
//  summary plot isn't working
 // adding more points to an inference - estimating plot gives way too many red points
 //    (clicking slider again fixes it)
	// for sample/resample store an array of 3 vectors: sample1, sample2, and diff in proportions
	// then use d3.sort to order the array by difference

var c2SummDiv = d3.select("#cat2Inference"),
    c2Tstdata,
    c2CIdata,
    cat2LabelOut1,
    cat2LabelOut2,
    cat2LabelPop1,
    cat2LabelPop2,
    cat2hdr,
    cat2CnfLvl = .80,
    cat2CLvl,
    cat2Diff,
    cat2lowerBd,
    cat2upperBd,
    cat2N11,
    cat2N21,
    cat2N12,
    cat2N22,
    cat2NullEst,
    cat2Pval,
    cat2Phat1,
    cat2Phat2,
    cat2Diff,
    cat2TestDirection,
    c2Data = [],
    c2bars,
    chartC2,
    cat2Color = [],
    confLevels = [{	key : "80%",	value : "0.80"}, 
    				{	key : "90%",	value : "0.90"}, 
    				{	key : "95%",	value : "0.95"}, 
    				{	key : "99%",	value : "0.99"}],
    c2Inference,
    c2InfOutput,
    hyperGeom =[],
    targetQuantile,
    upperBd,
    upperCI,
    resampleC21 = [],
    resampleC22 = [],
    resampleC2 = [],
    sampleC21 = [],
    sampleC22 = [],
    sampleC2 = [],
    total1,
    total2;

var svgCat2 = d3.select("#cat2InfSVG");

function summarizeP2() {
	// builds summary table and plot for 2 categorical variables
	var margin = 20,
	    barHeight = 20,
	    colors = [],
	    padding = 25,
	    w = 180,
	    h = 60;

	resampleC2 = [];
	sampleC2 = [];

	cat2LabelOut1 = document.getElementById("cat2LabelOut1").value;
	cat2LabelOut2 = document.getElementById("cat2LabelOut2").value;
	cat2LabelPop1 = document.getElementById("cat2LabelPop1").value;
	cat2LabelPop2 = document.getElementById("cat2LabelPop2").value;
	cat2N11 = +document.getElementById("cat2N11").value;
	cat2N12 = +document.getElementById("cat2N12").value;
	cat2N21 = +document.getElementById("cat2N21").value;
	cat2N22 = +document.getElementById("cat2N22").value;
	total1 = cat2N11 + cat2N21;
	total2 = cat2N12 + cat2N22;
	cat2Phat1 = cat2N11 / total1;
	cat2Phat2 = cat2N12 / total2;
	cat2Diff = cat2Phat1 - cat2Phat2;
	
	c2Data = [{	"label" : cat2LabelPop1,	"xx" : cat2Phat1}, 
	          {	"label" : cat2LabelPop2,	"xx" : cat2Phat2}];
	cat2Summ = document.getElementById("cat2SummaryText");
	cat2Summ.innerHTML = "p&#770;<sub>1</sub> =  " + cat2Phat1.toPrecision(4) + 
	   "&nbsp; &nbsp; p&#770;<sub>2</sub> =  " + cat2Phat2.toPrecision(4) + 
	   " <br> p&#770;<sub>1</sub> - p&#770;<sub>2</sub> = " + cat2Diff.toPrecision(5);
	cat2Summ.style = "display: block";
	
	document.getElementById("cat2ObsdDiff").innerHTML = "&nbsp;&nbsp;" + 
		   		cat2Diff.toPrecision(4) +" from above."

	var c2xScale = d3.scaleLinear().domain([0, 1]).range([0, w - 3 * margin]);

	var c2xAxis = d3.axisBottom(c2xScale)
					.ticks(3);
	if(typeof(chartC2) ==='function'){
		chartC2.data([cat2Phat1,cat2Phat2])
	} else{
		chartC2 = propBarChart().data([cat2Phat1,cat2Phat2]).height(140);		  
		d3.select('#cat2SummarySVGgoesHere').call(chartC2);
    }

}

function cat2CLChange(arg) {
	// set colors for dots to illustrate confidence interval
	var sC2Len, cnfLvl =0.60,
	    tempColors = [],
	    twoTail;
	if (arg.value) {
		cnfLvl = +arg.value;
	} 
	if (c2CIdata) {
		sC2Len = c2CIdata[0].length;
		tempColors = ciColor(c2CIdata[0], cnfLvl);
		cat2lowerBd = tempColors[1].toPrecision(4);
		cat2upperBd = tempColors[2].toPrecision(4);
		cnfLvl = tempColors[3];
		c2CIdata = [c2CIdata[0], tempColors[0]];
		c2InfOutput = discreteChart(c2CIdata, cat2InfSVG, cat2CIinteract);
		// Works for small counts, but is all flat for just a few points from large count data

		sC2Len = c2CIdata[0].length;
	} 
	cat2ftr = document.getElementById("cat2Results"); 
	cat2ftr.innerHTML = 
	"<div> Difference in proportions \" " + cat2LabelOut1 + "\" in  " + 
		sC2Len + " Re-samples" + "<br> <br>" + Math.round(cnfLvl * 100) + 
		"% Confidence Interval: (" + cat2lowerBd + ", " + cat2upperBd + " )</div>";
	cat2ftr.style.display = "block";
	document.getElementById("cat2Output").style.display = 'block';
	document.getElementById("cat2MoreSims").style.display = 'block';
}

var cat2CIrangeslide = rangeslide("#cat2ConfLvlInpt", {
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
		"valueChanged" : [cat2CLChange]
	}
});

function estimateP2() {
	//function to estimate the true proportion based on a sample of 'success/failure' data
	//  repeatedly samples from the observed data to build a distribution of possible 
	//	 differences in proportions

	var sC2Len, cnfLvl, CI = [];
	summarizeP2();
	// Gather Inputs:	 
	cat2hdr = document.getElementById("cat2Output");
	cat2hdr.innerHTML = "<h4>Estimate True Proportion with a Confidence Interval</h4>";
	cat2hdr.style.display = "block";
	// show data summary
	document.getElementById("cat2ConfLvlInpt").style.display = "block";
	document.getElementById("cat2TestInpt").style.display = "none";
	//document.getElementById("cat2WhichDot").style.display = "none";
	// show plot starting with 100 points
	resampleC21 = rbinom(total1, cat2Phat1, 100);
	resampleC22 = rbinom(total2, cat2Phat2, 100);
	sC2Len = resampleC21.length;
	
	for ( i = 0; i < sC2Len; i++) {
		resampleC2[i] = resampleC21[i] / total1 - resampleC22[i] / total2;
	}
	resampleC2 = resampleC2.sort(function(a, b) {return a - b;});

	CI = ciColor(resampleC2, cat2CnfLvl);
	
	cat2Color = CI[0];
	cat2lowerBd = CI[1].toPrecision(4);
	cat2upperBd = CI[2].toPrecision(4);
	cnfLvl = CI[3];

	cat2ftr = document.getElementById("cat2Results");
	cat2ftr.innerHTML = "<div style='width=50px'></div>" + 
		"<div style = 'width:360px'> Difference in proportion \"" + 
		cat2LabelOut1 + "\" in  " + sC2Len + " Re-samples" + "<br> <br>" + 
		Math.round(cnfLvl * 100) + "% Confidence Interval: (" + 
		cat2lowerBd + ", " + cat2upperBd + " )</div>";
	cat2ftr.style.display = "block";
	//console.log(cat2lowerBd, cat2upperBd);

	return ([resampleC2, cat2Color]);

}

function testP2(tailChoice) {
	//function to test 'Are two proportions equal?'
	// generates "null" data from hypergeometric distribution which assumes table margins are fixed.
	// output is a dataset ready to go into the discrete plot function
	var sC2Len;
	// Gather Inputs:
	 summarizeP2();
	 
	//cat2NullEst = (cat2N11 + cat2N12) / (total1 + total2);
	document.getElementById("cat2ConfLvlInpt").style.display = "none";
	//document.getElementById("cat2WhichDot").style.display = "none";
	document.getElementById("cat2TestInpt").style.display = "block";

	//if (tailChoice === 'undefined') 
	{
		cat2hdr = document.getElementById("cat2TestInpt");
		cat2ftr = document.getElementById("cat2Results");
		cat2ftr.style.display = 'block';
		cat2hdr.style.display = 'block';
		cat2hdr.innerHTML = "<div class = 'w3-cell-row'> "+
		   "<div class = 'w3-cell' style = 'width:40%'> Stronger evidence is a difference </div>" +
		    "<div class = 'w3-cell' style='width:35%'>" + 
		    "<select class = 'w3-select w3-card w3-border w3-mobile w3-pale-yellow' " + 
		    " id='cat2Extreme' onchange = 'cat2TestUpdate()' >" + 
		    "<option value='lower'>Less Than or Equal to </option>" + 
		    "<option value='both' selected >As or More Extreme Than</option>" + 
		    "<option value='upper'>Greater Than or Equal to</option>" + 
		    "</select> </div>  <div class ='w3-cell' style = 'width:30%'> &nbsp;&nbsp;" + 
		     cat2Diff.toPrecision(4) + " (from above) </div> </div> ";
		cat2ftr.innerHTML = "<div  style = 'width:600px'> Difference in proportions \"" + 
		          cat2LabelOut1 + "\" in samples from H<sub>0</sub>";
	} //else {	}
	hyperGeom = hypergeomPMF(total1, total2, cat2N11 + cat2N12);
	sampleC21 = sampleWrep(hyperGeom[0],100, hyperGeom[1])[0].sort(function(a, b) {return a - b;});
	sC2Len = sampleC21.length;
	for ( i = 0; i < sC2Len; i++) {
		sampleC22[i] =  cat2N11 + cat2N12 - sampleC21[i];
		sampleC2[i] = sampleC21[i] / total1 - sampleC22[i] / total2;
	}
	c2InfOutput = discreteChart(sampleC2, cat2InfSVG, cat2TestInteract);
	return (sampleC2);
}

function cat2TestUpdate() { 
	// after we get some data to display in the plot, we often want to add more.
	var check,
	    extCount = 0,
	    lowP,
	    hiP,
	    sC2Len;
	//moveOver, oldP;
	c2Inference = 'test';
	// get direction of evidence:
	cat2TestDirection = document.getElementById("cat2Extreme").value;

	if (!(sampleC2)) {
		sampleC2 = testP2();
	}
	sC2Len = sampleC2.length;
	if (cat2TestDirection === "lower") {
		for ( i = 0; i < sC2Len; i++) {
			check = 0 + (sampleC2[i] <= cat2Diff);
			extCount += check;
			cat2Color[i] = check;
		}
	} else if (cat2TestDirection === "upper") {
		for ( i = 0; i < sC2Len; i++) {
			check = 0 + (sampleC2[i] >= cat2Diff);
			extCount += check;
			cat2Color[i] = check;
		}

	} else {
		lowP = ((cat2Diff < 0.00) ? cat2Diff : -cat2Diff) + 1 / 1000000;
		hiP = ((cat2Diff > 0.00) ? cat2Diff : -cat2Diff) - 1 / 1000000;
		for ( i = 0; i < sC2Len; i++) {
			check = 0 + ((sampleC2[i] <= lowP) | (sampleC2[i] >= hiP));
			extCount += check;
			cat2Color[i] = check;
		}
	}
	//console.log(d3.sum(cat2Color));
	cat2Pval = extCount / sC2Len;
	c2Tstdata = [sampleC2, cat2Color];
	c2InfOutput = discreteChart(c2Tstdata, cat2InfSVG, cat2TestInteract);

	cat2ftr = document.getElementById("cat2Results");
	cat2ftr.innerHTML = "<div  style = 'width:600px'> Differences in proportions \"" + 
		cat2LabelOut1 + "\" in " + sC2Len + " Samples from H<sub>0</sub> <br>" + 
		"p-value (strength of evidence): " + formatPvalue(extCount, sC2Len) + "</div>";
	//

}

function cat2CIinteract(d, i) {
	// open  box to show success and failure counts in the selected resample;
	
	var cat2Modal = document.getElementById("cat2WhichDot"),
		 cat2ModalContent = document.getElementById("cat2SelectedSample");
	cat2Modal.style.display = "block";
	cat2ModalContent.innerHTML = "Difference in Proportions " + c2Data[0].label+ ": "+ 
			c2CIdata[0][i].toPrecision(4) +	  "<br> Click to Close" ;
	
} 

function cat2TestInteract(d, i) {
	//console.log(d.x);
	// open  box to show success and failure counts in the selected sample;
	var cat2Modal = document.getElementById("cat2WhichDot"),
		 cat2ModalContent = document.getElementById("cat2SelectedSample");
	cat2Modal.style.display = "block";
	cat2ModalContent.innerHTML = "Difference in Proportions " + c2Data[0].label+ ": "+ c2Tstdata[0][i].toPrecision(4) +
	  "<br> Click to Close" ;
} ;

function cat2MoreSimFn() {
	// function to add more points to an estimate or test of one proportion
	//  change from sampleC2 vector to a list containing A_counts, B_counts, and the difference in phats
	var sC2Len,
	    more = +document.getElementById("cat2More").value,
	    newValues1 = [],
	    newValues2 = [];
	if (more > 0) {
		document.getElementById("cat2WhichDot").style.display = "none";
		if (c2Inference === 'test') {
			if(! hyperGeom[0]){
				hyperGeom = hypergeomPMF(total1, total2, cat2N11 + cat2N12);
			}
			newValues1 =  sampleWrep(hyperGeom[0], more, hyperGeom[1])[0];
			for ( i = 0; i < more; i++) {
				sampleC21.push(newValues1[i]);
				sampleC22.push(cat2N11 + cat2N12 - newValues1[i]);
				sampleC2.push(newValues1[i] / total1 - sampleC22[i] / total2);
			}
			sampleC2 = sampleC2.sort(function(a, b) {
				return a - b;
			});
			cat2TestUpdate();
			//c2InfOutput = discreteChart(sampleC2, cat2InfSVG, cat2TestInteract );
			return (sampleC2);
		} else {
			resampleC21 = rbinom(total1, cat2Phat1, more);
			resampleC22 = rbinom(total2, cat2Phat2, more);
			for ( i = 0; i < more; i++) {
				resampleC2.push(resampleC21[i] / total1 - resampleC22[i] / total2);
			}
			resampleC2 = resampleC2.sort(function(a, b) {
				return a - b;
			});
			//console.log(cat2CnfLvl);
			cat2CLChange(cat2CnfLvl);
			return (resampleC2);
		}
	}
}

// subroutine to estimate a difference in proportions or test for no difference
// Inputs:
//     2 group labels (default = Group A, Group B)
//     2 outcome labels (default Success/Failure) and 4 counts
//TODO:
//
//  summary plot needs labels
// when clicking a point in the inference plot, show the resample in 2 colors

var c1q1SummDiv = d3.select("#C1Q1Inference"),
    c1q1Tstdata,
    c1q1CIdata,
    c1q1Label,
    c1q1Values,
    c1q1ftr,
    c1q1hdr,
    c1q1CnfLvl = 0.80,
    c1q1CLvl,
    c1q1Colors =[],
    c1q1Groups = [],
    c1q1lowerBd,
    c1q1upperBd,
    c1q1N1,
    c1q1N2,
    c1q1ObsdDiff,
    c1q1Pval,
    c1q1TestDirection,
    c1q1Data = [],
    c1q1RawData = [],
    c1q1DataName,
    c1q1Color = [],
    confLevels = [{
	key : "80%",
	value : "0.80"
}, {
	key : "90%",
	value : "0.90"
}, {
	key : "95%",
	value : "0.95"
}, {
	key : "99%",
	value : "0.99"
}],
    c1q1Inference,
	c1q1Keys = [],
    //c1q1InfOutput,
    c1q1Shifted = [],
    c1q1SumPlot,
    dataLength,
    diff,
    targetQuantile,
    x = [],
    x1 = [],
    x2 = [],
    xbar1,
    x1Var,
    xbar2,
    x2Var,
    upperBd,
    upperCI,
    sampleCIc1q1,
    sampleTstc1q1,
    shift;

var svgc1q1 = d3.select("#C1Q1InfSVG");




var C1Q1CIrangeslide = rangeslide("#C1Q1ConfLvl", {
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
		"valueChanged" : [c1q1CLChange]
	}
});

function summarizeDiff() {
	// builds summary table and dot plot for a quantitative variable split by a categorical variable
	var margin = 20,
	    w = 200,
	    h = 200,
	    c1q1Response;

	if (c1q1SumPlot) {
		c1q1CIdata = [];
		c1q1TstData = [];
		x1 = [];
		x2 = [];
		document.getElementById("C1Q1Results").style.display = "none";
		document.getElementById("C1Q1Output").style.display = "none";
		document.getElementById("C1Q1MoreSims").style.display = "none";
		document.getElementById("C1Q1ConfLvl").style.display = "none";
		document.getElementById("C1Q1Test").style.display = "none";
		document.getElementById("C1Q1Inference").style.display = "none";
		document.getElementById("C1Q1WhichDot").style.display = "none";
		document.getElementById("c1q1WhichDotA").style.display = "none";
	}

	c1q1DataName = document.getElementById("C1Q1DataName").value;
	c1q1RawData = (c1q1DataName === "SATprep") ? SATprep : (c1q1DataName === "smoker") ? smoker : (c1q1DataName === "music1") ? musicVSsilence1 : (c1q1DataName === "REDvsREDA") ? REDvsREDA : (c1q1DataName === "REDAvsCntrl") ? REDAvsCntrl : "undefined";

	dataLength = c1q1RawData.length;
	c1q1Response = Object.keys(c1q1RawData[0])[1];
	c1q1Keys = d3.map(c1q1RawData, function(d) {
		return d.group;
	}).keys();

	for ( i = 0; i < dataLength; i++) {
		if (c1q1RawData[i].group === c1q1Keys[0]) {
			x1.push(c1q1RawData[i][c1q1Response])
		} else {
			x2.push(c1q1RawData[i][c1q1Response]);
		}
	}

	xbar1 = d3.mean(x1);
	x1Var = d3.variance(x1);
	c1q1N1 = x1.length;
	xbar2 = d3.mean(x2);
	diff = xbar1 - xbar2;
	x2Var = d3.variance(x2);
	c1q1N2 = x2.length;
	x1 = x1.sort(function(a,b){return(a-b);});
	x2 = x2.sort(function(a,b){return(a-b);});
	c1q1Groups = repeat(0, c1q1N2).concat(repeat(1, c1q1N1));
	x = x2.concat(x1);

	c1q1SumPlot = dbl_histodot(x, c1q1Groups, c1q1Keys, C1Q1SumSVG, c1q1InteractFnA);

	c1q1Summ = document.getElementById("C1Q1SummaryText");
	c1q1Data = [{
		"label" : "Mean1",
		"xx" : xbar1
	}, {
		"label" : "Mean2",
		"xx" : xbar2
	}, {
		"label" : "Diff",
		"xx" : diff
	}];
	c1q1Summ.innerHTML = " Summary &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br><br>"+
						c1q1Keys[0] + "   Mean: " + xbar1.toPrecision(4) + 
										"<br> SD: " + Math.sqrt(x1Var).toPrecision(4) +
										"&nbsp;&nbsp; n: " + c1q1N1 + 	
										"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br> <br> <br>" + 
						c1q1Keys[1] + "   Mean: " + xbar2.toPrecision(4) + 
										"<br> SD: " + Math.sqrt(x2Var).toPrecision(4) +
										"&nbsp;&nbsp; n: " + c1q1N2 + 	
										"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
										"<br><br> Difference in Means: " + diff.toPrecision(4) ;
	c1q1Summ.style = "display: block";
	// display next step: select inference
	document.getElementById("C1Q1SelectInf").style.display = 'block';
	c1q1ObsdDiff = document.getElementById("C1Q1ObsdDiff")
	if(c1q1ObsdDiff){
		c1q1ObsdDiff.innerHTML =  "&nbsp;&nbsp;" + diff.toPrecision(4) + " from above.";
	}
}

function c1q1InteractFnA(d, i) {
	//console.log(d.x);
	// open modal box to show x value and group of selected dot;
	var c1q1Modal = document.getElementById("c1q1WhichDotA"),
	    c1q1ModalContent = document.getElementById("c1q1SelectedSampleA");
	c1q1Modal.style.display = "block";
	c1q1ModalContent.innerHTML = "Value: " + c1q1SumPlot[1][i].x.toPrecision(4) + ",  " + c1q1Keys[1 - c1q1SumPlot[1][i].color] + 
									" <br> Click to Close";
	// open modal box to show slope of the selected resample;
	//window.onclick = function(event) {
	//	if (event.target == c1q1Modal) {
	//		c1q1Modal.style.display = "none";
	//	}
	//}
}



function estimateDiff(nreps) {
	//function to estimate the differences in mean based on resamples of original numeric data
	
	//TODO: starts OK on first click to choose estimation, 
		// but afterwards, c1q1CIdata starts nesting in complexity.
	var cnfLvl = 0.60, 
	    CI = [], 
	    indices = sequence(0, nreps, 1);

	document.getElementById("C1Q1WhichDot").style.display = "none"; // close modals
	// Gather Inputs:

	c1q1hdr = document.getElementById("C1Q1Output");
	c1q1hdr.innerHTML = "<h4>Estimate True Mean with a Confidence Interval</h4>";

	c1q1CLvl = document.getElementById("C1Q1ConfLvl");
	c1q1CLvl.style.display = "block";
	c1q1Tst = document.getElementById("C1Q1Test");
	c1q1Tst.style.display = "none";
	// show plot

	sampleCIc1q1 = resampleDiffMeans(x1, x2, nreps);
	// Now sort the indices by order of the first element which is diff
	//console.log(sampleCIc1q1);
	indices.sort(function(a, b){ return sampleCIc1q1.diff[a] -sampleCIc1q1.diff[b];});
	//sampleCIc1q1.diff  = indices.map( function (ndx) {return sampleCIc1q1.diff[ndx];});
	sampleCIc1q1.mean1 = indices.map( function (ndx) {return sampleCIc1q1.mean1[ndx];});
	sampleCIc1q1.mean2 = indices.map( function (ndx) {return sampleCIc1q1.mean2[ndx];});
	for(i=0; i< nreps; i++){
		sampleCIc1q1.diff[i] = sampleCIc1q1.mean1[i] - sampleCIc1q1.mean2[i];
	}
	
	CI = ciColor(sampleCIc1q1.diff, c1q1CnfLvl);
	c1q1Color = CI[0];
	c1q1lowerBd = CI[1].toPrecision(4);
	c1q1upperBd = CI[2].toPrecision(4);
	cnfLvl = CI[3];

	document.getElementById("C1Q1Inference").style.display = "block";
	c1q1ftr = document.getElementById("C1Q1Results");
	c1q1ftr.style.display = 'block';
	c1q1ftr.innerHTML = "<div style='width=50px'></div>" + "<div style = 'width:400px'> " +
	   "Plot shows difference in means in  " + nreps + " Re-samples" + "<br> <br>" + Math.round(cnfLvl * 100) + "% Confidence Interval: (" + c1q1lowerBd + ", " + c1q1upperBd + " )</div>";
	document.getElementById("C1Q1MoreSims").style.display = 'block';

	//console.log("CI:", c1q1lowerBd, c1q1upperBd);

	return (sampleCIc1q1);
}

function testDiff(tailChoice) {
	//function to test 'Is difference in means  = 0?' for quantitative data grouped by a categorical variable
	// to force the null hypothesis to be true, we resample from
	// a shifted distribution of values which has sample mean equal to the hypothesized mean.
	// Gather Inputs:

	c1q1Null = 0.00;
	//+document.getElementById("c1q1trueDiff").value;
	//c1q1N = c1q1Values.length;
	document.getElementById("C1Q1WhichDot").style.display = "none"; // close modals

	c1q1CLvl = document.getElementById("C1Q1ConfLvl");
	c1q1CLvl.style.display = "none";

	c1q1Tst = document.getElementById("C1Q1Test");
	c1q1Tst.style.display = "block";

	var sc1q1Len, indices = [];
	shift = diff - c1q1Null;
	c1q1Shifted = [];

	for ( i = 0; i < c1q1N2;i++) {
		c1q1Shifted.push(x2[i] + shift);
	}

	if (tailChoice === 'undefined') {
		c1q1hdr = document.getElementById("C1Q1Test");
		c1q1ftr = document.getElementById("C1Q1Results");
		c1q1hdr.innerHTML = "<div class = 'w3-cell-row'> <div class = 'w3-cell' style = 'width:50%'> " + " Stronger evidence is sample difference </div>" + "<div class = 'w3-cell' style='width:40%'>" + "<select class = 'w3-select w3-card w3-border w3-mobile w3-pale-yellow' id='c1q1Extreme'" + " onchange = 'c1q1TestUpdate()' >" + "<option value='lower'>Less Than or =</option>" + "<option value='both' selected >As or More Extreme Than</option>" + "<option value='upper'>Greater Than or =</option>" + "</select> </div>  <div class ='w3-cell' style = 'width:30%'> &nbsp;&nbsp;" + diff.toPrecision(4) + "</div> </div> ";
		c1q1ftr.innerHTML = "<div  style = 'width:320px'> Difference in means in samples from H<sub>0</sub>";
		c1q1hdr.style.display = "block";
		c1q1ftr.style.display = "block";
		
		sampleTstc1q1 = resampleDiffMeans(x1, c1q1Shifted, 100);  
		indices = sequence(0, 100, 1);
		// Now sort the indices by order of the first element which is diff
		//console.log(sampleTstc1q1);
		indices.sort(function(a, b){ return sampleTstc1q1.diff[a] -sampleTstc1q1.diff[b];});
		sampleTstc1q1.diff = indices.map( function (ndx) {return sampleTstc1q1.diff[ndx];});
		sampleTstc1q1.mean1 = indices.map( function (ndx) {return sampleTstc1q1.mean1[ndx];});
		sampleTstc1q1.mean2 = indices.map( function (ndx) {return sampleTstc1q1.mean2[ndx];});

		sc1q1Len = sampleTstc1q1.diff.length;
		//console.log(d3.mean(samplec1q1), sc1q1Len);
	} else {
		// what if tailChoice is defined?
	}
	document.getElementById("C1Q1Inference").style.display = "block";
	document.getElementById("C1Q1MoreSims").style.display = 'block';
	return (sampleTstc1q1);
}
function c1q1CLChange(arg) {
	//update plot to illustrate confidence interval
	//TODO: starts OK on first click to choose estimation, 
		// but afterwards, c1q1CIdata starts nesting in complexity.
	var sc1q1Len,
	    cnfLvl = c1q1CnfLvl,
	    tempColors = [],
	    sampleCI = [];
	//console.log(arg);    
	if (arg.value) {
		cnfLvl  = c1q1CnfLvl  = +arg.value;
	} else {
		cnfLvl= 0.6;
	}
	if (!(sampleCIc1q1)) {
		sampleCIc1q1 = resampleDiffMeans(x1, x2, 100); //
		indices = sequence(0, 100, 1);
		// Now sort the indices by order of the first element which is diff
		indices.sort(function(a, b){ return sampleCIc1q1.diff[a] -sampleCIc1q1.diff[b];});
		sampleCIc1q1.diff = indices.map( function (i) {return sampleCIc1q1.diff[i];});
		sampleCIc1q1.mean1 = indices.map( function (i) {return sampleCIc1q1.mean1[i];});
		sampleCIc1q1.mean2 = indices.map( function (i) {return sampleCIc1q1.mean2[i];});
		sc1q1Len =100;
	} else {
		sc1q1Len = sampleCIc1q1.mean1.length;
		for(i=0; i < sc1q1Len; i++){
			sampleCIc1q1.diff[i] = sampleCIc1q1.mean1[i] - sampleCIc1q1.mean2[i]; 
		}
	}

	//console.log(c1q1CIdata.diff[0], sc1q1Len, c1q1CIdata.diff[sc1q1Len-1]);
	tempColors = ciColor(sampleCIc1q1.diff, cnfLvl);
		c1q1lowerBd = tempColors[1].toPrecision(4);
		c1q1upperBd = tempColors[2].toPrecision(4);
		cnfLvl = tempColors[3];
		c1q1Colors = tempColors[0];
		//sampleCI = sampleCIc1q1.diff;
		console.log(sampleCIc1q1.diff[0], sc1q1Len, sampleCIc1q1.diff[sc1q1Len-1]);
		//c1q1InfOutput = 
		histodot(sampleCIc1q1.diff, c1q1Colors, C1Q1InfSVG, c1q1CIinteract);
	
	document.getElementById("C1Q1MoreSims").style.display = 'block';
	c1q1ftr = document.getElementById("C1Q1Results");
	c1q1ftr.innerHTML = //"<div style = 'height = 10'> </div>" +
	"<div style = 'width:400px'> Plot shows difference in means in " + sc1q1Len + " resamples" + "<br> <br>" + Math.round(cnfLvl * 100) + "% Confidence Interval: (" + c1q1lowerBd + ", " + c1q1upperBd + " )</div>";
	c1q1ftr.style.display = 'block';
}


function c1q1TestUpdate() {
	// plots test inference, or updates an existing plot
	//TODO: starts OK on first click to choose testing, or if (!(sampleTstc1q1)) 
		// but afterwards, sampleTstc1q1 starts nesting in complexity.
	
	var check,
	    extCount = 0,
	    lowP,
	    hiP, indices =[],
	    diffMeans=[],
	    sc1q1Len;
	c1q1Inference = 'test';
	// get direction of evidence:
	c1q1TestDirection = document.getElementById("c1q1Extreme").value;
	document.getElementById("C1Q1WhichDot").style.display = "none"; // close modals

	if (!(sampleTstc1q1)) {
		sampleTstc1q1 = resampleDiffMeans(x1, c1q1Shifted, 100); //
		indices = sequence(0, 100, 1);
		// Now sort the indices by order of the first element which is diff
		indices.sort(function(a, b){ return sampleTstc1q1.diff[a] -sampleTstc1q1.diff[b];});
		sampleTstc1q1.diff = indices.map( function (i) {return sampleTstc1q1.diff[i];});
		sampleTstc1q1.mean1 = indices.map( function (i) {return sampleTstc1q1.mean1[i];});
		sampleTstc1q1.mean2 = indices.map( function (i) {return sampleTstc1q1.mean2[i];});
		sc1q1Len =100;
	} else {
		sc1q1Len = sampleTstc1q1.mean1.length;
		for(i=0; i < sc1q1Len; i++){
			sampleTstc1q1.diff[i] = sampleTstc1q1.mean1[i] - sampleTstc1q1.mean2[i]; 
		}
	}
	if (c1q1TestDirection === "lower") {
		for ( i = 0; i < sc1q1Len; i++) {
			check = 0.0 + (sampleTstc1q1.diff[i] <= diff);
			extCount += check;
			c1q1Color[i] = check;
		}
	} else if (c1q1TestDirection === "upper") {
		for ( i = 0; i < sc1q1Len; i++) {
			check = 0.0 + (sampleTstc1q1.diff[i] >= diff);
			extCount += check;
			c1q1Color[i] = check;
		}

	} else {
		lowP = diff * (diff <= c1q1Null) + (2 * c1q1Null - diff) * (diff > c1q1Null) + 1 / 1000000;
		hiP = diff * (diff >= c1q1Null) + (2 * c1q1Null - diff) * (diff < c1q1Null) - 1 / 1000000;
		for ( i = 0; i < sc1q1Len; i++) {
			check = 0.0 + ((sampleTstc1q1.diff[i] <= lowP) | (sampleTstc1q1.diff[i] >= hiP));
			extCount += check;
			c1q1Color[i] = check;
		}
	}
	//console.log(d3.sum(c1q1Color));
	c1q1Pval = extCount / sc1q1Len;
	c1q1Tstdata = sampleTstc1q1.diff;
	
	//  c1q1InfOutput = 
	
	// console.log(c1q1Tstdata[0], sc1q1Len, c1q1Tstdata[sc1q1Len-1]);
	histodot(c1q1Tstdata, c1q1Color, C1Q1InfSVG, c1q1TestInteract);

	c1q1ftr = document.getElementById("C1Q1Results");
	c1q1ftr.style.display = 'block';
	c1q1ftr.innerHTML = "<div  style = 'width:400px'> Difference in means in " + sc1q1Len + " Samples from H<sub>0</sub> <br>" + "p-value (strength of evidence): " + formatPvalue(extCount, sc1q1Len) + "</div>";
	document.getElementById("C1Q1MoreSims").style.display = 'block';
}

function c1q1CIinteract(d, i) {
	// open modal box to show diff in means in the selected resample;
	//console.log(d.x);
	var c1q1InfModal = document.getElementById("C1Q1WhichDot"),
	    c1q1InfModalContent = document.getElementById("C1Q1SelectedSample");
	c1q1InfModal.style.display = "block";
	c1q1InfModalContent.style.display = "block";
	c1q1InfModalContent.innerHTML = c1q1Keys[0] + "\t Mean: " + sampleCIc1q1.mean1[i].toPrecision(4) + 
							"<br>" + c1q1Keys[1] + "\t Mean: "  + sampleCIc1q1.mean2[i].toPrecision(4) +
							"<br> Difference: " + sampleCIc1q1.diff[i].x.toPrecision(4) +
							"<br> Click to Close"; 
	//window.onclick = function(event) {
	//	if (event.target == c1q1InfModal) {
	//		c1q1InfModal.style.display = "none";
	//	}
	//}
}

function c1q1TestInteract(d, i) {
	// open modal box to show diff in means in the selected resample;
	//console.log(d.x);
	var c1q1InfModal = document.getElementById("C1Q1WhichDot"),
	    c1q1InfModalContent = document.getElementById("C1Q1SelectedSample");
	c1q1InfModal.style.display = "block";
	c1q1InfModalContent.style.display = "block";
	c1q1InfModalContent.innerHTML = c1q1Keys[0] + "\t Mean: " + sampleTstc1q1.mean1[i].toPrecision(4) + 
							"<br>" + c1q1Keys[1] + "\t Mean: "  + sampleTstc1q1.mean2[i].toPrecision(4) +
							"<br> Difference: " + sampleTstc1q1.diff[i].x.toPrecision(4) +
							"<br> Click to Close"; 
	//window.onclick = function(event) {
	//	if (event.target == c1q1InfModal) {
	//		c1q1InfModal.style.display = "none";
	//	}
	//}
}

function c1q1MoreSimFn() {
	// function to add more points to an estimate or test of diff in means
	var indices = [],
	    more = +document.getElementById("C1Q1More").value,
	    newMns,
	    sampleLength;
   	document.getElementById("C1Q1WhichDot").style.display = "none"; // close modals

	if (more > 0) {
		if (c1q1Inference === 'test') {
			// assume difference in means is c1q1Null, generate resamples of x1 and of x2
			// and diff in means
			newMns = resampleDiffMeans(x1, c1q1Shifted, more);
			sampleTstc1q1.diff = sampleTstc1q1.diff.concat(newMns.diff);
			sampleTstc1q1.mean1 = sampleTstc1q1.mean1.concat(newMns.mean1);
			sampleTstc1q1.mean2 = sampleTstc1q1.mean2.concat(newMns.mean2);
			indices = sequence(0, sampleTstc1q1.diff.length, 1);
			// Now sort the indices by order of the first element which is diff
			indices.sort(function(a, b){ return sampleTstc1q1.diff[a] -sampleTstc1q1.diff[b];});
			sampleTstc1q1.diff = indices.map( function (ndx) {return sampleTstc1q1.diff[ndx];});
			sampleTstc1q1.mean1 = indices.map( function (ndx) {return sampleTstc1q1.mean1[ndx];});
			sampleTstc1q1.mean2 = indices.map( function (ndx) {return sampleTstc1q1.mean2[ndx];});
			//console.log("Test Diff[0]:", sampleTstc1q1.diff[0]);
			//update inference plot
			c1q1TestUpdate();
			//console.log(sampleTstc1q1);
			return (sampleTstc1q1);
		} else {
			// Estimating diff in means, so generate resamples from each group
			sampleLength = more + sampleCIc1q1.diff.length;
			console.log("CI Diff[0]:", sampleCIc1q1.diff[0] , sampleCIc1q1.mean1[0],sampleCIc1q1.mean2[0]);
			// it's already nested here, but testing above works ??!!!
			newMns = resampleDiffMeans(x1, x2, more);
			sampleCIc1q1.diff = sampleCIc1q1.diff.concat(newMns.diff);
			sampleCIc1q1.mean1 = sampleCIc1q1.mean1.concat(newMns.mean1);
			sampleCIc1q1.mean2 = sampleCIc1q1.mean2.concat(newMns.mean2);
			console.log("CI Diff[last]:", sampleCIc1q1.diff[sampleLength-2] , sampleCIc1q1.mean1[sampleLength-2],sampleCIc1q1.mean2[sampleLength-2]);
			
			indices = sequence(0, sampleLength, 1);
			// Now sort the indices by order of diff
			indices.sort(function(a, b){ return sampleCIc1q1.diff[a] - sampleCIc1q1.diff[b];});
			//sampleCIc1q1.diff = indices.map( function (ndx) {return sampleCIc1q1.diff[ndx];});
			sampleCIc1q1.mean1 = indices.map( function (ndx) {return sampleCIc1q1.mean1[ndx];});
			sampleCIc1q1.mean2 = indices.map( function (ndx) {return sampleCIc1q1.mean2[ndx];});
			for(i =0; i < sampleLength; i++){
				sampleCIc1q1.diff[i] = sampleCIc1q1.mean1[i] - sampleCIc1q1.mean2[i];
			}
			c1q1CLChange({value:c1q1CnfLvl});
			//console.log(sampleCIc1q1);
			return (sampleCIc1q1);
		}
	}
}

 // subroutine to estimate a slope or correlation or test for slope of zero
 // Inputs: 
 //     choose a prebuilt data set
 //TODO:
//  allow input of csv file and parse it.
//  summary plot -- show line of best fit
// Inference plot: coordinates are getting lost
 //  clicking a point should show resampled slope (and correlation?)
 
    var correlation,
    	intercept,
    	q2SummDiv = d3.select("#q2Inference"),
        q2Tstdata,
        q2CIdata,
        q2Label,
        q2Values,
		q2hdr,
		q2CnfLvl = 0.80,
		q2CLvl,
		q2lowerBd,
		q2upperBd,
		q2MuNull,
        q2N,
        q2Pval,
        q2TestDirection,
        q2SumData = [],
        q2RawData = [],
        q2DataName ,
        q2Color = [],
        confLevels = [
			{ key: "80%", value: "0.80" },
			{ key: "90%", value: "0.90" },
			{ key: "95%", value: "0.95" },
			{ key: "99%", value: "0.99" }
		], 
		q2Inference,
		q2InfOutput,
		q2SmryPlot,
		q2Values,
		slope,
		targetQuantile,
      	upperBd,
      	upperCI,
        resampleq2,
        sampleq2;

 var svgq2 = d3.select("#q2InfSVG"),    
      svgSumq2=d3.select("#quant2SumSVG");
 document.getElementById("quant2MoreSims").style.display = 'none';     
      
function summarizeSlope() {
      // builds summary table and dot plot for 2 quantitative variables
	var margin = 20,   
        colors = [],
        dataLength,
        w = 200,  
        h = 200,
        q2Keys,
        x=[], xbar, xVar,
        y=[], ybar, yVar, coVar=0;  
      
      q2DataName = document.getElementById("quant2DataName").value;
      q2RawData = (q2DataName === "shuttle")? shuttle:
      			(q2DataName === "women")?  womenJudgingMen:
      			(q2DataName === "men")?  menJudgeWomen :
      			(q2DataName === "dental")?  dental :
      			                 "undefined"; //(q2DataName === "other")
      			                 
      dataLength = q2RawData.length;
      q2Keys = Object.keys(q2RawData[0]);
      q2Values = [];
      
      for(i =0; i< dataLength; i++){
      	x.push( +q2RawData[i][q2Keys[0]]);
      	y.push( +q2RawData[i][q2Keys[1]]);
      	coVar += x[i] * y[i];     // add up cross product
      	q2Values.push({"x": x[i], "y": y[i]});
     }    
     xbar = d3.mean(x);
     xVar = d3.variance(x);
     ybar = d3.mean(y);
     yVar = d3.variance(y);
     coVar = (coVar - dataLength * xbar * ybar) /(dataLength-1) ;
     slope = coVar / xVar;
     intercept = ybar - (slope * xbar);
     correlation = coVar / Math.sqrt(yVar * xVar);
     
      q2Summ = document.getElementById("quant2SummaryText");
      q2SumData = [{"label": "Xbar", "xx": xbar},
			{"label": "Ybar", "xx": ybar},
			{"label": "Slope", "xx": slope},
			{"label": "Intercept", "xx": intercept}
			//{"label": "SD from line", "xx": }
			];
      q2Summ.innerHTML = "<br> x&#773; =  " + xbar.toPrecision(5) + "\t  y&#773; =  " + ybar.toPrecision(5) +
						 "<br> Sample Size = " + dataLength +
						 "<br> Slope of Least Squares Regression Line = " + slope.toPrecision(4) + 
						 "\t correlation = " + correlation.toPrecision(4) 
                         ;
      q2Summ.style = "display: block";  
      
	//if(typeof(q2SmryPlot) ==='function'){
	//	q2SmryPlot.data(q2Values)
	//} else{
	//	q2SmryPlot = xyChart().data(q2Values).height(300).width(300).fillColor('blue');		  
	//	d3.select('#quant2SummarySVGgoesHere').call(q2SmryPlot);
    //} 
	scatterPlot(q2Values, quant2SumSVG, q2InteractFnA );	
	document.getElementById("q2SelectInf").style.display = 'block';
}

function q2InteractFnA(d, i){
		// open box to show (x, y) coordinates in the selected point;
	var q2Modal = document.getElementById("q2WhichDotA"),
		 q2ModalContent = document.getElementById("q2SelectedSampleA");
	q2Modal.style.display = "block";
	q2ModalContent.innerHTML = "Coordinates: (" + q2Values[i].x.toPrecision(4) + ", "+
									q2Values[i].y.toPrecision(4) +
	  							") <br> Click to Close" ;
	// open modal box to show success and failure counts in the selected resample;
}
	
function q2CLChange(arg) {
	//update plot to illustrate confidence interval
	var sq2Len, cnfLvl = q2CnfLvl, 
		tempColors =[], twoTail;
      //q2Label = document.getElementById("q2Label").value;
      //q2Values = document.getElementById("q2Values").value.split(",");
      //q2N = q2Values.length;
	 if(arg.value){
			cnfLvl = q2CnfLvl =  +arg.value;
		};
	if(q2CIdata){
			sq2Len  = q2CIdata[0].length;
			tempColors = ciColor(q2CIdata[0], cnfLvl);
			 q2lowerBd = tempColors[1].toPrecision(4);
			 q2upperBd = tempColors[2].toPrecision(4);
			 cnfLvl = tempColors[3];
			q2CIdata = [q2CIdata[0], tempColors[0] ];
			q2InfOutput = discreteChart(q2CIdata, q2InfSVG, q2CIinteract);
		}
		q2ftr = document.getElementById("quant2Results");
		//q2ftr.style.display = 'block';
	 	q2ftr.innerHTML = //"<div style = 'height = 10'> </div>" +
	   "<div style = 'width:360px'> Plot shows Best Fit Lines in  "+ sq2Len + " Re-samples" +
	   "<br> <br>"+ Math.round(cnfLvl*100)+ 
	   "% Confidence Interval: (" + q2lowerBd +", "+ q2upperBd +" )</div>";
		//document.getElementById("quant2MoreSims").style.display = 'block';	   
}
	
var q2CIrangeslide = rangeslide("#quant2ConfLvlInpt", {
		data: confLevels,
		showLabels: true,
		startPosition: 0,
		showTicks: false,
        dataSource: "value",
        labelsContent: "key",
        valueIndicatorContent: "key",
		thumbWidth: 24,
		thumbHeight: 24,
		handlers: {
			"valueChanged": [q2CLChange]
		}
	});

  
function estimateSlope(){
	//function to estimate the true mean based on resamples of original numeric data
	var cnfLvl = q2CnfLvl,
		CI = [];
	
	// Gather Inputs:
      
	 q2hdr = document.getElementById("quant2Output");
	 q2hdr.innerHTML = 
	 "<h4>Estimate True Slope with a Confidence Interval</h4>"; 
	  
 	  q2CLvl = document.getElementById("quant2ConfLvlInpt");
	  q2CLvl.style.display ="";
	  q2Tst = document.getElementById("quant2TestInpt");
	  q2Tst.style.display ="none";
	 // show plot
	   
	  resampleq2 = resampleSlope4CI(q2Values,100).sort(function(a,b){return a - b});
	  sq2Len = resampleq2.length;
	  
	  CI =  ciColor(resampleq2, q2CnfLvl);
	  q2Color = CI[0];
	  q2lowerBd = CI[1].toPrecision(4);
	  q2upperBd = CI[2].toPrecision(4);
	  cnfLvl = CI[3];
	  
	 q2ftr = document.getElementById("quant2Results");
	 //q2ftr.style.display = 'block';
	 q2ftr.innerHTML = 
	   "<div style='width=50px'></div>"+
	   "<div style = 'width:360px'> Plot shows slopes in  "+ sq2Len + " Re-samples" +
	   "<br> <br>"+ Math.round(cnfLvl*100) + 
	   "% Confidence Interval: (" + q2lowerBd +", "+ q2upperBd +" )</div>"; 
	 //document.getElementById("quant2MoreSims").style.display = 'block';
 	  
	  //console.log(q2lowerBd, q2upperBd);
	  
	 return([resampleq2, q2Color]);		
} 	  
 
 

function testSlope(tailChoice){
	//function to test 'Is the true slope  = 0?' for 2 quantitative variables
	// to force the null hypothesis to be true, we resample from x and y independently 
	// Gather Inputs:
      
      //q2Label = document.getElementById("q2Label").value;
      //q2Values = document.getElementById("q2Values").value.split(",");
      q2Null = 0.00; //+document.getElementById("q2trueSlope").value;
      //q2N = q2Values.length;
      //q2Xbar = d3.mean(q2Values);
      
      q2CLvl = document.getElementById("quant2ConfLvlInpt");
	  q2CLvl.style.display ="none";
	    
	  q2Tst = document.getElementById("quant2TestInpt");
	  q2Tst.style.display ="block";
	 //q2Pval = undefined;
	  
      
	 if(tailChoice === 'undefined'){ 
	 	q2hdr = document.getElementById("quant2Output");
	 	q2hdr.innerHTML = "<div class = 'w3-cell-row'> <div class = 'w3-cell' style = 'width:50%'> "+
	 		" Stronger evidence is sample slope </div>"+ 
  	 	   "<div class = 'w3-cell' style='width:40%'>"+
  	 	   "<select class = 'w3-select w3-card w3-border w3-mobile w3-pale-yellow' id='q2Extreme'"+
  	 	   " onchange = 'q2TestUpdate()' >"+ 
  				"<option value='lower'>Less Than or =</option>"+
  				"<option value='both' selected >As or More Extreme Than</option>"+
  				"<option value='upper'>Greater Than or =</option>"+
		   	"</select> </div>  <div class ='w3-cell' style = 'width:30%'> &nbsp;&nbsp;" + slope.toPrecision(4) +
		   	"</div> </div> ";
		   q2ftr.innerHTML = 
		   "<div  style = 'width:320px'> Slope in samples from H<sub>0</sub>";
		 	sampleq2 = resampleSlope4Test(q2Values, 100);
		 	sq2Len = sampleq2.length;
		 	//console.log(d3.mean(sampleq2), sq2Len);
	 } else{
	 	
	 }
	 
	return(sampleq2);
} 	  

function resampleSlope4Test(data, reps){
	var coVar, 
		correlations =[],
		dataLength = data.length,
		slopes = [],
		xsample = [],
		ysample = [],
		xBar, 
		yBar, 
		xVar, 
		yVar,
	x = d3.map(data, function(d){return d.x}),
	y = d3.map(data, function(d){return d.y});
	
	for(i=0; i < reps; i++){
		coVar = xBar = xVar = yBar = yVar =0;
		xsample = d3.shuffle(x);
		ysample = d3.shuffle(y);
		for(j=0; j<dataLength; j++){
				coVar += xsample[j] * ysample[j];     // add up cross product
		}	
		 
     	xBar = d3.mean(xsample);
     	xVar = d3.variance(xsample);
     	yBar = d3.mean(ysample);
     	yVar = d3.variance(ysample);
     	coVar = (coVar - dataLength * xBar * yBar) /(dataLength-1) ;
     	slopes[j] = coVar / xVar;
     	correlations[j] = coVar / Math.sqrt(yVar * xVar);
    }
	return(slopes);	
}

function resampleSlope4CI(data, reps){
	var coVar, 
		correlations =[],
		dataLength = data.length,
		sample =[],
		slopes = [],
		xsample = [],
		ysample = [],
		xBar, 
		yBar, 
		xVar, 
		yVar;
	
	for(i=0; i < reps; i++){
		coVar = xBar = xVar = yBar = yVar =0;
		sample = d3.shuffle(sequence(0, dataLength-1,1) );
		
		for(j=0; j<dataLength; j++){
			xsample[j] = data[sample[j]].x;
			ysample[j] = data[sample[j]].y;
			coVar += xsample[j] * ysample[j];     // add up cross product
		}	
		xBar = d3.mean(xsample);
     	xVar = d3.variance(xsample);
     	yBar = d3.mean(ysample);
     	yVar = d3.variance(ysample);
     	coVar = (coVar - dataLength * xBar * yBar) /(dataLength-1) ;
     	slopes[j] = coVar / xVar;
     	correlations[j] = coVar / Math.sqrt(yVar * xVar);
    }
	return(slopes);	
}


function q2TestUpdate(){
	var check, 
		extCount = 0,
		lowP,
		hiP,
		sq2Len;
 	q2Inference = 'test';
 	// get direction of evidence:
 	 q2TestDirection = document.getElementById("q2Extreme").value;
 	
 	if(!(sampleq2)){
 		sampleq2 = testSlope();
 	}
 	sq2Len = sampleq2.length;
 	 if(q2TestDirection ==="lower"){
 	 	for(i = 0; i < sq2Len; i++){
 	 		check = 0 + (sampleq2[i] <= q2Xbar);
 	 		extCount += check;
			q2Color[i] =  check; 	 		
 	 	}
 	 } else if(q2TestDirection ==="upper"){
 	 	for(i = 0; i < sq2Len; i++){
 	 		check = 0 + (sampleq2[i] >= q2Xbar) ;
 	 		extCount += check;
			q2Color[i] =  check; 	 		
 	 	}
 	 	
 	 } else{
		lowP = q2Xbar * (q2Xbar <= q2MuNull) + (2*q2MuNull - q2Xbar)*(q2Xbar > q2MuNull)+ 1/1000000;
		hiP  = q2Xbar * (q2Xbar >= q2MuNull) + (2*q2MuNull - q2Xbar)*(q2Xbar < q2MuNull)- 1/1000000;
 	 	for(i = 0; i < sq2Len; i++){
 	 		check = 0 + ((sampleq2[i] <= lowP)|(sampleq2[i] >= hiP));
 	 		extCount += check;
			q2Color[i] =  check; 	 		
 	 	} 	 	
 	 }
 	 //console.log(d3.sum(q2Color));
 	 q2Pval = extCount / sq2Len;
 	 q2Tstdata = [sampleq2, q2Color];
  	q2InfOutput = dotChart(q2Tstdata, q2InfSVG, q2TestInteract ); 	
  	
	 q2ftr = document.getElementById("q2OutputResults");
	 //q2ftr.style.display = 'block';
	 q2ftr.innerHTML = 
	   "<div  style = 'width:320px'> Mean "+ q2Label +
	   " in " + sq2Len +" Samples from H<sub>0</sub> <br>"+
	   "p-value (strength of evidence): " + formatPvalue(extCount, sq2Len) + "</div>"; 
	 //document.getElementById("quant2MoreSims").style.display = 'block';
 	  
}

function q2CIinteract(d,i){
	//console.log(d.x);
	var q2modal = document.getElementById("q2SelectedSample");
	q2modal.style.display = "block";
	q2modal.innerHTML = slopes[i];
	// open modal box to show slopes in the selected resample;
	window.onclick = function(event) {
    if (event.target == q2modal) {
        q2modal.style.display = "none";
    	}
	}
} ;

function q2TestInteract(d,i){
	//console.log(d.x);
	// open modal box to show slope in the selected sample;
	//console.log(d.x);
	var q2modal = document.getElementById("q2SelectedSample");
	q2modal.style.display = "block";
	q2modal.innerHTML = slopes[i];
	// open modal box to show success and failure counts in the selected resample;
	window.onclick = function(event) {
    if (event.target == q2modal) {
        q2modal.style.display = "none";
    	}
	}
} ;

function quant2MoreSimFn(){
	// function to add more points to an estimate or test of slope
	var sq2Len,
		more = +document.getElementById("q2More").value,
		newValues =[];
	if(more > 0){
	 	if( q2Inference === 'test'){
			// assume slope is zero, generate samples of x and of y independently
	    		// fit new line to each
	    	for(i=0; i < more; i++){
	    		newx = sampleWoutRep(x, x.len);
	    		newy = sampleWoutRep(y, y.len);
	    		//get LSqs line slope & intercept
	      		sampleq2.push(newValues[i]);
	    	} 
	  		sampleq2 = sampleq2.sort(function(a,b){return a - b});
	  		//console.log(d3.mean(sampleq2), sampleq2.length);
	  		q2TestUpdate();
	  		return(sampleq2);
		} else{
		  // Estimating slope, so generate data using same (x,y) pairs, resampled errors
	    for(i=0; i < more; i++){
	        resampleq2.push(newValues[i]); 
	 	}
	 //console.log(q2CnfLvl);
	 q2CLChange(q2CnfLvl);  
     return(resampleq2);  
	}
  }
}

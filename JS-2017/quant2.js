 // subroutine to estimate a slope or correlation or test for slope of zero
 // Inputs: 
 //     choose a prebuilt data set
 //TODO:
//  allow input of csv file and parse it.
 //  clicking a point should show resampled slope (and correlation?)
 //  Clicking [Test] button first time in doesn't show the inference plot
 
    var correlation,
    	intercept,
    	i, j,
    	noChoice = "undefined",
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
		q2Null =0,
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
        resampleq2 =[],
        sampleq2 =[],
        sq2Len,
		targetQuantile,
      	upperBd,
      	upperCI,
		x=[],
		y=[];

 var svgq2 = d3.select("#quant2InfSVG"),    
      svgSumq2=d3.select("#quant2SumSVG");
 //document.getElementById("quant2MoreSims").style.display = 'none';     
      
function summarizeSlope() {
      // builds summary table and dot plot for 2 quantitative variables
	var margin = 20,   
        colors = [],
        dataLength,
        w = 200,  
        h = 200,
        q2Keys,
		xbar, xVar,
        ybar, yVar, 
        coVar=0;
		x=[];
		y=[];  
		sampleq2 = resampleq2 = []; 
      
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
      
	//check for any old output plots. If present, erase them due to change in data
	if (q2InfOutput) {
		q2CIdata = q2TstData = [];
		document.getElementById("quant2Results").style.display = "none";
		document.getElementById("quant2Output").style.display = "none";
		document.getElementById("quant2MoreSims").style.display = "none";
		document.getElementById("quant2ConfLvl").style.display = "none";
		document.getElementById("quant2Test").style.display = "none";
		document.getElementById("quant2Inference").style.display = "none";
		document.getElementById("quant2WhichSlope").style.display = "none";
	}
      
	scatterPlot(q2Values, quant2SumSVG, q2InteractFnA, intercept, slope );	
	// display next step: select inference
	document.getElementById("quant2SelectInf").style.display = 'block';
	document.getElementById("quant2ObsdSlope").innerHTML =
						"&nbsp;&nbsp;" + slope.toPrecision(4) +" from above.";
}

function q2InteractFnA(d, i){
		// open box to show (x, y) coordinates in the selected point;
	var q2Modal = document.getElementById("q2WhichDotA"),
		 q2ModalContent = document.getElementById("q2SelectedSampleA");
	q2Modal.style.display = "block";
	q2ModalContent.innerHTML = "Coordinates: (" + q2Values[i].x.toPrecision(4) + ", "+
									q2Values[i].y.toPrecision(4) +
	  							") <br> Click to Close" ;
	// open modal box to show slope of the selected resample;
	window.onclick = function(event) {
    if (event.target == q2Modal) {
        q2Modal.style.display = "none";
    	}
	}
}
	
function q2CLChange(arg) {
	//update plot to illustrate confidence interval
	var sq2Len, cnfLvl = q2CnfLvl, 
		tempColors =[], twoTail;
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
			//console.log(q2CIdata[0][1]);
			q2InfOutput = histogram(q2CIdata, quant2InfSVG, q2CIinteract);
		}
		document.getElementById("quant2Inference").style.display = "block";
		q2ftr = document.getElementById("quant2Results");
		q2ftr.style.display = 'block';
	 	q2ftr.innerHTML = //"<div style = 'height = 10'> </div>" +
	   "<div style = 'width:360px'> Plot shows slopes of Best Fit Lines in  "+ sq2Len + " Re-samples" +
	   "<br> <br>"+ Math.round(cnfLvl*100)+ 
	   "% Confidence Interval: (" + q2lowerBd +", "+ q2upperBd +" )</div>";
		document.getElementById("quant2MoreSims").style.display = 'block';	   
}
	
var q2CIrangeslide = rangeslide("#quant2ConfLvl", {
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

  
function estimateSlope(nReps){
	//function to estimate the true mean based on resamples of original numeric data
	var cnfLvl = q2CnfLvl,
		CI = [];
	
	// Gather Inputs:
      
	 q2hdr = document.getElementById("quant2Output");
	 q2hdr.innerHTML = 
	 "<h4>Estimate True Slope with a Confidence Interval</h4>"; 
	  
 	  q2CLvl = document.getElementById("quant2ConfLvl");
	  q2CLvl.style.display ="block";
	  q2Tst = document.getElementById("quant2Test");
	  q2Tst.style.display ="none";
	 // show plot
	   
	  resampleq2 = resampleSlope4CI(q2Values, nReps).sort(function(a,b){return a - b});
	  sq2Len = resampleq2.length;
	  
	  CI =  ciColor(resampleq2, q2CnfLvl);
	  q2Color = CI[0];
	  q2lowerBd = CI[1].toPrecision(4);
	  q2upperBd = CI[2].toPrecision(4);
	  cnfLvl = CI[3];
	  
	 q2ftr = document.getElementById("quant2Results");
	 q2ftr.style.display = 'block';
	 q2ftr.innerHTML = 
	   "<div style='width=50px'></div>"+
	   "<div style = 'width:360px'> Plot shows slopes of Best Fit Lines in  "+ sq2Len + " Re-samples" +
	   "<br> <br>"+ Math.round(cnfLvl*100) + 
	   "% Confidence Interval: (" + q2lowerBd +", "+ q2upperBd +" )</div>"; 
	 document.getElementById("quant2MoreSims").style.display = 'block';
 	  
	  //console.log(q2lowerBd, q2upperBd);
	  
	 return([resampleq2, q2Color]);		
} 	  
 
 

function testSlope(tailChoice){
	//function to test 'Is the true slope  = 0?' for 2 quantitative variables
	// to force the null hypothesis to be true, we resample from y independent of x 
	// Gather Inputs:
      
            
      q2CLvl = document.getElementById("quant2ConfLvl");
	  q2CLvl.style.display ="none";
	    
	  q2Tst = document.getElementById("quant2Test");
	  q2Tst.style.display ="block";
	 //q2Pval = undefined;
	  
      
	 if(tailChoice === 'undefined'){ 
		document.getElementById("quant2Inference").style.display = "block";
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
		   "<div  style = 'width:320px'> Plot shows slopes of Best Fit lines in  "+ sq2Len + " Re-ssamples from H<sub>0</sub>";
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
		seq = [],
		slopes = [],
		sample = [],
		xBar, 
		yBar, 
		xVar, 
		yVar,
		ysample =[];
		seq = sequence(0, dataLength-1, 1);
	
	    xBar = d3.mean(x);
     	xVar = d3.variance(x);
		//console.log(xVar);		
		
	for(i=0; i < reps; i++){
		coVar =0;
		yBar = 0;
		//xsample = sampleN(x, dataLength));
					 // could resample these as well as y's, but then we could get all x values equal
					 // instead, we'll assume it's a designed experiment with set (fixed) x levels
					   
		sample = sampleN(seq, dataLength);
		for(j=0; j < dataLength; j++){
				ysample[j] =  y[sample[j]];     // set y values
				coVar += x[j] * ysample[j];     // add up cross product
		}	
		// console.log(ysample);
		// console.log(coVar);
     	// xBar = d3.mean(xsample);
     	// xVar = d3.variance(xsample);
     	yBar = d3.mean(ysample);;
     	yVar = d3.variance(ysample);
     	coVar = (coVar - dataLength * xBar * yBar) /(dataLength-1) ;
     	slopes[i] = coVar / xVar;
     	correlations[i] = coVar / Math.sqrt(yVar * xVar);
    }
	return(slopes);	
}

function resampleSlope4CI(data, reps){
	var coVar, 
		correlations =[],
		dataLength = data.length,
		sample =[],
		seq = [],
		slopes = [],
		xsample = [],
		ysample = [],
		xBar, 
		yBar, 
		xVar, 
		yVar;
		seq = sequence(0, dataLength-1, 1);
	
	for(i=0; i < reps; i++){
		coVar  = 0;
		sample = sampleN(seq, dataLength );
		// resample from (x,y) pairs, keeping the connection between the two variables
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
     	slopes[i] = coVar / xVar;
     	correlations[i] = coVar / Math.sqrt(yVar * xVar);
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
 	 q2TestDirection = document.getElementById("quant2Extreme").value;
 	
 	if(!(sampleq2)){
 		sampleq2 = testSlope();
 	}
 	sq2Len = sampleq2.length;
 	 if(q2TestDirection ==="lower"){
 	 	for(i = 0; i < sq2Len; i++){
 	 		check = 0 + (sampleq2[i] <= slope);
 	 		extCount += check;
			q2Color[i] =  check; 	 		
 	 	}
 	 } else if(q2TestDirection ==="upper"){
 	 	for(i = 0; i < sq2Len; i++){
 	 		check = 0 + (sampleq2[i] >= slope) ;
 	 		extCount += check;
			q2Color[i] =  check; 	 		
 	 	}
 	 	
 	 } else{
		lowP = slope * (slope <= q2Null) + (2*q2Null - slope)*(slope > q2Null)+ 1/1000000;
		hiP  = slope * (slope >= q2Null) + (2*q2Null - slope)*(slope < q2Null)- 1/1000000;
 	 	for(i = 0; i < sq2Len; i++){
 	 		check = 0 + ((sampleq2[i] <= lowP)|(sampleq2[i] >= hiP));
 	 		extCount += check;
			q2Color[i] =  check; 	 		
 	 	} 	 	
 	 }
 	 //console.log(d3.sum(q2Color));
 	 q2Pval = extCount / sq2Len;
 	 q2Tstdata = [sampleq2, q2Color];
   	q2InfOutput = histogram(q2Tstdata, quant2InfSVG, q2TestInteract ); 	
	document.getElementById("quant2Inference").style.display = "block";
  	
	 q2ftr = document.getElementById("quant2Results");
	 q2ftr.style.display = 'block';
	 q2ftr.innerHTML = 
	   "<div  style = 'width:320px'> Slopes in " + sq2Len +" Resamples from H<sub>0</sub> <br>"+
	   "p-value (strength of evidence): " + formatPvalue(extCount, sq2Len) + "</div>"; 
	 document.getElementById("quant2MoreSims").style.display = 'block';
 	  
}

function q2CIinteract(d,i){
	console.log(d.x,i);
	var q2modal = document.getElementById("quant2SelectedSample");
	q2modal.style.display = "block";
	q2modal.innerHTML = "Slope: " + q2CIdata[0][i];
	// open modal box to show slopes in the selected resample;
	window.onclick = function(event) {
    if (event.target == q2modal) {
        q2modal.style.display = "none";
    	}
	}
} ;

function q2TestInteract(d,i){
	// open modal box to show slope in the selected sample;
	console.log(d.x, i);
	var q2modal = document.getElementById("quant2SelectedSample");
	q2modal.style.display = "block";
	q2modal.innerHTML = "Slope: " + q2Tstdata[0][i];
	// open modal box to show slope in the selected resample;
	window.onclick = function(event) {
    if (event.target == q2modal) {
        q2modal.style.display = "none";
    	}
	}
} ;

function quant2MoreSimFn(){
	// function to add more points to an estimate or test of slope
	var sq2Len,
		more = +document.getElementById("quant2More").value,
		newValues =[];
	if(more > 0){
	 	if( q2Inference === 'test'){
			// assume slope is zero, generate samples of x and of y independently
	    		// fit new line to each
	    	newValues = resampleSlope4Test(q2Values, more);
	    	for(i=0; i < more; i++){
	      		sampleq2.push(newValues[i]);
	    	} 
	  		sampleq2 = sampleq2.sort(function(a,b){return a - b;});
	  		//console.log(d3.mean(sampleq2), sampleq2.length);
	  		q2TestUpdate();
	  		return(sampleq2);
		} else{
		  // Estimating slope, so generate data using same (x,y) pairs
	     	newValues = resampleSlope4CI(q2Values, more)
	    
	    	for(i=0; i < more; i++){
	        	resampleq2.push(newValues[i]); 
	 		}
	 		resampleq2 = resampleq2.sort(function(a,b){return a - b;});
	 	//  console.log(q2CnfLvl);
	 	q2CLChange(q2CnfLvl);  
     	return(resampleq2);  
	}
  }
}

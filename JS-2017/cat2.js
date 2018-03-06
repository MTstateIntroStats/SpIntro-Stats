 // subroutine to estimate a proportion or test for a special value
 // Inputs: 
 //     2 sets of category labels (default = Success/Failure) and a count for each of 4 outcomes
 // TODO:
 // need to clear out resamples if data change
 
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
        c2bars ,
        chartC2 ,
        cat2Color = [],
        confLevels = [
			{ key: "80%", value: "0.80" },
			{ key: "90%", value: "0.90" },
			{ key: "95%", value: "0.95" },
			{ key: "99%", value: "0.99" }
		], 
		c2Inference,
		c2InfOutput,
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
	var margin = 30, 
    	barHeight = 20,  
        colors = [],
        w = 160,  
        h = 60,  
        
        x = d3.scale.linear()
         .domain([0, 1])
         .range([14, w - margin*2]);

	  var xAxis = d3.svg.axis()
    	.scale(x)
    	.ticks(5)
    	.orient("bottom");
      
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
      cat2Phat1 = cat2N11/ total1;
      cat2Phat2 = cat2N12/ total2;
      cat2Summ = document.getElementById("cat2SummaryText");
      c2Data = [{"label": cat2LabelPop1, "xx": cat2Phat1},
      			{"label": cat2LabelPop2, "xx": cat2Phat2}];
      cat2Summ.innerHTML = "p&#770;<sub>1</sub> =  " + cat2Phat1.toPrecision(4) +
                           "&nbsp; &nbsp; p&#770;<sub>2</sub> =  " + cat2Phat2.toPrecision(4) +
                           " <br> p&#770;<sub>1</sub> - p&#770;<sub>2</sub> = " +(cat2Phat1 - cat2Phat2).toPrecision(5) ;
      cat2Summ.style = "display: block"; 
    
    
   	if(!chartC2){
	    chartC2 = d3.select(".chartC2")
    	  .attr("width", w + margin*2)
      		.attr("height", h + margin);
     } else{
     		var oldbars = chartC2.selectAll("g").data(c2Data);
     		oldbars.remove();
     }
   // adding axis throws off the bar heights and doesn't allow nice updates.
   // TODO: use an unfilled rectangle instead??

    barC2 = chartC2.selectAll("g")
      .data(c2Data);
     barC2.enter().append("g")
        //.attr("fill", "blue")
        //.attr("transform", function(d, i) { return "translate(14," + i * barHeight + ")"; })
	    .each(function(d) {this._current = d;} );
	    //resets the figure to the current data
    
	  barC2.append("rect")
    	.attr("width", function(d){return x(d.xx );})// - 14;} )
    	.attr("height", barHeight -1)
        .attr("fill", "blue")
        .attr("transform", function(d, i) { return "translate(14," + i * barHeight + ")"; });
    
  	  barC2.append("text")
 		.attr("x",  function(d){return 16 + x(d.xx) ;})
 		.attr("y", function(d, i) { return (i+.5) * barHeight;})
 		.attr("dy", ".35em")
 		.text(function(d) {return d.label}) 
        .attr("fill", "blue");	
}
	
function cat2OnChange(arg) {
	// set colors for dots to illustrate confidence interval
	var sC2Len, 
		tempColors, twoTail;
		if(arg.value){
			cat2CnfLvl = +arg.value;
		};
		if(c2CIdata){
			sC2Len  = c2CIdata[0].length;
			tempColors = colorP2(c2CIdata[0]);
			 cat2lowerBd = tempColors[1].toPrecision(4);
			 cat2upperBd = tempColors[2].toPrecision(4);
			c2CIdata = [c2CIdata[0], tempColors[0] ];
			c2InfOutput = discreteChart(c2CIdata, cat2InfSVG, cat2CIinteract);
			//FIXME: this is not giving good y values when x values are differences (continuous vbles) 
			sC2Len = c2CIdata[0].length;
			twoTail = Math.round((1 - cat2CnfLvl)* sC2Len);
			if(twoTail % 2 === 1){ // check for odd number
				cat2CnfLvl = cat2CnfLvl - 1/sC2Len; // reduce to lower confidence
				//console.log(cat2CnfLvl);
			}  
		} else{
			console.log("No resampled data for CI");
		}
		cat2ftr = document.getElementById("cat2OutputFoot1");
	 	cat2ftr.innerHTML = //"<div style = 'height = 10'> </div>" +
	   "<div style = 'width:600px'> Difference in proportions \" "+ cat2LabelOut1 +"\" in  "+ sC2Len + " Re-samples" +
	   "<br> <br>"+ Math.round(cat2CnfLvl*100)+ 
	   "% Confidence Interval: (" + cat2lowerBd +", "+ cat2upperBd +" )</div>";	 
	   cat2ftr.style.display = "block"; 
   	   document.getElementById("cat2MoreSims").style.display = 'block'; 
}
	
var cat2CIrangeslide = rangeslide("#cat2ConfLvl", {
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
			"valueChanged": [cat2OnChange]
		}
	});

function colorP2(resample){
	// changes colors for CI illustration
	var color = [],
		lowerBd,
		upperBd,
		quantile, twoTail,
	    sC2Len = resample.length;
	  if(sC2Len > 0){  
	  	//console.log(cat2CnfLvl, sC2Len);
	  	twoTail = (1 - cat2CnfLvl)* sC2Len;
	  	quantile = Math.floor(twoTail / 2);
	  	if(! twoTail % 2){ // check for odd number 
	  		cat2CnfLvl = cat2CnfLvl - 1/sC2Len; // reduce to lower confidence
	  	} 
	  
		for(i=quantile; i <= sC2Len-quantile ; i++){
	      color[i] = 0; // color for middle circles
	  	} 
		for(i=0; i<= quantile; i++){
	  		color[i] = 1;   
	  		// color lower tail
	  		color[sC2Len-i-1] = 1;
	  		 // color upper tail
	  		lowerBd = resample[i];   // move lowerBd up
	  		upperBd = resample[sC2Len - i -1];  // move upperBd down
	  	}
	  } else{
	  	 console.log("No Data for CI");
	  }
	  return [color, lowerBd, upperBd];
}	
  
function estimateP2(){
	//function to estimate the true proportion based on a sample of 'success/failure' data
	// Gather Inputs:
      
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
      cat2Phat1 = cat2N11/ total1;
      cat2Phat2 = cat2N12/ total2;
      
      var sC2Len;

	 cat2hdr = document.getElementById("cat2OutputHead1");
	 cat2hdr.innerHTML = 
	 "<h4>Estimate True Proportion with a Confidence Interval</h4>"; 
	  
 	  cat2CLvl = document.getElementById("cat2ConfLvl");
	  cat2CLvl.style.display ="block";
	   cat2Tst = document.getElementById("cat2Test");
	  cat2Tst.style.display ="none";
	 // show plot
	   
	  resampleC21 = rbinom(total1, cat2Phat1, 100);
	  resampleC22 = rbinom(total2, cat2Phat2, 100);
	  sC2Len = resampleC21.length;
	  for(i=0; i < sC2Len; i++){
	      resampleC2[i] = resampleC21[i]/total1 - resampleC22[i]/total2; 
	  } 
	  resampleC2 = resampleC2.sort(function(a,b){return a - b;});
	  
	  CI =  colorP2(resampleC2);  // TODO: check colors when adding points
	  cat2Color = CI[0];
	  cat2lowerBd = CI[1].toPrecision(4);
	  cat2upperBd = CI[2].toPrecision(4);
	  
	 cat2ftr = document.getElementById("cat2OutputFoot1");
	 cat2ftr.innerHTML = 
	   "<div style='width=50px'></div>"+
	   "<div style = 'width:360px'> Difference in proportion \""+ cat2LabelOut1 +"\" in  "+ sC2Len + " Re-samples" +
	   "<br> <br>"+ Math.round(cat2CnfLvl*100) + 
	   "% Confidence Interval: (" + cat2lowerBd +", "+ cat2upperBd +" )</div>"; 
 	  cat2ftr.style.display = "block";
	  //console.log(cat2lowerBd, cat2upperBd);
	  
	 return([resampleC2, cat2Color]);		
	 // TODO  
	   // input to get more samples
} 	  
 
 

function testP2(tailChoice){
	//function to test 'Are two proportions equal?'
      var sC2Len;
	// Gather Inputs:
      
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
      cat2Phat1 = cat2N11/ total1;
      cat2Phat2 = cat2N12/ total2;
      cat2Diff = cat2Phat1 - cat2Phat2;
      
      cat2NullEst = (cat2N11 + cat2N12) / (total1 + total2);
      cat2CLvl = document.getElementById("cat2ConfLvl");
	  cat2CLvl.style.display ="none";
	  //cat2Pval = undefined;
	  
	  cat2Tst = document.getElementById("cat2Test");
	  cat2Tst.style.display ="";
	 
	 if(tailChoice === 'undefined'){ 
	 	cat2hdr = document.getElementById("cat2OutputHead1");
	 	cat2hdr.innerHTML = "<div class = 'w3-cell-row'> <div class = 'w3-cell' style = 'width:40%'> Stronger evidence is a difference </div>"+ 
  	 	   "<div class = 'w3-cell' style='width:40%'>"+
  	 	   "<select class = 'w3-select w3-card w3-border w3-mobile w3-pale-yellow' id='cat2Extreme'"+
  	 	   " onchange = 'cat2TestUpdate()' >"+ 
  				"<option value='lower'>Less Than or =</option>"+
  				"<option value='both' selected >As or More Extreme Than</option>"+
  				"<option value='upper'>Greater Than or =</option>"+
		   	"</select> </div>  <div class ='w3-cell' style = 'width:30%'> &nbsp;&nbsp;"+
		   	  cat2Diff.toPrecision(4) +
		   		"</div> </div> ";
		   cat2ftr.innerHTML = 
		   "<div  style = 'width:600px'> Difference in proportions \""+ cat2LabelOut1 +
		      "\" in samples from H<sub>0</sub>";
		 	sampleC21 = rbinom(total1, cat2NullEst, 100);
		 	sampleC22 = rbinom(total2, cat2NullEst, 100);
		 	sC2Len = sampleC2.length;
		  for(i=0; i < sC2Len; i++){
	    	  sampleC2[i] = sampleC21[i]/total1 - sampleC22[i]/total2;
	  	} 
	 } else{
	 	
	 }
	 // TODO: clicking a point changes a table to show that proportion
	return(sampleC2);
} 	  

function cat2TestUpdate(){
	var check, 
		extCount = 0,
		lowP,
		hiP,
		sC2Len; //moveOver, oldP;
 	c2Inference = 'test';
 	// get direction of evidence:
 	 cat2TestDirection = document.getElementById("cat2Extreme").value;
 	
 	if(!(sampleC2)){
 		sampleC2 = testP2();
 	}
 	sC2Len = sampleC2.length;
 	 if(cat2TestDirection ==="lower"){
 	 	for(i = 0; i < sC2Len; i++){
 	 		check = 0 + (sampleC2[i] <= cat2Diff);
 	 		extCount += check;
			cat2Color[i] =  check; 	 		
 	 	}
 	 } else if(cat2TestDirection ==="upper"){
 	 	for(i = 0; i < sC2Len; i++){
 	 		check = 0 + (sampleC2[i] >= cat2Diff) ;
 	 		extCount += check;
			cat2Color[i] =  check; 	 		
 	 	}
 	 	
 	 } else{
		lowP = ((cat2Diff < 0.00)? cat2Diff: -cat2Diff) + 1/1000000;
		hiP  = ((cat2Diff > 0.00)? cat2Diff: -cat2Diff) - 1/1000000;
 	 	for(i = 0; i < sC2Len; i++){
 	 		check = 0 + ((sampleC2[i] <= lowP)|(sampleC2[i] >= hiP));
 	 		extCount += check;
			cat2Color[i] =  check; 	 		
 	 	} 	 	
 	 }
 	 //console.log(d3.sum(cat2Color));
 	 cat2Pval = extCount / sC2Len;
 	 c2Tstdata = [sampleC2, cat2Color];
  	 c2InfOutput = discreteChart(c2Tstdata, cat2InfSVG, cat2TestInteract ); 	
  	
	 cat2ftr = document.getElementById("cat2OutputFoot1");
	 cat2ftr.innerHTML = 
	   "<div  style = 'width:600px'> Differences in proportions \""+ cat2LabelOut1 +
	   "\" in " + sC2Len +" Samples from H<sub>0</sub> <br>"+
	   "p-value (strength of evidence): " + formatPvalue(extCount, sC2Len) + "</div>"; //
 	  
}

//function c1InteractWith(infOut){
//	var sample = infOut[1],  // values
//	    dots = infOut[0][0];    // circles on the chart
	//dots.style("fill","steelblue");
//}

function cat2CIinteract(d,i){
	console.log(d.x);
	var C2modal = document.getElementById("cat2SelectedSample");
	C2modal.style.display = "block";
	C2modal.innerHTML = d.x;
	// open modal box to show success and failure counts in the selected resample;
	window.onclick = function(event) {
    if (event.target == C2modal) {
        C2modal.style.display = "none";
    	}
	}
} ;

function cat2TestInteract(d,i){
	//console.log(d.x);
	// open modal box to show success and failure counts in the selected sample;
} ;

function cat2MoreSimFn(){
	// function to add more points to an estimate or test of one proportion
	var sC2Len,
		more = +document.getElementById("cat2More").value,
		newValues1 =[],
		newValues2 = [];
	if(more > 0){
	  	//total = cat2N1 + cat2N2;
        //cat2Phat = cat2N1/ total;
	 	
	if( c2Inference === 'test'){ 
	    newValues1 = rbinom(total1, cat2NullEst, more);
	    newValues2 = rbinom(total2, cat2NullEst, more);
	    for(i=0; i < more; i++){
	      sampleC2.push(newValues1[i] /total1 - newValues2[i]/total2);
	    } 
	  sampleC2 = sampleC2.sort(function(a,b){return a - b});
	  cat2TestUpdate();
	  //c2InfOutput = discreteChart(sampleC2, cat2InfSVG, cat2TestInteract );
	  return(sampleC2);
	} else{
		resampleC21 = rbinom(total1, cat2Phat1, more);
	  	resampleC22 = rbinom(total2, cat2Phat2, more);
	    for(i=0; i < more; i++){
	      resampleC2.push(resampleC21[i]/total1 - resampleC22[i]/total2); 
	  	}
	  	resampleC2 = resampleC2.sort(function(a,b){return a - b}); 	
	  //console.log(cat2CnfLvl);
	  cat2OnChange(cat2CnfLvl);
	  
	  //cat2ftr = document.getElementById("cat2OutputFoot1");
	  //cat2ftr.innerHTML = 
	    //"<div style='width=50px'></div>"+
	    //"<div style = 'width:360px'> Proportion "+ cat2LabelOut1 +" in "+ sC2Len + " Re-samples" +
	    //"<br> <br>"+ Math.round(cat2CnfLvl*100) + 
	    //"% Confidence Interval: (" + cat2lowerBd +", "+ cat2upperBd +" )</div>"; 	
	   return(resampleC2);  
	}
  }
}

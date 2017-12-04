 // subroutine to estimate a proportion or test for a special value
 // Inputs: 
 //     2 category labels (default = Success/Failure) and a count for each 
 
    var c1SummDiv = d3.select("#cat1Inference"),
        c1Tstdata,
        c1CIdata,
        cat1Label1,
        cat1Label2,
		cat1hdr,
		cat1CnfLvl,
		cat1CLvl,
        cat1N1,
        cat1N2,
        cat1Pval,
        cat1Direction,
        c1Data = [],
        c1bars ,
        chartC1 ,
        cat1Color = [],
        confLevels = [
			{ key: "80%", value: "0.80" },
			{ key: "90%", value: "0.90" },
			{ key: "95%", value: "0.95" },
			{ key: "99%", value: "0.99" }
		], 
		c1Inference,
		c1InfOutput,
		targetQuantile,
      	upperBd,
      	upperCI,
        phat,
        resampleC1,
        sampleC1,
        total;

 var svgCat1 = d3.select("#cat1InfSVG");      
               
function summarizeP1() {
      // builds summary table and plot for 1 categorical variable
	var margin = 30, 
    	barHeight = 20,  
        colors = [],
        w = 300,  
        h = 60,  
        
        x = d3.scale.linear()
         .domain([0, 1])
         .range([14, w - margin*2]);

	  var xAxis = d3.svg.axis()
    	.scale(x)
    	.ticks(5)
    	.orient("bottom");
      
      cat1Label1 = document.getElementById("cat1Label1").value;
      cat1Label2 = document.getElementById("cat1Label2").value;
      cat1N1 = +document.getElementById("cat1N1").value;
      cat1N2 = +document.getElementById("cat1N2").value;
      phat = cat1N1/ (cat1N1 + cat1N2);
      cat1Summ = document.getElementById("cat1SummaryText");
      c1Data = [{"label": cat1Label1, "xx": phat},
      			{"label": cat1Label2, "xx": 1-phat}];
      cat1Summ.innerHTML = "p&#770; =  " + phat.toPrecision(5) +" <br> sd(p&#770) = " + 
                (Math.sqrt(phat*(1-phat))/(cat1N1+cat1N2)).toPrecision(5);
      cat1Summ.style = "display: block"; 
    

	function updateP1(chart, data) {
  		// DATA JOIN
  		var bars = chart.selectAll("rect")
    		.data(data);
	  	// UPDATE
   		bars.enter().append("g")
        	.attr("fill", "blue")
        	.attr("transform", function(d, i) { return "translate(14," + i * barHeight + ")"; });
	    bars.append("rect")
    	  .attr("width", function(d){return x(d.xx ) - 14;} )
    	  .attr("height", barHeight -1);

 	  // EXIT
  		// Remove old elements as needed.
 	 	bars.exit().remove();
	}
   	if(!chartC1){
	    chartC1 = d3.select(".chart")
    	  .attr("width", w + margin*2)
      		.attr("height", h + margin);
     } else{
     		var oldbars = chartC1.selectAll("g").data(c1Data);
     		oldbars.remove();
     		//var oldtxt = chartC1.selectAll("text").data(c1Data);
     		//oldtxt.remove();
     }
   // adding axis throws off the bar heights and doesn't allow nice updates.
   // TODO: use an unfilled rectangle instead??
    
    barC1 = chartC1.selectAll("g")
      .data(c1Data);
     barC1.enter().append("g")
        //.attr("fill", "blue")
        //.attr("transform", function(d, i) { return "translate(14," + i * barHeight + ")"; })
	    .each(function(d) {this._current = d;} );
	    //resets the figure to the current data
    
	  barC1.append("rect")
    	.attr("width", function(d){return x(d.xx );})// - 14;} )
    	.attr("height", barHeight -1)
        .attr("fill", "blue")
        .attr("transform", function(d, i) { return "translate(14," + i * barHeight + ")"; });
    
  	  barC1.append("text")
 		.attr("x",  function(d){return 16 + x(d.xx) ;})
 		.attr("y", function(d, i) { return (i+.5) * barHeight;})
 		.attr("dy", ".35em")
 		.text(function(d) {return d.label}) 
        .attr("fill", "blue");	
}
	
function cat1OnChange(arg) {
		cat1CnfLvl = +arg.value;
		// when level changes we recompute the colors and redraw the plot (not regenerate the data).
		// print the new CI below the plot.
		//console.log(cat1CnfLvl);
}
	
var rangeslide2 = rangeslide("#cat1ConfLvl", {
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
			"valueChanged": [cat1OnChange]
		}
	});

function colorP1(resample){
	var color = [],
		quantile,
	    sC1Len = resample.length;
	    
	  quantile = Math.round((1 - cat1CnfLvl)/2 * sC1Len);
	  
	 // CI =  colorP1(resampleC1);
	 // color = CI[0];
	 // lowerBd = CI[1];
	 // upperBd = CI[2];
	  
	for(i=quantile; i < sC1Len-quantile ; i++){
	      color[i] = 0; // color for middle circles
	  } 
	  for(i=1; i<= quantile; i++){
	  	color[i-1] = 1;
	  	color[sC1Len -i] = 1;  // color for tails
	  }
	  lowerBd = resampleC1[i-2];
	  upperBd = resampleC1[sC1Len - i];
	  return [color, lowerBd, upperBd];
}	
  
function estimateP1(){
	//function to estimate the true proportion based on a sample of 'success/failure' data
	// Gather Inputs:
      cat1Label1 = document.getElementById("cat1Label1").value;
      cat1Label2 = document.getElementById("cat1Label2").value;
      cat1N1 = +document.getElementById("cat1N1").value;
      cat1N2 = +document.getElementById("cat1N2").value;
      var sC1Len,
      	total = cat1N1 + cat1N2;
      phat = cat1N1/ total;

	 cat1hdr = document.getElementById("cat1OutputHead1");
	 cat1hdr.innerHTML = 
	 "<h4>Estimate True Proportion with a Confidence Interval</h4>"; //+
	 // "&nbsp; &nbsp; &nbsp; &nbsp; Proportion "+ cat1Label1 +" in Re-samples"; 
	  
 	  cat1CLvl = document.getElementById("cat1ConfLvl");
	  cat1CLvl.style.display ="";
	   cat1Tst = document.getElementById("cat1Test");
	  cat1Tst.style.display ="none";
	 // show plot
	   
	  resampleC1 = rbinom(total, phat, 1000).sort(function(a,b){return a - b});
	  sC1Len = resampleC1.length;
	  for(i=0; i < sC1Len; i++){
	      resampleC1[i] *= 1/total; 
	  } 
	  targetQuantile = Math.round((1 - cat1CnfLvl)/2 * sC1Len);
	  
	  CI =  colorP1(resampleC1, targetQuantile);
	  cat1Color = CI[0];
	  lowerBd = CI[1].toPrecision(4);
	  upperBd = CI[2].toPrecision(4);
	  
	 cat1ftr = document.getElementById("cat1OutputFoot1");
	 cat1ftr.innerHTML = 
	   "<div class='w3-display-middle' style = 'width:160'> Proportion "+ cat1Label1 +" in Re-samples" +
	   "<br> <br>"+ (cat1CnfLvl*100)+ 
	   "% Confidence Interval: (" + lowerBd +", "+ upperBd +" )</div>"; 
 	  
	  //console.log(lowerBd, upperBd);
	 return([resampleC1, cat1Color]);		
	 // TODO  
	   // this goes into discrete plot with output saved as c1InfOutput.
	   // Now need another function to allow interaction:
	 // click buttons for more re-samples
	 // plot:
	   // sort resampled phats and find appropriate quantiles:
	     // [alpha/2, 1 - alpha/2] where alpha = 1- confLevel
	   // change point colors based on in/outside the CI
	     // to change colors, we need to build sample as an array with a color attribute
	     // do we need (x,y) coordinates to use for 'click to see the underlying sample'? NO
	       //  I did get onclick to work for each circle w/out (x,y).
	 //print CI
	
} 	  
 
 

function testP1(){
	//function to test 'Is the true proportion  = some value?' for 'success/failure' data
	// Gather Inputs:
      cat1Label1 = document.getElementById("cat1Label1").value;
      cat1Label2 = document.getElementById("cat1Label2").value;
      cat1N1 = +document.getElementById("cat1N1").value;
      cat1N2 = +document.getElementById("cat1N2").value;
      cat1Pnull = +document.getElementById("cat1trueP").value;
      cat1CLvl = document.getElementById("cat1ConfLvl");
	  cat1CLvl.style.display ="none";
	  
      var sC1Len,
      	  total = cat1N1 + cat1N2;
      phat = cat1N1/ total;
	  cat1Tst = document.getElementById("cat1Test");
	  cat1Tst.style.display ="";
	  
	 cat1hdr = document.getElementById("cat1OutputHead1");
	 cat1hdr.innerHTML = "<div class = 'w3-cell-row'> <div class = 'w3-cell' style = 'width:40%'> Stronger evidence means data </div>"+ 
  	    "<div class = 'w3-cell' style='width:40%'><select class = 'w3-select w3-card w3-border w3-mobile w3-pale-yellow' id='cat1Extreme' >"+ 
  			"<option value='lower'>Less Than</option>"+
  			"<option value='both' selected >More Extreme Than</option>"+
  			"<option value='upper'>Greater Than</option>"+
	   	"</select> </div>  <div class ='w3-cell' style = 'width:30%'> &nbsp;&nbsp;" + phat.toPrecision(4) +
	   	"</div> </div> ";
	  
	 	sampleC1 = rbinom(total, cat1Pnull, 1000);
	 	sC1Len = sampleC1.length;
	  for(i=0; i < sC1Len; i++){
	      sampleC1[i] *= 1/total;
	      cat1Color[i] = 0;
	  } 
	 cat1ftr = document.getElementById("cat1OutputFoot1");
	 cat1ftr.innerHTML = 
	   "<div class='w3-display-middle' style = 'width:160'> Proportion "+ cat1Label1 +
	   " in Samples from H<sub>0</sub> <br>"+
	   "p-value (strength of evidence): " + 0.066 + "</div>"; //cat1Pval.toPrecision(5)
 	  
	 // click buttons for more samples -- same for both test and estimate
	 // choose bounds (less, greater, more extreme) and cutoff (phat)
	 // change point colors based on in/outside the bounds
	 // print p-value
	 // clicking a point changes a table to show that proportion
	return([sampleC1, cat1Color]);
} 	  

//function c1InteractWith(infOut){
//	var sample = infOut[1],  // values
//	    dots = infOut[0][0];    // circles on the chart
	//dots.style("fill","steelblue");
//}

var cat1CIinteract = function(d,i){
	console.log(d.x);
	var C1modal = document.getElementById("cat1SelectedSample");
	C1modal.style.display = "block";
	C1modal.innerHTML = d.x;
	// open modal box to show success and failure counts in the selected resample;
	window.onclick = function(event) {
    if (event.target == C1modal) {
        C1modal.style.display = "none";
    }
}
} ;
var cat1TestInteract = function(d,i){
	console.log(d.x);
	// open modal box to show success and failure counts in the selected sample;
} ;
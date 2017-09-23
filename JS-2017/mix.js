 // subroutine to draw balls randomly from a box in D3 javascript.
 // Inputs: 
 //     category labels and relative probabilities
 
    var margin = [{top: 50}, {right: 20}, {bottom: 50}, {left: 20}],
        w =  Number(400), // - margin.right - margin.left,
        h = Number(300), // - margin.top - margin.bottom    
        balls = [],
        colors = [], 
        hideMix = false,
        nCat,
        nMix,
        spacing =12,
        mixDiv = d3.select("#mixSVGgoesHere"),
       // mixSmryDiv = d3.select("#mixSmrySVGdiv"),
        boxData = [ { "x": w/2 -40,   "y": h/2-2 },  { "x": -w/2 +22,  "y": h/2-2},
                  { "x": -w/2+22,  "y": -h/+2}, { "x":w/2 -40 ,  "y": -h/2+2},
                  { "x": w/2 -40,  "y": h/2 - 40}],
        mixData = [],
        mixDraws=[],     // cumulative probabilities
 		mixDuration = 400,
        mixSlideDuration = 400,
        mixGroups =[],
        mixMatch,
        mixNs = [],
        mixRepResults = [],
        mixStopRule,
        mixRadius = 10,
        stepSize = mixRadius/10;
       
   var svgMix = mixDiv.append("svg")        
      .attr("width",  w)
      .attr("height", h)
      .append("g")
      .attr("transform", "translate(" +  (w/2 -20) + "," + (h/2 ) +")");
         
   var lineFunction = d3.svg.line()
         .x(function(d) { return d.x; })
         .y(function(d) { return d.y; })
         .interpolate("linear");
       // now draw the container
   var box = svgMix.append("path")
         .attr("d", lineFunction(boxData))
         .attr("stroke", "blue")
         .attr("stroke-width", 2)
         .attr("fill", "white");   

	
function initialMixState(){
	//setup the original batch of balls in the box
	var colorSeq = [],
		k = 0,
		grdSize = Math.floor(Math.min(w,h)/( mixRadius*2)),
		xyvalues = jStat.seq(0, grdSize -1, grdSize); // integer values for a lattice
		        
    mixGroups =  Papa.parse( document.getElementById("mixCats").value).data[0]; // labels of each
    mixNs =   jStat.map( Papa.parse( document.getElementById("mixNs").value).data, Number); 
    mixNCat = mixGroups.length;

    if(mixNCat < 2){
    	alert("Must have more than one label and more than one ball count");
    }

    // force group length to = prob length
    if( mixNCat > mixNs.length){
    	mixGroups.length = mixNCat = mixNs.length;
    } else if(mixNs.length < mixNCat){
    	mixNs.length = mixNCat;
    }
    mixNballs = d3.sum(mixNs);
    colorSeq = jStat.seq(30, 300, mixNCat)

    for ( i=0; i < mixNCat; i++)  { 
            mixData[i]  = { "label": mixGroups[i] , 
 			                      "value": mixNs[i]
						};
    	colors[i] = d3.hcl(colorSeq[i] , 50, 80, 0.8);
	}

    var x = sampleWrep(xyvalues, mixNballs, repeat(1, xyvalues.length)),
	   	y = sampleWrep(xyvalues, mixNballs, repeat(1, xyvalues.length));
	//console.log(x, y);   	
       
    k=0;
    for(i = 0; i < mixNCat; i++){
    	for(j=0; j < mixNs[i]; j++){
    		//check that this spot is not already taken
    		if(balls.length > 0){
    			while(d3.min(Math.abs(balls.x - x[k]) + Math.abs(balls.y -x[k]) < 1)){
    				if(Math.random() > 0.5){
    					x[k] = (x[k] >= grdSize)? 0 : x[k] + 1;
    				} else{
    					y[k] = (y[k] >= grdSize)? 0 : y[k] + 1;
    				}
    			}
    		}
    		balls.push({x: x[k], 
    					y: y[k++], 
    					group : i,
    					r: mixRadius - .75} );
    	}
     }
                
     mixCircles = svgMix.selectAll("circle")
        .data(balls)
        .enter().append("circle")
         .attr("fill", function(d, i){ return colors[d.group]; } )
         .attr("cx", function(d){ return d.x * mixRadius - w/5;} ) // 
         .attr("cy", function(d){ return d.y * mixRadius - h/4;} )  // 
         .attr("r",  function(d){ return d.r;} )
         .attr("text",function(d){return mixGroups[d.group];})  // 
         ;//.attr("class", "circle") ; 
         	
       
function ignore(){
     var textLabels = svgMix.selectAll("text")
         .data(sample)
       .enter().append("text")
           .attr("x", function(d,i){ return -w/2 + xspace(i)  ;} ) // 
         .attr("y", h/2 +34 )
         .text( function(d){return mixGroups[d.group];})  // 
         .style("text-anchor", "middle")
         .attr("font-family", "sans-serif")
         .attr("opacity",0)
         .attr("font-size", "20px");
  }
}  //end of initialMixState

    // Transitions and timers
  
function turn(i) {// rotate the whole batch
	mixCircles.transition()
		.delay(mixDuration * (i + (i > 0)* 1.4) *2 )
		.duration(mixDuration )
		.ease("cubic-out")
		.attrTween("transform", function() {
			return d3.interpolateString("rotate( 0, 0, 0)", "rotate(-720, 0, 0)");
		});
}
    

function mixTest(draws) {
	// for testing
	var i;
	for (i = 0; i < draws.length; i++) {
		turn(i);
	}
	draws.each(function(d, i) {
		turn(i);  // need to NOT turn the ones we've already used
		d3.select(this)
			.transition()		// move the selected ball to opening
			.delay(mixDuration * 2 * (i + 1.1))
			.attr("cx", w/2 -40  )
			.attr("cy", h / 2 - 21)
			.style("stroke", "black")
			.transition()       // moveover to line up at right
			.delay(mixDuration * 2 * (i + 1.4))
			.duration(mixDuration)
		//  .ease("cubic-in")
			.attr("cx", w / 2 + 3 * mixRadius  )
			.transition()       // move up to its row
		    .delay( mixDuration * 2 * (i + 1.6) )
			.duration(mixDuration)
//			.ease("cubic-out")
			.attr("cy",  i * 2*spacing - h/2 + mixRadius);
		if(mixReplace === 0)  {
		      balls.splice(balls.length -1, 1);
		mixCircles.exit().remove();
		}
	});
}

function mixNtimes(){
	// generate a fixed number of draws
    var nBalls,
    	mixSeq = [];
    
	initialMixState();
    mixReplace =  document.getElementById("mix_Replace").value;
    
    //if(mixStopRule !== "Fixed"){
    	// zap any results hanging around
    //	mixData = [];
    //}
    mixStopRule = "Fixed";
   	mixData = [];
	
	//check state of replacement. If "no" just use sampleWOrep, otherwise use sampleWrep
	if(mixReplace === 0){
		mixData = sampleWOrep(balls, nMix);
	} else{
		mixData = sampleWrep(balls, nMix,  repeat(1, balls.length))
	}
		
	//for(i=0; i < nDraws; i++){
	//	mixData[i] = balls[mixData[i]];	
	//}
	//console.log(mixData);
	
	//mixData.length = nDraws;
	//console.log(mixData);
	//showmixSequence(mixData);
} 
   //  end of mixMore

function mixTill1(){
	// generate a random sample of draws ending with one of the right color
	var mixStopper =  document.getElementById("mixTil").value,
    	i =0,
    	mixLength,
    	otherBalls =[],
    	theseBalls = [];        
    mixData = [];
    
//     if(mixStopRule !== "OneOfOneType"){
    	// zap any results hanging around
  //  	mixRepResults = [];
    //}
	mixStopRule = "OneOfOneType";
	
	initialMixState();
	mixMatch = mixGroups.indexOf(mixStopper);
    if (mixMatch < 0){
    		alert("You must choose one of the labels.")
    }
    //check state of replacement. If "no" just use sampleWOrep, otherwise use sampleWrep
	if(mixReplace === 0){
		mixData = sampleWOrep(balls, nBalls);
		for (i=0; i < nBalls; i++){
			if(mixMatch === mixData[i].group){
				mixData.length = i+1;
				break;
			}
		}
	} else{
		mixLength = rgeom(1 - mixNs[mixMatch]/mixNballs);
		otherBalls = balls.filter(function(d) {return d.group !== mixMatch;} );
		theseBalls = balls.filter(function(d) {return d.group === mixMatch;} );
		mixData = sampleWrep(otherBalls, mixLength-1,  repeat(1, otherBalls.length));
		mixData.push(sampleWrep(theseBalls, 1, repeat(1, theseBalls.length)));
	}
	
	console.log(mixData);
	//showmixSequence(mixData);
	//}
}

function mixTillAll(){        
	var newBall,
		error=" ",
		i = 0,
		ndx=0,
		mixColor,
		table = [];  
    mixData = [];
    //if(mixStopRule !== "OneOfEach"){
    	// zap any results hanging around
    //	mixRepResults = [];
    //}
	mixStopRule = "OneOfEach";
	
	initialMixState();
    
    for(i=0; i<mixNCat; i++){
    	table[i] = 0;
    }
    table.length = mixNCat;
    
    //check state of replacement. If "no" just use sampleWOrep once.
	if(mixReplace === 0){
		mixData = sampleWOrep(balls, nBalls);
    	while (d3.min(table) < 1){
		   mixColor =  	mixData[ndx].group;
  		   table[mixColor] += 1;
  		   ndx++;
  		  } 
  		mixData.length == ndx;  
 	} else{    // sampling with replacement
   	    while(d3.min(table) < 1){
   	    	newBall = sampleWOrep(balls, 1, repeat(1, balls.length)); 
  			mixData.push(newBall);
  			table[newBall.group] += 1;
  			ndx++;
  			if(ndx > 10000){ error="10K";
  				break;
  				}
  		}
   }
    //console.log(table);
    if(error !== "10K"){
    	showmixSequence(mixData);
    }
}

function showmixSequence(mixData){
	var nDraws = mixData.length;
	var spacing = (h -20) / (nDraws + 1); //for sampled circles
	 // console.log([nDraws, spacing]);
    
    var mixDraws = svgMix.selectAll("g.circle")
         .data(mixData)
       .enter().append("circle")
         .attr("fill", function(d, i){ return colors[d.group]; } )
         .attr("cx", function(d){ return d.x * mixRadius - w/5;} ) // 
         .attr("cy", function(d){ return d.y * mixRadius - h/4;} )  // 
         .attr("r",  2 )
         .attr("text",function(d){return mixGroups[d.group];})  // 
         .attr("class", "circle") ; 
      // these are copies of the first set of circles above

  
  	for(i=0;i<nDraws;i++){
  	    tween(i);  
	}
	// Create the sampled circles (output)
      //  but hide them with r = 0 
    circles = svgMix.selectAll("g.circle")
         .data(mixData);
       circles.enter().append("circle")
          .attr("fill", function(d){ return colors[d.group]; } )
          .attr("cx", function(d){return  93 * Math.cos((90 - +d.angle*360)*Math.PI/180 );})  
          .attr("cy", function(d){return -93 * Math.sin((90 - +d.angle*360)*Math.PI/180);})
          .attr("r", 0)     // -> 20  
          ;
	  
    textLabels = svgMix.selectAll("g.text")
         .data(mixData)
       .enter().append("text")
         .attr("x", function(d,i){ return xspace(i)  ;} )  
         .attr("y", 140)
         .text( function(d){return  pieData[d.group].label ;}) 
         .style("text-anchor", "middle")
         .attr("font-family", "sans-serif")
         .attr("opacity",0)
         .attr("font-size", "20px");

	  circles.each(function(d,i){
      d3.select(this).transition()
          // toss out circle
          .delay(mixDuration + (mixSlideDuration + mixDuration) * i )
          .duration( mixSlideDuration )
          .ease("linear")
          .attr("cx", xspace(i))
          .attr("cy", + r + 20)
          .attr("r", 20);
	});

    textLabels.each(function(d,i){
	// move the selected ball out
        d3.select(this)
          .transition()
           .delay( ( mixSlideDuration + mixDuration) * (i+1) )
          .attr("opacity", 1)
       ;  
   });
   document.getElementById("repeatMix").style.display = "block"; 
}

function hideShowMix() {
    hideMix = !hideMix;
    var xDiv = document.getElementById("mixSVGgoesHere");
    
    xDiv.style.display = hideMix ? "none" : "block";  
    
}

function mixRepeat(times){
	var i,
		thisProb;
	
    if(mixStopRule === "Fixed"){
    	thisProb = mixProb[0];
    	mixRepResults = mixRepResults.concat(rbinom(nMix, thisProb, times));
    	// track  number of first type?                    
    } else if(mixStopRule === "OneOfOneType"){
    	// track number of mixs needed
    	thisProb = mixProb[mixMatch];
    	mixRepResults[0] = mixData.length;
    	for(i=0; i<times; i++ ){
    		mixRepResults.push(rgeom(thisProb));
    	}
    } else if(mixStopRule === "OneOfEach"){
    	// track number of mixs needed
    	mixRepResults[0] = mixData.length;
    	mixRepResults = mixRepResults.concat(draws2get1ofEach(times));
    } else {
    	console.log("Bad option for mixStopRule");
    }	
    //plot mixResults as a histogram
}

function draws2get1ofEach(reps) {
	// randomly mix til we get one of each category
	// returns the number of mixs needed
	var i = 0,
	    j = 0,
	    table = [],
	    temp = [],
	    draw1 = [],
	    nDraws = jStat.ones(1,reps),
	    nCat = mixProb.length,
	    probs = mixProb.slice(0);
	    
	if (nCat < 2) {
		return nDraws; // with only 1 category, we get all (only one) categories right away
	} // at least 2 categories
	draw1 = sample(jStat.seq(0,nCat-1,nCat), reps, probs);
	for( i=0; i < reps; i++){
		probs = mixProb.slice(0);  // need to reset for each rep
		//console.log(probs);
		probs.splice(draw1[i],1);
		//console.log(probs);
		if(d3.sum(probs) > 0){
			nDraws[i] = 1 + recursiveDraws(probs );
		}
		//console.log(nDraws[i]);
	}
	return nDraws;
}

function recursiveDraws(probs){
	 var sumProb = d3.sum(probs), // this needs to be less than 1 for rgeom to work
	     draw,
	     group,
	     nCat = probs.length;
	 //console.log(probs);
	 if(sumProb >= 1.00){
	 	console.log("Error in recursiveDraw: probs sum to one");
	 	return(NaN);
	 }
	 draw = rgeom(sumProb);
	 if(nCat === 1){ 
	 	return draw;
	 } else{
	 	group = sample(jStat.seq(0,nCat-1,nCat), 1, probs ) ;
	 	probs.splice(group, 1); // remove the observed probability
	 	return draw + recursiveDraws(probs);
	 }    
}


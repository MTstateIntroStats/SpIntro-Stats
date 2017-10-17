 // subroutine to draw balls randomly from a box in D3 javascript.
 // Inputs: 
 //     category labels and relative probabilities
 
    var margin = [{top: 50}, {right: 20}, {bottom: 50}, {left: 20}],
        w =  Number(400), // - margin.right - margin.left,
        h = Number(300), // - margin.top - margin.bottom    
        balls = [],
        boxData = [ { "x": w/2 -40,   "y": h/2-2 },  { "x": -w/2 +22,  "y": h/2-2},
                  { "x": -w/2+22,  "y": -h/+2}, { "x":w/2 -40 ,  "y": -h/2+2},
                  { "x": w/2 -40,  "y": h/2 - 40}],
        colors = [], 
        hideMix = false,
        mixDiv = d3.select("#mixSVGgoesHere"),
        mixCircles =[],          
        mixData = [],
        mixDraws=[],     // cumulative probabilities
 		mixDuration = 400,
        mixSlideDuration = 400,
        mixGroups =[],
        mixMatch,
        mixNs = [],
        mixStopRule,
        mixRadius = 10,
        mixText = [],
        nCat,
        nMix,
        spacing =12;
       
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
	//setup the original batch of balls in the box -- mixCircles with data: balls
	var //balls = [],
		colorSeq = [],
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

    var x = sampleWrep(xyvalues, mixNballs, repeat(1, xyvalues.length))[0],
	   	y = sampleWrep(xyvalues, mixNballs, repeat(1, xyvalues.length))[0];
	//console.log(x, y);   	
	
	// get rid of old hanging stuff
	
     if(mixCircles.length > 0){
     	mixCircles.exit().remove();
        mixText.exit().remove();
        mixDraws.exit().remove();
     	mixCircles = [];
		mixDraws=[];
		mixText=[];
     }
       
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
     mixCircles = svgMix.selectAll("f.circle")
        .data(balls);
     mixCircles.enter().append("circle")
         .attr("fill", function(d, i){ return colors[d.group]; } )
         .attr("cx", function(d){ return d.x * mixRadius - w/5;} ) // 
         .attr("cy", function(d){ return d.y * mixRadius - h/4;} )  // 
         .attr("r",  function(d){ return d.r;} )
         //.attr("text",function(d){return mixGroups[d.group];})  // 
         ;//.attr("class", "circle") 
      mixCircles.exit().remove(); 
         	
       
}  //end of initialMixState

    // Transitions and timers
  
function turn(i) {// rotate the whole batch of mixCircles
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
	var i ;
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
		if(mixReplace === "no")  {
		      balls.splice(balls.length -1, 1);
			mixCircles.exit().remove();
		}
	});
}

function mixNtimes(){
	// generate a fixed number of draws
    var 
    	mixSeq = [];
    
	initialMixState();
    mixReplace =  document.getElementById("mix_Replace").value;
	nMix =   +document.getElementById("nDraws").value;
    //if(mixStopRule !== "Fixed"){
    	// zap any results hanging around
    //	mixData = [];
    //}
    mixStopRule = "Fixed";
   	mixData = [];
	
	//check state of replacement. If "no" use sampleWOrep, otherwise use sampleWrep
	if(mixReplace === "no"){
		mixData = sampleWOrep(balls, nMix);
	} else if(mixReplace === "yes"){
		mixData = sampleWrep(balls, nMix,  repeat(1, balls.length));
	} else{
		alert("error in mixReplace");
	}
	//console.log(mixData[0]);
	
	showmixSequence(mixData);
} 

function mixTill1(){
	// generate a random sample of draws ending with one of the right color
	var mixStopper =  document.getElementById("mixTil").value,
    	i =0,
    	mixLength,
    	otherBalls =[],
    	theseBalls = [],
    	tempDraw;        
    mixData = [[],[]];
    
//     if(mixStopRule !== "OneOfOneType"){
    	// zap any results hanging around
  //  	mixRepResults = [];
    //}
	mixStopRule = "OneOfOneType";
	
    
	initialMixState();
    mixReplace =  document.getElementById("mix_Replace").value;
	mixMatch = mixGroups.indexOf(mixStopper);
    if (mixMatch < 0){
    		alert("You must choose one of the labels.")
    }
    //check state of replacement. If "no" just use sampleWOrep, otherwise use sampleWrep
	if(mixReplace === "no"){
		mixData = sampleWOrep(balls, mixNballs);
		for (i=0; i < mixNballs; i++){
			if(mixMatch === mixData[0][i].group){
				mixData[0].length = i+1;
				mixData[1].length = i+1;
				nDraws = i+1;
				break;
			}
		}
	} else{
		nDraws = rgeom(mixNs[mixMatch]/mixNballs);
		otherBalls = balls.filter(function(d) {return d.group !== mixMatch;} ); // other colors
		theseBalls = balls.filter(function(d) {return d.group === mixMatch;} ); // target color
		if(nDraws > 1){
			mixData = sampleWrep(otherBalls, nDraws-1,  repeat(1, otherBalls.length));
		}
		// last ball is of right color to give the right random geometric.
		tempDraw = sample1(theseBalls.length);
		mixData[0][nDraws -1] = theseBalls[tempDraw];
		// trouble finding the index number of the ball of drawn, but then why do I need it?
		mixData[1][nDraws -1] = mixNballs; // indexOfXY(balls, theseBalls[tempDraw].x, theseBalls[tempDraw].y); 
	}
	//console.log(nDraws);
	
	//console.log(mixData[0], mixData[1]);
	showmixSequence(mixData);
	//}
}

function mixTillAll(){        
	var newBall,
		error=" ",
		i = 0,
		ndx=0,
		mixColor,
		table = [];  
    mixData = [[],[]];
    //if(mixStopRule !== "OneOfEach"){
    	// zap any results hanging around
    //	mixRepResults = [];
    //}
	mixStopRule = "OneOfEach";
	
	initialMixState();
        
    mixReplace =  document.getElementById("mix_Replace").value;

    for(i=0; i < mixNCat; i++){
    	table[i] = 0;
    }
    table.length = mixNCat;
    
    //check state of replacement. If "no" just use sampleWOrep once.
	if(mixReplace === "no"){
		mixData = sampleWOrep(balls, mixNballs);
    	while (d3.min(table) < 1){
		   mixColor =  	mixData[0][ndx].group;
  		   table[mixColor] += 1;
  		   ndx++;
  		  } 
  		mixData[0].length = ndx; 
  		mixData[1].length = ndx;  
 	} else{    // sampling with replacement
   	    mixData = sampleWrep(balls, nCat, repeat(1,balls.length));
   	    for(ndx=0; ndx< nCat; ndx++){
  			mixColor = mixData[0][ndx].group;
   	    	table[mixColor] += 1;
   	    }
   	    newBall = sampleWrep(balls, 10, repeat(1,balls.length));
   	    while(d3.min(table) < 1){   
   	    	for(i=0; i<10; i++){
  				mixData[0][ndx] = newBall[0][i]; 
  				mixData[1][ndx] = newBall[1][i];
  				ndx++;
		   		mixColor = newBall[0][i].group;
   	    		table[mixColor] += 1;
   	    		if(d3.min(table) >= 1){
   	    			break;
   	    		}	
   	    	}
  			//console.log(newBall[0][0].group);
  			if(ndx > 10000){ error="10K";
  				break;
  				}
  			newBall = sampleWrep(balls, 10, repeat(1, balls.length));
  		}
   }
   //console.log(table);
   //console.log(mixData[0]);
   //console.log(mixData[1]);
   if(error !== "10K"){
    	showmixSequence(mixData);
    }
}

function showmixSequence(mixData){
	var nDraws = mixData[0].length, //values sampled
		mixSeq = mixData[1];        // indices of those sampled
	var spacing = (h -20) / (nDraws + 1); //for sampled circles going outside the box
	 //console.log(mixSeq);

	// create new circles for the selected sample.
	// hide them by setting radius to zero
	mixDraws = svgMix.selectAll("g.circle")
         .data(mixData[0]);
    mixDraws.enter().append("circle")
         .attr("fill", function(d, i){ return colors[d.group]; } )
         .attr("cx", function(d){ return d.x * mixRadius - w/5;} ) // 
         .attr("cy", function(d){ return d.y * mixRadius - h/4;} )  // 
         .attr("r",  0 )
         .attr("text",function(d,i){return mixGroups[mixSeq[i]];})  // 
         ;//.attr("class", "circle") ; 
      
	          
  	function isDrawn(d, i) {
      return  inArray(mixSeq, i); 
  	}

	  mixDraws.each(function(d, i) {
		turn(i);  
		d3.select(this)
			.transition()		// move the selected ball to opening
			.delay(mixDuration * 2 * (i + 1.1))
			.attr("cx", w/2 -40  )
			.attr("cy", h / 2 - 21) 
         	.attr("r",  mixRadius )
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
			.attr("cy",  i *spacing - h/2 + mixRadius)
			.attr("opacity", 1)
			.style("stroke", "black");
		})
		if(mixReplace === "no")  { 
			// note: if replace =="no", the number of circles decreases with each draw.
			mixCircles.filter(isDrawn).each(function(d,i){
				d3.select(this).transition()
        		.delay(mixDuration * 2 * (mixSeq[i] + 1.4))
        		.attr("opacity", 0)
        		.remove;
    		} );
    	for(i=0;i<nDraws;i++){	
  		    balls.splice(mixSeq[i], 1); //remove from the list
		}
	};

     mixText = svgMix.selectAll("text")
         .data(mixData[0]);
     mixText.enter().append("text")
         .attr("x", w/2 - 2*mixRadius ) // 
         .attr("y", function(d, i){ return i * spacing - h/2 + 1.2 * mixRadius;} )
         .text( function(d){return mixGroups[d.group];})  // 
         .style("text-anchor", "middle")
         .attr("font-family", "sans-serif")
         .attr("opacity",0)
         .attr("font-size", "20px");
         
   mixText.each(function(d,i){
	// show the label
        d3.select(this)
          .transition()
           .delay( 200+ ( mixSlideDuration + mixDuration) * (i + 1.6) )
          .attr("opacity", 1)
       ;  
   });
   //document.getElementById("repeatMix").style.display = "block";
   // need to get rid of old draws when miReplace is "no" 
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


 // subroutine to draw balls randomly from a box in D3 javascript.
 // Inputs: 
 //     category labels and relative probabilities
 
    var margin = [{top: 50}, {right: 20}, {bottom: 50}, {left: 20}],
        w =  Number(400), // - margin.right - margin.left,
        h = Number(300), // - margin.top - margin.bottom    
        balls = [],
        colors = [], 
        //color = d3.scale.ordinal()
        //    .range(["#a05d56","#ff8c00","#d0743c","#98abc5", "#8a89a6", 
        //            "#7b6888", "#6b486b" ]),      
        //dColor = mixData.drawColor,
        hideMix = false,
        nCat,
        nMix,
        spacing =10,
        mixDiv = d3.select("#mixSVGgoesHere"),
       // mixSmryDiv = d3.select("#mixSmrySVGdiv"),
        boxData = [ { "x": w/2 -40,   "y": h/2 +5},  { "x": -w/2,  "y": h/2+5},
                  { "x": -w/2,  "y": -h/2-5}, { "x":w/2 +10 ,  "y": -h/2-5},
                  { "x": w/2 +10,  "y": h/2 - 20}],
        mixData = [],
        mixCumProb=[],     // cumulative probabilities
 		mixDuration = 400,
        mixSlideDuration = 400,
        mixGroups =[],
        mixMatch,
        mixNs = [],
        mixRepResults = [],
        mixStopRule,
        radius = 20,
        stepSize = radius/10;
       
   var svgMix = mixDiv.append("svg")             // 440w x 440h
      .attr("width",  (440))
      .attr("height", (300))
      .append("g")
      .attr("transform", "translate(" +  (w/2 + 20) + "," + (h/2 + 5 ) +")");
         
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
	var k = 0,
		xyvalues = jStat.seq(0, h, radius*2),
	    x = sample(xyvalues,mixNballs, jStat.ones(xyvalues.length)),
	   	y = sample(xyvalues,mixNballs, jStat.ones(xyvalues.length));
	     
    //w = width;        
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
    
    for ( i=0; i < mixNCat; i++)  { 
            mixData[i]  = { "label": mixGroups[i] , 
 			                      "value": mixNs[i]
						};
    	colors[i] = d3.hcl(30 + 300 * i/mixNCat , 25, 80, 0.8);
	}

    
    radius <-  Math.min(16, round( h/2 / sqrt(2 * mixNballs)))
    k=0;
    for(i = 0; i < mixNCat; i++){
    	for(j=0; j < mixNs[i]; j++){
    		//check that this spot is not already taken
    		while(d3.min(Math.abs(balls.x - x[k]) + Math.abs(balls.y -x[k]) < 0.0001)){
    			if(Math.random() > 0.5){
    				x[k] = x[k] >= h? 0 : x[k] + 1;
    			} else{
    				y[k] = y[k] >= h? 0 : y[k] + 1;
    			}
    		}
    		balls.push({x: x[k], 
    					y: y[k], 
    					group : i,
    					r: radius - .75} );
    	}
     }
           
     var mixCircles = svgMix.selectAll("g.circle")
         .data(balls)
       .enter().append("circle")
         .attr("fill", function(d, i){ return colors[d.group]; } )
         .attr("cx", function(d){ return d.x * radius;} ) // 
         .attr("cy", function(d){ return d.y * radius;} )  // 
         .attr("r",  function(d){ return d.r;} )
         .attr("text",function(d){return mixGroups[d.group];})  // 
         .attr("class", "circle") ; 
	
    
  
        mixColorFn = d3.scale.ordinal().range(colors);      
 
     var draws = svgMix.selectAll("g.circle")
         .data(sample)
       .enter().append("circle")
         .attr("fill", function(d, i){ return color[d.group]; } )
         .attr("cx", function(d){ return d.x;} ) // 
         .attr("cy", function(d){ return d.y;} )  // 
         .attr("r",  0 )
         .attr("text",function(d){return mixGroups[d.group];})  // 
         .attr("class", "circle") ; 
      // these are copies of the first of the circles above
       
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

}  //end of initialMixState

  
    // Transitions and timers
       // First Spin around nDraw times.

   var turn = function(i){  // rotate the whole batch
	mixCircles.transition()
          .delay(mixDuration *  (i+1))
	  .duration(mixDuration)
	  .ease("cubic-out")   
	  .attrTween("transform", function (){
            return d3.interpolateString("rotate( 0, 0, 0)", 
                                   "rotate(720, 0, 0)");
          })
          ;
	 
    }


    
function mixTest(nDraws){
	// for testing

    for( var i = 0; i < nDraws; i++){ 
        turn(i);
    }
     draws.each(function(d,i){
       turn(i); 	
	   // move the selected ball out
        d3.select(this)
          .transition()
            .delay( mixDuration * (i+ 1.1) )
          .attr("cx", w/2 - radius - 2)
          .attr("r", radius - .75)
          .style("stroke", "black")
          .transition()
            .duration( slideDuration )
           // .ease("cubic-in")
            .attr("cx", w/2 - radius - 2)
            .attr("cy", h/2  + 30)
            .transition()
             //.delay( duration0  )
             .duration( slideDuration )
             .ease("cubic-out")
             .attr("cy", h/2  + 30)
             .attr("cx", -w/2 + xspace(i) ); 
          //if(data.replace == "no")  {
        //      balls.length = balls.length -1;
              //circles.exit().remove();
	  //}
    });
}

function xspace(i){
           return i * spacing; 
    }

function mixNtimes(){
    nMix = +document.getElementById("nDraws").value;
     
    mixGroups =  Papa.parse( document.getElementById("mixCats").value).data[0]; // labels of each
    mixProb =   jStat.map( Papa.parse( document.getElementById("mixNs").value).data, Number); 
    mixNCat = mixGroups.length;
    
    if(mixStopRule !== "Fixed"){
    	// zap any results hanging around
    	mixRepResults = [];
    }
   mixStopRule = "Fixed";
   
   mixMore(nMix);	
}

function mixMore(nDraws){
	// grab a random set of n draws and plot each as a spinner outcome in showSequence()
	var angle,
	mixColor;
	mixData = [];
	
	//drawDonut();
	
  	for(i=0;i<nDraws;i++){
  		angle = Math.random();
  		mixColor =  cut( angle, mixCumProb);
  		mixData[i] = {angle: angle, group: mixColor};
	}
	//console.log(mixData);
	mixData.length = nDraws;
	showmixSequence(mixData);
} 
   //  end of mixMore

function mixTill1(){
	var mixStopper =  document.getElementById("mixTil").value,
    	angle,
    	i =0,
    	mixColor = -1,
    	w = width;        
    mixData.length = 0;
    
     if(mixStopRule !== "OneOfOneType"){
    	// zap any results hanging around
    	mixRepResults = [];
    }
	mixStopRule = "OneOfOneType";
	
    mixGroups =  Papa.parse( document.getElementById("mixCats").value).data[0]; // labels of each
    mixProb =   jStat.map( Papa.parse( document.getElementById("mixNs").value).data, Number); 
    mixNCat = mixGroups.length;

    //clear out old arrow, arcs, circles, and text
    if(typeof(arrow) !== "undefined"){
    	arrow.remove();
    }
    
	drawDonut();
	
	//console.log(mixStopper);
	//console.log(mixStopper.length);
    //if(mixStopper.length > 1){
    	mixMatch = mixGroups.indexOf(mixStopper);
    	if (mixMatch < 0){
    		alert("You must choose one of the labels.")
    	}
    	while (mixColor !== mixMatch){
  			angle = Math.random();
  			mixColor =  cut( angle, mixCumProb);
  			mixData[i++] = {angle: angle, group: mixColor};
    	}

	showmixSequence(mixData);
	//}
}

function mixTillAll(){        
	var angle,
		error=" ",
		i = 0,
		ndx=0,
		mixColor,
		table = [];  
    mixData = [];
    if(mixStopRule !== "OneOfEach"){
    	// zap any results hanging around
    	mixRepResults = [];
    }
	mixStopRule = "OneOfEach";
	
    mixGroups =  Papa.parse( document.getElementById("mixCats").value).data[0]; // labels of each
    mixProb =   jStat.map( Papa.parse( document.getElementById("mixNs").value).data, Number); 
    mixNCat = mixGroups.length;
    

    //clear out old arrow, arcs, circles, and text
    if(typeof(arrow) !== "undefined"){
    	arrow.remove();
    }
	drawDonut();
	
    for(i=0;i<mixNCat;i++){
    	table[i] = 0;
    }
    
    table.length = mixNCat;
    
    while (d3.min(table) < 1){
  		angle = Math.random();
  		mixColor =  cut( angle, mixCumProb);
  		mixData[ndx] = {angle: angle, group: mixColor};
  		table[mixColor] += 1;
  		ndx++;
  		if(ndx > 10000){ error="10K";
  			break;}
    }
    //console.log(table);
    if(error !== "10K"){
    	showmixSequence(mixData);
    }
}

function showmixSequence(mixData){
	var nDraws = mixData.length;
	var spacing = (width -20) / (nDraws + 1); //for sampled circles
	 // console.log([nDraws, spacing]);
    function xspace(i){
           return i * spacing - r + 10; 
    }

    //clear out old arrow, arcs, circles, and text
    if(typeof(arrow) !== "undefined"){
    	arrow.remove();
    }
    if(typeof(circles) !=="undefined"){
    	circles.remove();
    }
    if(typeof(textLabels) !=="undefined"){
    	textLabels.remove();
    }
    if(typeof(arcs) !== "undefined"){
    	arcs.remove();
    }
    
	// draw new spinner
    drawDonut();
     arcs.append("svgMix:text")     //add a label to each slice
        .attr("transform", function(d) { 
         d.innerRadius = 0;
         d.outerRadius = tr ;
            return "translate(" + arc.centroid(d) + ")";   
              })
         .attr("text-anchor", "middle")               
         .text(function(d, i) { return pieData[i].label; });  
         
  	for(i=0;i<nDraws;i++){
  	    tween(i);  
	}
	//mixData.length = nDraws;
	//console.log(mixData);				
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
        
	//console.log(mixAngle);
	//console.log(drawColor);
	  
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

//TODO:
//  Need a generic plotting function for dotcharts.
//  Here's the one adapted from boot.js, not tested

var dotChart = function(sample){
	var margin = 40,
		myArray =[],
	    nN = sample.length,
	    plotX,
	    ypos =0,
	    wdth = 440 - margin * 2,
	    hght = 320 - margin * 2,
	    xlegend = mixStopRule === "Fixed"? "mixs to get one of the first type":
	       			mixStopRule === "OneOfOneType"? "mixs to get a " + mixGroups[mixMatch]:
	       			"mixs to get one of each type";
	    
	sample.sort(function(a,b){return a - b}) ;   
          // numeric sort to build bins for y values
          // start on left with smallest x.

	
	var radius = (nN < 101)? 6:
	    (nN < 501)? 5:
	    (nN < 1001)? 4:
	    (nN < 5001)? 3: 2; // perhaps this should relate to width/height of svg]
    var gees = d3.select("#mixSmrySVG").selectAll("g");
	if(typeof(gees) === "object"){
		gees.remove();
	}    
	//  first dot goes at y=0, then add one to each from there
	var j = 0;
	while( j <  nN ){    
	    plotX = sample[j];	    // start a fresh bin with left edge at sample[j]
	    ypos = 0;	            // bin y starts at 0
	    myArray[j] = {"x": sample[j++], "y": ypos++};
        while( (sample[j] - plotX < radius/6) & (j < nN)){
		  //stay in same bin -- increment yposition
		  myArray[j] = {"x": sample[j++], "y": ypos++};
	    };
	     // console.log(x(plotX));
	}
	sampMax = d3.max(myArray, function(d) { return d.y;});

   var DCyScale = d3.scale.linear()
    	.range([hght, 0])
    	.domain([0, sampMax + .5]);

	var DCxScale = d3.scale.linear()
    	.range([margin, wdth])
    	.domain([-0.1, sample[nN - 1] +0.5]);

	// change scales to hold all x, all y
   var DCxAxis = d3.svg.axis()
      .scale(DCxScale)
      .orient("bottom");

   var DCyAxis = d3.svg.axis()
      .scale(DCyScale)
      .orient("left");
      
  var graph = d3.select("#mixSmrySVG")
    .attr("width", wdth + margin*2)
    .attr("height", hght + margin*2)
  .append("g")
   .attr("transform", "translate("+ (2 * margin) + "," + margin + ")");
    
    
    graph.append("g")
      .attr("class", "y axis")
      .call(DCyAxis);
      
      
	graph.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + (radius + hght) +")")
      .call(DCxAxis);

    graph.append("g")
    	.attr("class", "text")
    	.attr("x", 20)
    	.attr("y", hght + 5)
    	.text(xlegend);  
      
   	Dots =  graph.selectAll("g.circle")
            .data(myArray); 
	Dots.enter().append("circle")
            .attr("cx", function(d){ return DCxScale(d.x);} ) 
            .attr("r", radius ) 
            .attr("cy", function(d){ return DCyScale(d.y);} ) 
            .style("fill","steelblue")
            .style("fill-opacity", 0.6);
    //return Dots; // and myArray?
 }


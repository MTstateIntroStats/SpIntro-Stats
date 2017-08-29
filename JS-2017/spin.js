 // subroutine to make a gameboard spinner in D3 javascript.
 // Inputs: 
 //     category labels and relative probabilities
 
    var margin = [{top: 50}, {right: 20}, {bottom: 50}, {left: 20}],
        width =  Number(400), // - margin.right - margin.left,
        height = Number(300), // - margin.top - margin.bottom;       
        r = 110,         //radius
        tr = 150,        //text radius
        ir = 75,         //inner radius
        //circles = [],
        colors = [], 
        //color = d3.scale.ordinal()
        //    .range(["#a05d56","#ff8c00","#d0743c","#98abc5", "#8a89a6", 
        //            "#7b6888", "#6b486b" ]),      
        //dColor = spinData.drawColor,
        nCat,
        nSpin,
        spacing =10,
        spinDiv = d3.select("#spinSVGgoesHere"),
       // spinSmryDiv = d3.select("#spinSmrySVGdiv"),
        pieData = [],
        spinData = [],
        spinCumProb=[],     // cumulative probabilities
 		spinDuration = 400,
        spinSlideDuration = 400,
        spinGroups =[],
        spinMatch,
        spinProb = [],
        spinRepResults = [],
        spinStopRule;
       
   var svgSpin = spinDiv.append("svg")             // 440w x 440h
      .attr("width",  (440))
      .attr("height", (300))
      .append("g")
      .attr("transform", "translate(" + (+r +10) + ","+ ( +r+10) + ")");     
   
//   var svgSpinSmry = spinSmryDiv.append("svg")             // 440w x 440h
//      .attr("width",  (440))
//      .attr("height", (200))
//      .append("g")
//      .attr("transform", "translate(" + (+r +10) + ","+ ( +r+10) + ")");     
    
  var arrowData = [ { "x": 4,   "y": 78},  { "x": 0,   "y": 0},  
                       { "x": -4,   "y": 78},  { "x": 0,   "y": 0}, 
                       { "x": 0,   "y": 80},  { "x": 0,   "y": 0}, 
		       { "x": 0,  "y": -93}, { "x": -4,  "y": -83}, 
                       { "x": 4,  "y": -83}, { "x":  0,  "y": -93}];
      //using this method
  var lineFunction = d3.svg.line()
                          .x(function(d) { return d.x; })
                          .y(function(d) { return d.y; })
                         .interpolate("linear");
       // now draw the pointer

	
	
function drawDonut(){
    var  w = width;        
    spinGroups =  Papa.parse( document.getElementById("spinCats").value).data[0]; // labels of each
    spinProb =   jStat.map( Papa.parse( document.getElementById("spinProbs").value).data, Number); 
    spinNCat = spinGroups.length;
    if(spinNCat < 2){
    	alert("Must have more than one label and more than one probability");
    }

    // force group length to = prob length
    if( spinNCat > spinProb.length){
    	spinGroups.length = spinNCat = spinProb.length;
    } else if(spinProb.length < spinNCat){
    	spinProb.length = spinNCat;
    }
    
    arc = d3.svg.arc()  // create <path> elements  in arcs
       .outerRadius(r)
       .innerRadius(ir);   
	   
    var spinTotalProb = jStat.sum(spinProb),
    stdize = function(x){return x/spinTotalProb;};
    spinProb =  jStat.map(spinProb, stdize);
    spinCumProb = jStat.cumsum(spinProb);
	spinCumProb.unshift(0);
	
    for ( i=0; i < spinNCat; i++)  { 
            pieData[i]  = { "label": spinGroups[i] , 
 			                      "value": spinProb[i]
						};
    	colors[i] = d3.hcl(30 + 300 * i/spinNCat , 25, 80, 0.8);
	}
	pieData.length = spinNCat;
	
    spinColorFn = d3.scale.ordinal().range(colors);      
 
    svgSpin.data([pieData])     
       .append("svgSpin:g")
       .attr("transform", "translate(" + (r + 10) + "," + ( r + 35) + ")");

     var pie = d3.layout.pie().sort(null)
              .value(function(d) { return d.value; });  
        // create arc data for us given a list of values


    arcs = svgSpin.selectAll("g.slice")
         .data( d3.layout.pie().value(function(d, i) { return d.value; } )
		.sort(null))
          //  .data([drawData])
       .enter().append("svgSpin:g")      
         .attr("class", "slice");

     arcs.append("svgSpin:path")
          .attr("fill", function(d, i) { return colors[i]; } ) 
          .attr("d", arc); 
                     
     arcs.append("svgSpin:text")     //add a label to each slice
        .attr("transform", function(d) { 
         d.innerRadius = 0;
         d.outerRadius = tr ;
            return "translate(" + arc.centroid(d) + ")";   
              })
         .attr("text-anchor", "middle")               
         .text(function(d, i) { return pieData[i].label; });  
         
       svgSpin.append("circle")
       .attr("cx",0)
       .attr("cy",0)
       .attr("fill","blue")
       .attr("r",5);

     arrow = svgSpin.append("path")
              .attr("d", lineFunction(arrowData))
              .attr("stroke", "blue")
              .attr("stroke-width", 4)
              .attr("fill", "blue");   
   
}  //end of drawDonut

  // -----  Transitions --- //
  // t1  spin it to angle
  // t2  toss out the sampled circle
   var tween = function(i){
	arrow.transition()
	.delay((spinSlideDuration + spinDuration) * i)
	.duration(spinDuration)
	.ease("cubic-out")
	.attrTween("transform", function (){
          return d3.interpolateString("rotate( 0, 0, 0)", 
                                   "rotate(" + (Number(spinData[i].angle) * 360 + 360)+ ", 0, 0)");
        });
   };
    
function spin1(){
	// for testing
	var nDraws = 1;
      spinAngle = Math.random(nDraws) * 360;
      //drawColor = cut( spinAngle, spinCumProb);
   arrow.transition()
	//.delay((slideDuration + spinDuration) * i)
	.duration(spinDuration)
	.ease("cubic-out")
	.attrTween("transform", function (){
          return d3.interpolateString("rotate( 0, 0, 0)", 
                                   "rotate(" + (spinAngle + 720) + ", 0, 0)");
        });
	//console.log(spinAngle);
	//console.log(drawColor);
}

function xspace(i){
           return i * spacing; 
    }

function spinNSpins(){
    nSpin = +document.getElementById("nSpins").value;
     
    spinGroups =  Papa.parse( document.getElementById("spinCats").value).data[0]; // labels of each
    spinProb =   jStat.map( Papa.parse( document.getElementById("spinProbs").value).data, Number); 
    spinNCat = spinGroups.length;
    
    if(spinStopRule !== "Fixed"){
    	// zap any results hanging around
    	spinRepResults = [];
    }
   spinStopRule = "Fixed";
   
    //clear out old arrow, arcs, circles, and text
    if(typeof(arrow) !== "undefined"){
    	arrow.remove();
    }

	drawDonut();
   spinMore(nSpin);	
}

function spinMore(nDraws){
	// grab a random set of n draws and plot each as a spinner outcome in showSequence()
	var angle,
	spinColor;
	spinData = [];
	
	//drawDonut();
	
  	for(i=0;i<nDraws;i++){
  		angle = Math.random();
  		spinColor =  cut( angle, spinCumProb);
  		spinData[i] = {angle: angle, group: spinColor};
	}
	//console.log(spinData);
	spinData.length = nDraws;
	showSpinSequence(spinData);
} 
   //  end of spinMore

function spinsTill1(){
	var spinStopper =  document.getElementById("spinTil").value,
    	angle,
    	i =0,
    	spinColor = -1,
    	w = width;        
    spinData.length = 0;
    
     if(spinStopRule !== "OneOfOneType"){
    	// zap any results hanging around
    	spinRepResults = [];
    }
	spinStopRule = "OneOfOneType";
	
    spinGroups =  Papa.parse( document.getElementById("spinCats").value).data[0]; // labels of each
    spinProb =   jStat.map( Papa.parse( document.getElementById("spinProbs").value).data, Number); 
    spinNCat = spinGroups.length;

    //clear out old arrow, arcs, circles, and text
    if(typeof(arrow) !== "undefined"){
    	arrow.remove();
    }
    
	drawDonut();
	
	//console.log(spinStopper);
	//console.log(spinStopper.length);
    //if(spinStopper.length > 1){
    	spinMatch = spinGroups.indexOf(spinStopper);
    	if (spinMatch < 0){
    		alert("You must choose one of the labels.")
    	}
    	while (spinColor !== spinMatch){
  			angle = Math.random();
  			spinColor =  cut( angle, spinCumProb);
  			spinData[i++] = {angle: angle, group: spinColor};
    	}

	showSpinSequence(spinData);
	//}
}

function spinsTillAll(){        
	var angle,
		error=" ",
		i = 0,
		ndx=0,
		spinColor,
		table = [];  
    spinData = [];
    if(spinStopRule !== "OneOfEach"){
    	// zap any results hanging around
    	spinRepResults = [];
    }
	spinStopRule = "OneOfEach";
	
    spinGroups =  Papa.parse( document.getElementById("spinCats").value).data[0]; // labels of each
    spinProb =   jStat.map( Papa.parse( document.getElementById("spinProbs").value).data, Number); 
    spinNCat = spinGroups.length;
    

    //clear out old arrow, arcs, circles, and text
    if(typeof(arrow) !== "undefined"){
    	arrow.remove();
    }
	drawDonut();
	
    for(i=0;i<spinNCat;i++){
    	table[i] = 0;
    }
    
    table.length = spinNCat;
    
    while (d3.min(table) < 1){
  		angle = Math.random();
  		spinColor =  cut( angle, spinCumProb);
  		spinData[ndx] = {angle: angle, group: spinColor};
  		table[spinColor] += 1;
  		ndx++;
  		if(ndx > 10000){ error="10K";
  			break;}
    }
    //console.log(table);
    if(error !== "10K"){
    	showSpinSequence(spinData);
    }
}

function showSpinSequence(spinData){
	var nDraws = spinData.length;
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
     arcs.append("svgSpin:text")     //add a label to each slice
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
	//spinData.length = nDraws;
	//console.log(spinData);				
	// Create the sampled circles (output)
      //  but hide them with r = 0 
    circles = svgSpin.selectAll("g.circle")
         .data(spinData);
       circles.enter().append("circle")
          .attr("fill", function(d){ return colors[d.group]; } )
          .attr("cx", function(d){return  93 * Math.cos((90 - +d.angle*360)*Math.PI/180 );})  
          .attr("cy", function(d){return -93 * Math.sin((90 - +d.angle*360)*Math.PI/180);})
          .attr("r", 0)     // -> 20  
          ;
        
	//console.log(spinAngle);
	//console.log(drawColor);
	  
    textLabels = svgSpin.selectAll("g.text")
         .data(spinData)
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
          .delay(spinDuration + (spinSlideDuration + spinDuration) * i )
          .duration( spinSlideDuration )
          .ease("linear")
          .attr("cx", xspace(i))
          .attr("cy", + r + 20)
          .attr("r", 20);
	});

    textLabels.each(function(d,i){
	// move the selected ball out
        d3.select(this)
          .transition()
           .delay( ( spinSlideDuration + spinDuration) * (i+1) )
          .attr("opacity", 1)
       ;  
   });
   document.getElementById("repeatSpins").style.display = "block"; 
}


function spinRepeat(times){
	var i,
		thisProb;
	
    if(spinStopRule === "Fixed"){
    	thisProb = spinProb[0];
    	spinRepResults = spinRepResults.concat(rbinom(nSpin, thisProb, times));
    	// track  number of first type?                    
    } else if(spinStopRule === "OneOfOneType"){
    	// track number of spins needed
    	thisProb = spinProb[spinMatch];
    	spinRepResults[0] = spinData.length;
    	for(i=0; i<times; i++ ){
    		spinRepResults.push(rgeom(thisProb));
    	}
    } else if(spinStopRule === "OneOfEach"){
    	// track number of spins needed
    	spinRepResults[0] = spinData.length;
    	spinRepResults = spinRepResults.concat(draws2get1ofEach(times));
    } else {
    	console.log("Bad option for spinStopRule");
    }	
    //plot spinResults as a histogram
}




function draws2get1ofEach(reps) {
	// randomly spin til we get one of each category
	// returns the number of spins needed
	var i = 0,
	    j = 0,
	    table = [],
	    temp = [],
	    draw1 = [],
	    nDraws = jStat.ones(1,reps),
	    nCat = spinProb.length,
	    probs = spinProb.slice(0);
	    
	if (nCat < 2) {
		return nDraws; // with only 1 category, we get all (only one) categories right away
	} // at least 2 categories
	draw1 = sample(jStat.seq(0,nCat-1,nCat), reps, probs);
	for( i=0; i < reps; i++){
		probs = spinProb.slice(0);  // need to reset for each rep
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
	    xlegend = spinStopRule === "Fixed"? "Spins to get one of the first type":
	       			spinStopRule === "OneOfOneType"? "Spins to get a " + spinGroups[spinMatch]:
	       			"Spins to get one of each type";
	    
	sample.sort(function(a,b){return a - b}) ;   
          // numeric sort to build bins for y values
          // start on left with smallest x.

	
	var radius = (nN < 101)? 6:
	    (nN < 501)? 5:
	    (nN < 1001)? 4:
	    (nN < 5001)? 3: 2; // perhaps this should relate to width/height of svg]
    var gees = d3.select("#spinSmrySVG").selectAll("g");
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
      
  var graph = d3.select("#spinSmrySVG")
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


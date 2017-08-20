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
        spacing =10,
        spinDiv = d3.select("#spinSVGgoesHere"),
        pieData = [],
        spinAngle = [],
        spinData = [],
        spinDrawColor = [],
        spinCumProb=[],     // cumulative probabilities
 		spinDuration = 400,
        spinSlideDuration = 400;
       
   var svgSpin = spinDiv.append("svg")             // 440w x 440h
      .attr("width",  (440))
      .attr("height", (300))
      .append("g")
      .attr("transform", "translate(" + (+r +10) + ","+ ( +r+10) + ")");     
   
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

    // force group length to = prob length
    if( spinNCat > spinProb.length){
    	spinGroups.length = spinNCat = spinProb.length;
    } else if(spinProb.length < spinNCat){
    	spinProb.length = spinNCat;
    }
    
    var spinTotalProb = jStat.sum(spinProb),
    stdize = function(x){return x/spinTotalProb;};
    spinProb =  jStat.map(spinProb, stdize);
    spinCumProb = jStat.cumsum(spinProb);
	spinCumProb.unshift(0);
	
    for ( i=0; i < spinNCat; i++)  { 
            pieData[i]  = { "label": spinGroups[i] , 
 			                      "value": spinProb[i]
						};
    	colors[i] = d3.hcl(30 + 300 * i/spinNCat , 25, 80, .9);
	}
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
   
}

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

 //   for( var i = 0; i < data.nDraws; i++){ 
 //       tween(i);
 //   }
    
function getNSpin() {
	var stopper = document.getElementById("spinStopper");
	if (stopper == "Fixed number") {
		document.getElementById("getSpinStop").style.display = "block";
	} else {
		document.getElementById("getSpinStop").style.display = "none";
	}
}

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

function spinMore(nDraws){
	// for testing
	var angle,
	spinColor,
    spacing = (width -20) / (nDraws + 1); //for sampled circles

    function xspace(i){
           return i * spacing - r + 10; 
    }

    arc = d3.svg.arc()  // create <path> elements  in arcs
       .outerRadius(r)
       .innerRadius(ir);   

    //clear out old arrow, circles, and text
    if(typeof(arrow) !== "undefined"){
    	arrow.remove();
    }
    //circles = svgSpin.selectAll("circle");
    if(typeof(circles) !=="undefined"){
    	circles.remove();
    }
    //textLabels = svgSpin.selectAll("text");
    if(typeof(textLabels) !=="undefined"){
    	textLabels.remove();
    }

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
  		angle = Math.random();
  		spinColor =  cut( angle, spinCumProb);
  		spinData[i] = {angle: angle, group: spinColor};
  		//spinDrawColor[i] = colors[spinColor];
  	    tween(i);  
	}
	spinData.length = nDraws;
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

}


function draws2get1(prob, reps) {
	// randomly spin till we get one of the first category
	// returns the number of spins needed
	var nCat = prob.length,
	    totalProb = jStat.sum(prob),
	    i,
	    out = [];
	stdize = function(x) {
		return x / totalProb;
	};
	prob = jStat.map(prob, stdize);
	var cumProb = jStat.cumsum(prob);
	if (nCat < 2) {
		return 1 ;
	}
	if (reps == 1) {
		return rgeom(prob[0]);  // rgeom defined in helpers.js
	}
	for ( i = 0; i < reps; i++) {
		out.push = rgeom(prob[0])
	}
	return out;
} 



function draws2get1ofEach(prob, reps,fullOut) {
	// randomly spin til we get one of each category
	// returns the number of spins needed
	// if fullOut = TRUE, gives info to trace the critical steps
	//  of each sequence of spins: Category seen, and
	//  spins to the next new category
	var nCat = prob.length,
	    //totalProb = jStat.sum(prob),
	    i = 0,
	    j = 0,
	    table = [],
	    temp = [],
	    nDraws = [],
	    draws = [];
	    //cols = [];
	//if (totalProb !== 1.000) {
		//prob = jStat.map(prob, function(x) {
		//	return x / totalProb;
		//});
	//}
	//reps = Number(reps);
	if (nCat < 2) {
		return 1;
	}
	temp = sample(jStat.seq(1, nCat, nCat), reps * nCat * Math.ceil(4 / Math.min(prob)), prob);
	for ( i = 0; i < reps; i++) {
		// reset the table
		for(j =0; j < nCat; j++){
			table[j] = 0;
		}  
		for ( j = 0; j < temp.length; j++) {
			table[temp[j]] += 1;
			// after nCat spins, table the row to see if we have every category
			if (j >= nCat) {
				if (Math.min(table) > 0) {
					break;
				}
			}
		}
		while (j >= temp.length) {
			// we ran out of draws, get more
			console.log("Ran out of draws in 262 of spin.js");
			temp = sample(jStat.seq(1, nCat, nCat), reps * nCat * Math.ceil(4 / Math.min(prob)), prob);
			
			for ( j = 0; j < temp.length; j++) {
				table[temp[j]] += 1;
				// after nCat spins, table the row to see if we have every category
				if (j >= nCat) {
					if (Math.min(table) > 0) {
						break;
					}
				}
			}
		}
		nDraws[i] = j;
		// do a shift on temp to get to new portion of the sequence
		draws[i] = table;
		temp.splice(0, j); //removes the first j values in temp
	}
	if (!fullOut) {
		return [{"nDraws": nDraws}];
	} else{	return [{"nDraws" : nDraws}, {"draws" : draws}];
	}

}




function reconstructSpins(output, prob) {
	// uses fullOut from 'draws2get1ofEach()' or a count from 'draws2get1()'
	// and reconstructs a history of spins.  With more than 2 categories,
	// the sequence is not unique, as intermediate draws could have come
	// from any of several sequences which have the same 'new categories'
	// in the same positions, but differ in the "filler" spots.
	var catObs = [],
	    val,
	    i,
	    nCat = prob.length;
	if (output.length == 1) { // from draws2get1()
		if (output == 1) {
			return 1;
		}
		if (nCat == 2) {
			catObs[0] = 1;
			for(i = 1; i < output; i++){
				catObs.unshift(0);        // push a 0 onto the front of the vector
			}
			return catObs;
		}
		return [sample(jStat.seq(2, nCat, nCat-1), output - 1, shift(prob)), 1];
	}
	// else we're doing 'draws2get1ofEach()

	//if(Math.abs(nCat - (output.length -1) / 2) > .01){
	//  return "Output dimensions don't match length(prob)";}
	return output.draws;
	// catObs = output[ 2:(nCat + 1)];
	//  trial =  output[ -(1 + 0:nCat)];

	//  val = [ jStat.seq(1, trial[2]-trial[1],1) * 0 + catObs[0], 1];
	// uses first 2 categories observed
	//  if(nCat > 2){
	//    for(i = 2; i < nCat; i++){
	//      covered = catObs[1:(i-1)];
	// print(covered)
	// print( trial[ndx] - trial[ndx-1] )
	//      val =  [val, sample(covered, trial[i] - trial[i-1] -1,  prob = prob[covered]),
	//                catObs[i]];
	//    }
	//  }
	//  return val;
}

   
  

  //function repData() {
  	// 
   // run = spinRunButton;
   // prob = sapply(strsplit( spinProbs, ","), as.numeric);
   // sumProb = jStat.sum(prob);
   // groups = sapply(strsplit( spinCategories, ","), function(x){
   //   gsub("[[:space:]]", "", x);});  //[order(-prob)]
    //prob = -sort(-prob);
   // nCat =  length(groups);
   // nReps =  ifelse(is.null( spinReps), 1, as.numeric( spinReps));
   // fixedN = ( spinStopRule == "Fixed number");
   // if(fixedN){ 
//      nDraws = Number( spinNDraws);
//      nCat = Number( spinNCat);
      //samplData = matrix(sample(arange(1,nCat,1), nReps * nDraws,
      //                           prob = prob,replace = TRUE), ncol=nDraws);
      //fn = match( spinFn, spinFunctionList);
      //if(fn==3){
      //  return(apply(samplData, 1 , function(x) { max(rle(x)[[1]]);}));
      //} else{
      //  return(apply(samplData, 1 , function(x) {table([arange(1,fn,1),x](fn)-1);}));
      // }
    //} else {
      // apply stopping rule
    //  if( spinStopRule =="One of first type"){
    //    nDraws = draws2get1( prob, nReps);
    //    return(nDraws)
    //  } else {  //  spin_stopRule =="One of each"
    //    nDraws =  draws2get1ofEach( prob, nReps);
    //    return(nDraws)
    //  }
   // }
  //}
  
//  function spin_Plot() {
//    if(spinRunButton == 0) {return null ;}
//    return spinData;
//  }
  
  
//   function spin_Summary () {
//    if(spinRunButton == 0) return( null)
//    var i,
//        dataDf = spinData,
//        out1 = [];
//     for(i=0,i < dataDf.drawColor.length;i++){
//     	out1[i] += 1;
//     }  
//     out1 = [{labels: dataDf.pieLabels},{counts: out1}];
     
//    if(spinStopRule =="Fixed number"){
//        runs =  rle(dataDf.drawColor[arange(1, dataDf.nDraws, 1)]);  
//        out1 =  [ out1, runs[0].max];                               // fixme
//        names(out1)[dataDf.nCat + 1] = "maxRunLength";
//      } else{
//        out1 =  [ out1, dataDf.drawColor.length];
//        names(out1)[dataDf.nCat ] = "spins";
//      };
//      out1 = jStat.transpose(out1);
//      rownames(out1) = " ";
//      return(out1);
//    }
  
  
  // run more:
//  function spin_Summry2 (){
//    var rData =  repData();
//    as.table(matrix(c(quantile(rData,c(0,.25,.5,.75,1)), mean(rData), sd(rData)),nrow=1,
//                    dimnames = list(" ", c("Min","Q1","Median","Q3","Max","Mean","SD") )));
//  }
  
//  function spin_Summry3(){
//    var counts  = repData();
//    if(spinStopRule =="Fixed number"){
//      temp = jStat.transpose(table(counts));
//    } else {
//    	if(spinStopRule =="One of first type"){
//      	temp =  jStat.transpose(table([counts, arange(1,counts.max,1)])); // -1??
//        } else  if(spinStopRule =="One of each"){
//      temp =   jStat.transpose(table([counts, arange(spinNCat,counts.max, 1)]))// -1) ??
//    }
//    }
    //print(temp)
    //rownames(temp) <- "Counts"
//    return(temp);
//  }
  
//   function spin_Histogrm(){
//    var stat, xlimits, x = sort(repData());
//    var y = jStat.map(x, function(z) {arange(1,z.length,1);} ); 
//    if(spinStopRule =="Fixed number"){
//      stat =  spinFn;
//      begin = 0 + (spinFn == spinFunctionList[3]); 
//      xlimits = x.range // c(begin,  spinNDraws)
//    } else  if(spinStopRule == "One of each") {                // or one of first fixme
//      stat = "Number of Spins";
//      xlimits = [x.Math.min, x.Math.max];
//    }  // fixme plot below
//    plot(jitter(x, .3), y, main = paste("Distribution of ", stat), xlab = "", cex=2, 
//         ylab = "Frequency",  xlim=xlimits, ylim=c(.5, pmax(10, max(y))), pch = 16, col = blu);
    //
//  }

//  D3 stuff  

   function spinner(){
   	// create data for spinner
    var nDraws=5, // number of repeats (spins)
    spinAngle=[],  // angle for each spin
    drawColor=[],  // color of each spin 
    drawSumry=[],  // results summary
    cumProb=[];     // cumulative probabilities
    // force group length to = prob length
    if( groups.length > prob.length){
    	groups.length = prob.length;
    } else if(prob.length < groups.length){
    	prob.length =groups.length;
    }
    var totalProb = jStat.sum(prob),
     spinStopRule = "Fixed number"; //document.getElementById("spinNStop").value,         // when to stop
     spinNstop =  1; //parseInt( document.getElementById("spinNStop").value); // if stopping after fixed number
    stdize = function(x){return x/totalProb;};
    prob =  jStat.map(prob, stdize);
    cumProb = jStat.cumsum(prob);
    //console.log(groups);
    //console.log(prob);
    //console.log(cumProb);

    if(spinStopRule === "Fixed number"){
      spinAngle = jStat.rand(spinNstop,1);   
      drawColor = jStat.map(spinAngle, function(x){ cut(x, spinCumProb);});  
      nDraws =  spinNStop;
    } else {
      // apply a stopping rule
      if(spinStopRule == "One of first type"){
        drawSumry <- draws2get1( prob, 1);               //fixme, I'm  not defined
        if(drawSumry === 1){                 // got it on 1st spin
          drawColor =  1;
          nDraws = 1;
          spinAngle =  Math.random() * prob[0];
        } else{                            // take a few spins first
          cumProb =  [0, cumProb];
          drawColor = reconstructSpins( drawSumry, prob);
          nDraws = drawSumry;
          spinAngle =  jStat.rand(drawSumry[0],1) * prob[drawColor] + cumProb[drawColor];
          //  gives random spin in 1st category for last draw,
          //  in other categories for prior draws.
        }
      } else{
        // Stop after we get one of each type
        drawSumry = draws2get1ofEach( prob, 1, fullOut = true);
        nDraws = drawSumry.nDraws;
        cumProb =  [0, cumProb];
        drawColor = reconstructSpins( drawSumry , prob);
        spinAngle =  Math.random() * prob[drawColor] + cumProb[drawColor]; // howto get more reps?
      }
    }
    
    spinData  = [{"nCat": spinNCat},   // define globally without using var
                    {"nDraws": nDraws},
                    {"pieValues": prob},
                    {"pieLabels": groups},
                    {"spinAngle": (spinAngle + 1 ) * 360},
                    {"drawColor": drawColor }
    			];
      //D3 variables
      

      // data  pointer arrow 
    // Create the sampled circles (output)
   //  but hide them with r = 0 
 
}

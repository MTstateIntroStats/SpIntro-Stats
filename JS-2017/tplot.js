var tchanged = "N",
	ndx,
	margin = {top: 10, right: 20, bottom: 30, left: 50},
    probt,
    plusminus = "+/-",
    ptText,
    tseq = jStat.seq(-5.2, 5.2, 303),
    t1,
    t2,
    ttText;

   
var width = 540 - margin.left - margin.right,
    height = 320 - margin.top - margin.bottom;


 //ytAxis = d3.svg.axis().scale(ytRange)
   // .orient("left").ticks(5);
    
drawTcurve = function(){   
	//var new = true;
  tdf= +document.getElementById("dfT").value;
  xtRange = d3.scaleLinear().range([0, width]).domain(d3.extent(tseq));
  ytRange = d3.scaleLinear().range([height, margin.top]).domain([0, jStat.studentt.pdf(0, tdf)]);
 xtAxis = d3.axisBottom(xtRange)
    .ticks(7);

  pdftline = d3.line()
    .x(function(d) { return xtRange(d); })
    .y(function(d) { return ytRange(jStat.studentt.pdf(d, tdf)); });
	
	if(typeof(tsvg) === "object"){
	  d3.selectAll("path").remove();
	} else{
  	tsvg = d3.select("#tPlotGoesHere")
     .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
    .append("g")
        .attr("transform", 
              "translate(" + margin.left + "," + margin.top + ")");
  }
  tsvg.append("g")			// Add the X Axis
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xtAxis);

  pdftLine = tsvg.append("path")
                      .attr("d", pdftline(tseq))
                      .attr("stroke", "blue")
                      .attr("stroke-width", 2)
                      .attr("fill", "none");



// add buttons for desired area
//if(typeof(tsvg) === "object"){  
        tsvg.append("text")      // create Lower Area Button
          .attr("x",  4  )
          .attr("y", 20)
          .attr("font-size", 18 + "px")
          .text("Lower");

   var   Ltbox = tsvg.append("rect")    // add frame and activation
           .attr("class", "rect")
           .attr("x", 0)
           .attr("y", 0)
           .attr("width", 66)
           .attr("height",30)
           .attr("rx", 4)
           .attr("ry", 4)
           .style("stroke", "blue" )
           .style("stroke-width",2 )
           .style("fill","lightblue")
           .style("fill-opacity", 1.0E-6)
           .on("click",  function(){ 
				area = "L";
				filtert( area);});
           
         tsvg.append("text")      // create Upper Area Button
          .attr("x", 425   )
          .attr("y", 20)
          .attr("font-size", 18 + "px")
          .text("Upper");

   var   Utbox = tsvg.append("rect")    // add frame and activation
           .attr("class", "rect")
           .attr("x", 420)
           .attr("y", 0)
           .attr("width", 66)
           .attr("height",30)
           .attr("rx", 4)
           .attr("ry", 4)
           .style("stroke", "blue" )
           .style("stroke-width",2 )
           .style("fill","lightblue")
           .style("fill-opacity", 1.0E-6)
           .on("click",  function(){ 
				area = "U";
				filtert( area);});
           
         tsvg.append("text")      // create Center Area Button
          .attr("x", 275   )
          .attr("y", 24)
          .attr("font-size", 18 + "px")
          .text("Center");

   var   Ctbox = tsvg.append("rect")    // add frame and activation
           .attr("class", "rect")
           .attr("x", 270)
           .attr("y", 0)
           .attr("width", 73)
           .attr("height",30)
           .attr("rx", 4)
           .attr("ry", 4)
           .style("stroke", "blue" )
           .style("stroke-width",2 )
           .style("fill","lightblue")
           .style("fill-opacity", 1.0E-6)
           .on("click",  function(){ 
				area = "C";
				filtert( area);});
           
         tsvg.append("text")      // create Extremes Area Button
          .attr("x", 4   )
          .attr("y", 114)
          .attr("font-size", 18 + "px")
          .text("Extremes");

   var   Etbox = tsvg.append("rect")    // add frame and activation
           .attr("class", "rect")
           .attr("x", 0)
           .attr("y", 94)
           .attr("width", 92)
           .attr("height",30)
           .attr("rx", 4)
           .attr("ry", 4)
           .style("stroke", "blue" )
           .style("stroke-width",2 )
           .style("fill","lightblue")
           .style("fill-opacity", 1.0E-6)
           .on("click", function(){ 
				area = "E";
				filtert( area);});
           
         tsvg.append("text")      // create 2nd Extremes Area Button
          .attr("x", 397   )
          .attr("y", 114)
          .attr("font-size", 18 + "px")
          .text("Extremes");

   var   Etbox2 = tsvg.append("rect")    // add frame and activation
           .attr("class", "rect")
           .attr("x", 394)
           .attr("y", 94)
           .attr("width", 92)
           .attr("height",30)
           .attr("rx", 4)
           .attr("ry", 4)
           .style("stroke", "blue" )
           .style("stroke-width",2 )
           .style("fill","lightblue")
           .style("fill-opacity", 1.0E-6)
           .on("click",  function(){ 
				area = "E";
				filtert( area);});
  
  
}

function filtert( area) {
	var ptOut, ptIn,
		tadd = false, toutput, 
	    xptLoc, xttLoc, yptLoc, yttLoc,
	    tscoreIn, tscoreAbs, tscoreOut, zero = 0.000 ;  
    if (tchanged ==="T") {
   		tscoreIn = +document.getElementById("tInput").value; 
   		//console.log(tIn.typeof);
    	tscoreAbs = Math.abs(tscoreIn);
	     //console.log(typeof(tscoreIn));
		if (area === "L") {
			toutput = jStat.seq(-7.0, tscoreIn, 200);
			ptOut = jStat.studentt.cdf(tscoreIn, tdf).toPrecision(4);
		} else 	if (area === "U") {
			toutput =  jStat.seq(tscoreIn, 7.0,  200);
			ptOut = (1 - jStat.studentt.cdf(tscoreIn, tdf)).toPrecision(4);
		} else 	if (area === "E") {
			//Lower end
			toutput =  jStat.seq(-7.0, -tscoreAbs, 150);
			tscoreOut = -tscoreAbs;
			drawtArea( jStat.seq(tscoreAbs, 7.0,  150), false);
			tadd = true;  // add in upper end
			tscoreOut = tscoreAbs;
			ptOut = (2 * jStat.studentt.cdf(-tscoreAbs, tdf)).toPrecision(4);
		} else{   // center
			toutput =  jStat.seq(-tscoreAbs, tscoreAbs, 200);
			ptOut = (jStat.studentt.cdf(tscoreAbs, tdf) - jStat.studentt.cdf(-tscoreAbs, tdf)).toPrecision(4);
		}
		printPtResults(ptOut);
    } else if (tchanged === "P"){
    	ptIn = +document.getElementById("ptInput").value;
    	tscoreIn = jStat.studentt.inv(ptIn, tdf);  // set for Lower, reverse sign for upper
		//console.log(typeof(tscoreIn));
	if (area === "L") {
		toutput = jStat.seq(-7.0, tscoreIn, 200);
		tscoreOut =  tscoreIn.toPrecision(4);
	} else 	if (area === "U") {
		toutput =  jStat.seq(-tscoreIn, 7.0,  200);
		tscoreOut =   -tscoreIn.toPrecision(4);
	} else 	if (area === "E") {
   	    tscoreAbs = -jStat.studentt.inv(ptIn/2.0, tdf); 
		toutput =  jStat.seq(-7.0, -tscoreAbs, 150);  // lower end
		drawtArea( toutput, false);
		toutput = jStat.seq(tscoreAbs, 7.0,  150);
		tadd = true;
		tscoreOut = plusminus.concat(tscoreAbs.toPrecision(4));
	} else{   // center
   	    tscoreAbs = -jStat.studentt.inv((1.0 - ptIn)/2, tdf); 
		toutput =  jStat.seq(-tscoreAbs, tscoreAbs, 200);
		tscoreOut = plusminus.concat(tscoreAbs.toPrecision(4));
	}
    printtResults(tscoreOut );  
   }
	//console.log(typeof(toutput));
   drawtArea(toutput, tadd) ; 
}  

  function drawtArea(filteredts, tadd){
  	 // clear out old areas
  	 //console.log(typeof(filteredts));
  	 if(tadd == false){
  	  tsvg.append("path")
                      .attr("d", pdftline(tseq))
                      .attr("stroke", "blue")
                      .attr("stroke-width", 2)
                      .attr("fill", "white");
        }
        var startData = filteredts.map( function(d) { return 0; } );
        var ydata = filteredts.map(function(d){return jStat.studentt.pdf(d, tdf); });
        var area = d3.area()
            //.interpolate("linear")
            .x( function(d, i ) {
              return xtRange( filteredts[i] );  // x_coord's don't change
            })
            .y0(yRange(0))
            .y1(function(d) {
              return ytRange(d);    // y_coords will shift from 0 to full value 
            });
        Tpath = tsvg.append("path")
        .datum(filteredts)
        .attr("fill", 'white')
        .attr("stroke", "red")
        .attr("opacity", .20)
        .attr("stroke-width", 2)
        .attr("d", area)
        .transition()
        .duration(2000)
        .attr("fill", "red")
        .ease(d3.easeBounceOut)
        .attrTween( 'd', function() {
           var interpolator = d3.interpolateArray( startData, ydata );
        		return function( t ) {
          			return area( interpolator( t ) );
        }
      } );	    
  };                       


function printPtResults(pTxt ){
	var prob = "Probability: ";
	if (ptText){
		ptText.remove();
	}
	if(ttText){ 
		ttText.remove();
	}
	ptText = tsvg.append("text")
	.attr("x", 5)
	.attr("y", 70)
	.attr("style","none")
	.text( prob.concat(pTxt));
}

function printtResults(tTxt ){
	var tscore = "t score: ";
	if (ptText){
		ptText.remove();
	}
	if(ttText){ 
		ttText.remove();
	}
	ttText = tsvg.append("text")
	.attr("x", 310)
	.attr("y", 70)
	.attr("style","none")
	.text( tscore.concat( tTxt));
}

var changed = "N",
	ndx,
	margin = {top: 10, right: 20, bottom: 30, left: 50},
    p,
    plusminus = "+/-",
    pText,
    z = jStat.seq(-3.2,3.2, 303),
    z1,
    z2,
    zText;

   
var width = 540 - margin.left - margin.right,
    height = 320 - margin.top - margin.bottom;
var	xRange = d3.scaleLinear().range([0, width]).domain(d3.extent(z)),
	yRange = d3.scaleLinear().range([height, margin.top]).domain([0, jStat.normal.pdf(0, 0, 1)]);

var xAxis = d3.axisBottom(xRange)
    .ticks(7);

var yAxis = d3.axisLeft(yRange)
    .ticks(5);
           
var pdfline = d3.line()
    .x(function(d) { return xRange(d); })
    .y(function(d) { return yRange(jStat.normal.pdf(d, 0, 1)); });
    
   
var Zsvg = d3.select("#zPlotGoesHere")
     .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
    .append("g")
        .attr("transform", 
              "translate(" + margin.left + "," + margin.top + ")");
  
  Zsvg.append("g")			// Add the X Axis
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);

 //Zsvg.append("g")			// Add the Y Axis
 //    .attr("class", "y axis")
 //    .call(yAxis);

var pdfLine = Zsvg.append("path")
                      .attr("d", pdfline(z))
                      .attr("stroke", "blue")
                      .attr("stroke-width", 2)
                      .attr("fill", "none");


 //TODO:
  //  consider: move prob and z input into the svg?


// add buttons for desired area  
        Zsvg.append("text")      // create Lower Area Button
          .attr("x",  4  )
          .attr("y", 20)
          .attr("font-size", 18 + "px")
          .text("Lower");

   var   Lbox = Zsvg.append("rect")    // and it's frame and activation
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
				filterZ( area);});
           
         Zsvg.append("text")      // create Upper Area Button
          .attr("x", 425   )
          .attr("y", 20)
          .attr("font-size", 18 + "px")
          .text("Upper");

   var   Ubox = Zsvg.append("rect")    // and it's frame and activation
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
				filterZ( area);});
           
         Zsvg.append("text")      // create Center Area Button
          .attr("x", 275   )
          .attr("y", 24)
          .attr("font-size", 18 + "px")
          .text("Center");

   var   Cbox = Zsvg.append("rect")    // and it's frame and activation
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
				filterZ( area);});
           
         Zsvg.append("text")      // create Extremes Area Button
          .attr("x", 4   )
          .attr("y", 114)
          .attr("font-size", 18 + "px")
          .text("Extremes");

   var   Ebox = Zsvg.append("rect")    // and it's frame and activation
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
				filterZ( area);});
           
         Zsvg.append("text")      // create 2nd Extremes Area Button
          .attr("x", 397   )
          .attr("y", 114)
          .attr("font-size", 18 + "px")
          .text("Extremes");

   var   Ebox2 = Zsvg.append("rect")    // and it's frame and activation
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
				filterZ( area);});
// See form validation example https://www.w3schools.com/js/js_validation.asp

function filterZ( area) {
	var add = false,
	    output, pOut, pIn,
	    xpLoc, xzLoc, ypLoc, yzLoc,
	    zIn, zAbs, zOut,zero = 0.000 ;  
    if (changed ==="Z") {
   		zIn = +document.getElementById("zInput").value + zero; // trouble if I input 1 with no decimal, it gets 10??
   		//console.log(zIn.typeOf);
    	zAbs = Math.abs(zIn);
		if (area === "L") {
			output = jStat.seq(-4.0, zIn, 200);
			pOut = jStat.normal.cdf(zIn, 0, 1).toPrecision(4);
		} else 	if (area === "U") {
			output =  jStat.seq(zIn, 4.0,  200);
			pOut = (1 - jStat.normal.cdf(zIn, 0, 1)).toPrecision(4);
		} else 	if (area === "E") {
			//Lower end
			output =  jStat.seq(-4.0, -zAbs, 150);
			zOut = -zAbs;
			drawZArea( jStat.seq(zAbs, 4.0,  150), false);
			add = true;  // add in upper end
			zOut = zAbs;
			pOut = (2 * jStat.normal.cdf(-zAbs, 0, 1)).toPrecision(4);
		} else{   // center
			output =  jStat.seq(-zAbs, zAbs, 200);
			pOut = (jStat.normal.cdf(zAbs, 0, 1) - jStat.normal.cdf(-zAbs, 0, 1)).toPrecision(4);
		}
		printPResults(pOut);
    } else if (changed === "P"){
    	pIn = document.getElementById("pInput").value +0.0;
    	zIn = jStat.normal.inv(pIn, 0, 1);  // set for Lower, reverse sign for upper
	if (area === "L") {
		output = jStat.seq(-4.0, zIn, 200);
		zOut =  zIn.toPrecision(4);
	} else 	if (area === "U") {
		output =  jStat.seq(-zIn, 4.0,  200);
		zOut =   -zIn.toPrecision(4);
	} else 	if (area === "E") {
   	    zAbs = -jStat.normal.inv(pIn/2.0, 0, 1); 
		output =  jStat.seq(-4.0, -zAbs, 150);  // lower end
		drawZArea( output, false);
		output = jStat.seq(zAbs, 4.0,  150);
		add = true;
		zOut = plusminus.concat(zAbs.toPrecision(4));
	} else{   // center
   	    zAbs = -jStat.normal.inv((1.0 - pIn)/2, 0, 1); 
		output =  jStat.seq(-zAbs, zAbs, 200);
		zOut = plusminus.concat(zAbs.toPrecision(4));
	}
    printZResults(zOut );  
   }
   drawZArea(output, add) ; 
}  

  function drawZArea(filteredZs, add){
  	 // clear out old areas
  	 if(add == false){
  	  Zsvg.append("path")
                      .attr("d", pdfline(z))
                      .attr("stroke", "blue")
                      .attr("stroke-width", 2)
                      .attr("fill", "white");
        }
        var startData = filteredZs.map( function(d) { return 0; } );
        var ydata = filteredZs.map(function(d){return jStat.normal.pdf(d, 0,1); });
        var area = d3.area()
            //.interpolate("linear")
            .x( function(d, i ) {
              return xRange( filteredZs[i] );  // x_coord's don't change
            })
            .y0(yRange(0))
            .y1(function(d) {
              return yRange(d);    // y_coords will shift from 0 to full value 
            });
        var path = Zsvg.append("path")
        .datum(filteredZs)
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


function printPResults(pTxt ){
	var prob = "Probability: ";
	if (pText){
		pText.remove();
	}
	if(zText){ 
		zText.remove();
	}
	pText = Zsvg.append("text")
	.attr("x", 5)
	.attr("y", 70)
	.attr("style","none")
	.text( prob.concat(pTxt));
}
function printZResults(zTxt ){
	var zscore = "Z score: ";
	if (pText){
		pText.remove();
	}
	if(zText){ 
		zText.remove();
	}
	zText = Zsvg.append("text")
	.attr("x", 310)
	.attr("y", 70)
	.attr("style","none")
	.text( zscore.concat( zTxt));
}

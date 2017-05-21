var changed,
	i,
	margin = {top: 30, right: 20, bottom: 30, left: 50},
    width = 540 - margin.left - margin.right,
    height = 320 - margin.top - margin.bottom,
    z = jStat.seq(-3.2,3.2, 303),
    zlb = -2;
    
  function zfilter(z){
  	if (z > 1.00) {return  jStat.normal.pdf(0, 0, 1);}
    else {
    	return 0;
    }
  }  
  
  function condition(z){
  	return (z > 1.0);
  }

   
var x = d3.scale.linear().range([0, width]),
	y = d3.scale.linear().range([height, 0]);

    x.domain(d3.extent(z));
    y.domain([0, jStat.normal.pdf(0, 0, 1)]);

var xAxis = d3.svg.axis().scale(x)
    .orient("bottom").ticks(7);

//var yAxis = d3.svg.axis().scale(y)
//    .orient("left").ticks(5);


var pdfline = d3.svg.line()
    .x(function(d) { return x(d); })
    .y(function(d) { return y(jStat.normal.pdf(d, 0, 1)); });
    
var pdfarea = d3.svg.line()
    .x(function(d) { return x(d ); })
    .y(function(d) { if (d > zlb) { return y(0);} else {return y(jStat.normal.pdf(d, 0, 1))}; });

var initarea = d3.svg.line()
    .x(function(d) { return x(d ); })
    .y(function(d){ return y(0.00);});
    
var Zsvg = d3.select("#zPlotGoesHere")
     .append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
    .append("g")
        .attr("transform", 
              "translate(" + margin.left + "," + margin.top + ")");

var pdfLine = Zsvg.append("path")
                            .attr("d", pdfline(z))
                            .attr("stroke", "blue")
                            .attr("stroke-width", 2)
                            .attr("fill", "none");
var pdfArea = Zsvg.append("path")
                            .attr("d", pdfarea(z))
                            .attr("stroke", "red")
                            .attr("stroke-width", 2)
                            .attr("fill", "red")
                            .on("mousedown", changeColor );
  function changeColor(){
    d3.select(this)
      .transition()            
        .delay(0)            
        .duration(5000)
        .attr("fill", "blue");
//        .attr("d", pdfarea(z) );
// for some reason starting with initarea and changing "d" to pdfarea does a weird shrink. 
  };                       
  
  Zsvg.append("g")			// Add the X Axis
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);

    //Zsvg.append("g")			// Add the Y Axis
    //    .attr("class", "y axis")
    //    .call(yAxis);

 
function computeZ(myForm) {
	var ZprobDiv = document.getElementById(myForm);
	var z = 0 + ZprobDiv.z,
	    p = 0 + ZprobDiv.p;
	if ( typeof z == undefined) {
		z = computeZcrit(p, ZprobDiv.area);
		ZprobDiv.p = undefined;
	} else {
		p = computeNormalProb(z, ZprobDiv.area);
		ZprobDiv.z = null;
	}
}

 
function computeZcrit(p, area) {
	var z1,
	    z2;
	z1 = jStat.normal.inv(p, 0, 1);
	if (area == "L")
		return z1;
	if (area == "U")
		return -z1;
	z2 = jStat.normal.inv(p / 2, 0, 1);
	if (area == "E")
		return [z2, -z2];
	z2 = jStat.normal.inv((1 - p) / 2, 0, 1);
	return [z2, -z2];
};

function computeNormalProb(z1, area) {
	var p;
	p = jStat.normal.cdf(z1, 0, 1);
	if (area == "L") {
		return p;
	}
	if (area == "U") {
		return 1 - p;
	}
	p = jStat.normal.cdf(Math.abs(z1), 0, 1) - jStat.normal.cdf(-Math.abs(z1), 0, 1);
	if (area == "C") {
		return p;
	}
	return 1 - p;
}

function showLower(zUpBd) {
	// use filter to subset the data to values data.z < z
	condition = function(z) {return (z <= zUpBd);};
}

function showUpper(zLwBd) {
	// use filter to subset the data to values data.z > z
	condition = function(z) {return (z >= zLwBd);};
}

function showMiddle(zBd) {
	// use filter to subset the data to values -z < data.z < z
	condition = function(z) {return (z >= -zBd & z <= zBd); };
}
       // extremes uses both lower and upper.
       
function showTails(zBd) {
	// use filter to subset the data to values -z < data.z < z
	condition = function(z) {return (z <= -zBd | z >= zBd );};
}


 // subroutine to estimate a proportion or test for a special value
 // Inputs: 
 //     2 category labels (default = Success/Failure) and a count for each 
 // Test routine needs a null hypothesis value
 
    var c1SummDiv = d3.select("#cat1SummarySVGgoesHere"),
        cat1Label1,
        cat1Label2,
        cat1N1,
        cat1N2,
        c1Data = [],
        c1bars ,
        chartC1 ,
        phat;

               
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
    
    //if(chartC1 === "a"){
     //}
	function updateP1(chart, data) {
  		// DATA JOIN
  		var bars = chart.selectAll("rect")
    		.data(data);

	  	// UPDATE
	  	// Update old elements as needed.
    		bars.enter().append("g")
        	.attr("fill", "blue")
        	.attr("transform", function(d, i) { return "translate(14," + i * barHeight + ")"; });

	  // ENTER
	   bars.append("rect")
    	.attr("width", function(d){return x(d.xx ) - 14;} )
    	.attr("height", barHeight -1);
        //.attr("class", "enter");

//        bars.append("text")
// 		.attr("x",  function(d){return 15 + x(d.xx) ;})
// 		.attr("y", barHeight/2)
// 		.attr("dy", ".35em")
// 		.text(function(d) {return d.label}) ;	

 	  // EXIT
  		// Remove old elements as needed.
 	 	bars.exit().remove();
}
//  if(c1FirstDraw){
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
//	   	chartC1.append("g")
//    		.attr("class", "x axis")
//    		.attr("transform", "translate(0,"+ h  +")")
//    		.call(xAxis);
    
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

    
//    c1FirstDraw = false;
  //	} else {
  	//console.log("redrawing");
   	//barC1 = chartC1.selectAll("g")
    //  .data(c1Data);
  	//barC1.transition()
  	//  .attr("width", function(d){return x(d.xx ) - 14;} );
  //}
//  updateP1(chartC1, c1Data);
  
 }

 	  

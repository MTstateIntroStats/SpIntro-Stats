<script src="http://d3js.org/d3.v3.js"></script>
<script type="text/javascript">
 var networkOutputBinding = new Shiny.OutputBinding();
   $.extend(networkOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-network-output');
    },
    renderErr: function(el, err) {
          print(err$message)
      },
    renderValue: function(el, data) {

    var spinDuration = 400,
        slideDuration = 400;

    var margin = {top: 50, right: 20, bottom: 50, left: 20},
        w =  540, // - margin.right - margin.left,
        h = 300, // - margin.top - margin.bottom;       
        r = 110,          //radius
        tr = 150,         //text radius
        ir = 75,         //inner radius 
        color = d3.scale.ordinal()
            .range(["#a05d56","#ff8c00","#d0743c","#98abc5", "#8a89a6", 
                    "#7b6888", "#6b486b" ]),      
        dColor = data.drawColor[0]; 

        var spacing = w / (data.nDraws + 1); //for sampled circles
 
        var pieData = [];
        for (var i=0; i < data.nCat; i++)  { 
            pieData[i]  = { label: data.pieLabels[i] , 
 			                      value: data.pieValues[i]
			      };
	}
        
        var drawData =  [];
        for (var i=0; i < data.nDraws; i++)  { 
            drawData[i]  = {angle: data.spinAngle[i],
			                      group: data.drawColor[i] 
            };
	}
	//console.log(drawData);

        //remove the old graph
        var vis = d3.select(el).select("svg");
        vis.remove();
        $(el).html(""); 
          
        //append a new one 
        vis = d3.select(el).append("svg")
            .attr("width",  w) //  + margin.right + margin.left)
            .attr("height", h) // + margin.top + margin.bottom)
            .data([pieData])     
            .append("svg:g")
            .attr("transform", "translate(" + (r + w/3) + "," + 
                          ( r + 5) + ")");

     var arc = d3.svg.arc()  // create <path> elements  in arcs
       .outerRadius(r)
       .innerRadius(ir);   

     var pie = d3.layout.pie().sort(null)
              .value(function(d) { return d.value; });  
        // create arc data for us given a list of values


    var arcs = vis.selectAll("g.slice")
         .data( d3.layout.pie().value(function(d, i) { return d.value; } )
		.sort(null))
          //  .data([drawData])
       .enter().append("svg:g")      
         .attr("class", "slice");

     arcs.append("svg:path")
          .attr("fill", function(d, i) { return color(i); } ) 
          .attr("d", arc); 
                     
     arcs.append("svg:text")     //add a label to each slice
        .attr("transform", function(d) { 
         d.innerRadius = 0;
         d.outerRadius = tr ;
            return "translate(" + arc.centroid(d) + ")";   
              })
         .attr("text-anchor", "middle")               
         .text(function(d, i) { return pieData[i].label; });  

      // data  pointer arrow 
    // Create the sampled circles (output)
   //  but hide them with r = 0 
    var circles = vis.selectAll("g.circle")
         .data(drawData)
       .enter().append("circle")
         .attr("fill", function(d, i){ return color(d.group); } )
            .attr("cx", function(d){return 93 * Math.cos( (810 - d.angle)*Math.PI/180 );})  
            .attr("cy", function(d){return -93 * Math.sin((810 - d.angle)*Math.PI/180);})
         //.attr("cy", -93)  // -> 150
         .attr("r", 0)     // -> 20  
         .attr("class", "circle") ; 
		
	//console.log([circles, drawData]);

    vis.append("circle")
       .attr("cx",0)
       .attr("cy",0)
       .attr("fill","blue")
       .attr("r",5);

      var lineData = [ { "x": 4,   "y": 78},  { "x": 0,   "y": 0},  
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
      var arrow = vis.append("path")
              .attr("d", lineFunction(lineData))
              .attr("stroke", "blue")
              .attr("stroke-width", 4)
              .attr("fill", "blue");   

     
  // -----  Transitions --- //
  // t1  spin it to angle
  // t2  toss out the sampled circle
   var tween = function(i){
	arrow.transition()
	.delay((slideDuration + spinDuration) * i)
	.duration(spinDuration)
	.ease("cubic-out")
	.attrTween("transform", function (){
          return d3.interpolateString("rotate( 0, 0, 0)", 
                                   "rotate(" + data.spinAngle[i] + ", 0, 0)");
        });
    }

    for( var i = 0; i < data.nDraws; i++){ 
        tween(i);
    }
    
    var xspace = function(i){
           return i * spacing - w /2 ; 
    }

    var textLabels = vis.selectAll("g.text")
         .data(drawData)
       .enter().append("text")
           .attr("x", function(d,i){ return xspace(i)  ;} ) // 
         .attr("y", 140)
         .text( function(d){return data.pieLabels[d.group];})  // 
         .style("text-anchor", "middle")
         .attr("font-family", "sans-serif")
         .attr("opacity",0)
         .attr("font-size", "20px");

    circles.each(function(d,i){
      d3.select(this).transition()
          // toss out circle
          .delay(spinDuration + (slideDuration + spinDuration) * i )
          .duration( slideDuration )
          .ease("linear")
          .attr("cx", xspace(i))
          .attr("cy", 135)
          .attr("r", 20);
	});



   textLabels.each(function(d,i){
	// move the selected ball out
        d3.select(this)
          .transition()
           .delay( ( slideDuration + spinDuration) * (i+1) )
          .attr("opacity", 1)
       ;   
//          .attr("transform", function(d) {
//              return "rotate(-45)" 
//            });   // see fiddle:  http://jsfiddle.net/eremita/BujPJ/
   });


    }
   });
  Shiny.outputBindings.register(networkOutputBinding, 'jimrc.networkbinding');
    // http://jsfiddle.net/xu4sz/4/
</script>



<script src="http://d3js.org/d3.v3.js"></script>
<script type="text/javascript">
  // based on bostocks example 1129492
  var networkOutputBinding = new Shiny.OutputBinding();
    $.extend(networkOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-network-output');
    },
   renderErr: function(el, err) {
     print(err$message)
   },
   renderValue: function(el, data) {
    var w = data.height + 30,
       h = data.height ,
       radius = data.radius,
       labels = data.labels,
       color = d3.scale.ordinal()
               .range(["#a05d56","#ff8c00","#d0743c","#98abc5", "#8a89a6", 
                       "#7b6888", "#6b486b" ]),
	darker = d3.scale.ordinal()
               .range(["#b06d66","#ff9c10","#e0844c","#a8bbd5", "#9a99b6", 
                       "#8b7898","#7b587b" ]);

     var spacing = w / (data.nDraws + 1), //for sampled circles
       stepSize = radius/10,
       mixDuration = 700,
       slideDuration = mixDuration/2
         ;
       //console.log(radius);

       var  xspace = function(i){
           return i * spacing +30 ; 
       }

       //remove the old graph
      var vis = d3.select(el).select("svg");
        vis.remove();
        $(el).html(""); 
          
   //append a new one 
      vis = d3.select(el).append("svg")
        .attr("width",  w + 60) 
        .attr("height", h + 80)
        .append("svg:g")
           .attr("transform", "translate(" +  (w/2 + 20) + "," + 
              (h/2 + 5 ) +")");
    
      // data for a container
     var boxData = [ { "x": w/2 -40,   "y": h/2 +5},  { "x": -w/2,  "y": h/2+5},
                  { "x": -w/2,  "y": -h/2-5}, { "x":w/2 +10 ,  "y": -h/2-5},
                  { "x": w/2 +10,  "y": h/2 - 20}];
      //using this method
     var lineFunction = d3.svg.line()
         .x(function(d) { return d.x; })
         .y(function(d) { return d.y; })
         .interpolate("linear");
       // now draw the container
     var box = vis.append("path")
         .attr("d", lineFunction(boxData))
         .attr("stroke", "blue")
         .attr("stroke-width", 2)
         .attr("fill", "white");   
 
       var balls =  [];
      for (var i=0; i < data.nBalls; i++)  { 
        balls[i]  = {x: data.x[i],
		     y: data.y[i],
		     group: data.drawColor[i],
                     r: radius - .75 };
      }
      // ^^  these get displayed at the start
      // 

      var sample = [];
      for (var i=0; i < data.nDraws; i++)  { 
           sample[i]  ={
              x: data.x[data.draws[i]],
	      y: data.y[data.draws[i]],
	      group: data.drawColor[data.draws[i]],
	      r: radius - .75};
       }
      // these are sampled copies of balls from above
      
    var circles = vis.selectAll("g.circle")
         .data(balls)
       .enter().append("circle")
         .attr("fill", function(d, i){ return color(d.group); } )
         .attr("cx", function(d){ return d.x;} ) // 
         .attr("cy", function(d){ return d.y;} )  // 
         .attr("r",  function(d){ return d.r;} )
         .attr("text",function(d){return labels[d.group];})  // 
         .attr("class", "circle") ; 
	
     var draws = vis.selectAll("g.circle")
         .data(sample)
       .enter().append("circle")
         .attr("fill", function(d, i){ return color(d.group); } )
         .attr("cx", function(d){ return d.x;} ) // 
         .attr("cy", function(d){ return d.y;} )  // 
         .attr("r",  0 )
         .attr("text",function(d){return labels[d.group];})  // 
         .attr("class", "circle") ; 
      // these are copies of the first of the circles above
      console.log( data.drawColor[data.draws[0]]);
      console.log(draws);

    var textLabels = vis.selectAll("text")
         .data(sample)
       .enter().append("text")
           .attr("x", function(d,i){ return -w/2 + xspace(i)  ;} ) // 
         .attr("y", h/2 +34 )
         .text( function(d){return labels[d.group];})  // 
         .style("text-anchor", "middle")
         .attr("font-family", "sans-serif")
         .attr("opacity",0)
         .attr("font-size", "20px");
  
     // console.log(textLabels);

    // Transitions and timers
       // First Spin around nDraw times.

     var turn = function(i){  // rotate the whole batch
	circles.transition()
          .delay(mixDuration *  (i+1))
	  .duration(mixDuration)
	  .ease("cubic-out")   
	  .attrTween("transform", function (){
            return d3.interpolateString("rotate( 0, 0, 0)", 
                                   "rotate(720, 0, 0)");
          })
          ;
	 
    }

    for( var i = 0; i < data.nDraws; i++){ 
        turn(i);
    }

    // second, show the drawn ball coming out of the square
    // operates on the draws, not the original balls
    draws.each(function(d,i){
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
          if(data.replace == "no")  {
              balls.length = balls.length -1;
              //circles.exit().remove();
	  }
    });


  // Third,  remove the ball from the center if replace = "no"

  function isDrawn(d, i) {
    return (i < data.nDraws);  // sampled values are first
  }

  if(data.replace == "no"){
    circles.filter(isDrawn).each(function(d,i){
      d3.select(this).transition()
        .delay(mixDuration *  (i+1.4))
        .attr("opacity", 0)
        .remove;
    } );
  }
		
    // and add the labels
 

   textLabels.each(function(d,i){
	// move the selected ball out
        d3.select(this)
          .transition()
          .delay( mixDuration * (i+ 2.4) )
          .attr("opacity", 1)
       ;   });

  
    }
    });

  Shiny.outputBindings.register(networkOutputBinding, 'jimrc.networkbinding');


</script>

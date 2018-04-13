 //TODO:  remove one sample when getting a new one.
 //			change 'Many resamples' to match that of 2cat test/estimate
 //			remove red CI labels at top when Conf level changes 
 
  var   BootCount = 200,   // number of points plotted
  CI = [],
  CIline = [],
  CItext = [],
  confidence = 0.90,
  cumsum = 0,
  k,
  lowerCount = 0,
  lowerBd = 0,
  minx=50, 
  margin = {top: 20, right: 20, bottom: 80, left: 30},
  maxx = 0,
  meansArray = [],
  meanDots = [],
  meanSpots = [],
  oneSample = [],
  overlap = 2,
  ndx,
  popCost = [],
  popSize = 100,
  Rbox = [],
  reData = [],
  resamp = [],
  resampMean = [],
  resampMax = 8,
  resample = [],
  resampTime = 300,
  radius = 4,
  sampSize =  8,
  sampTime = 300,  
  sampleText = [],
  sFontSize = 30,
  //showCI = [],
  //showStats = [],
  spots=[],
  spotTime=200,
  summaryStats = [],
  tailProb = .025,
  upperCount = 0,
  upperBd = 32
  ypos=0;

  var speed = function(multiplier){
             resampTime =  resampTime * multiplier;
	   };
 

var width = 640 - margin.left - margin.right,   //20 to 590
    height = 440 + margin.top - margin.bottom;
var  yht = height * .7 - margin.top;          // 250

     //need range of x's to determine x plot axis
 for( ndx = 0; ndx< popSize; ndx++){
    popCost[ndx] = popData[ndx].cost;
    if (popCost[ndx] < minx) minx = popCost[ndx];
    if (popCost[ndx] > maxx) maxx = popCost[ndx];
 }
 
 // population mean cost is 35.54, sd = 19.88
  
// set up storage for resampled Means with (x,y) coords 
    meansArray[0] = { "x": 20, "y":0};
for(ndx = 1; ndx < 100 ; ndx++){
    meansArray[ndx] = { "x": 35, "y":0};
 }
 
 //document.write(document.getElementByID(Page));
 
//if( document.getElementsByClassName("Page")[0] == "BootDemo"){
var Bootsvg = d3.select("#BootDemo").append("svg")             // 640w x 440h
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var bootX = d3.scaleLinear()
    .range([30, width -20])
    .domain([0, 80]);

  var bootY = d3.scaleLinear()
    .range([height * .7, margin.top])
    .domain([0, 12.5]);

  var bootxAxis = d3.axisBottom(bootX);

  var bootyAxis = d3.axisRight(bootY);

   Bootsvg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0,272)")
      .call(bootxAxis);
       
   CI = [ {"x": 11, "y": height/4, "txt": "10" },
              {"x": 31, "y": height/4, "txt": "32" }];

   CIline = Bootsvg.selectAll("g.line")
    .data(CI)
    .enter().append("line")
    .attr("x1", bootY(12))
    .attr("x2", width)
    .attr("y1", bootY(0))
    .attr("y2", 28)
    .style("stroke-width",2)
    .style("stroke","red")
    .style("display","none");

function rescale() {
    bootY.domain([0, resampMax + .5]);  // change scale to 0, max(y)
    Bootsvg.select("bootyAxis")
         .transition().duration(300).ease(d3.easeSinInOut) 
           .call(bootyAxis);  
 }

  // display the population of costs
  var popText = Bootsvg.selectAll("g.text")
          .data(popData)
        .enter().append("text")
         .attr("y", function(d){ return d.y*10 + 12;})  
         .attr("x", function(d,i){ return i*5+20 ;} ) 
         .text( function(d){return d.cost;});

   // add pop title
 var Ptext =  Bootsvg.append("text")
     .attr("x", width/5 )
     .attr("y",0.03)
     .attr("font-size", 24 + 'px')
     .attr("fill", "blue")
     .text("Population of Book Costs");

   // create sample button
  var Stext = Bootsvg.append("text")
     .attr("x",30  )
     .attr("y",100)
     .attr("font-size", 20 + 'px')
     .text("Sample");

 for( ndx = 0; ndx < 16; ndx++){
    oneSample[ndx] = {"x": 30 + ndx * 50, 
                       "y": 10, "txt": ndx};
 }

   // frame the button and make it clickable  
  var SampleBox= Bootsvg.append("rect")
     .attr("x", 12)
     .attr("y", 80)
     .attr("width", 102)
     .attr("height",30)
     .attr("rx", 4)
     .attr("ry", 4)
     .style("stroke", "blue" )
     .style("stroke-width",2 )
     .style("fill","lightblue")
     .style("fill-opacity", 0.1)
    .on("hover", function() {this.transition().style("fill","green");} )
      .on("click", Sample);

    // when SAMPLE is clicked do this
  function Sample() {
	// sample from the popData
	popCost = d3.shuffle(popCost);
	oneSample.length =sampSize;

	for( ndx = 0; ndx < 16; ndx++){
	    oneSample[ndx] = {"x": 30 + ndx * 50, 
			      "y": 10, "txt": ndx};
	}

	for( ndx = 0;  ndx < sampSize -0.1; ndx++ ){
            oneSample[ndx].txt =  popCost[ndx] ;
	}
	if(sampSize < 16){
            for( k = 15; k >= sampSize; k-- ){
		oneSample[k].txt =  null ;
	    }
	}
	//console.log(oneSample);
	SampleBox.transition()
            .style("stroke","white")
            .style("fill","white")
            .remove();          // remove the sample button frame
	Stext.transition()      // move the sample text to the bottom
	    .attr("y", height * 1.16 )
	    .attr("duration", 100);

	sFontSize = (sampSize < 15) ? "30px": "20px";
	xStepSize = (sampSize > 8) ? 30: (sampSize > 4)? 50: 100;

       sampleText = Bootsvg.selectAll("text2")  // create the sample text objects
       	.remove()
           .data(oneSample)
         .enter().append("text")
           .attr("y", function(d){ return d.y ;})  
           .attr("x", function(d){ return d.x ;} ) 
           .text( function(d) { return d.txt ;})
           .style("font-size","2px")
	    .style("fill","black");
        sampleText.transition()
			// move sample to bottom
           .delay(function(d, i) { return (i+1) * sampTime; })
          .duration(sampTime)
           .attr("y", height * 1.15)  
           .attr("x", function(d,i){ return 120 + i * xStepSize ;} ) 
           .style("font-size", sFontSize);   

         popText.transition().delay(sampTime * sampSize).remove(function(d) { d.remove;}); 
         Ptext.transition().delay(sampTime * sampSize).remove(function(d) { d.remove;}); 


         Bootsvg.append("text")      // create 1 Resample button
          .attr("x",  4  )
          .attr("y",height * 1.25)
          .attr("font-size", 18 + "px")
          .style("fill-opacity", 1.0E-6)
          .text("1 Resample")
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
           .style("fill-opacity", 1);

      Rbox = Bootsvg.append("rect")    // and it's frame and activation
           .attr("class", "rect")
           .attr("x", 0)
           .attr("y", height *1.25 - 20)
           .attr("width", 112)
           .attr("height",30)
           .attr("rx", 4)
           .attr("ry", 4)
           .style("stroke", "blue" )
           .style("stroke-width",2 )
           .style("fill","lightblue")
           .style("fill-opacity", 1.0E-6)
           .on("click", reSample1);
//          .transition(this)
//           .delay( sampTime * sampSize)
//           .duration(sampTime)
//           .style("fill-opacity", 0.2)
//           .style("stroke", "blue" )
//           .style("stroke-width",2 );

         Bootsvg.append("text")      // create speed buttons
          .attr("x",  126  )
          .attr("y",height * 1.25 - 12)
          .attr("font-size", "9px")
          .style("fill-opacity", 1.0E-6)
          .text("Slower")
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
           .style("fill-opacity", 1);

         Bootsvg.append("text")   // faster label
          .attr("x",  127  )
          .attr("y",height *1.25 +8)
          .attr("font-size", "9px")
          .style("fill-opacity", 1.0E-6)
          .text("Faster")
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
           .style("fill-opacity", 1);

      SlowBox = Bootsvg.append("rect")    // slower button
           .attr("class", "rect")
           .attr("x", 122)
           .attr("y", height * 1.25 - 22)
           .attr("width", 40)
           .attr("height",14)
           .attr("rx", 4)
           .attr("ry", 4)
           .style("stroke", "white" )
           .style("stroke-width",1 )
           .style("fill","lightblue")
           .style("fill-opacity", 1.0E-6)
           .on("click", function(){ resampTime = resampTime * 1.5;} )
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
           .style("fill-opacity", 0.1)
           .style("stroke", "blue" )
           .style("stroke-width", 1);

      FastBox = Bootsvg.append("rect")    // faster button
           .attr("class", "rect")
           .attr("x", 122)
           .attr("y", height * 1.25 - 2 )
           .attr("width", 40)
           .attr("height",14)
           .attr("rx", 4)
           .attr("ry", 4)
           .style("stroke", "white" )
           .style("stroke-width",1 )
           .style("fill","lightblue")
           .style("fill-opacity", 1.0E-6)
           .on("click", function(){ resampTime = resampTime * .67;} )
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
           .style("fill-opacity", 0.1)
           .style("stroke", "blue" )
           .style("stroke-width",1 );

       Bootsvg.append("text")      // Many Resamples
            .attr("x", 0  )
          .attr("y",  height * 1.38)
          .attr("font-size", "18px")
          .text("Many Resamples: ")
          .style("fill-opacity", 1.0E-6)
          .transition(this)
           .delay( sampSize * sampTime  )
           .duration(sampTime)
          .style("fill-opacity", 1);
 
   // TODO:  should be able to build all resample buttons at once, 
   //         but not sure how to do it with text and rectangle.
   //		 Here's a data build as a start.   Does display, but only the last box is active.
          
    var resampButtonData = [
    	{x: 200,  wd: 40, txt: "100", n: 100, clr: "red"},
    	{x: 250,  wd: 40, txt: "500", n: 500, clr: "orange"},
    	{x: 300,  wd: 48, txt: "1000", n: 1000, clr: "green"},
    	{x: 360,  wd: 48, txt: "5000", n: 5000, clr: "blue"},
    	{x: 430,  wd: 54, txt: "10,000", n: 10000, clr: "purple"}
    ]
    	

    var manyTexts =   Bootsvg.selectAll("text4")
          .data(resampButtonData)
          .enter()
          .append("text")
          .attr("x",  function(d){ return d.x;}  )
          .attr("y",  height * 1.38)
          .attr("font-size", "16px")
          .text(function(d){return d.txt;})
          .style("fill-opacity", 1.0E-6)
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
           .style("fill-opacity", 1)
          ;
     var manyBoxes = Bootsvg.selectAll("rect")   
          .data(resampButtonData)
          .enter()
          .append("rect")
          .attr("x",  function(d){ return d.x -5 ;}  )
           .attr("y", height * 1.38 -20)
           .attr("width",  function(d){ return d.wd;} )
           .attr("height",30)
           .style("fill",function(d){return(d.clr);})
           .style("fill-opacity", 1.0E-6)
           .on("click", function(d){ 
  				reSampleMany(d.n);})
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
           .style("fill-opacity", 0.2)
            ;

    //   Bootsvg.append("text")      // create Resample 100 button
    //      .attr("x",  200  )
    //      .attr("y", height * 1.44 )
    //      .attr("font-size", "16px")
    //      .text(" 100 ")
    //      .style("fill-opacity", 1.0E-6)
    //      .transition(this)
    //       .delay( sampTime * sampSize)
    //       .duration(sampTime)
    //       .style("fill-opacity", 1);

    // var overlayC = Bootsvg.append("rect")    // 100 frame and activation
    //       .attr("class", "rect")
    //        .attr("x", 195)
    //       .attr("y", height * 1.38 -20)
    //       .attr("width", 40)
    //       .attr("height",30)
    //       .style("fill","red")
    //       .style("fill-opacity", 1.0E-6)
    //        .on("click", function(){ 
	//				BootCount = 100;	
	//				reSampleMany(BootCount);})
    //      .transition(this)
    //       .delay( sampTime * sampSize)
    //       .duration(sampTime)
    //       .style("fill-opacity", 0.2);

       Bootsvg.append("text")      // create Resample 500 button
            .attr("x",  250  )
          .attr("y", height * 1.38)
          .attr("font-size", "16px")
          .text(" 500 ")
          .style("fill-opacity", 1.0E-6)
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
           .style("fill-opacity", 1);

     var overlayD = Bootsvg.append("rect")    // 500 frame and activation
           .attr("class", "rect")
            .attr("x", 245)
           .attr("y", height * 1.38 -20)
           .attr("width", 40)
           .attr("height",30)
           .style("fill","orange")
           .style("fill-opacity", 1.0E-6)
           .on("click", function(){ 
		       BootCount = 500;
    	       reSampleMany(BootCount);})
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
          .style("fill-opacity", 0.4);

       Bootsvg.append("text")      // create Resample 1000 button
            .attr("x", 300 )
          .attr("y",  height * 1.38)
          .attr("font-size", "16px" )
          .text(" 1000 ")
          .style("fill-opacity", 1.0E-6)
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
          .style("fill-opacity", 1);
 
    var overlayM =  Bootsvg.append("rect")    // 1000 frame and activation
           .attr("class", "rect")
           .attr("x", 295 )
           .attr("y", height * 1.38 -20)
           .attr("width", 48)
           .attr("height",30)
           .style("fill","green")
           .style("fill-opacity", 1.0E-6)
           .on("click", function(){ 
	       BootCount = 1000;
               reSampleMany(BootCount);})
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
          .style("fill-opacity", 0.15);

      Bootsvg.append("text")      // create Resample 5000 button
            .attr("x", 360  )
          .attr("y",  height * 1.38)
          .attr("font-size", "14px")
          .text(" 5000 ")
          .style("fill-opacity", 1.0E-6)
          .transition(this)
           .delay( sampTime * sampSize)
           .duration(sampTime)
          .style("fill-opacity", 1);

 //    var overlayDX = Bootsvg.append("rect")    // 5000 frame and activation
 //          .attr("class", "rect")
 //           .attr("x", 355)
 //          .attr("y", height * 1.38 -20)
 //          .attr("width", 48)
//           .attr("height",30)
//           .style("fill","blue")
//           .style("fill-opacity", 1.0E-6)
 //          .on("click", function(){ 
//	       		BootCount = 5000;
//	       		reSampleMany(BootCount);})
//          .transition(this)
//           .delay( sampTime * sampSize)
//           .duration(sampTime)
//          .style("fill-opacity", 0.15);

//      Bootsvg.append("text")      // create Resample 10000 button
//            .attr("x", 430 )
//          .attr("y",  height * 1.38)
//          .attr("font-size", "14px")
//          .text(" 10,000 ")
//          .style("fill-opacity", 1.0E-6)
//          .transition(this)
//           .delay( sampTime * sampSize)
//           .duration(sampTime)
//          .style("fill-opacity", 1);

//     var overlayMX = Bootsvg.append("rect")    // 10000 frame and activation
//           .attr("class", "rect")
//            .attr("x", 425)
//           .attr("y", height * 1.38 -20)
//           .attr("width", 54)
//           .attr("height",30)
//           .style("fill","purple")
//           .style("fill-opacity", 1.0E-6)
//           .on("click", function(){ 
//	       BootCount = 10000;
//               reSampleMany(BootCount);})
//          .transition(this)
//           .style("fill-opacity", 0.1)
//           .delay( sampTime * sampSize)
//           .duration(sampTime)
//          .style("fill-opacity", 0.2);
    }

   // Remove old spots rather than transition and redraw CI
      
     var reSample1 = function( ){
       // when 1 resample is clicked, show the resample process
	 if(reData.length > 0){
            reData.length = 0;
	 }
	 if(typeof(showStats) != "undefined"){
	 	showStats.style("display","none");
	 	showCI.style("display","none");
	 }
	 var resampMn = 0;
         for( i =0; i < sampSize; i++){
             // select the datapoint in the sample
	     resample[i] =  Math.floor(Math.random() * sampSize);
             // stick it into the reData array
             reData[i] = oneSample[resample[i]];
             resampMn = resampMn + reData[i].txt / sampSize;
             // move the frame to show sampling
             Rbox.transition()
             // move frame over to selected value
		 .duration(resampTime)
		 .delay( 2* resampTime * i)
		 .attr("x",  xStepSize * resample[i] + 120)
		 .attr("y", height * 1.15 - 27)
		 .attr("height",36)
		 .attr("width", 50)
		 .transition()  
             // slide it back quickly
		 .attr("x",  0)
		 .attr("y", height * 1.25 - 20)
		 .attr("height",30)
		 .attr("width", 112);
         }
         // extract the selected number from sample and pull into resample
          resamp = Bootsvg.selectAll("text3")
             .data(reData)
           .enter().append("text")
             .attr("y", height * 1.3)  
             .attr("x", function(d,i){ return 120 + resample[i] * xStepSize ;}) 
             .text( function(d) { return d.txt ;})
             .style("fill","green")
             .style("font-size","2px");
           resamp.transition()
             .delay(function(d, i) { return (i + 0.5) * 2 * resampTime ; })
             .duration(0.75 * resampTime)
             .ease(d3.easeLinear)
             .attr("y", height * 1.26)  
             .attr("x", function(d,i){ return 180 + i * xStepSize * .9 ;} ) 
             .style("font-size", sFontSize);
	      // let that sit a bit, then take its mean and move it
           resamp.transition()
	     .delay(resampTime *2.2* sampSize)
	     .duration(resampTime)
	     .style("font-size", "5px")
	     .attr("y", bootY(1) )
             .attr("x", bootX(resampMn) );
           resamp.transition() 
	     .delay(resampTime *3.3* sampSize)
	     .style("fill","white").remove();
	    // .attr("font-size","0px").remove()  ; 
             
	 //use transparent circles to show numbers of resamples		     
         spots = Bootsvg.selectAll("g.circle")
             .data(reData)
           .enter().append("circle")
             .attr("cx", function(d,i){ return 145 + resample[i] * xStepSize +
                         (i-sampSize) * 1 ;} ) 
             .attr("cy", height * 1.15 - 10)  
             .attr("r", 0)
             .style("fill","white")
             .style("fill-opacity", 0.15)
           .transition(this)
             .delay(function(d, i) { return (i + .5) * 2 * resampTime ; })
             .attr("r", 20)
             .style("fill","green")
             .style("fill-opacity", 0.15)
	      // let that sit a bit, then  remove it
           .transition()
	     .delay(resampTime * (sampSize -i -.5)*4* (sampSize+2))
	     .attr("r",0).remove()  ; 

     };
  
    var reSampleMany = function(BootCount){
      // shrink down existing means, then call for new resamples
	 if(typeof(showStats) != "undefined"){
	 	showStats.style("display","none");
	 	showCI.style("display","none");
	 }
 	if(meanDots.length >0){
          meanDots.transition()
            .style("fill","white")
            .remove(function(d) { d.remove;});
          //remove CIs and text
          CItext.transition().style("fill","white").remove(function(d) { d.remove;});
          CIline.transition().style("display","block"); 
	}
        resampleMeans(BootCount);
	drawCI(BootCount);
    };

    
var resampleMeans = function(BootCount){
      // sample n from sample with replacement, average them and 
      //  plop into the array. Repeat count times.  update points.
       var j, 
       	i =0;   // set up storage for resampled Means with (x,y) coords 
        // radius: shrinks as # of samples increases
	radius = (BootCount < 101)? 6:
	    (BootCount < 501)? 5:
	    (BootCount < 1001)? 4:
	    (BootCount < 5001)? 3: 2;
	oneSample.length = sampSize;
	resample.length  = sampSize;
	resampMean.length = BootCount;  
	meansArray.length = BootCount; 
    for(  j = 0; j <  BootCount; j++){         
        // ###  Do the Resampling here ##################	 
        for( i =0; i < sampSize; i++){
          resample[i] =  oneSample[Math.floor(Math.random() * sampSize)].txt + 0.0;
		};  // close i loop
        resampMean[j] = d3.mean(resample); // compute mean
      };   // done resampling##################################

	resampMean.sort(function(a,b){return a - b}) ;   
          // numeric sort to build bins for y values
        //console.log(resampMean[99] > resampMean[50]);
         // start on left with smallest x.
	//  first dot goes at 0, then add one to each from there
	var j = 0;
	while( j <  BootCount ){    
	    // start a fresh bin
	    plotX = resampMean[j];
	    ypos = 0;	
	    meansArray[j] = {"x": bootX(resampMean[j++] + 0.0), "y": ypos++};
            while( (resampMean[j] - plotX < radius/6) & (j < BootCount)){
		//stay in same bin -- increment yposition
		meansArray[j] = {"x": bootX(resampMean[j++] + 0.0), "y": ypos++};
	    };
	}
	//meansArray[BootCount-1]={"x": bootX(resampMean[BootCount]), "y": 0};
	resampMax = d3.max(meansArray, function(d) { return d.y;});
	// change scales to hold all x, all y
        //x.domain([resampMean[0], resampMean[BootCount-1] ]);
	bootY.domain([0, resampMax + .5]);  

	meanDots =  Bootsvg.selectAll("g.circle")
            .data(meansArray);
	//meanDots.length = BootCount; 
	meanDots.enter().append("circle")
            .attr("cx", function(d){ return d.x;} ) 
            .attr("r", radius ) 
            .attr("cy", function(d){ return bootY(d.y);} ) 
            .style("fill","steelblue")
            .style("fill-opacity", 0.6);
    };

    var drawCI = function(BootCount){  // draw percentile conf interval
	 if(typeof(showCI) != "undefined"){
	 	showStats.style("display","none");
	 	showCI.style("display","none");
	 }
      if(BootCount > 90){	
	if(CItext.length >0){
          //remove CI text
          CItext.transition().style("fill","white").remove(function(d) { d.remove;});
	}
	if(BootCount < 200){
	    if(confidence > .98){
	    confidence = .98;
	    }  // with only 100 resample, we can't split .01 into 2 tails
	} 
	if((confidence > .95) && (BootCount > 100)){
	    confidence = .99;
	}
	tailProb = (1 - confidence)/2;
	upperCount = BootCount * (1 - tailProb);
	lowerCount = BootCount * tailProb;
        lowerBd = resampMean[Math.floor(lowerCount)];
        upperBd = resampMean[Math.floor(upperCount) ];
	    upperCount--;
	while( upperBd < lowerBd){
	    upperBd = resampMean[Math.floor(upperCount--) ];
	}
       CI = [ {"x": lowerBd, "y": height/4, "txt": lowerBd},
              {"x": upperBd, "y": height/4, "txt": upperBd}];

       CIline.data(CI)
          //.transition()
            //.delay(resampTime * BootCount/1000)
            .attr("x1", function(d) {return bootX(d.x + 0.0);})
            .attr("x2", function(d) {return bootX(d.x + 0.0);})
            .attr("y1", bootY(0))
            .attr("y2", margin.top + 20)
            .style("stroke-width",2)
            .style("stroke","red")
            .style("display","block");

       CItext = Bootsvg.selectAll("g.text")
            .data(CI)
         .enter().append("text")
           //.transition().delay(resampTime * BootCount/1000)
           .attr("x",  function(d) {return bootX(d.x -0.5);})
           .attr("y",  25 )
           .text(function(d) {return d.txt;} )
           .style("font-size","12px")
           .style("fill","red");

    showCI = Bootsvg.append("text")
     .attr("x",bootX(20))
     .attr("y", height *1.48)
     .text( (confidence*100) + "% CI is (" + CI[0].x + ", " + CI[1].x +") based on " + BootCount + " resamples");
   
    showStats = Bootsvg.append("text")
     .attr("x",bootX(28))
     .attr("y", height * 1.28) 
     .text( "Mean: " + d3.mean( resampMean).toPrecision(4) +
                   ",  SD: " + d3.deviation(resampMean).toPrecision(4));
	}

 }

  // when newSAMPLE is clicked. Clear all and start over
 var newSample = function() {
    // show population again
     //sampSize = 
     //confidence = 
    popText = Bootsvg.selectAll("g.text")
      .data(popData)
      .enter().append("text")
      .attr("y", function(d){ return d.y*10 + 12;})  
      .attr("x", function(d,i){ return i*5+20 ;} ) 
      .text( function(d){return d.cost;});
             // problem: pop text returns, but Sample can't remove it.
 	if(meanDots.length >0){
          meanDots.transition()
           // .attr("cx", function(d) { return x(d[0]); })
           // .attr("cy", function(d) { return y(0); })
            .style("fill","white")
            .remove(function(d) { d.remove;});
	    CItext.transition().style("fill","white").remove(function(d) { d.remove;});
	    CIline.style("display","none"); 
	}
            //remove old sample
     if(sampleText.length >0){
	  sampleText.transition().style("fill","white");
	  sampleText.transition().style("display","none");
	  sampleText.transition().remove(function(d) { d.remove;});
     }

       // remove old output
     if(typeof(showStats) !== "undefined"){  
     	//showCI.transition().style("stroke","white");
     	//showStats.transition().style("stroke","white");
     	showCI.transition().style("display","none");
     	showStats.transition().style("display","none");
     }
      Sample();
    }

//}
# IntroStatApps #

Web Apps built in R/Shiny to provide permutation tests and bootstrap confidence intervals for use in Introductory Statistics classes.


We are now teaching introductory statistics at Montana State
University using active learning lessons which get students right
into permutation tests and bootstrap confidence intervals very early
in the course.  (Materials are available on our companion site, [https://github.com/MTstateIntroStats/IntroStatActivities](https://github.com/MTstateIntroStats/IntroStatActivities) )  

This repo is intended to build and share R/Shiny web apps which automate
permutation testing and bootstrapping.  

Initially, students need to understand the difference between categorical and quantitative data, as the menu system divides summaries and analysis into these type:  
  - One Categorical  
  - One Quantitative  
  - Two Categorical  
  - Two Categorical  
  - One of Each  (currently expects a quantitative variable and a two level categorical variable.)

In the last 1/3  of our course, normal and t distribution methods are
be used as short cut approximations of the simulation based
inference, so the lookup tables are part of this suite.  Standard normal lookups are under both the One and the Two Categorical menus while t distributions are available under 1  Quantitative or "One of Each".

To use the app choose the type of data and enter or import it.

 * Data input  
  -- for 1 and 2 categorical variables, user will type counts into a form.  
      Click [Use These Data] to proceed.  
  -- Other variable types get a choice:  
     select a "Pre-loaded" data frame,   Upload a local csv file,  or
     Type or paste into a text box

 * Descriptive stats and plots are created to suit the type of data.

 * After data is imported, user chooses  
  -- Test  or  
  -- Estimate  
 
 * Permutation Tests:  
   one proportion  
   equality of two proportions  
   one mean  
   equality of two means  
   correlation and slope  
   
 * Bootstrap CI's for:  
   one proportion  
   difference in two proportions  
   one mean  
   difference in two means  
   correlation and slope  

 *  Demos:  
  -- "What does 'confidence' mean?" under 1 Categorical allows user to generate many CI's under the artificial assumption of known "p", and check the coverage rate.  
  -- "Lurking Demos"" under '1 Categ' and '1 Quant' show how randomly assigning treatments evens out the effects of lurking variables (in the long run).  
  -- Bootstrap Demo under [One Quant] is an animation of the resamping process.

### How to run these web apps?  

You are welcome to fork these pages and/or clone them to your own computer.  
You can also run them without cloning in this way:  

 * Install R and RStudio  
 * install.packages("shiny")  
 * shiny::runGitHub("MTstateIntroStats/SpIntro-Stats")  
 
 Alternately, you could explore them on our departmental server:  [http://shiny.math.montana.edu/jimrc/IntroStatShinyApps/](http://shiny.math.montana.edu/jimrc/IntroStatShinyApps/)
 
### Known issues:  

  Need to reduce margins around some of the plots.  

  It would be nice to use rhandsontable for data entry. Trying it on the handsontable website I am able to paste in multiple cells, but that doesn't work for me in the r packaged version.
  
  We'd like to add a tab which takes simple linear regression data and allows the user to find a line which fits well, then compare to the least squares line.  

### Comments? Suggestions?  

 Please make comments in our google forum: [spintro-stats](https://groups.google.com/forum/#!forum/spintro-stats)


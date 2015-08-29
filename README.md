# IntroStatApps #

Web Apps built in R/Shiny to provide permutation tests and bootstrap confidence intervals for use in Introductory Statistics classes.


We are now teaching introductory statistics at Montana State
University using active learning lessons which get students right
into permutation tests and bootstrap confidence intervals very early
in the course.  (Materials are available on our companion site, [https://github.com/MTstateIntroStats/IntroStatActivities](https://github.com/MTstateIntroStats/IntroStatActivities) )  

This repo is intended to build and share R/Shiny web apps to automate
permutation testing and bootstrapping.  

Initially, students need to understand the difference between categorical and quantitative data, as the menu system divides summaries and analysis into these type:  
  - One categorical  
  - One Quantitative  
  - Two Categorical  
  - Two Categorical  
  - One of Each  (currently expects a quantitative variable and a two level categorical variable.)

In the last 1/3  of our course, normal and t distribution methods are
be used as short cut approximations of the simulation based
inference, so the lookup tables are part of this suite.  

To use the app choose the type of data and enter or import it.

 * Data input  
  -- for 1 and 2 categorical variables, user will type counts into a form.  
      Click [Use These Data] to proceed.  

  -- Other varaible types get a choice:  
     select existing data frame,   
     Upload a local csv file,  
     type or copy into a table using rhandsontable  
       --- need to allow user to set the number of rows.

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
  -- What does 'confidence' mean under 1 Categorical allows user to generate many CI's under the artifical assumption of known "p", and check the coverage rate.  
  -- Lurking Demos under '1 Categ' and '1 Quant' show how randomly assigning treatments evens out the effects of lurking variables (in the long run).  

  -- Bootstrap Demo under '1 Quant'  takes a known population, shows an animation of the sampling process, then an animation of resampling from the sample to get a bootstrap distribution.  Builds CI using percentile method and reposrts bootstrap SD.  

### How to run these apps:

1)  Use a recent version of R (3.1 or better) and install packages:  
    shiny,shinythemes, rhandsontable (from github), gridExtra, ggplot2, and their dependencies.  Then in R type:

    shiny::runGitHub("MTstateIntroStats/IntroStatApps")  

and wait a bit for it to unpack the functions. To close, use cntrl-C.

2)  You may test our installation at [shiny.math.montana.edu/jimrc/IntroStatShinyApps'](our server), but if you are going to use them regularly, please:

3) Set up your own shiny server, clone the repository to it, and run it there.


### Known Issues  

Currently you can click on a resampled mean in the CI Demo plot and the interval associated with that point will show in the CI plot below. 
We plan to implement similar "on click" interaction in the other plots, for example, when testing $\mu_1 == \mu_2$, user should be able to click on a difference in sample means and see what shuffled dataset produced that mean.  

"One of Each" allows you to get descriptive plots for each level of a multi-level categorical predictor, but testing and estimation assume one is using only  2-levels.    



    
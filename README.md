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
 
 * Demo to show coverage of CI's for the artificial case of a known proportion or mean.  

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

### Known issues:  
  Under 1 Categ and 1 Quant, user can click on one of the points in the samplin/resampling distribution and see the data that generated that statistic.  Need to make that work for 2 Categ, 2 Quant, and one-of-each.  

  Need to figure out how packrat works with github.  

  Need to reduce margins around some of the plots.  

  Need to improve the use of rhandsontable for data entry.  Currently the number of rows is locked. Could just ask user how many rows they need.



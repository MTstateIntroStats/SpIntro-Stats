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
     select existing data frame,   
     Upload a local csv file, 
     Type or paste into a text box (Currently only for 1 Quant.)  
     Type or copy into a table using rhandsontable  
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
  -- "What does 'confidence' mean?" under 1 Categorical allows user to generate many CI's under the artificial assumption of known "p", and check the coverage rate.  
  -- "Lurking Demos"" under '1 Categ' and '1 Quant' show how randomly assigning treatments evens out the effects of lurking variables (in the long run).  

### Known issues:  

  Need to reduce margins around some of the plots.  

  Need to improve the use of rhandsontable for data entry.  Currently the number of rows is locked. 

## Comments?  

 Please click on our Wiki page to comment, suggest improvements, or otherwise contribute.  
 


# IntroStatShinyApps
Web Apps built in R/Shiny to provide permutation tests and bootstrap confidence intervals for use in Intro Stat class.

We are now teaching introductory statistics at Montana State
University using active learning lessons which get students right
into permutation tests and bootstrap confidence intervals very early
in the course.  

This repo is intended to build and share R/Shiny web apps to automate
permutation testing and bootstrapping.  

We will include some descriptive statistics and plots.  

At the end of the course, normal and t distribution methods will
be used as short cut approximations of the simulation based
inference, so the lookup tables are part of this suite.  

 * Data input  
  -- for 1 and 2 categorical variables, type counts into a form.  
  -- Other types get a choice:  
     select existing data frame,   
     Upload a local csv file,  
     type or copy into a table using rhandsontable  

  Progress:  This is working for all types of variables, but we need to allow switching predictor and response for the 2 quantitative variable case.  
 
 * Descriptive stats and plots
     OK for all data types.
  
 * Bootstrap Demo is ready under 1-quantitative variable.  
   It uses only a single dataset, not user-setable data.  
  
 * TODO: Permutation Tests:  
   one proportion  
   equality of two proportions  
   one mean  
   equality of two means  
   correlation and slope  
   
 * Bootstrap CI's for the same settings.
 
 * Need a demo to show coverage of CI's for the artificial case of a known proportion or mean.  

 

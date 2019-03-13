updated at 2019-03-13  

## What is dicmo?
*dicmo* is an R package for estimating various discrete choice models. In addition, it provides some useful functions to manipulate data before estimating the model. The current version of *dicmo* is 0.1.0.

## What can dicmo do?
* data manipulation
  * convert a date from a wide format to a long format
  * generate the data with long format that is directly used in model estimation
* multinomial logit model estimation using maximum likelihood approach
  * help to convert the data from a wide format to a long format automatically
  * help to code the categorical attributes/variables automatically
  * allow the existence of observations that some alternatives are not available in the choice set
  * allow the existence of none-option in the choice set
  * allow the existence of case that some alternative-specific attributes are not available for certain alternatives
  * allow to estimate alternative-specific parameters for alternative-specific attributes, context varialbes ans ASCs
  * allow to estimate interaction effect between alternative-specific attributes
  * allow to estimate interaction effect of context variables with alternative-specific attributes and ASCs
  * allow to compute the AVC matrix using numeric hessian matrix or BHHH algorithm
  * report various goodness of fit, i.e. initial log-likelihood, convergent log-likelihood, AIC, BIC, Rho-squared, and adjusted Rho-squared

## How to install dicmo?
* first make sure the *devtools* package is installed and loaded:  
  install.packages("devtools")  
  library(devtools)  
* then install *dicmo* from GitHub and load it:  
  devtools::install_github("xf-pan/dicmo")  
  library(dicmo)  
  
* the package source and Windows binary can be found in: https://xf-pan.github.io/dicmo/  
  

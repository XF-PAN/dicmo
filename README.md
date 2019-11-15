updated at 2019-11-14 

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
  * allow to customize the initial values of the estimated parameters
  * allow to fix the values of certain estimated parameters 
  * report various goodness of fit, i.e. initial log-likelihood, convergent log-likelihood, AIC, BIC, Rho-squared, and adjusted Rho-squared
* 2-level nested logit model estimation using maximum likelihood approach
* ordered logit/probit model estimation using maximum likelihood approach
* exploded (2-level) nested logit model, including best-worst scaling case 3 

## How to install dicmo?
* first make sure the *devtools* package is installed and loaded:  
  install.packages("devtools")  
  library(devtools)  
* then install *dicmo* from GitHub and load it:  
  devtools::install_github("xf-pan/dicmo")  
  library(dicmo)  
  
* the package source and Windows binary can be found in: https://xf-pan.github.io/dicmo/  
  
## Attentions  
There are some rules have to be followed:  
* The input data should be a tibble with a wide format.
* The column name of the input data could consist of English letters, numbers and underline - others are not accepted - and start with English letters.
* The name of alternatives could consist of English letters, numbers and under line, and it also could only have numbers as long as the numbers are treated as characters.
* Names of columns that indicating alternative-specific attributes (including the alternative-available column) should consiste of the names of alternatives and attributes (or the "avi" argument) - they are combined by a colon, for instance, "attribute:alternative" or "alternative:attribute"
* The artument "attrs" could have no slice "context" and "asc", that means they are all set as 0 - same function to set them as NULL.
* The order of the slice in the argument "attrs" does not matters.
* In the argument "attrs", negative number is not allowed.
* The elemets in the argument "attr_coding" and "attr_level" do not necessary
have one-to-one correspondence in sequence.
* Highly recommend to always include the alternative-available column, i.e. the
"avi" argument.
* Highly recommend to first estimate the model without setting anything for the argument "interact", "param_start" and "param_fixed" to know the names of estimated parameters, then set these arguments if it is needed.
* For ordered choice model, the argument "attrs" has no "asc" element.
* For ordered choice model, the elements in the argument "rate" must be sorted from the worst to the best.

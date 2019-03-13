updated at 2019-03-13  

## Attentions  
There are some rules have to be followed:  
* The input data should be a tibble with a wide format.
* The column name of the input data could consist of English letters, numbers and underline - others are not accepted - and start with English letters. So does
the names of alternatives
* Names of columns that indicating alternative-specific attributes (including the alternative-available column) should consiste of the names of alternatives and attributes (or the "avi" argument) - they are combined by a colon, for instance, "attribute:alternative"" or "alternative:attribute"
* If no context varialbes are considered, the slice "context" in the argument
"attrs" should be NULL.
* In the argument "attrs", negative number is not allowed.
* The elemets in the argument "attr_coding" and "attr_level" do not necessary
have one-to-one correspondence in sequence.
* Highly recommend to always include the alternative-available column, i.e. the
"avi" argument.
* Highly recommend to first estimate the model without setting anything for the argument "interact", "param_ini" and "param_fixed" to know the names of estimated parameters. Then set these arguments if it is needed.

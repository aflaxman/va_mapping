# va_mapping
==========================================

**Description** 	

R-Converter for converting verbal autopsy records collected using the WHO VA 2014 instrument to be used as input for different coding algorithms. 

**Input**		
- CSV file containing submissions of the 2014 WHO VA questionnaire (exported from ODK Aggregate)
- One or more mapping files (text file, semi-colon as column separator). Minimal content: the first columns contains the names of all variables needed by the coding algorithm (called "target variables" here). The second column contains the mapping to each target variable, as a valid R expression. Expressions can be functions of zero or more variables of the WHO VA instrument, or any of the preceding target variable. In addition to standard R functions, a small set of convenience functions is provided in xda.R.

**Output**		
A CSV file intended for processing by a coding algorithm.


**Status**		

Initial, not intended for production use


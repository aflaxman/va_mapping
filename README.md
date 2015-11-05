# va_mapping
VA -> InterVA4/SmartVA file format mapping
==========================================

**Description** 	

R-Converter for converting WHO VA 2014 Tool submissions to InterVA4 (WHO_INTERVA4_MAPPING.R) and SmartVA (WHO_SMARTVA_MAPPING.R) file format, ready to be coded in InterVA4 and SmartVA

**Input**		
- mapping file 2014 WHO  -> InterVA4 variables OR
- mapping file 2014 WHO  -> SmartVA variables AND
- CSV file containing submissions of the 2014 WHO VA Questionnaire (exported from ODK Aggregate)

**Run**
If you cloned the repo into workspace under c:\dev:\workspace_R, run the converter with:

source("C:/dev/workspace_R/va_mapping/WHO_SMARTVA_MAPPING.R") for SmartVA mapping OR
source("C:/dev/workspace_R/va_mapping/WHO_INTERVA4_MAPPING.R") for InterVA4 mapping

**Output**		
A CSV file for direct usage in either InterVA4 or SmartVA. The InterVA4 converter will also directly display the Cause of Death by feeding the converted data into the InterVA4 package.

**Files**		
In the /data folder there is the current 2014 WHO VA Tool to InterVA4/SmartVA variables mapping file and also a test data file with several simulated submissions

**Dependencies**		
R-Packages:
- foreach
- InterVA4 (for InterVA4 convertion)


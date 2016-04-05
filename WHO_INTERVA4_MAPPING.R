########### WHO VA 2014 -> InterVA4 file format mapping ################
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tools submissions to InterVA4 file 
##			format ready to be coded in InterVA4.
## @input:		mapping file who 2014 -> interva4 variables, and
##			who 2014 submission csv (exported from odk aggregate)
## @output:		csv for usage in InterVA4
## @author: 	RMI
## ----------------------------------------------------------------------
#Clear variables
rm(list=ls(all=TRUE))
library(InterVA4)
library(foreach)

cat("\nWHO VA Instrument 2014 -> InterVA4 Conversion\n\n")

## Define your file path variables here###############################
workingDir = file.path(getwd(), "data")
mappingFileName = file.path(workingDir, "interva4_mapping.csv")
submissionFileName = file.path(workingDir, "output.csv")
outputFileName = file.path(workingDir, "output_for_interva4.csv")
######################################################################

#load who submission file:
records = read.csv(submissionFileName)

loadAndSetAllVariablesFromWHOInstrument<-function(entryLevel){
	entry = records[entryLevel,] #Get current entry
	for(j in 1:ncol(records)){
		header = names(records)[j]
		value =  as.character(entry[1,j])
		if(is.na(value)){
		  value = -1;
		}
		header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))
		assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
	}
}

#Load mapping csv file:
mapping = read.csv2(mappingFileName)

#number of variables required by interva 4
iv4_n = nrow(mapping)
outputData <- data.frame(matrix(ncol=iv4_n+1)) #Initialize output dataframe
colnames(outputData) <- c("ID", toupper(mapping[, 2]))
for(record in 1:nrow(records)){
	loadAndSetAllVariablesFromWHOInstrument(record)
	currentData <- data.frame(matrix(ncol=iv4_n+1))
	currentData[1] = record
	for(i in 1:iv4_n) {
		expression = as.character(mapping[i,5])
		##Evaluate expression and set InterVA4 variable accordingly
		retVal = eval(parse(text=expression))
		if(retVal == TRUE){
			currentData[i+1] = 'y'
		}
	}
	outputData[record,] <- currentData
}
write.table(outputData, outputFileName, quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = ",")

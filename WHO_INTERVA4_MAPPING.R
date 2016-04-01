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
who = read.csv(submissionFileName)

#store column names
v <- colnames(who)
n = ncol(who);
entries = nrow(who);

loadAndSetAllVariablesFromWHOInstrument<-function(entryLevel){
	entry = who[entryLevel,] #Get current entry
	x <- foreach(j=1:n) %do% {
		header = names(who)[j]
		value =  as.character(entry[1,j])
		header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))
		assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
	}
}

#Load mapping csv file:
mapping = read.csv2(mappingFileName)

#Run through mappings file and fill in value for every InterVA4 variable
who_n = nrow(mapping)
counter = 1

outputData <- data.frame(matrix(ncol=who_n+1)) #Initialize output dataframe
colnames(outputData) <- c("ID", toupper(mapping[, 2]))

rows <- foreach(entryCount=1:entries ) %do%{
	loadAndSetAllVariablesFromWHOInstrument(entryCount)
	#Prepare output
	currentData <- data.frame(matrix(ncol=who_n+1))
	currentData[1] = entryCount
	x <- foreach(i=1:who_n) %do%{	
		expression = as.character(mapping[i,5])
		interva = as.character(mapping[i, 2])
		id = mapping[i, 1]
		##Evaluate expression and set InterVA4 variable accordingly
		retVal = eval(parse(text=expression))
		if(retVal == TRUE){
			currentData[i+1] = 'y'
		}
		counter = counter + 1
	}
	outputData[entryCount,] <- currentData
}
write.table(outputData, outputFileName, quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = ",")

#########################################################################
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tools submissions to file 
##			format for use with a coding algorithm.
## @input:		mapping file who 2014 -> target variables required by coding algo, and
##			who 2014 submission csv (exported from odk aggregate)
## @output:		csv for usage in InterVA4
## ----------------------------------------------------------------------
#Clear variables
rm(list=ls(all=TRUE))
source('mapping_lib.R')
## Define your file path variables here###############################
workingDir = file.path(getwd(), "data")
mappingFileName = file.path(workingDir, "interva4_mapping.csv")
submissionFileName = file.path(workingDir, "output.csv")
outputFileName = file.path(workingDir, "output_for_interva4.csv")
######################################################################

#load who submission file:
records = read.csv(submissionFileName)
records[is.na(records)]<-""
headers = names(records)

#Load mapping csv file:
mapping = read.csv2(mappingFileName)

#number of variables required by coding algorithm
target_n = nrow(mapping)
outputData <- data.frame(matrix(ncol=target_n+1)) #Initialize output dataframe
colnames(outputData) <- c("ID", toupper(mapping[, 2]))
for(record in 1:nrow(records)){
  entry = records[record,] #Get current entry
	loadAndSetAllVariablesFromWHOInstrument(entry, headers)
	currentData <- data.frame(matrix(ncol=target_n+1))
	currentData[1] = record
	for(i in 1:target_n) {
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
write.table(outputData, "data/output_for_interva4.txt", quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = "\t")

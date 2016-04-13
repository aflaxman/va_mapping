#########################################################################
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tools submissions to file 
##			format for use with a coding algorithm.
## @input:		mapping file who 2014 -> target variables required by coding algo, and
##			who 2014 submission csv (exported from odk aggregate)
## @output:		csv for usage in InterVA4
## ----------------------------------------------------------------------
source('mapping_lib.R')

workingDir = file.path(getwd(), "data")
mappingFileName = file.path(workingDir, "interva4_mapping.csv")
submissionFileName = file.path(workingDir, "output.csv")
outputFileName = file.path(workingDir, "output_for_interva4.csv")

#load who submission file:
records = read.csv(submissionFileName)
records[is.na(records)]<-""
headers = names(records)

#Load mapping csv file:
mapping = read.csv2(mappingFileName)

#number of variables required by coding algorithm
target_n = nrow(mapping)
outputData <- data.frame(matrix(ncol=target_n)) 
colnames(outputData) <- mapping[, 1]
for(record in 1:nrow(records)){
  entry = records[record,] #Get current entry
	loadAndSetAllVariablesFromWHOInstrument(entry, headers)
	currentData <- data.frame(matrix(ncol=target_n))
	
	for(i in 1:target_n) {
	  target_var = as.character(mapping[i, 1])
	  expr = as.character(mapping[i,2])
	  currentData[i] = evalExpr(expr)
		#make the value available for reference later in the destination var set
		name = paste('t_', regmatches(target_var, regexpr("[^\\-]*$", target_var)), sep='')
		assign(name, as.character(currentData[i][[1]]), envir = .GlobalEnv) 
	}
	outputData[record,] <- currentData
}

write.table(outputData, outputFileName, quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = ",")
write.table(outputData, "data/output_for_interva4.txt", quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = "\t")

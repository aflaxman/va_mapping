#########################################################################
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tools submissions to SmartVA file 
##			format ready to be coded in SmartVA.
## @input:		mapping file who 2014 -> smartva variables, and
##			who 2014 submission csv (exported from odk briefcase)
## @output:		csv for usage in SmartVA
## ----------------------------------------------------------------------
source('mapping_lib.R')

workingDir = file.path(getwd(), "data")
mappingFileName = file.path(workingDir, "tariff_mapping_full.csv")
submissionFileName = file.path(workingDir, "output.csv")
outputFileName = file.path(workingDir, "output_for_smartva.csv")

#load who submission file:
records = read.csv(submissionFileName)
records[is.na(records)]<-""
headers = names(records)

#Load mapping csv file:
mapping = read.csv2(mappingFileName)

#number of variables required by coding algorithm
target_n = nrow(mapping)
outputData <- data.frame(matrix(ncol=target_n)) #Initialize output dataframe
for(record in 1:nrow(records)){
  entry = records[record,] #Get current entry
  loadAndSetAllVariablesFromWHOInstrument(entry, headers)
	currentData <- data.frame(matrix(ncol=target_n))

	for(i in 1:target_n){
		destination_var = as.character(mapping[i, 1])
		expr = as.character(mapping[i,2])
		who_var = as.character(mapping[i,3])
		colnames(currentData)[i] <- destination_var
		currentData[i] = evalExpr(expr)
		#make the value available for reference later in the destination var set
		name = paste('t_', regmatches(destination_var, regexpr("[^\\-]*$", destination_var)), sep='')
		assign(name, as.character(currentData[i][[1]]), envir = .GlobalEnv) 
	}
	colnames(outputData) <- colnames(currentData)
	outputData[record,] <- currentData
}

write.table(outputData, outputFileName, quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = ",")
write.table(outputData, "data/output_for_smartva.txt", quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = "\t")

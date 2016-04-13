#########################################################################
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tools submissions to SmartVA file 
##			format ready to be coded in SmartVA.
## @input:		mapping file who 2014 -> smartva variables, and
##			who 2014 submission csv (exported from odk briefcase)
## @output:		csv for usage in SmartVA
## @author: 	RMI
## ----------------------------------------------------------------------
#Clear variables
rm(list=ls(all=TRUE))


## Define your file path variables here###############################
workingDir = file.path(getwd(), "data")
mappingFileName = file.path(workingDir, "tariff_mapping_full.csv")
submissionFileName = file.path(workingDir, "output.csv")
outputFileName = file.path(workingDir, "output_for_smartva.csv")

######################################################################

#load who submission file:
who = read.csv(submissionFileName)
who[is.na(who)]<-""
n = ncol(who);
entries = nrow(who);

loadAndSetAllVariablesFromWHOInstrument<-function(entryLevel){
	entry = who[entryLevel,] #Get current entry
	for (j in 1:n){
		header = names(who)[j]
		value =  as.character(entry[1,j])
		header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))
		assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
	}
}

multipleSelectContains<-function(what, whoName){
	if(whoName == -1){
		return(FALSE)
	}
	split_expression <- as.list(strsplit(whoName," ")[[1]])
	listLength = length(split_expression)
	found = FALSE;
	for (selection in split_expression){
    		if(grepl(what, selection)){ 
			found = TRUE 
		}
	}
	return(found)
}

yesToCode <- function(qlist, clist, default){
  code<-''
  for (i in 1:length(qlist)){
    if (get(qlist[i])=='yes'){
      code<-paste(code, clist[i]) 
    }
  }
  #use default if code is empty
  if(nchar(code)==0){
    code = default
  }
  return(trimws(code))
}

#fromList: upper limits of range
#toList: codes to map to 
rangeToCode <- function(fromList, toList, default, whoName){
  code=default
  value=get(whoName)
  for (i in 1:length(toList)){
    if (value > fromList[i] && value<=fromList[i+1]){
      code<-toList[i] 
    }
  }
  #leave empty if no match
  return(code)
}

mapCode <- function(fromList, toList, whoName){
  code=''
  value=get(whoName)
  for (i in 1:length(fromList)){
    if (fromList[i]==value){
      code<-toList[i] 
    }
  }
  #leave empty if no match
  return(code)
}

mapMultiCode <- function(fromList, toList, whoName){
  code=''
  values=strsplit(as.character(get(whoName)), ' ')[[1]]
  for (i in 1:length(fromList)){
    if (any(fromList[i]==values)){
      code<-paste(code, toList[i])
    }
  }
  #leave empty if no match
  return(trimws(code))
}

ed<-function(expr, default){
  value <- evalExpr(expr)
  if (nchar(value)>0){
    return(value)
  }
  else{
    return(default)
  }
}

#wrapper around eval, with some extra functionality
evalExpr<-function(expr){
  if (nchar(expr)==0){
    return('')
  }
  value<-''
  value<-tryCatch(eval(parse(text=expr)))
  return(value)
}

#Load mapping csv file:
mapping = read.csv2(mappingFileName, stringsAsFactors=F, dec=".", sep = ";")

#Run through mappings file and fill in value for every SmartVA variable
variables_n = nrow(mapping)

outputData <- data.frame(matrix(ncol=variables_n)) #Initialize output dataframe

for (entryCount in 1:entries){
  loadAndSetAllVariablesFromWHOInstrument(entryCount)
	#Prepare output
	currentData <- data.frame(matrix(ncol=variables_n))

	for(i in 1:variables_n){
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
	outputData[entryCount,] <- currentData
}

write.table(outputData, outputFileName, quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = ",")
write.table(outputData, "data/output_for_smartva.txt", quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = "\t")

########### WHO VA 2014 -> SmartVA file format mapping ################
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tools submissions to SmartVA file 
##			format ready to be coded in SmartVA.
## @input:		mapping file who 2014 -> smartva variables, and
##			who 2014 submission csv (exported from odk briefcase)
## @output:		csv for usage in SmartVA
## @author: 	RMI
## @date: 		08.2015
##
## ----------------------------------------------------------------------

# load the required libraries:

#load foreach package
library(foreach)

cat("\nWHO VA Instrument 2014 -> SmartVA Conversion\n\n")

#Clear variables
rm(list=ls(all=TRUE))

#Start time
ptm <- proc.time()

## Define your file path variables here###############################
######################################################################
######################################################################
workingDir = "C:/dev/workspace_R/va_mapping/data";
mappingFileName = "tariff_mapping_full_v6.csv"
submissionFileName = "2014_WHO_Verbal_Autopsy_Form__version_2_15_10__results_13.csv"
outputFileName = "output_for_smartva.csv"
######################################################################

# store the current directory
initial.dir<-getwd()
print(paste("initial Working directory:" , initial.dir))
# change to the new directory
print(paste("Set to working directory:" , workingDir))
setwd(workingDir)
print(paste("Set to working directory to:" , getwd()))

#load who submission file:
who = read.csv(submissionFileName)

#store column names
v <- colnames(who)
n = ncol(who);
entries = nrow(who);

getValueInSubmissionForWHOId<-function(id){
	#print(paste("Get object with id:", id))
	value = get(id)
	return(value)
}

loadAndSetAllVariablesFromWHOInstrument<-function(entryLevel){
	entry = who[entryLevel,] #Get current entry
	x <- foreach(j=1:n) %do% {
		header = names(who)[j]
		value =  as.character(entry[1,j])
		header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))

		#Set value to 0 if NA to prevent NA when evaluating expression
		#Check how to replace this, since this introduces some errors (e.g. value < 10)
		if(is.na(value)){
			value = -1;
		}
		else if(nchar(value) == 0){
			value = -1;		
		}

		#if(header_cleaned == "id3E102"){
			#print(paste("Found who val: ", header_cleaned, "Value:", value))
		#}

		assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
	}
}

multipleSelectContains<-function(what, where){
	print(paste("Searching for", what, "in", where))
	split_expression <- as.list(strsplit(where," ")[[1]])
	listLength = length(split_expression)
	#print(paste("split_expression:",split_expression, ", Length:",listLength ))

	found = 0;
	for (selection in split_expression){
    		if(grepl(what, selection)){ 
			found = 1 
			print("found")
		}
	}
	return(found)
}

mapValues <- function(from, to, value){

	from_vector = unlist(strsplit(from, ","))
	to_vector = unlist(strsplit(to, ","))

	elementAt = match(value,from_vector)

	if(value == -1 && is.na(elementAt)){
		#print("Value is undefined (-1)")
		return(-1)
	}
	#print(paste("from:",from))
	#print(paste("to:", to))


	#from_vector = scan(textConnection(from), what="character()", sep=",")
	#to_vector = scan(textConnection(to), what="character()", sep=",")



	#print(paste("from:",is.character(from_vector)))
	#print(paste("to:", is.character(to_vector)))

	#print(paste("from:",length(from_vector)))
	#print(paste("to:", length(to_vector)))

	#print(paste("Mapping", from_vector, "to", to_vector, "Actual value:", value))



	#print(paste("Class of from vector:",class(from_vector)))

	#print(paste("to_vector[2]",to_vector[2]))

	#print(paste("from_vector:",from_vector))
	#print(paste("to_vector:",to_vector))
	#print(paste("value:",value))
	#print(paste("ElementAt:", elementAt))
	#print(paste("Mapped value:", to_vector[elementAt]))

	if(is.na(elementAt)){
		return(-1)
	}
	else{
		return(to_vector[elementAt])
	}
}

handleRow <- function(who_var){

}


#Load mapping csv file:
mapping = read.csv2(mappingFileName, stringsAsFactors=F, dec=".")

#Run through mappings file and fill in value for every SmartVA variable
variables_n = nrow(mapping)
#variables_n = 1 # Limit to first x entries for testing purposes

print(paste("No. of variables to map:", variables_n))
counter = 1

outputData <- data.frame(matrix(ncol=variables_n)) #Initialize output dataframe

rows <- foreach(entryCount=1:entries ) %do%{	
	loadAndSetAllVariablesFromWHOInstrument(entryCount)

	#Prepare output
	currentData <- data.frame(matrix(ncol=variables_n))

	x <- foreach(i=1:variables_n) %do%{	

		##Assign who variable
		who_var = as.character(mapping[i,5]) # Convert to class character from factors
		destination_var = as.character(mapping[i, 2])
		id = mapping[i, 1]
		question = mapping[i,3]
		#6 = neonatal, 7 = child, 8 = adult
		neonatal = as.character(mapping[i,6])
		child = as.character(mapping[i,7])
		adult = as.character(mapping[i,8])
		male = as.character(mapping[i,9])
		female = as.character(mapping[i,10])
		expression = as.character(mapping[i,11])
		fix_value = as.character(mapping[i,12])
		dynamic_value = as.character(mapping[i,13])
		mapping_from = as.character(mapping[i,14])
		mapping_to = as.character(mapping[i,15])
		custom = as.character(mapping[i,16])
		
		colnames(currentData)[i] <- destination_var

		#Specific cases
		if(destination_var == "Generalmodule-general2-sid"){
			currentData[i] = paste("sid_",entryCount,sep="")
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-gen_5_4"){
			#if(get('isNeonatal') == 1 && (get('id1A200') != 'yes' || get('id1A220') != 'yes')){
			if(get('isNeonatal') == 1){
				currentData[i] = 4;
			}
			#else if(get('isChild') == 1 && (get('id1A200') != 'yes' || get('id1A220') != 'yes')){
			else if(get('isChild') == 1){
				currentData[i] = 2;
			}
			#else if(get('isAdult') == 1 && (get('id1A200') != 'yes' || get('id1A220') != 'yes')){
			else if(get('isAdult') == 1){
				currentData[i] = 1;
			}
		}
		else if(destination_var == "childModule-Child1-child_1_15" && 1 == 2){
			if(get('isNeonatal') == '1' && get('id3D285') == 'no' && get('id3D298') == 'no' && get('id3D299') == 'no'){
				#print('Child stillbirth')
				currentData[i] = 1
				assign('id3D320', '1', envir = .GlobalEnv) # put variables in global environment
			}
			else if(get('isNeonatal')  == '1' ){
				#print('Child alive birth')
				currentData[i] = 0
				assign('id3D320', '0', envir = .GlobalEnv) # put variables in global environment
			}
			#print(paste('id3D320:',get('id3D320')))
			#print(paste(i, "childModule-Child1-child_1_15---------------------------", get(who_var),dynamic_value_parsed, fix_value, "----------------------------id3H150"))
		}
		else if(destination_var == "childModule-Child1-child_1_19" && get('id3D320') == 'yes' && get('id3D230') == 'yes'){
			abnormalities = "";
			if(get('id3D240') == 'yes'){
				abnormalities  = paste(abnormalities, '3')
			}
			if(get('id3D241') == 'yes'){
				abnormalities  = paste(abnormalities, '2')
			}
			if(get('id3D242') == 'yes'){
				abnormalities  = paste(abnormalities, '1')
			}
			currentData[i] = abnormalities
		}
		else if(destination_var == "childModule-Child1-ageIllnessStartDetails-child_1_20"){
			if(get('isNeonatal') == '1' && nchar(get('id3D090')) > 0){
				currentData[i] = 4;
			}
			else if(get('isChild') == '1' && nchar(get('id3D070')) > 0){
				currentData[i] = 1;
			}
		}
		else if(destination_var == "childModule-Child2-child_2_1" && get('isNeonatal') == '1'){
			complications = "";
			if(get('id3D273') == 'yes'){ #Convulsions
				complications = paste(complications , '1')
			}
			if(get('id3D269') == 'yes'){ #High blood pressure
				complications = paste(complications , '2')
			}
			if(get('id3D277') == 'yes'){ #Child not delivered head first
				complications = paste(complications , '5')
			}
			if(get('id3D278') == 'yes'){ #Cord around child's neck
				complications = paste(complications , '7')
			}
			if(get('id3D276') == 'yes'){ #Excessive bleeding
				complications = paste(complications , '8')
			}
			if(nchar(complications) > 0){
				currentData[i] = complications
			}
			else{
				currentData[i] = 10 #No complications
			}
		}
		else if(destination_var == "childModule-Child2-child_2_5" && get('isNeonatal') == '1'){
			if(get('id3D251b') > 0 && get('id3D251a') == 0 && get('id3D251') == 'yes'){ # id3D251b == hours
				currentData[i] = 5
			}
			else if(get('id3D251b') == 0 && get('id3D251a') > 0 && get('id3D251') == 'yes'){ #id3D251a == days
				currentData[i] = 4
			}
			else if(get('id3D251') == 'yes'){
				currentData[i] = 4
			}
		}
		#Check if we have to set the hours 
		else if(destination_var == "childModule-Child2-child_2_5a" && get('id3D251') == 'yes'  && get('isNeonatal') == '1'){
			if(get('id3D251b') > 0 && get('id3D251a') == 0 && get('id3D251') == 'yes'){ # id3D251b == hours
				currentData[i] = get('id3D251b')
			}
			else if(get('id3D251b') == 0 && get('id3D251a') > 0 && get('id3D251') == 'yes'){ #id3D251a == days
				currentData[i] = get('id3D251a')
			}
			else if(get('id3D251') == 'yes'){
				currentData[i] = get('id3D251a')
			}
		}
		#Check if we have to set the days 
		else if(destination_var == "childModule-Child2-child_2_5b" && get('id3D251') == 'yes'  && get('isNeonatal') == '1'){
			if(get('id3D251b') > 0 && get('id3D251a') == 0 && get('id3D251') == 'yes'){ # id3D251b == hours
				#currentData[i] = get('id3D251b')
			}
			else if(get('id3D251b') == 0 && get('id3D251a') > 0 && get('id3D251') == 'yes'){ #id3D251a == days
				currentData[i] = get('id3D251a')
			}
			else if(get('id3D251') == 'yes'){
				currentData[i] = get('id3D251a')
			}
		}
		else if(destination_var == "childModule-Child2-child_2_12" && get('isNeonatal') == '1'){
			if(get('id3D263') == 1 && get('id3D261') == 'yes'){
				currentData[i] = 1
			}
			else if(get('id3D263') == 2 && get('id3D261') == 'yes'){
				currentData[i] = 2
			}
			else if(get('id3D263') == 3 && get('id3D261') == 'yes'){
				currentData[i] = 3
			}
			else if(get('id3D263') == 4 && get('id3D261') == 'yes'){
				currentData[i] = 4
			}
			else if(get('id3D263') >= 5 && get('id3D261') == 'yes'){
				currentData[i] = 5
			}
			else if(get('id3D261') == 'yes'){
				currentData[i] = 9
			}
		}
		else if(destination_var == "childModule-Child2-child_2_17" && get('isNeonatal') == '1'){
			deliveryType = 9
			if(get('id3C450') == 'yes'){ #normal vaginal without foreceps (=2)
				deliveryType = 2
			}
			else if(get('id3C460') == 'yes'){ #vaginal with forceceps (=1)
				deliveryType = 1
			}
			else if(get('id3C470') == 'yes'){ #caesarian (=4)
				deliveryType = 4
			}
			else{
				deliveryType = 9
			}
			currentData[i] = deliveryType
		}
		else if(destination_var == "childModule-Child3-child_3_3" && get('isNeonatal') == '1' && get('id3D320') != 'yes' && get('id3D230') == 'yes'){
			abnormalities = "";
			if(get('id3D240') == 'yes'){
				abnormalities  = paste(abnormalities, '3')
			}
			if(get('id3D241') == 'yes'){
				abnormalities  = paste(abnormalities, '2')
			}
			if(get('id3D242') == 'yes'){
				abnormalities  = paste(abnormalities, '1')
			}
			currentData[i] = abnormalities
		}
		else if(destination_var == "childModule-Child3-child_3_4" && get('isNeonatal') == '1' && get('id3D320') != 'yes'){
			didBabyBreathe = 9;
			if(get('id3D300') == 'yes'){
				didBabyBreathe = 1
			}
			currentData[i] = didBabyBreathe
		}
		else if(destination_var == "childModule-Child3-child_3_7" && get('isNeonatal') == '1' && get('id3D320') != 'yes'){
			didBabyBreathe = 9;
			if(get('id3D290') == 'yes'){
				didBabyBreathe = 1
			}
			currentData[i] = didBabyBreathe
		}
		else if(destination_var == "childModule-Child3-child_3_8" && get('isNeonatal') == '1' && get('id3D320') != 'yes' && get('id3D290') == 'yes'){
			duration = 9;
			if(get('id3D292') <= 5){
				duration = 1
			}
			else if(get('id3D292') > 5 && get('id3D292') <= 30){
				duration = 2
			}
			else if(get('id3D292') > 30){
				duration = 3
			}
			currentData[i] = duration
		}
		else if(destination_var == "childModule-Child3-child_3_9" && get('isNeonatal') == '1' && get('id3D320') != 'yes'){
			stoppedCrying = 9;
			if(nchar(get('id3D294')) > 0 && get('id3D294') != -1){
				mapped_value = mapValues("yes,no,dk,ref", "1,0,9,8", get('id3D294'))
				stoppedCrying = mapped_value
			}
			currentData[i] = stoppedCrying 
		}
		else if(destination_var == "childModule-Child3-child_3_10" && get('isNeonatal') == '1' && get('id3D320') != 'yes' && get('id3D294') == 'yes'){
			whenStoppedCrying = 9;
			if(get('id3D296') <= 0 && get('id3D296') < 24){
				whenStoppedCrying = 1
			}
			else if(get('id3D296') >= 24){
				whenStoppedCrying = 2
			}
			currentData[i] = whenStoppedCrying 
		}
		else if(destination_var == "childModule-Child3-child_3_24" && get('isNeonatal') == '1' && get('id3D320') != 'yes'){
			whenStoppedCrying = 9;
			if(get('id3D296') <= 0 && get('id3D296') < 24){
				whenStoppedCrying = 1
			}
			else if(get('id3D296') >= 24){
				whenStoppedCrying = 2
			}
			currentData[i] = whenStoppedCrying 
		}
		else if(destination_var == 'childModule-Child4-breathingdetails-child_4_23' && get('isChild') == 1){
			if(multipleSelectContains("grunting",get('id3B260')) == "1"){
				currentData[i] = 1;
			}
			else{
				currentData[i] = 0;
			}
		}
		else if(destination_var == 'childModule-Child4-child_4_27' && get('isChild') == 1 && get('id3B440') == 'yes'){
			hours = get('id3B445')
			unconstart = 9;
			if(hours >= 0 && hours < 6){
				unconstart = 1
			}
			else if(hours >= 6 && hours <= 23){
				unconstart = 2
			}
			else if(hours >= 24){
				unconstart = 3
			}
			currentData[i] = unconstart
		}
		else if(destination_var == 'childModule-Child4-child_4_48' && get('isChild') == 1 && get('id3E100') == 'yes'){
			accidentchild = '';
			if(get('id3E115') == 'yes'){ # road
				accidentchild = paste(accidentchild, '1')
			}
			if(get('id3E310') == 'yes'){ # fall
				accidentchild = paste(accidentchild, '2')
			}
			if(get('id3E320') == 'yes'){ # drowning
				accidentchild = paste(accidentchild, '3')
			}
			if(get('id3E510') == 'yes'){ # poisoning
				accidentchild = paste(accidentchild, '4')
			}
			if(get('id3E340') == 'yes'){ # animal
				accidentchild = paste(accidentchild, '5')
			}
			if(get('id3E330') == 'yes'){ # Burns
				accidentchild = paste(accidentchild, '6')
			}
			if(get('id3E115') == 'yes'){ # violence
				accidentchild = paste(accidentchild, '7')
			}
			if(nchar(accidentchild) > 0){
				currentData[i] = accidentchild
			}
			else{
				currentData[i] = 9;
			}
		}
		#/specific cases
		else if(!is.na(fix_value) && nchar(fix_value) > 0 && nchar(expression) == 0 && nchar(mapping_from) == 0 && nchar(dynamic_value) == 0){
			#print(paste(i,"CONTAINS fixed ENTRY", fix_value))
			currentData[i] = fix_value
		}
		#Custom
		else if(!is.na(custom) && nchar(custom) > 0 && 1 == 2){
			print(paste("CUSTOM","-----------------",custom))
			split_expression <- as.list(strsplit(custom,"?")[[1]])
			print(typeof(split_expression))
			split_values <- as.list(strsplit(split_expression[2], ":")[[1]])
			print(paste("split_expression:",split_expression))
			print(paste("split_values:",split_values))
			listLength = length(split_values)
			if(listLength == 1){
				if(nchar(expression) > 0){
					print(paste("Expression evaluated to false. Setting to single value:", custom))
					currentData[i] = custom
				}
				else{
					print(paste("Expression evaluated to false. Not setting value."))
				}
			}
			if(listLength == 2){
				#print(split)
				value1 = split_values[1]
				value2 = split_values[2]
				print(paste("case1:",value1))
				print(paste("case2:",value2))

				if(nchar(expression) > 0){
					evalBool = eval(parse(text=expression))
					print(paste("Expression:",expression,"evalBool :",evalBool ))
					if(evalBool == TRUE){	
						print(paste("Expression evaluated to true. Setting value to:", value1))
						currentData[i] = value1
					}
					else{
						print(paste("Expression evaluated to false. Setting value to:", value2))
						currentData[i] = value2
					}
				}
			}
			else{
				print(paste("List is of length", listLength))
			}
		}
		#/Custom
		else if(!is.na(dynamic_value) && nchar(dynamic_value) > 0 && nchar(expression) == 0){
			#print(paste(i,"CONTAINS Dynamic ENTRY", dynamic_value))
			dynamic_value_parsed = eval(parse(text=dynamic_value))

			if(dynamic_value_parsed != -1 && nchar(dynamic_value_parsed) > 0){
				#print(paste(i,"CONTAINS DYNAMIC VALUE::", dynamic_value_parsed))
				currentData[i] = dynamic_value_parsed
			}
			else{
				#print(paste(i,"DYNAMIC VALUE::invalid", fix_value, nchar(fix_value)))
				if(nchar(fix_value) > 0){
					#print(paste(i,"setting default"))
					currentData[i] = fix_value
				}
				else{
					#currentData[i] = 0
				}
			}
		}
		else if(nchar(who_var) > 0 && nchar(mapping_from) > 0 && nchar(mapping_to) > 0 && nchar(expression) == 0){
			#print(paste(i, "Mapping",mapping_from,"to",mapping_to))
			##print(paste(class(mapping_from), class(mapping_to)))
			##print(paste("Value:", get(who_var),"(" ,who_var, ")"))

			if(get(who_var) != -1){
				mapped_value = mapValues(mapping_from, mapping_to, get(who_var))
				#print(paste("Value", get(who_var), "mapped to", mapped_value))
				if(mapped_value == -1){
					#currentData[i] = 9
				}
				else{
					currentData[i] = mapped_value
				}
			}
			else{
				#currentData[i] = 9 # Set unmapped variable to 9 (DK)
			}
		}
		else if(!is.na(expression) && nchar(expression) > 0 && nchar(mapping_from) == 0){
			#print(paste("Expression:", expression))
			evalBool = eval(parse(text=expression))

			if(evalBool == TRUE){
				#print("Expression fullfilled!")
				if(!is.na(fix_value) && nchar(fix_value) > 0){
					#print(paste(i,"CONTAINS fixed ENTRY", fix_value))
					currentData[i] = fix_value
				}
				else if(!is.na(dynamic_value) && nchar(dynamic_value) > 0){
					#print(paste(i,"CONTAINS Dynamic ENTRY", dynamic_value))
					dynamic_value_parsed = eval(parse(text=dynamic_value));
					#print(paste(i,"CONTAINS DYNAMIC ENTRY:", dynamic_value_parsed))
					currentData[i] = dynamic_value_parsed
				}
			}
			else{
				#If expression evaluated to false, do nothing
				#print(paste(i,"evaluated to FALSE"))
			}
		}
		else if(!is.na(expression) && nchar(expression) > 0 && nchar(who_var) > 0 && nchar(mapping_from) > 0 && nchar(mapping_to) > 0){
			#print(paste(i,"At WHO var: ", who_var, "Xpression:", expression, "Value:", get(who_var) ))

			evalBool = eval(parse(text=expression))
			if(evalBool == TRUE){
				mapped_value = mapValues(mapping_from, mapping_to, get(who_var))
				print(paste(i, "Value::", get(who_var), "mapped to", mapped_value))

				if(mapped_value != -1){
					currentData[i] = mapped_value
				}
			}
			else{
				#print(paste(i, "evaluation resulted in FALSE!"))
				currentData[i] = fix_value
			}
		}
		else{
			#print(paste(i,"IS EMPTY"))
			currentData[i] = fix_value
		}

		counter = counter + 1
	}

	colnames(outputData) <- colnames(currentData) #Set column names for output
	outputData[entryCount,] <- currentData
}

#print(outputData) #print

write.csv(outputData, outputFileName, quote=FALSE, row.names = FALSE, na="")

cat("\n\n\n")
etm<-proc.time() - ptm # End time
print(paste("Total run time:", etm[3], "s")) #print elapsed time

# change back to the original directory
setwd(initial.dir)

# unload the libraries
detach("package:foreach")

cat("\nWHO VA Instrument -> SmartVA Mapping Done!\n\n")
########### WHO VA 2014 -> SmartVA file format mapping ################
## ----------------------------------------------------------------------
##
## @description: 	Converts 2014 WHO VA Tools submissions to SmartVA file 
##			format ready to be coded in SmartVA.
## @input:		mapping file who 2014 -> smartva variables, and
##			who 2014 submission csv (exported from odk briefcase)
## @output:		csv for usage in SmartVA
## @author: 	RMI
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
workingDir = file.path(getwd(), "data")
mappingFileName = file.path(workingDir, "tariff_mapping_full.csv")
submissionFileName = file.path(workingDir, "output.csv")
outputFileName = file.path(workingDir, "output_for_smartva.csv")

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

		#Set value to 0 if NA to prevent NA when evaluating expression
		#Check how to replace this, since this introduces some errors (e.g. value < 10)
		if(is.na(value)){
			value = -1;
		}
		else if(nchar(value) == 0){
			value = -1;		
		}

		assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
	}
}

multipleSelectContains<-function(what, where){
	found = 0;

	if(where == -1){
		return(found)
	}
	split_expression <- as.list(strsplit(where," ")[[1]])
	listLength = length(split_expression)

	for (selection in split_expression){
    		if(grepl(what, selection)){ 
			found = 1 
		}
	}
	return(found)
}

mapValues <- function(from, to, value){
	from_vector = unlist(strsplit(from, ","))
	to_vector = unlist(strsplit(to, ","))

	elementAt = match(value,from_vector)

	if(value == -1 && is.na(elementAt)){
		return(-1)
	}
	#--------------Multi match-----------------
	values_vector = unlist(strsplit(as.character(value), " "))
	result = ""

	for(entry in values_vector){
		elementAt = match(entry,from_vector)
		if(!is.na(elementAt)){
			mapped_value = to_vector[elementAt]
			if(nchar(result) == 0)
			{
				result = paste(result, mapped_value, sep="")
			}
			else{
				result = paste(result, mapped_value)
			}
		}
	}

	if(nchar(result) == 0){
		return(-1);
	}
	else{
		return(result);
	}
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
  return(code)
}

mapCodes <- function(fromList, toList, whoName){
  code=''
  for (i in 1:length(fromList)){
    if (fromList[i]==get(whoName)){
      code<-toList[i] 
    }
  }
  #use default if code is empty
  if(nchar(code)==0){
    code = default
  }
  return(code)
}


#Load mapping csv file:
mapping = read.csv2(mappingFileName, stringsAsFactors=F, dec=".", sep = ";")

#Run through mappings file and fill in value for every SmartVA variable
variables_n = nrow(mapping)

print(paste("No. of variables to map:", variables_n))
counter = 1

outputData <- data.frame(matrix(ncol=variables_n)) #Initialize output dataframe

rows <- foreach(entryCount=1:entries ) %do%{	
	loadAndSetAllVariablesFromWHOInstrument(entryCount)

	#Additional variables
	yearsfill = 0
	monthsfill = 0
	daysfill = 0
	adult = 0;
	child = 0;
	neonatal = 0;

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
		#TODO: mapping2 and mapping3 are temp columns used during code cleanup
		mapping2 = as.character(mapping[i,19])
		mapping3 = as.character(mapping[i,20])
		if(nchar(who_var) == 0){
			if(nchar(dynamic_value) > 0){
				print("dynamic value > 0")
			}
		}
		
		colnames(currentData)[i] <- destination_var

		#Set row-variables
		if(i == 1){
			if(isNeonatal == 1){
				neonatal = 1
				print(paste(entryCount,"ageInDays", ageInDays))
				print(paste(entryCount,"age_neonate_hours_calc", age_neonate_hours_calc))
				print(paste(entryCount,"age_neonate_minutes_calc", age_neonate_minutes_calc))
				print(paste(entryCount,"age_neonate_days", age_neonate_days))
				print(paste(entryCount,"age_neonate_hours", age_neonate_hours))
				print(paste(entryCount,"age_neonate_minutes", age_neonate_minutes))
			}
			else if(isChild == 1){
				child = 1
				print(paste(entryCount,"ageInYears", ageInYears))
				print(paste(entryCount,"ageInMonths", ageInMonths))
				print(paste(entryCount,"ageInMonthsRemain", ageInMonthsRemain))
				print(paste(entryCount,"age_child_days", age_child_days))
				print(paste(entryCount,"age_child_months", age_child_months))
				print(paste(entryCount,"age_child_years", age_child_years))
			}
			else if(isAdult == 1){
				adult = 1;
				print(paste(entryCount,"ageInYears", ageInYears))
				print(paste(entryCount,"age_adult", age_adult))
				if(ageInYears > 0){
					ageYears = ageInYears
				}
				else if(age_adult > 0){
					ageYears = ageInYears
				}
			}
		}

		#Specific cases
		if(destination_var == "Generalmodule-general5-qAgeInfo-gen_5_4"){
			if(as.numeric(get('isNeonatal')) == 1 && as.numeric(get('ageInDays')) > 0){
				currentData[i] = 4;
			}
			else if(as.numeric(get('isChild')) == 1){
				if(as.numeric(get('ageInDays')) > 0 && as.numeric(get('ageInDays')) < 365.25){
					currentData[i] = 2;
				}
				else if(as.numeric(get('ageInDays')) > 0){
					currentData[i] = 1;
				}
				else{
					currentData[i] = 9;
				}
			}
			else if(as.numeric(get('isAdult')) == 1){
				if(as.numeric(get('ageInYears')) > 0){
					currentData[i] = 1;
				}
				else if(as.numeric(get('age_adult')) > 0){
					currentData[i] = 1;
				}
			}
			else{
				currentData[i] = 9;
			}
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-gen_5_4a"){ # Age In Years
			if(as.numeric(get('isChild')) == 1){
				if(as.numeric(get('ageInDays')) >= 365.25){
					yearsfill = round(as.numeric(get('ageInDays')) / 365.25);
					currentData[i] = yearsfill;
				}
			}
			else if(as.numeric(get('isAdult')) == 1){
				if(as.numeric(get('ageInYears')) > 0){
					yearsfill = round(as.numeric(get('ageInYears')));
					currentData[i] = yearsfill;
				}
				else if(as.numeric(get('age_adult')) > 0){
					yearsfill = as.numeric(get('age_adult'))
					currentData[i] = yearsfill;
				}
			}
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-gen_5_4b"){ # Age In Months
			if(as.numeric(get('isChild')) == 1){
				if(as.numeric(get('ageInDays')) > 0 && as.numeric(get('ageInDays')) < 365.25){
					monthsFill = round(as.numeric(get('ageInDays')) / 30.4)
					currentData[i] = monthsFill;
				}
			}
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-gen_5_4c"){ # Age In Days
			if(as.numeric(get('isNeonatal')) == 1){
				if(as.numeric(get('ageInDays')) > 0){
					daysfill = get('ageInDays')
					currentData[i] = daysfill;
				}
			}
			else if(as.numeric(get('isChild')) == 1){
				if(as.numeric(get('ageInDays')) > 0 && as.numeric(get('ageInDays')) < 365.25){
					daysfill = get('ageInDays')
					currentData[i] = daysfill;
				}
			}
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-yearsfill"){
			if(yearsfill > 0){
				currentData[i] = yearsfill
			}
			else{
				currentData[i] = 0
			}
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-monthsfill"){
			if(monthsfill > 0){
				currentData[i] = monthsfill
			}
			else{
				currentData[i] = 0
			}
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-daysfill"){
			if(daysfill > 0){
				currentData[i] = daysfill
			}
			else{
				currentData[i] = 0
			}
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-agehours"){
			agehours = (yearsfill*8765.81) + (monthsfill*730.484) + (daysfill*24)
			currentData[i] = agehours
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-agedays"){
			agedays = (yearsfill*365) + (monthsfill*28) + (daysfill)
			currentData[i] = agedays
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-ageweeks"){
			ageweeks = (yearsfill*52) + (monthsfill*4) + (daysfill*0.142857)
			currentData[i] = ageweeks 
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-agemonths"){
			agemonths = (yearsfill*12) + (monthsfill*1) + (daysfill*0.0328549)
			currentData[i] = agemonths
		}
		else if(destination_var == "Generalmodule-general5-qAgeInfo-ageyears"){
			ageyears = (yearsfill*1) + (monthsfill*.083333) + (daysfill*0.00273791)
			currentData[i] = ageyears
		}
		else if(destination_var == "childModule-Child1-child_1_15" && 1 == 2){
			if(get('isNeonatal') == '1' && get('id3D285') == 'no' && get('id3D298') == 'no' && get('id3D299') == 'no'){
				currentData[i] = 1
				assign('id3D320', '1', envir = .GlobalEnv) # put variables in global environment
			}
			else if(get('isNeonatal')  == '1' ){
				currentData[i] = 0
				assign('id3D320', '0', envir = .GlobalEnv) # put variables in global environment
			}
		}
		else if(destination_var == "childModule-Child1-child_1_19" && get('id3D320') == 'yes' && get('id3D230') == 'yes'){
			currentData[i] = eval(parse(text=mapping2))
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
			currentData[i] = eval(parse(text=mapping2))
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
			currentData[i] = eval(parse(text=mapping2))
		}
		else if(destination_var == "childModule-Child3-child_3_3" && get('isNeonatal') == '1' && get('id3D320') != 'yes' && get('id3D230') == 'yes'){
			currentData[i] = eval(parse(text=mapping2))
		}
		else if(destination_var == "childModule-Child3-child_3_4" && get('isNeonatal') == '1' && get('id3D320') != 'yes'){
			currentData[i] = eval(parse(text=mapping2))
		}
		else if(destination_var == "childModule-Child3-child_3_7" && get('isNeonatal') == '1' && get('id3D320') != 'yes'){
		  currentData[i] = eval(parse(text=mapping2))
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
		else if(destination_var == "childModule-Child3-child_3_10" && get('id3D320') != 'yes'){
			whenStoppedCrying = 9;
			if(get('id3D296') >= 0 && get('id3D296') < 24){
				whenStoppedCrying = 1
			}
			else if(get('id3D296') >= 24){
				whenStoppedCrying = 2
			}
			currentData[i] = whenStoppedCrying 
		} 
		else if(destination_var == "childModule-Child3-child_3_24" && get('id3D320') != 'yes'){
			if(multipleSelectContains("grunting",id3B260) == "1"){
				currentData[i] = 1
			}
			else{
				if(multipleSelectContains("dk",id3B260) == "1"){
					currentData[i] = 9
				}
				else{
					currentData[i] = 0
				}
			}
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
		else if((destination_var == 'childModule-Child4-child_4_48' || destination_var == 'adultModule-adult5-adult_5_2') && get('id3E100') == 'yes'){
			currentData[i] = eval(parse(text=mapping2))
		}
		else if(destination_var == 'childModule-Child1-child_1_7'){
			birthSize = 9;
			if(get('id3D180') == 'yes'){ # Usual size
				birthSize = 3 # About average
			}
			else if(get('id3D190') == 'yes'){ # smaller than usual
				birthSize = 2 # Smaller than usual
			}
 			else if(get('id3D195') == 'yes'){ # very much smaller than usual
				birthSize = 1 # Very small
			}
			else if(get('id3D200') == 'yes'){ # larger than usual
				birthSize = 4 # Larger than usual
			}
			else if(get('id3D180') == 'dk' || get('id3D190') == 'dk' || get('id3D195') == 'dk' || get('id3D200') == 'dk'){
				birthSize = 9
			}
			else if(get('id3D180') == 'ref' || get('id3D190') == 'ref' || get('id3D195') == 'ref' || get('id3D200') == 'ref'){
				birthSize = 8
			}
			currentData[i] = birthSize;
		}
		#/specific cases
		else if(!is.na(fix_value) && nchar(fix_value) > 0 && nchar(expression) == 0 && nchar(mapping_from) == 0 && nchar(dynamic_value) == 0){
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
				value1 = split_values[1]
				value2 = split_values[2]
				print(paste("case1:",value1))
				print(paste("case2:",value2))

				if(nchar(expression) > 0){
					evalBool = eval(parse(text=expression))
					if(evalBool == TRUE){	
						currentData[i] = value1
					}
					else{
						currentData[i] = value2
					}
				}
			}
			else{
				print(paste("List is of length", listLength))
			}
		}
		#/Custom
		else if(!is.na(dynamic_value) && nchar(dynamic_value) > 0 && nchar(expression) == 0 && nchar(mapping_to) == 0){
			dynamic_value_parsed = eval(parse(text=dynamic_value))

			if(dynamic_value_parsed != -1 && nchar(dynamic_value_parsed) > 0){
				currentData[i] = dynamic_value_parsed
			}
			else{
				if(nchar(fix_value) > 0){
					currentData[i] = fix_value
				}
				else{
					#currentData[i] = 0
				}
			}
		}
		else if(nchar(who_var) > 0 && nchar(mapping_from) > 0 && nchar(mapping_to) > 0 && nchar(expression) == 0){
			if(get(who_var) != -1){
				mapped_value = mapValues(mapping_from, mapping_to, get(who_var))
        if(mapped_value == -1){
          #currentData[i] = 9
        }
        else{
          currentData[i] = mapped_value
        }
      }
      else{
        #currentData[i] = 9 # Set unmapped variable to 9 (DK)
			  currentData[i] = mapped_value
			}
		}
		else if(!is.na(expression) && nchar(expression) > 0 && nchar(mapping_from) == 0){
			evalBool = eval(parse(text=expression))
			if(evalBool == TRUE){
				if(!is.na(dynamic_value) && nchar(dynamic_value) > 0){
					dynamic_value_parsed = eval(parse(text=dynamic_value));
					currentData[i] = dynamic_value_parsed
				}
				else if(!is.na(fix_value) && nchar(fix_value) > 0){
					currentData[i] = fix_value
				}
			}
			else{
				#If expression evaluated to false, do nothing
				if(!is.na(fix_value) && nchar(fix_value) > 0 && !is.na(dynamic_value) && nchar(dynamic_value) > 0){
					currentData[i] = fix_value
				}
			}
		}
		else if(!is.na(expression) && nchar(expression) > 0 && nchar(who_var) > 0 && nchar(mapping_from) > 0 && nchar(mapping_to) > 0){
			evalBool = eval(parse(text=expression))
			if(evalBool == TRUE){
				mapped_value = mapValues(mapping_from, mapping_to, get(who_var))
				if(mapped_value != -1){
					currentData[i] = mapped_value
				}
			}
			else{
				currentData[i] = fix_value
			}
		}
		else if (!is.na(mapping3) && nchar(mapping3) > 0){
		  currentData[i] = eval(parse(text=mapping3))
		}
		else{
			currentData[i] = fix_value
		}
		counter = counter + 1
	}

	colnames(outputData) <- colnames(currentData) #Set column names for output
	outputData[entryCount,] <- currentData
}

write.table(outputData, outputFileName, quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = ",")

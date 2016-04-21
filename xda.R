rm(list=ls(all=TRUE))

load_source_variables<-function(entry, headers){
  for(j in 1:length(headers)){
    value =  as.character(entry[1,j])
    header = headers[j]
    header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))
    assign(header_cleaned, value, envir = .GlobalEnv)
  }
}

#map true to character 'y'
true_to_y<-function(expr){
  value<-eval(parse(text=expr))
  if (value==TRUE){
    return('y')
  }
  else{
    return('')
  }
}

multi_select_contains<-function(what, whoName){
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

yes_to_code <- function(qlist, clist, default){
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
range_to_code <- function(fromList, toList, default, whoName){
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

map_code <- function(fromList, toList, whoName){
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

map_multi_code <- function(fromList, toList, whoName){
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

#evaluate expression, return default on empty
exp_def<-function(expr, default){
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


map_records <- function(data_file, mapping_file, output_file) {
  data_dir = 'data'
  mapping_f_name= file.path(data_dir, mapping_file)
  record_f_name= file.path(data_dir, data_file)
  output_f_name= file.path(data_dir, paste(output_file, ".csv", sep = ''))
  debug_f_name= file.path(data_dir, paste(output_file, ".txt", sep = ''))
  
  #load who submission file:
  records = read.csv(record_f_name)
  records[is.na(records)]<-""
  headers = names(records)
  
  #Load mapping csv file:
  mapping = read.csv2(mapping_f_name)
  
  #number of variables required by coding algorithm
  target_n = nrow(mapping)
  output_data <- data.frame(matrix(ncol=target_n))
  colnames(output_data) <- mapping[, 1]
  for(rec_count in 1:nrow(records)){
    assign('rec_id', rec_count, envir = .GlobalEnv)
    record = records[rec_count,]
    load_source_variables(record, headers)
    current_data <- data.frame(matrix(ncol=target_n))
    for(i in 1:target_n){
      target_var = as.character(mapping[i, 1])
      expr = as.character(mapping[i,2])
      current_data[i] = evalExpr(expr)
      #make the value available for reference later in the destination var set
      name = paste('t_', regmatches(target_var, regexpr("[^\\-]*$", target_var)), sep='')
      assign(name, as.character(current_data[i][[1]]), envir = .GlobalEnv)
    }
    output_data[rec_count,] <- current_data
  }
  write.table(output_data, output_f_name, quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = ",")
  write.table(output_data, debug_f_name, quote=FALSE, row.names = FALSE, na="", qmethod = "escape", sep = "\t")
}

map_records("who_va_output.csv", "interva4_mapping.csv", "output_for_interva4")
map_records("who_va_output.csv", "tariff2_mapping.csv", "output_for_smartva")
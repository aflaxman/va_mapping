rm(list=ls(all=TRUE))

loadAndSetAllVariablesFromWHOInstrument<-function(entry, headers){
  for(j in 1:length(headers)){
    value =  as.character(entry[1,j])
    header = headers[j]
    header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))
    assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
  }
}

true_to_y<-function(expr){
  value<-evalExpr(expr)
  if (value==TRUE){
    return('y')
  }
  else{
    return('')
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



loadAndSetAllVariablesFromWHOInstrument<-function(entry, n_col, headers){
  for(j in 1:n_col){
    header = headers[j]
    value =  as.character(entry[1,j])
    if(is.na(value)){
      value = -1;
    }
    header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))
    assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
  }
}
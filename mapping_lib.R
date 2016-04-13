
loadAndSetAllVariablesFromWHOInstrument<-function(entry, headers){
  for(j in 1:length(headers)){
    value =  as.character(entry[1,j])
    header = headers[j]
    header_cleaned = regmatches(header, regexpr("[^\\.]*$", header))
    assign(header_cleaned, value, envir = .GlobalEnv) # put variables in global environment
  }
}
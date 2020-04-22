# #####################################################################
#
# Auxiliary Functions
#
# #####################################################################
bounded.f<-function(data.input){

  cf<-data.input[1]
  max_val<-data.input[2]
  output<-ifelse(cf>max_val  | is.na(cf) ,max_val,cf)
  return(output)
}
# Function substract
substract<-function(data.input){
  output<-data.input[2]-data.input[1]
  return(output)
}
# Function percentage
percentage<-function(input){
  output<-(input[2]-input[1])/input[1]
  return(output)
}


replacement<-function(data.input){
  output<-ifelse(!is.na(data.input[2]),data.input[2],data.input[1])
  return(output)
}

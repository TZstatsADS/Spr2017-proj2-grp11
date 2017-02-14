Match.flora <- function(input_data){
  tmp = match(input_data[2],long_lat$Country)
  return(as.matrix(c(long_lat$Longitude[tmp],long_lat$Latitude[tmp])))
}

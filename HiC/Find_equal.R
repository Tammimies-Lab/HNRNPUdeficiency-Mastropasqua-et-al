Find_equal <- function(df1, df2){
  ordered_df1 <- df1[order(df1[,1]),]
  ordered_df2 <- df2[order(df2[,1]),]
  vector1 <- ordered_df1[,4]
  vector2 <- ordered_df2[,4]
  compartment <- rep(NA, length(vector1))
  for (i in 1:length(vector1)) {
    if(vector1[i] == vector2[i] ){
      compartment[i] <- vector1[i]
    }
    if(vector1[i] != vector2[i] ){
      if(vector1[i] == "A" ){
        compartment[i] <- "AB"
      }
      if(vector1[i] == "B" ){
        compartment[i] <- "BA"
      }
    }
    
    
  }
  result <- as.data.frame(table(compartment))
  resulttable <- cbind(result, 100* prop.table(result$Freq))
  names(resulttable)[3]  <- "Percentages"
  return(resulttable)
}
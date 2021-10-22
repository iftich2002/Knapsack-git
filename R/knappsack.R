#'Knapsack problem solver
#'
#'take the weights, values and maximum capacity as parameters
#'@param w:weights vector
#'@param v;value vector
#'@param c:capacity (numeric)
#'@return Return the maximum of the w*v which are less than or equal to capacity




knapsackgit<-function(w,v,c){
  library(dplyr)
  ##w is weights, v values and c capacity
  ##w and v should be of equal length
  ##capacity depends upon the user
  ##multiplication
  w_v<-as.data.frame(cbind(w,v,w*v))
  colnames(w_v)<-c("weights","value","capacity")
  if (min(w_v[,3]) > c) {
    stop("Given capacity is not greater than the minimum value of weight*value, the minimum value is ", round(min(w_v[,3]),0))
  } else {
    ##selecting rows where the multiplication is less than or equal to capacity
    w_v_s <- filter(w_v, capacity <= c)
    result<-filter(w_v_s, capacity == max(w_v_s[,3]))
  }
  return(result)   
}


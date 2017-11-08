#' @title Brute Force Search
#' @description  use the brute force search to solves the problem
#' @param x Data Frame that contain v(value) and w(weight)
#' @param W Vector eith the knapsack size
#' @return a list with the value and elements of the knapsack
#' @refereces \url{https://en.wikipedia.org/wiki/Knapsack_problem} 
#' @examples
#' set.seed(42) 
#' n <- 2000 
#' knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#' 
#' 
#' @export 
#' 


brute_force_knapsack <- function(x, W){
  
  if(!(is.data.frame(x)) || !(is.numeric(W)) || W<0) stop()
  
  elem <- which.max(x$w <= W)
  
  weight <- x$w[elem]
  value <- x$v[elem]
  
  result <- list(value = value, elements = elem)
  
  vect <- seq(from = 2, to = floor(W/min(x$w)))
  for (i in vect) {
    comb <- combn(as.integer(row.names(x)), i)
    weights <- combn(x$w, i, sum)
    values <- combn(x$v, i, sum)
    
    poscomb <- which(weights <= W)
    max_value <- which.max(values[poscomb])
    
    value_aux <- values[poscomb[max_value]]
    
    if (any(value_aux > value, is.na(value))) {
      weight <- weights[poscomb[max_value]]
      value <- value_aux
      elem <- comb[, poscomb[max_value]]
      
      if ((weight + min(x$w)) >= W) {
        result$value <- value
        result$elements <- elem
        return(result)
      }
    }else {
      
      return(result)
    }
  }
}

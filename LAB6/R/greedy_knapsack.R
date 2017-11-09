#' @title Greedy Heuristic
#' @description  use the a heuristic or approximation for the problem
#' @param x Data Frame that contain v(value) and w(weight)
#' @param W Vector eith the knapsack size
#' @return a list with the value and elements of the knapsack
#' @refereces \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm} 
#' @examples
#' set.seed(42) 
#' n <- 2000 
#' knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#' 
#' @export 
#' 


greedy_knapsack <- function(x, W){
  
  if(!(is.data.frame(x)) || !(is.numeric(W)) || W<0) stop()
  
  x$values <- x$v/x$w
  sort_values <- x[order(x$values,decreasing=TRUE),]
  
  aux <- sort_values$w[1]
  res <- vector()
  
  i <- 0
  while(aux < W){
    i <- i+1
    aux <- sum(sort_values$w[1:i])
    val <- sum(sort_values$v[1:i])
    res[i] <- rownames(sort_values)[i]
  }
  
  value <- round(val - sort_values$v[i],0)
  elements <- as.numeric(res[1:(i-1)])
  
  result<- list(value=value, elements=elements)

  return(result)
}

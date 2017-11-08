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
  
  values <- x$v/x$w
  n <- nrow(x)
  v <- x$v
  w <- x$w
  W_val <- W
  
  elem <- list()
  elem$value <- 0
  elem$elements <- vector()

  s <- data.frame(values = values)
  s$elements <- c(1:n)
  sort <- s[order(-s$values),]
  

  for(i in 1:n){
    if(w[sort$elements[i]] <= W_val){
      
      elem$value <- elem$value + v[sort$elements[i]]
      elem$elements <- c(elem$elements , sort$elements[i])
      W_val <- W_val - w[sort$elements[i]]
      
    }
  }
  return(elem)
}


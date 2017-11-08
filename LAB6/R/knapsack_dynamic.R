#' @title Dynamic Programming
#' @description  use the dynamic programming to solve the knapsack problem
#' @param x Data Frame that contain v(value) and w(weight)
#' @param W Vector eith the knapsack size
#' @return a list with the value and elements of the knapsack
#' @refereces \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem} 
#' @examples
#' set.seed(42) 
#' n <- 2000 
#' knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#' 
#' @export 
#' 

knapsack_dynamic <- function(x, W){
  
  if(!(is.data.frame(x)) || !(is.numeric(W)) || W<0) stop()
  
  n <- nrow(x)
  v <- x$v
  w <- x$w

  m <- matrix(NA, nrow = n, ncol = W + 1)
  m[1,] <- 0
  
  for(i in 2:n){
    for(j in 1:W){
      if(w[i] > j){
        
        m[i,j] = max(m[i-1,j], 0)
        
      }else{
        
        m[i,j]= max(m[i-1,j], m[i-1, j-w[i]] + v[i])
        
      }
    }
  }
  
  aux <- W
  elem <- list()
  elem$value <- m[n, W]
  
  while(m[n,aux] > 0){
    if(m[n, aux] == m[n-1,aux]){
      n <- n-1
      
    }else{
      elem$elements <- c(elem$elements, n)
      aux <- aux - w[n]
      n <- n-1
    }
  }
  
  elem$elements <- rev(elem$elements)
  return(elem)
}


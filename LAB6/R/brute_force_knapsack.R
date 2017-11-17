#' @title Brute Force Search
#' @description  use the brute force search to solves the problem
#' @param x Data Frame that contain v(value) and w(weight)
#' @param W Vector eith the knapsack size
#' @param parallel initially parallel=FALSE.
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

brute_force_knapsack <- function(x,W,parallel=FALSE){
  
  #stop the program if is true this condition
  if(!(is.data.frame(x)) || !(is.numeric(W)) || W<0) stop()
  
  v <- x$v
  w <- x$w
  n <- nrow(x)
  len <- 1:nrow(x)
  
  
  #if the parallel is TRUE
  if(parallel == TRUE){
    
    detCores <- parallel::detectCores()
    clust <- parallel::makeCluster(spec = detCores)
    
    l <- parallel::parLapply(cl=clust, X = len, fun = function(m){
     
       comb <- combn(rownames(x), m, paste, collapse=" ")
      
      if(all(w > W)){
        condit <- comb[all(w > W)]
        len_condit <- 1:ncol(condit)
        values <- vapply(len_condit, fun = function(i){
          sum(v[condit[i]])
      })
        
        value <- max(values[which(values)])
        elements <- max(condit[which(values)])
    
      }
      result <- list(value = value, elements = elements)
      return(result)
    })
  
    
  #if the parallel is FALSE  
  }else{
    l <- lapply(len,FUN= function(m){
      comb <- combn(len,m)
      weight <- combn(w,m)
      
      #if the condition (colSums(weight)<=W) is true any time
      if(any(colSums(weight) <= W)){
        condit2 <- comb[,colSums(weight)<=W,drop=FALSE]
        
        values <- vapply(1:ncol(condit2), FUN=function(i) {
          sum(v[condit2[,i]])
          }, FUN.VALUE=0)
        
        value <- values[which.max(values)]
        elements <- condit2[,which.max(values)]
      }else{
        value <- 0
        elements <- 0
        
      }
      result <- list(value = value, elements = elements)
      return(result)
    })
    
    res <- sapply(X = len, FUN = function(x){
      l[[x]]$value
      })
    
  }
  
  result <- l[[which.max(res)]]
  return(result)
}

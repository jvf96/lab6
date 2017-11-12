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

brute_force_knapsack <- function(x,W,parallel=FALSE){
  
  #stop the program if is true this condition
  if(!(is.data.frame(x)) || !(is.numeric(W)) || W<0) stop()
  
  v <- x$v
  w <- x$w
  n <- nrow(x)
  len <- 1:n
  
  #we create this function
  brute <- function(m, n=n, v=v, w=w, W=W){
    comb <- combn(1:n,m=m)
    weight <- combn(w,m=m)
    condit <- (colSums(weight)<=W)
    
    #if the condition (colSums(weight)<=W) is true any time
    if(any(condit)){
      condit2 <- comb[,condit,drop=FALSE]
      values <- vapply(1:ncol(condit2), FUN=function(i) {sum(v[condit2[,i]])}, FUN.VALUE=0)
      value <- values[which.max(values)]
      elements <- condit2[,which.max(values)]
    }else{
      value <- 0
      elements <- 0
      
    }
    return(list(value=value,elements=elements))
  }
  
  #if the parallel is TRUE
  if(parallel == TRUE){
    detCores <- parallel::detectCores()
    cl <- parallel::makeCluster(detCores,type="PSOCK")
    options <- parallel::parLapply(cl=cl, len, FUN=brute, n, v, w, W)
    
    res <- parallel::parSapply(cl=cl, len, FUN=function(x, options=options){options[[x]]$value}, options)
    parallel::stopCluster(cl)
    
  }else{
    options <- lapply(len,FUN=brute,n,v,w,W)
    res <- sapply(len,FUN=function(x) options[[x]]$value)
    
  }
  
  result <- options[[which.max(res)]]
  
  return(result)
}

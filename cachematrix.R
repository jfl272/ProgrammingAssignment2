## Project assignment R: creating 2 functions that can cache the invers of an
## inversible matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<- function(x=matrix()){
  
  i<-matrix()
  i<-NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setInv<- function(sol) i <<- sol
  getInv<- function() i
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then cachesolve retrieves the inverse from the cache.

cacheSolve<-function(x,...){
  
  i<-x$getInv()
  
  if(!is.null(i)) {
    
    message("getting cached data")
    return(i)
    
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
  
}

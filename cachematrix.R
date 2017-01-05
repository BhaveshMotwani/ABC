##There are two functions,the makeCachematrix creates a specialized matrix object
##while the cachesolve computes the inverse if matrix is not present in cache
## 

## This function creates a special vector object 
##that has the property of inverse attached to it.

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The cachesolve function checks if the matrix is present in cache
## and calculates the inverse if not.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m 
}

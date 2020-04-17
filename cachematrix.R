
## Compute the inverse of a matrix by returning it from cache memory if it exists,
## if not calculate it


## Upload "matlib" package to use solve() function
library(matlib)


## Create a special "matrix" object has setters & getters functions to use it globally

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse  <- function(inverse ) i <<- inverse 
  getInverse  <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## Compute the inverse of the "matrix" created by "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

## These functions allow the caching of the inverse of a matrix.

## Creates a matrix whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #Sets the matrix to be inverted
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inver) inv <<- inver
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the provided matrix.
#If the inverse is already calculated, it is returned.
#If the inverse is not calculated yet, it is calculated, stored in the cahce and then returned

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

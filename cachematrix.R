## A pair of functions for computing and caching the
## inverse of a matrix

## Creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setInverse <- function (inv) i <<- inv
  getInverse <- function () i
  list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## Computes the inverse of a makeCacheMatrix object

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

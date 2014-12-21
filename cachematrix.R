## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly
## makeCacheMatrix if the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  # x is the origin matrix
  # r is the reversed matrix
  set <- function (y) {
    x <<- y
    r <<- NULL
  }
  get <- function () {
    return (x)
  }
  setreverse <- function (reverser) {
    r <<- reverser
  } 
  getreverse <- function () {
    return (r)
  }
  return (list(set=set, get=get, setreverse=setreverse, getreverse=getreverse))
  
}


## This function computes the inverse of the special "matrix" returned by

cacheSolve <- function(x, ...) {
  r <- x$getreverse()
  if (! is.null(r)) {
    message ("getting the cached data")
    return (r)
  }
  
  data <- x$get()
  r <- solve(data)
  x$setreverse(r)
  return (r)  ## Return a matrix that is the inverse of 'x'
}

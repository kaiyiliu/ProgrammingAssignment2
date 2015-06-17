## The first function creats a special matrix object,
## and the second fucntion caches the inverse of the matrix
## in the form of the first function

## This fucntion creates a special matrix object that can
## cache its inverse. Set and Get inner function provided

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This functions computes the inverse of the matrix returned
## by makeCacheMatrix above. If reverse already calculated and
## has not changed, then it should return the inverse from the cahce

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

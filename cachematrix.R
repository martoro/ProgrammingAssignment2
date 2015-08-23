## Programming assignment 2 in coursera class R programming.
##
## The file contains two functions:
## - one that creates a special matrix that caches its inverse
## - another that computes the inverse of such a matrix

## Create a special matrix that caches its inverse in its closure.
makeCacheMatrix <- function(x = matrix()) {
  ## Cached inverse.
  inv <- NULL
  ## Data setter function.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Data getter function
  get <- function () x
  ## Inverse setter function.
  setinverse <- function(inverse) inv <<- inverse
  ## Inverse getter function.
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Get the inverse of a matrix created by the above function.
## If the inverse is cached, it is returned directly.
## Otherwise, it is computed.
## The function assumes the data in x is a square invertible matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Getting cached inverse.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

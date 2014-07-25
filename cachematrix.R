## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that will cache its inverse, a costly operation
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  list(
    set = function (y) {
      x <<- y
      inv <<- NULL
    },
    get    = function() x,
    setInv = function(new_inv)  inv <<- new_inv,
    getInv = function() inv
  )
}

# Find the inverse of the passed matrix and return it and cache it in the passed param 
# note that x must be a value returned from 'makeCachedMatrix' 
# The following function calculates the inverse of the special matrix
# created with 'makeCachedMatrix' However, it first checks to see if the
# inverse has already been calculated. If so, it gets the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value using the setInv function on x
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    return(inv)
  }
  inv <- solve(x$get())
  x$setInv(inv)
  inv
}

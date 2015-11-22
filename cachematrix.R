## makeCacheMatrix and cacheSolve implement a matrix wrapper holding a cache for its inverse, and a 
## solve function that uses the cached inverse if it's available.

## makeCacheMatrix wraps a matrix and a cache of its inverse after the first time it's calculated.
## the get() and set() functions use their parent function scope to hold their state
##
## usage:  cachedMatrix <- makeCacheMatrix(matrix(c(2,2,3,2),2,2))

makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverse <- NULL
  
  ## store x into the makeCacheMatrix scope (the parent of set()), null out the inverse
  set <- function(y) {
    x <<- y     
    cachedInverse <<- NULL
  }
  
  ## simply gets the wrapped matrix. Now with improved {} for safety.
  get <- function() {x}
  
  ## sets the inverse and makes sure its saved into the parent scope
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  ##gets the cached inverse from the parent scope, returns null if unavailable
  getInverse <- function()  {cachedInverse}
  
  ##pretty print the function listing when you echo just the wrapper object 
  list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)

}


## cacheSolve operates on a cacheable matrix and returns the inverse matrix if the 
## inverse has been cached in the cacheable matrix, otherwise solves for the 
## inverse and stores the inverse matrix into the cacheable matrix for future access.

cacheSolve <- function(x, ...) {
  
  ##get and return the inverse of the cacheable matrix if available
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  ##if unavailable, calculate the inverse and store it into the cacheable matrix's inverse cache.
  data <-x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}

## These functions were created for Programming Assignment 2 in 
## Coursera's R course.
##
## Author: Dan Cogswell


## This function creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(myMatrix = matrix()) {
  invValue <- NULL
  set <- function(y) {
    myMatrix <<- y
    invValue <<- NULL
  }
  get <- function() { myMatrix }
  setInverse <- function(solveValue) { invValue <<- solveValue }
  getInverse <- function() { invValue }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invValue <- x$getInverse()
  if(!is.null(invValue)) {
    message("getting cached data")
    return(invValue)
  }
  myMatrix <- x$get()
  invValue <- solve(myMatrix, ...)
  x$setInverse(invValue)
  invValue
}

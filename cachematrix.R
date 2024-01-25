

# makeCacheMatrix function creates a special "x" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix cache
  inverse <- NULL
  
  # set the matrix function to update the matrix and reset the inverse cache
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  
  # get the matrix function to retrieve the current matrix
  getMatrix <- function() {
    x
  }
  
  # get the inverse function to retrieve the cached inverse, if available
  getInverse <- function() {
    inverse
  }
  
  # set the inverse function to compute and cache the inverse of the matrix
  setInverse <- function() {
    inverse <<- solve(x)
  }
  
  # return a list of the functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse, setInverse = setInverse)
}


# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  # retrieve the cached inverse
  inverse <- x$getInverse()
  
  # if the inverse is not cached, compute it and cache it
  if (is.null(inverse)) {
    cat("Calculating inverse and caching...\n")
    inverse <- solve(x$getMatrix(), ...)
    x$setInverse()
  } else {
    cat("Retrieving cached inverse...\n")
  }
  
  # return the cached or computed inverse
  inverse
}

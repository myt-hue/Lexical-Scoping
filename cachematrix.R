## These functions cache the inverse of a matrix to avoid redundant computations.
## The makeCacheMatrix function creates a special matrix object that stores
## both the matrix and its inverse. The cacheSolve function computes or retrieves
## the cached inverse of the matrix.

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse cache when matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # Return cached inverse if it exists
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Compute the inverse if not cached
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


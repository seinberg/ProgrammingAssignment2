## This file contains functions that:
### 1) lets a user create a caching matrix to cache the inverse of a matrix
### 2) uses the caching matrix to either get the cached inverse or compute and
###    cache the inverse before returning the value of the inverse

## Function to create a special 'matrix' that allows caching of the inverse of
## the underlying matrix.

makeCacheMatrix <- function(matrixToCache = matrix()) {
  inverse <- NULL
  set <- function(matrixToSet) {
    matrixToCache <<- matrixToSet
    inverse <<- NULL
  }
  get <- function() matrixToCache
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
}


## Function that takes a caching matrix, x, and returns the inverse of x
## using the caching matrix's special caching functionality.

cacheSolve <- function(cachingMatrix, ...) {
  inverse <- cachingMatrix$getinverse()

  # If the inverse of this caching matrix has already been set, use it
  if (!is.null(inverse)) {
    message("getting cached data")    
    return(inverse)
  }

  # Otherwise, compute the inverse and cache it
  actualMatrix <- cachingMatrix$get()
  inverse <- solve(actualMatrix, ...)
  cachingMatrix$setinverse(inverse)

  ## Return a matrix that is the inverse of 'x'
  inverse
}

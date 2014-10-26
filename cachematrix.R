## MATRIX SOLVING TOOLS
## Provides procedures to store a matrix and its inverse, saving
## repeated computations for inversion.

## makeCacheMatrix - Stores a matrix and a set of basic procedures that 
##can be performed against it

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(invertedmatrix) inverse <<- invertedmatrix
  getinverse <- function() inverse
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve - Checks for cached inverse to return, computes and caches if 
## it does not already exist in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    ## If cached data exists, fetch it
    message("getting cached inverse")
    return(inverse)
  }
  ## No cached data, so compute inverse and store it
  message("computing inverse - no cached inverse available")
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
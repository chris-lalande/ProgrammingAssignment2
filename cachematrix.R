## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) cm <<- solve
  getmatrix <- function() cm
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cm <- x$getmatrix()
    if(!is.null(cm)) {
      message("getting cached data")
      return(cm)
    }
    data <- x$get()
    cm <- solve(data, ...)
    x$setmatrix(cm)
    cm
}

## makeCacheMatrix implements a list of functions for 
## associating a matrix (set/get) and for storing or retrieving
## (setsolve/getsolve) a cached version of the inverse of 
## the associated matrix (x).

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(slv) s <<- slv
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## cachesolve returns the inverse of the matrix that has
## earlier is assiociated to the list of functions of makeCacheMatrix.
## It is either computed or delivered from cache (if available).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the one associated with 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve (data, ...)
  x$setsolve(s)
  s
}

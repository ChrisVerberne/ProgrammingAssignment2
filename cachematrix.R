## makeCacheMatrix implements population of the matrix
## and computation of the inverted matrix 
## as well as the retrieval of it from its cache

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

## cachesolve returns the inverted matrix that has
## earlier been created with makeCacheMatrix.
## It is either computed or delivered from cache,
## this is not notable for the end-user.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

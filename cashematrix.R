## These functions provide the ability to store a matrix and/or it's inverse in a cached variable x.

## This function sets/gets a square matrix and provides the functionality to retrieve or set it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(theInverse) m <<- theInverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Checks to see whether an inverse version of the stored matrix has been stored in x.
## If it has been calculated and stored the inverse matrix is returned.
## If not, an inverse is calculated and stored in the cached variable x.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
 
   m
}

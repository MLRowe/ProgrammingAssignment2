## Functions to obtain the inverse of a matrix. Stores previous results in the cache to save time for
## for future computations.
## Matt Rowe 12/27/2015

## Function that creates a list of functions that:
# 1) sets the matrix
# 2) gets the matrix
# 3) sets the inverse
# 4) gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will check for previous inverse computations using the above functions or
## calculate the inverse if there are no cached results

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

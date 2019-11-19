## These functions are used for caching the inverse of a matrix
##
## Use:
## > m <- matrix(c(2,4,-3,-7),nrow=2,ncol=2)
## > mvec <- makeCacheMatrix(m)
## > minv <- cacheSolve(mvec)
## > print(minv)
##     [,1] [,2]
## [1,]  3.5 -1.5
## [2,]  2.0 -1.0
## > amean <- cachemean(avec)
## getting cached data

## This function creates a list of functions to get/set a matrix and get/set the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This functions solves for the matrix inverse of x and caches the result
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
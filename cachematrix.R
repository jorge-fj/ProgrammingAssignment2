## Put comments here that give an overall description of what your
## functions do

## With makeCacheMatrix what is done is function which creates a matrix that 
## actually is a list that contains a function for:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix created with 
## makeCacheMatrix function, but if the inverse was already calculated, 
## it gets the inverse from cache.

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}

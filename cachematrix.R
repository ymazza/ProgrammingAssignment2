## This program is comprised of 2 functions makeCacheMatrix and cacheSolve.

## makeCacheMatrix is a function that creates a 'special matrix' object that can cache its inverse.
## initialize object 'x' as matrix in function
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function ()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <-function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## cacheSolve is a function that computes the inverse of the 'special matrix' object returned by
## the function makeCacheMatrix. if the inverse has already been calculated(and there are no
## changes to the matrix) then cacheSolve retrieves the inverse from the cache.
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...){
  i <- x$getInverse()
  if(!is.null(i)) {
  message("getting cached data")
  return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
